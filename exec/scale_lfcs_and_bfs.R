suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(pROC))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(rcrispr))

###############################################################################
#* --                                                                     -- *#
#* --                             OPTIONS                                 -- *#
#* --                                                                     -- *#
###############################################################################

option_list = c(
  basic_infile_options(),
  scaling_options(),
  bagel_gene_infile_options(),
  shared_output_options(),
  infile_format_options(),
  infile_column_index_options()
)

opt_parser <- OptionParser(option_list = option_list)
opt <- tryCatch({
  parse_args(opt_parser)
}, error = function(e) {
  # Stop if there is an error
  stop(paste("Cannot parse arguments (error):", e, sep = "\n"))
}, warning = function(w) {
  # Stop if there is a warning
  stop(paste("Cannot parse arguments (warning):", w, sep = "\n"))
})

###############################################################################
#* --                                                                     -- *#
#* --                          OPTION VALIDATION                          -- *#
#* --                                                                     -- *#
###############################################################################

# Check whether required fields are provided
for (n in c('infile', 'infile_gene_column_index', 'infile_data_column_index',
            'ess', 'noness', 'threshold')) {
  check_option(n, opt[[n]])
}

# Set is_lfc to true if option used
is_fc = FALSE
if (opt$is_fc) {
  is_fc = TRUE
}

# Set is_bf to true if option used
is_bf = FALSE
if (opt$is_bf) {
  is_bf = TRUE
}

# Error if is_bf and is_fc are both set
if (is_fc && is_bf)
  stop("Cannot scale data, only one of is_fc and is_bf can be used.")

# Check that infile_gene_column_index has been set
if (is.null(opt$infile_gene_column_index))
  stop("Cannot scale data, infile_gene_column_index is required.")

# Check that infile_data_column_index has been set
if (is.null(opt$infile_data_column_index))
  stop("Cannot scale data, infile_data_column_index is required.")

###############################################################################
#* --                                                                     -- *#
#* --                             MAIN SCRIPT                             -- *#
#* --                                                                     -- *#
###############################################################################

# Read in non-essential genes
message("Reading input file...")
sample_data <- read_file_to_dataframe(filepath = opt$infile,
                                      file_separator = opt$infile_delim,
                                      file_header = ifelse(opt$no_infile_header, FALSE, TRUE),
                                      column_indices = c(opt$infile_gene_column_index,
                                                         opt$infile_data_column_index),
                                      check.names = FALSE)

# Read in essential genes
message("Reading essential gene file...")
ess <- read_file_to_dataframe(filepath = opt$ess,
                              file_separator = opt$ess_delim,
                              file_header = ifelse(opt$no_ess_header, FALSE, TRUE),
                              column_indices = opt$ess_gene_column_index)

# Read in non-essential genes
message("Reading non-essential gene file...")
noness <- read_file_to_dataframe(filepath = opt$noness,
                                 file_separator = opt$noness_delim,
                                 file_header = ifelse(opt$no_noness_header, FALSE, TRUE),
                                 column_indices = opt$noness_gene_column_index)

# Average across replicates and set gene column as rownames
if (ncol(sample_data) > 2) {
  message("Averaging replicates...")
  avg_sample_data <- average_replicates(data = sample_data,
                                        gene_column = 1,
                                        data_columns = 2:(ncol(sample_data)))
} else {
  message("Only one data column, skipping averaging replicates.")
  avg_sample_data <- sample_data
}

# Add BAGEL classifications
message("Adding BAGEL classifications...")
avg_sample_data <- add_bagel_classifications(avg_sample_data,
                                             gene_column = 1, # Assume is first column based on column_indices
                                             ess = ess[,1],
                                             noness = noness[,1])

# Prepare essentiality data
message("Preparing essentiality data...")
essentiality_data <- prepare_essentiality_data(data = avg_sample_data,
                                               gene_column = 1,
                                               data_column = 2,
                                               classification_column = 3,
                                               threshold = opt$threshold)

# Get ROC
message("Preparing ROC...")
screen_roc <- tryCatch({
                suppressMessages(roc(essentiality_data[['essentiality']],
                                     essentiality_data[['modified_predictor']]))
              }, error = function(e) {
                # Stop if there is an error
                stop(paste("Cannot get ROC:", e))
              })
screen_roc.coords <- coords(screen_roc,
                            ret = c( 'all' ),
                            transpose = T)

# Process ROC
message("Processing ROC...")
perfTH <- process_roc(screen_roc,
                      essentiality_data[['min']])

# Write thresholds to output file
message("Writing thresholds to file...")
outfile <- write_dataframe_to_file(data = perfTH,
                                   outfile = 'ROC_summary.tsv',
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Thresholds written to:", outfile))

# Scaled data
message("Scaling data...")
avg_sample_mat <- matrix(avg_sample_data[,2])
scaled_data <- matrix(avg_sample_mat - (perfTH[[1]][1]),
                      nrow(avg_sample_data),
                      1,
                      dimnames = list(avg_sample_data[,1], ifelse(is_fc, 'LFC', 'BF')))
scaled_data <- scaled_data %>%
  as.data.frame() %>%
  rownames_to_column('gene')

# Write thresholds to output file
message("Writing scaled data to file...")
outfile <- write_dataframe_to_file(data = scaled_data,
                                   outfile = ifelse(is_fc, 'fold_change.scaled.gene.tsv', 'BF.scaled.gene.tsv'),
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Scaled data written to:", outfile))

# Scaled depletion matrix
message("Preparing scaled depletion matrix...")
if (is_fc) {
  scaled_depletion_matrix <- scaled_data %>%
    mutate(is_depleted = ifelse(LFC < 0 , 1, 0)) %>%
    select(-LFC)
} else {
  scaled_depletion_matrix <- scaled_data %>%
      mutate(is_depleted = ifelse(BF > 0 , 1, 0)) %>%
      select(-BF)
}
message(paste("Number of depleted genes:", sum(scaled_depletion_matrix$is_depleted), 'of', nrow(scaled_depletion_matrix)))

# Write depletion matrix to output file
message("Writing depletion matrix to file...")
outfile <- write_dataframe_to_file(data = scaled_depletion_matrix,
                                   outfile = ifelse(is_fc, 'fold_change.scaled_depletions_matrix.gene.tsv', 'BF.scaled_depletions_matrix.gene.tsv'),
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Scaled depletion matrix written to:", outfile))

###############################################################################
#* --                                                                     -- *#
#* --                       MAIN SCRIPT - PLOTS                           -- *#
#* --                                                                     -- *#
###############################################################################

# Set up empty plot filename vector
plot_files <- NULL
plot_list <- plot_list <- list()

if (!opt$no_plot) {
  # Plot ROC
  message("Generating ROC plot...")
  roc_plot_name <- ifelse(is_fc, 'fold_change.gene.ROC', 'BF.gene.ROC')
  plot_list[[roc_plot_name]] <- plot_roc(screen_roc)

  # Save plots
  message("Saving plots...")
  plot_files <- save_plot_list(plot_list = plot_list,
                               outdir = opt$outdir,
                               prefix = opt$prefix,
                               suffix = opt$suffix,
                               dpi = 300)
}

# Write processed data to .Rdata
if (!is.null(opt$rdata)) {
  message("Writing R data to file...")
  rdata_outfile <- write_rdata_to_file(prefix = opt$prefix,
                                       suffix = opt$suffix,
                                       outfile = opt$rdata,
                                       outdir = opt$outdir,
                                       data = list(sample_data,
                                                   ess,
                                                   noness,
                                                   avg_sample_data,
                                                   essentiality_data,
                                                   screen_roc,
                                                   scaled_data,
                                                   scaled_depletion_matrix,
                                                   plot_files,
                                                   plot_list))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")
