suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(rcrispr))

###############################################################################
#* --                                                                     -- *#
#* --                             OPTIONS                                 -- *#
#* --                                                                     -- *#
###############################################################################

option_list = c(
  basic_infile_options(),
  sample_metadata_options(),
  intermediate_qc_options(),
  bagel_gene_infile_options(),
  basic_outfile_options(),
  shared_output_options(),
  infile_format_options(),
  sample_metadata_format_options(),
  infile_column_index_options(),
  sample_metadata_sample_filename_column_index_options(),
  sample_metadata_sample_label_column_index_options(),
  sample_metadata_sample_type_column_index_options(),
  sample_metadata_sample_group_column_index_options()
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
for (n in c('infile', 'info', 'infile_id_column_index', 'ess', 'noness')) {
  check_option(n, opt[[n]])
}

# Set is_gene to true if option used
is_gene = FALSE
if (opt$is_gene) {
  is_gene = TRUE
}

# Set is_lfc to true if option used
is_fc = FALSE
if (opt$is_fc) {
  is_fc = TRUE
}

# If is_gene is FALSE expect the infile_gene_column_index to be set
if (!is_gene && is.null(opt$infile_gene_column_index))
  stop("When is_gene option is used, infile_gene_column_index is required.")

# If is_gene is TRUE and is_lfc is FALSE
if (is_gene && !is_fc)
  stop("When is_gene option is used, is_fc option must also be used.")

# Set default stats file name
if (is.null(opt$outfile))
  opt$outfile <- 'bagel_classification_summary.tsv'

###############################################################################
#* --                                                                     -- *#
#* --                       MAIN SCRIPT - STATISTICS                      -- *#
#* --                                                                     -- *#
###############################################################################

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

# Read in sample data matrix
# NOTE: although options say counts, this can be either a count or lfc matrix (where count column should be set to lfc columns)
if (is_fc) {
  message("Reading sample fold change matrix...")
  sample_data <- read_fold_change_matrix_file(
    filepath = opt$infile,
    id_column = opt$infile_id_column_index,
    gene_column = opt$infile_gene_column_index,
    fc_column = opt$infile_data_column_index,
    file_separator = opt$infile_delim,
    file_header = ifelse(opt$no_infile_header, FALSE, TRUE),
    is_gene = is_gene,
    processed = TRUE,
    check.names = FALSE
  )
} else {
  message("Reading sample count matrix...")
    sample_data <- read_count_matrix_file(
      filepath = opt$infile,
      id_column = opt$infile_id_column_index,
      gene_column = opt$infile_gene_column_index,
      count_column = opt$infile_data_column_index,
      file_separator = opt$infile_delim,
      file_header = ifelse(opt$no_infile_header, FALSE, TRUE),
      processed = TRUE,
      check.names = FALSE
    )
}

# Read in sample metadata (optional)
message("Reading sample metadata...")
sample_metadata_object <-
  read_sample_metadata_file(
    filepath = opt$info,
    filename_column = opt$info_filename_column_index,
    label_column = opt$info_label_column_index,
    plasmid_column = opt$info_plasmid_column_index,
    control_column = opt$info_control_column_index,
    treatment_column = opt$info_treatment_column_index,
    group_column =  opt$info_group_column_index,
    file_separator = opt$info_delim,
    file_header = ifelse(opt$no_info_header,FALSE,TRUE),
    check.names = FALSE
  )
sample_metadata <- get_sample_metadata(sample_metadata_object, processed = TRUE)

# Get ordered sample labels
ordered_sample_metadata <- sample_metadata %>%
  arrange(desc(plasmid), desc(control), desc(treatment), desc(label))

# Set sample columns
if (!is_gene) {
  sample_columns = 3:ncol(sample_data)
} else {
  sample_columns = 2:ncol(sample_data)
}

# If --check_names is used, check that count/lfc sample names exist in the sample metadata
if (!opt$no_check_names & !is_gene) {
  message("Comparing matrix column names to sample metadata labels...")
  if (!is_gene) {
    sample_name_check <- compare_matrix_to_sample_metadata(
      data = sample_data,
      id_column = 1,
      gene_column = 2,
      sample_columns = sample_columns,
      sample_metadata_object = sample_metadata_object
    )
  } else {
    sample_name_check <- compare_matrix_to_sample_metadata(
      data = sample_data,
      id_column = NULL,
      gene_column = 1,
      sample_columns = sample_columns,
      sample_metadata_object = sample_metadata_object
    )
  }
  # No need to add another check here, if the names mismatch the function will error out
}

# Add BAGEL classifications
sample_data <- add_bagel_classifications(data = sample_data,
                                          gene_column = which(colnames(sample_data) == 'gene'),
                                          ess = ess[,1],
                                          noness = noness[,1])

# Collapse data for stats and plots
message("Collapsing data...")
if (!is_gene) {
  sample_data_narrow <- sample_data %>%
    gather(sample, values, -sgRNA, -gene, -classification)
} else {
  sample_data_narrow <- sample_data %>%
    gather(sample, values, -gene, -classification)
}

# Build BAGEL statistics
sample_data_statistics <- get_bagel_statistics(data = sample_data_narrow,
                                               is_fc = is_fc,
                                               is_gene = is_gene)

# Write raw count statistics to output file
message("Writing BAGEL classification statistics to file...")
outfile <- write_dataframe_to_file(data = sample_data_statistics,
                                   outfile = opt$outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("BAGEL classification statistics written to:", outfile))

###############################################################################
#* --                                                                     -- *#
#* --                       MAIN SCRIPT - PLOTS                           -- *#
#* --                                                                     -- *#
###############################################################################

# Get number of samples
num_sample_columns <- length(process_column_indices(sample_columns))

# Set up empty plot filename vector
plot_files <- NULL

# Were any BAGEL essential and non-essential genes found
bagel_genes_identified <- sample_data %>%
  filter(classification != 'unknown') %>%
  nrow()
found_bagel_genes <- ifelse(bagel_genes_identified > 0, TRUE, FALSE)
if (opt$no_plot || !found_bagel_genes) {
  if (!found_bagel_genes)
    message("Cannot generate plots, no BAGEL genes were idenitfied in dataset.")
  # Message when --no_plot
  message("Plots are disabled.")
} else {
  message("Generating plots...")
  # Generate list of plots for saving
  plot_list <- list()

  # Check that the union of sample_order and sample is the right length
  message("Ordering sample names...")
  if (length(intersect(ordered_sample_metadata$label, sample_data_narrow$sample)) == 0) {
    message("Will not apply sample order, sample names do not exist in metadata.")
  } else {
    sample_data_narrow$sample <- factor(sample_data_narrow$sample, levels = intersect(ordered_sample_metadata$label, sample_data_narrow$sample))
  }

  # Ridgeline density plot of count or lfc densities
  message("Generating ridgeline density plot...")
  density_plot_name <- ifelse(is_fc, 'fold_change.density', 'count_matrix.density')
  density_plot_name <- ifelse(is_gene, paste0('gene.', density_plot_name), paste0('sgrna.', density_plot_name))
  plot_list[[density_plot_name]] <- plot_common_density_ridges(sample_data_narrow %>%
                                                                 filter(classification != 'unknown') %>%
                                                                 rename('group' = 'classification'),
                                                                xcol = 'values',
                                                                xlab = case_when(is_fc & is_gene ~ 'Gene fold changes',
                                                                                 is_fc & !is_gene ~ 'sgRNA fold changes',
                                                                                 !is_fc & is_gene ~ 'Gene counts',
                                                                                 !is_fc & !is_gene ~ 'sgRNA counts',
                                                                                 TRUE ~ 'Density'))
  # Plot and save correlation plot separately
  if (num_sample_columns < 2) {
    message("Cannot generate correlation plot as there are less than 2 sample columns.")
    correlation_plot <- NULL
    correlation_plot_file <- NULL
  } else {
    # Correlation plot
    message("Generating correlation plot...")
    correlation_plot_name <- ifelse(is_fc, 'fold_change.correlation', 'count_matrix.correlation')
    correlation_plot_name <- ifelse(is_gene, paste0('gene.', correlation_plot_name), paste0('sgrna.', correlation_plot_name))
    correlation_plot <- plot_correlation(df = sample_data %>% filter(classification != 'unknown'),
                                         cor_columns = c(sample_columns, ncol(sample_data)),
                                         group_column = which(colnames(sample_data) == 'classification'))

    message("Saving correlation plot...")
    correlation_plot_file <- save_plot_with_ggsave(
                                data = correlation_plot,
                                outfile = paste0(correlation_plot_name, '.png'),
                                outdir = opt$outdir,
                                prefix = opt$prefix,
                                suffix = opt$suffix,
                                dpi = 400)
  }

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
                                                   sample_metadata_object,
                                                   sample_metadata,
                                                   sample_data_narrow,
                                                   sample_data_statistics,
                                                   correlation_plot,
                                                   correlation_plot_file,
                                                   plot_list,
                                                   plot_files))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")
