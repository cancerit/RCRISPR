suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(rcrispr))

###############################################################################
#* --                                                                     -- *#
#* --                             OPTIONS                                 -- *#
#* --                                                                     -- *#
###############################################################################

option_list = c(
  count_path_options(),
  calculate_lfc_options(),
  shared_output_options(),
  count_format_options(),
  count_column_index_options()
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
for (n in c('counts', 'pseudocount', 'control_indices', 'treatment_indices')) {
  check_option(n, opt[[n]])
}

# Set default output filenames
if (is.null(opt$sgrna_outfile))
  opt$sgrna_outfile <- 'fold_change_matrix.sgrna.tsv'
if (is.null(opt$gene_outfile))
  opt$gene_outfile <- 'fold_change_matrix.gene.tsv'
if (is.null(opt$count_matrix_outfile))
  opt$count_matrix_outfile <- 'count_matrix.lfc.tsv'

###############################################################################
#* --                                                                     -- *#
#* --                          MAIN SCRIPT                                -- *#
#* --                                                                     -- *#
###############################################################################

# Read in sample count matrix
message("Reading count matrix...")
sample_count_matrix <- read_count_matrix_file(
  filepath = opt$counts,
  id_column = opt$count_id_column_index,
  gene_column = opt$count_gene_column_index,
  count_column = opt$count_count_column_index,
  file_separator = opt$counts_delim,
  file_header = ifelse(opt$no_counts_header,FALSE,TRUE),
  processed = TRUE,
  check.names = FALSE
)

# Calculate sgRNA-level log fold changes
message("Calculating sgRNA LFCs...")
sgrna_lfc <- calculate_lfc(sample_count_matrix,
                           id_column = opt$count_id_column_index,
                           gene_column = opt$count_gene_column_index,
                           control_indices = opt$control_indices,
                           treatment_indices = opt$treatment_indices,
                           pseudocount =  convert_variable_to_numeric(opt$pseudocount))

# Calculate gene-level log fold changes
message("Calculating gene LFCs...")
gene_lfc <- calculate_gene_lfc(sgrna_lfc, id_column = 1, gene_column = 2)

# Write sgRNA-level log fold changes to file
message("Writing sgRNA LFCs to file...")
outfile <- write_dataframe_to_file(data = sgrna_lfc,
                                   outfile = opt$sgrna_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("sgRNA LFCs written to:", outfile))


# Write gene-level log fold changes to file
message("Writing gene LFCs to file...")
outfile <- write_dataframe_to_file(data = gene_lfc,
                                   outfile = opt$gene_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Gene LFCs written to:", outfile))

# Write subset count matrix to file
message("Writing count matrix to file...")
control_indices <- process_column_indices(opt$control_indices)
treatment_indices <- process_column_indices(opt$treatment_indices)
subset_sample_count_matrix <- sample_count_matrix[,c(1:2, control_indices, treatment_indices)]
outfile <- write_dataframe_to_file(data = subset_sample_count_matrix,
                                   outfile = opt$count_matrix_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Count matrix written to:", outfile))

# Write Rdata to file
if (!is.null(opt$rdata)) {
  message("Writing R data to file...")
  rdata_outfile <- write_rdata_to_file(prefix = opt$prefix,
                                       suffix = opt$suffix,
                                       outfile = opt$rdata,
                                       outdir = opt$outdir,
                                       data = list(sample_count_matrix,
                                                   sgrna_lfc,
                                                   gene_lfc))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")
