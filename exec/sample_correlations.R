suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rcrispr))

###############################################################################
#* --                                                                     -- *#
#* --                             OPTIONS                                 -- *#
#* --                                                                     -- *#
###############################################################################

option_list = c(
  count_path_options(),
  sample_metadata_options(),
  correlation_options(),
  basic_outfile_options(),
  shared_output_options(),
  count_format_options(),
  sample_metadata_format_options(),
  count_column_index_options(),
  sample_metadata_sample_filename_column_index_options(),
  sample_metadata_sample_label_column_index_options(),
  sample_metadata_sample_type_column_index_options(),
  sample_metadata_sample_group_column_index_options(),
  sample_metadata_sample_read_count_column_index_options()
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
for (n in c('counts')) {
  check_option(n, opt[[n]])
}

if (is.null(opt$outfile))
  opt$outfile <- 'raw_count_qc_summary.tsv'

# Convert column indices to integers
low_counts <- opt$low_counts
for (i in c('low_counts')) {
  if (!is.null(get(i)))
    assign(i, convert_variable_to_integer(get(i)))
}

###############################################################################
#* --                                                                     -- *#
#* --                       MAIN SCRIPT - STATISTICS                      -- *#
#* --                                                                     -- *#
###############################################################################

# Read in sample matrix
message("Reading sample matrix...")
sample_matrix <- read_count_matrix_file(
  filepath = opt$counts,
  id_column = opt$count_id_column_index,
  gene_column = opt$count_gene_column_index,
  count_column = opt$count_count_column_index,
  file_separator = opt$counts_delim,
  file_header = ifelse(opt$no_counts_header,FALSE,TRUE),
  processed = T
)

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
    reads_column = opt$info_reads_column_index,
    group_column =  opt$info_group_column_index,
    file_separator = opt$info_delim,
    file_header = ifelse(opt$no_info_header,FALSE,TRUE),
    check.names = FALSE
  )
sample_metadata <- get_sample_metadata(sample_metadata_object, processed = T)

# Calculate sample correlations
sample_correlation_matrix <- cor(sample_matrix %>% select(-sgRNA, -gene), method = opt$cor_method)




