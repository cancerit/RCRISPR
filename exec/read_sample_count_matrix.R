suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(rcrispr))

###############################################################################
#* --                                                                     -- *#
#* --                             OPTIONS                                 -- *#
#* --                                                                     -- *#
###############################################################################

option_list = c(
  count_path_options(),
  library_annotation_options(),
  sample_metadata_options(),
  basic_outfile_options(),
  shared_output_options(),
  count_format_options(),
  count_column_index_options(),
  library_annotation_format_options(),
  library_annotation_column_index_options(),
  sample_metadata_format_options(),
  sample_metadata_sample_filename_column_index_options(),
  sample_metadata_sample_label_column_index_options(),
  sample_metadata_sample_type_column_index_options()
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
for (n in c('counts', 'library', 'info')) {
  check_option(n, opt[[n]])
}

###############################################################################
#* --                                                                     -- *#
#* --                          MAIN SCRIPT                                -- *#
#* --                                                                     -- *#
###############################################################################

# Read in library
message("Reading library annotation...")
library_annotation_object <-
  read_library_annotation_file(
    filepath = opt$library,
    id_column = opt$library_id_column_index,
    gene_column = opt$library_gene_column_index,
    file_separator = opt$library_delim,
    file_header = ifelse(opt$no_library_header,FALSE,TRUE),
    check.names = FALSE
  )

# Read in sample metadata
message("Reading sample metadata...")
sample_metadata_object <-
  read_sample_metadata_file(
    filepath = opt$info,
    filename_column = opt$info_filename_column_index,
    label_column = opt$info_label_column_index,
    plasmid_column = opt$info_plasmid_column_index,
    control_column = opt$info_control_column_index,
    treatment_column = opt$info_treatment_column_index,
    file_separator = opt$info_delim,
    file_header = ifelse(opt$no_info_header,FALSE,TRUE),
    check.names = FALSE
  )

# Read in sample count matrix
message("Reading count matrix...")
sample_count_matrix <- read_count_matrix_file(
  filepath = opt$counts,
  id_column = opt$count_id_column_index,
  gene_column = opt$count_gene_column_index,
  count_column = opt$count_count_column_index,
  file_separator = opt$counts_delim,
  file_header = ifelse(opt$no_counts_header,FALSE,TRUE),
  processed = T
)

# Compare sgRNA IDs and gene names between count matrix and library
message("Comparing count matrix to library...")
mat_lib_match <- compare_count_matrix_to_library(
                  count_matrix = sample_count_matrix,
                  id_column = 1,
                  gene_column = 2,
                  library_annotation_object = library_annotation_object)

# Reorder matrix using sample types in sample metadata
message("Reordering count matrix...")
sample_count_matrix <- reorder_count_matrix_by_sample_type(sample_count_matrix,
                                                           sample_metadata_object)

# Write count matrix to output file
message("Writing count matrix to file...")
outfile <- write_dataframe_to_file(data = sample_count_matrix,
                                   outfile = opt$outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Count matrix written to:", outfile))

# Write processed data to .Rdata
if (!is.null(opt$rdata)) {
  message("Writing R data to file...")
  rdata_outfile <- write_rdata_to_file(prefix = opt$prefix,
                                       outfile = opt$rdata,
                                       outdir = opt$outdir,
                                       data = list(library_annotation_object,
                                                   sample_metadata_object,
                                                   sample_count_matrix))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")
