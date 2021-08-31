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
  remove_guide_options(),
  count_library_outfile_options(),
  shared_output_options(),
  count_format_options(),
  count_column_index_options(),
  library_annotation_format_options(),
  library_annotation_column_index_options()
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
for (n in c('counts', 'library', 'guides_to_remove')) {
  check_option(n, opt[[n]])
}

# Set default output filenames
if (is.null(opt$duplicate_guides_outfile))
  opt$duplicate_guides_outfile <- 'duplicate_guides.tsv'
if (is.null(opt$count_matrix_outfile))
  opt$count_matrix_outfile <- 'count_matrix.duplicate_guides_removed.tsv'
if (is.null(opt$library_outfile))
  opt$library_outfile <- 'library.duplicate_guides_removed.tsv'


###############################################################################
#* --                                                                     -- *#
#* --                          MAIN SCRIPT                                -- *#
#* --                                                                     -- *#
###############################################################################

# Read in guides to remove
guides_to_remove <- read_file_to_dataframe(opt$guides_to_remove, file_header = F)

# Read in library
message("Reading library annotation...")
library_annotations_object <-
  read_library_annotation_file(
    filepath = opt$library,
    id_column = opt$library_id_column_index,
    gene_column = opt$library_gene_column_index,
    file_separator = opt$library_delim,
    file_header = ifelse(opt$no_library_header,FALSE,TRUE),
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
  processed = T,
  check.names = FALSE
)

# Compare sgRNA IDs and gene names between count matrix and library
message("Comparing count matrix to library...")
mat_lib_match <- compare_count_matrix_to_library(
  count_matrix = sample_count_matrix,
  id_column = 1,
  gene_column = 2,
  library_annotation_object = library_annotations_object)


# Remove guides from counts
message("Removing guides from counts...")
processed_count_matrix <- remove_guides_from_count_matrix(count_matrix = sample_count_matrix,
                                                          id_column = 1,
                                                          guides_to_remove = guides_to_remove)

# Remove guides from library
message("Removing guides from library...")
processed_library <- remove_guides_from_library_annotations_object(
                        library_annotations_object = library_annotations_object,
                        guides_to_remove = guides_to_remove)
message(paste("Guides removed:", length(guides_to_remove), 'of', nrow(sample_count_matrix)))

# Compare sgRNA IDs and gene names between count matrix and library
message("Comparing processed count matrix to processed library...")
mat_lib_match <- compare_count_matrix_to_library(
  count_matrix = processed_count_matrix,
  id_column = 1,
  gene_column = 2,
  library_annotation_object = processed_library)

# Write processed count matrix to file
message("Writing count matrix to file...")
outfile <- write_dataframe_to_file(data = processed_count_matrix,
                                   outfile = opt$count_matrix_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Count matrix written to:", outfile))

# Write processed library to file
message("Writing library to file...")
outfile <- write_dataframe_to_file(data = get_library_annotations(processed_library),
                                   outfile = opt$library_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Library annotations written to:", outfile))

# Write Rdata to file
if (!is.null(opt$rdata)) {
  message("Writing R data to file...")
  rdata_outfile <- write_rdata_to_file(prefix = opt$prefix,
                                       outfile = opt$rdata,
                                       outdir = opt$outdir,
                                       data = list(sample_count_matrix,
                                                   library_annotations_object,
                                                   guides_to_remove,
                                                   processed_count_matrix,
                                                   processed_library))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")






