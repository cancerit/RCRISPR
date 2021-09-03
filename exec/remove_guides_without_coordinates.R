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
  remove_no_coordinate_guide_options(),
  count_library_outfile_options(),
  shared_output_options(),
  count_format_options(),
  count_column_index_options(),
  library_annotation_format_options(),
  library_annotation_column_index_options(),
  library_annotation_genomic_column_index_options()
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
for (n in c('counts', 'library')) {
  check_option(n, opt[[n]])
}

# Set default output filenames
if (is.null(opt$excluded_guides_outfile))
  opt$excluded_guides_outfile <- 'excluded_guides.tsv'
if (is.null(opt$count_matrix_outfile))
  opt$count_matrix_outfile <- 'count_matrix.coord_filtered.tsv'
if (is.null(opt$library_outfile))
  opt$library_outfile <- 'library.coord_filtered.tsv'

###############################################################################
#* --                                                                     -- *#
#* --                          MAIN SCRIPT                                -- *#
#* --                                                                     -- *#
###############################################################################

# Read in library
message("Reading library annotation...")
library_annotations_object <-
  read_library_annotation_file(
    filepath = opt$library,
    id_column = opt$library_id_column_index,
    gene_column = opt$library_gene_column_index,
    chr_column = opt$library_chr_column_index,
    chr_start_column = opt$library_start_column_index,
    chr_end_column = opt$library_end_column_index,
    file_separator = opt$library_delim,
    file_header = ifelse(opt$no_library_header,FALSE,TRUE),
    check.names = FALSE
  )
library_annotations <- get_library_annotations(library_annotations_object)

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

# Identify guides which have no coordinates in library
message("Identifying guides with no coordinates in library...")
excluded_guides <- get_guides_with_no_coordinates(library_annotations_object)
num_guides_excluded <- length(excluded_guides)
total_guides <- nrow(library_annotations)
pct_guides_excluded <- round((num_guides_excluded / total_guides) * 100, 2)
message(paste0("Number of guides with no coordinates to exclude: ",
              num_guides_excluded, " of ", total_guides, " (", pct_guides_excluded, "%)"))

# Removing guides from counts and library
count_matrix_with_guides_removed <- sample_count_matrix
library_with_guides_removed <- library_annotations
if (length(excluded_guides) > 0) {
  message("Removing guides with no coordinates from library...")
  library_with_guides_removed <- get_library_annotations(
                                  remove_guides_from_library_annotations_object(library_annotations_object,
                                                                                guides_to_remove = excluded_guides))
  message("Removing guides with no coordinates from counts...")
  count_matrix_with_guides_removed <- remove_guides_from_count_matrix(sample_count_matrix,
                                                                      id_column = count_id_column_index,
                                                                      guides_to_remove = excluded_guides)
}

# Build dummy processed library annotation object
library_with_guides_removed_object <- new(
        "LibraryAnnotations",
        filepath = opt$library,
        id_column = opt$library_id_column_index,
        gene_column = opt$library_gene_column_index,
        annotations = library_with_guides_removed
      )

# Compare sgRNA IDs and gene names between count matrix and library
message("Comparing processed count matrix to processed library...")
mat_lib_match <- compare_count_matrix_to_library(
  count_matrix = count_matrix_with_guides_removed,
  id_column = 1,
  gene_column = 2,
  library_annotation_object = library_with_guides_removed_object)


# Write excluded guides to file
message("Writing excluded guides to file...")
outfile <- write_dataframe_to_file(data = excluded_guides,
                                   outfile = opt$excluded_guides_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t",
                                   col.names = FALSE,
                                   ignore_empty = TRUE)
message(paste("Excluded guides written to:", outfile))

# Write processed count matrix to file
message("Writing count matrix to file...")
outfile <- write_dataframe_to_file(data = count_matrix_with_guides_removed,
                                   outfile = opt$count_matrix_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Count matrix written to:", outfile))

# Write processed library to file
message("Writing library to file...")
outfile <- write_dataframe_to_file(data = library_with_guides_removed,
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
                                                   count_matrix_with_guides_removed,
                                                   library_with_guides_removed,
                                                   excluded_guides))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")
