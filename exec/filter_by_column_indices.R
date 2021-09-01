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
  filter_by_index_options(),
  count_library_outfile_options(),
  filtered_guide_output_options(),
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
for (n in c('counts', 'library', 'filter_indices', 'filter_method', 'min_reads')) {
  check_option(n, opt[[n]])
}

# Set default output filenames
if (is.null(opt$filtered_guides_outfile))
  opt$filtered_guides_outfile <- 'filtered_guides.tsv'
if (is.null(opt$count_matrix_outfile))
  opt$count_matrix_outfile <- 'count_matrix.filtered_guides_removed.tsv'
if (is.null(opt$library_outfile))
  opt$library_outfile <- 'library.filtered_guides_removed.tsv'


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

# Identify guides that need to be filtered
message("Identifying guides failing filter...")
filtered_guides <- get_guides_failing_filter(sample_count_matrix,
                                             id_column = opt$count_id_column_index,
                                             count_column = opt$count_count_column_index,
                                             filter_indices = opt$filter_indices,
                                             filter_method = opt$filter_method,
                                             min_reads = opt$min_reads)
num_guides_filtered <- length(filtered_guides)
total_guides <- nrow(library_annotations)
pct_guides_filtered <- round((num_guides_filtered / total_guides) * 100, 2)
message(paste0("Number of guides failing filter: ",
              num_guides_filtered, " of ", total_guides, " (", pct_guides_filtered, "%)"))

# Removing guides from counts and library
count_matrix_with_guides_removed <- sample_count_matrix
library_with_guides_removed <- library_annotations
if (length(filtered_guides) > 0) {
  message("Removing guides failing filter from library...")
  library_with_guides_removed <- get_library_annotations(
                                  remove_guides_from_library_annotations_object(library_annotations_object,
                                                                                guides_to_remove = filtered_guides))
  message("Removing guides failing filter from counts...")
  count_matrix_with_guides_removed <- remove_guides_from_count_matrix(sample_count_matrix,
                                                                      id_column = opt$count_id_column_index,
                                                                      guides_to_remove = filtered_guides)
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

# Write filtered guides to file
message("Writing filtered guides to file...")
outfile <- write_dataframe_to_file(data = filtered_guides,
                                   outfile = opt$filtered_guides_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t",
                                   col.names = FALSE,
                                   ignore_empty = TRUE)
message(paste("Filtered guides written to:", outfile))

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
                                                   filtered_guides))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")
