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
  crisprcleanr_output_options(),
  crisprcleanr_correction_options(),
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
for (n in c('counts', 'library', 'lfc_matrix')) {
  check_option(n, opt[[n]])
}

if (is.null(opt$count_matrix_outfile))
  opt$count_matrix_outfile <- 'count_matrix.CRISPRcleanR_input.tsv'
if (is.null(opt$lfc_matrix_outfile))
  opt$lfc_matrix_outfile <- 'fold_change_matrix.sgrna.CRISPRcleanR_input.tsv'
if (is.null(opt$library_outfile))
  opt$library_outfile <- 'library.CRISPRcleanR_input.tsv'

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

# Read in fold change matrix
message("Reading fold change matrix...")
fold_change_matrix <- read_count_matrix_file(
  filepath = opt$lfc_matrix,
  id_column = opt$lfc_id_column_index,
  gene_column = opt$lfc_gene_column_index,
  count_column = opt$lfc_lfc_column_index,
  file_separator = opt$lfc_delim,
  file_header = ifelse(opt$no_lfc_header,FALSE,TRUE),
  processed = T
)

# Compare sgRNA IDs and gene names between count matrix and library
message("Comparing count matrix to library...")
mat_lib_match <- compare_count_matrix_to_library(
  count_matrix = sample_count_matrix,
  id_column = 1,
  gene_column = 2,
  library_annotation_object = library_annotations_object)

# Compare sgRNA IDs and gene names between fold change matrix and library
message("Comparing fold change matrix to library...")
mat_lib_match <- compare_count_matrix_to_library(
  count_matrix = fold_change_matrix,
  id_column = 1,
  gene_column = 2,
  library_annotation_object = library_annotations_object)

# Get CRISPRcleanR-formatted library
message("Preparing CRISPRcleanR-formatted library...")
processed_library <- get_library_annotations(library_annotations_object, processed = T, crisprcleanr = T)

# Get CRISPRcleanR-formatted count matrix
message("Preparing CRISPRcleanR-formatted count matrix...")
processed_count_matrix <- sample_count_matrix
colnames(processed_count_matrix)[1:2] <- c('sgRNA', 'gene')

# Get CRISPRcleanR-formatted count matrix
message("Preparing CRISPRcleanR-formatted fold change matrix...")
processed_fold_change_matrix <- fold_change_matrix
colnames(processed_fold_change_matrix)[1:2] <- c('sgRNA', 'gene')

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

# Write processed fold change matrix to file
message("Writing fold change matrix to file...")
outfile <- write_dataframe_to_file(data = processed_fold_change_matrix,
                                   outfile = opt$lfc_matrix_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Fold change matrix written to:", outfile))

# Write processed library to file
message("Writing processed library to file...")
outfile <- write_dataframe_to_file(data = processed_library,
                                   outfile = opt$library_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Fold change matrix written to:", outfile))

# Write Rdata to file
if (!is.null(opt$rdata)) {
  message("Writing R data to file...")
  rdata_outfile <- write_rdata_to_file(prefix = opt$prefix,
                                       outfile = opt$rdata,
                                       outdir = opt$outdir,
                                       data = list(sample_count_matrix,
                                                   fold_change_matrix,
                                                   library_annotations_object,
                                                   processed_count_matrix,
                                                   processed_fold_change_matrix,
                                                   processed_library))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")


