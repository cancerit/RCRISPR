suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(rcrispr))
suppressPackageStartupMessages(suppressWarnings(library(CRISPRcleanR)))

###############################################################################
#* --                                                                     -- *#
#* --                             OPTIONS                                 -- *#
#* --                                                                     -- *#
###############################################################################

option_list = c(
  count_path_options(),
  library_annotation_options(),
  crisprcleanr_normalisation_options(),
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
for (n in c('counts', 'library', 'min_reads', 'n_controls')) {
  check_option(n, opt[[n]])
}

if (is.null(opt$count_matrix_outfile))
  opt$count_matrix_outfile <- 'count_matrix.CRISPRcleanR.normalised.tsv'
if (is.null(opt$lfc_matrix_outfile))
  opt$lfc_matrix_outfile <- 'fold_change_matrix.CRISPRcleanR.normalised.tsv'
if (is.null(opt$library_outfile))
  opt$library_outfile <- 'library.CRISPRcleanR.normalised.tsv'

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

# Compare sgRNA IDs and gene names between count matrix and library
message("Comparing count matrix to library...")
mat_lib_match <- compare_count_matrix_to_library(
  count_matrix = sample_count_matrix,
  id_column = 1,
  gene_column = 2,
  library_annotation_object = library_annotations_object)

# Format library for CRISPRcleanR normalisation (doesn't care about coordinates)
message("Formatting library for CRISPRcleanR normalisation...")
processed_library_annotation <- get_library_annotations(library_annotations_object, processed = TRUE)
processed_library_annotation <- processed_library_annotation[,c('sgRNA', 'gene')]
colnames(processed_library_annotation) <- c( 'CODE', 'GENES')
rownames(processed_library_annotation) <- processed_library_annotation$CODE

# Run CRISPRcleanR normalisation
message("Running CRISPRcleanR normalisation...")
min_reads <- convert_variable_to_integer(opt$min_reads)
n_controls <- convert_variable_to_integer(opt$n_controls)
ccr_normalised <- ccr.NormfoldChanges(  Dframe = sample_count_matrix,
                                        display = F,
                                        saveToFig = F,
                                        outdir = opt$outdir,
                                        min_reads = min_reads,
                                        EXPname = 'CRISPRcleanR',
                                        libraryAnnotation = processed_library_annotation,
                                        ncontrols = n_controls )

# If min reads is anything other than 0, CRISPRcleanR will perform additional filtering
# In this case, we need to filter the library according to the guides removed
message("Formatting library post-normalisation...")
if (opt$min_reads > 0) {
  # Get identifiers of removed guides
  removed_guides <- setdiff(processed_library_annotation$CODE, ccr_normalised$norm_counts[,1])
  # Tell users how many guides were removed
  message(paste("Guides removed by CRISPRcleanR min_reads filter:", length(removed_guides), 'of', nrow(processed_library_annotation)))
  # Remove guides from original library
  processed_library_annotation_obj <- remove_guides_from_library_annotations_object(library_annotations_object,
                                                                                    removed_guides)
} else {
  processed_library_annotation_obj = library_annotations_object
}

# Compare sgRNA IDs and gene names between count matrix and library
message("Comparing count matrix to library...")
mat_lib_match <- compare_count_matrix_to_library(
  count_matrix = ccr_normalised$norm_counts,
  id_column = 1,
  gene_column = 2,
  library_annotation_object = processed_library_annotation_obj)

# Compare sgRNA IDs and gene names between fold change matrix and library
message("Comparing fold change matrix to library...")
mat_lib_match <- compare_count_matrix_to_library(
  count_matrix = ccr_normalised$logFCs,
  id_column = 1,
  gene_column = 2,
  library_annotation_object = processed_library_annotation_obj)

# Write processed count matrix to file
message("Writing count matrix to file...")
outfile <- write_dataframe_to_file(data = ccr_normalised$norm_counts,
                                   outfile = opt$count_matrix_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Count matrix written to:", outfile))

# Write processed fold change matrix to file
message("Writing fold change matrix to file...")
outfile <- write_dataframe_to_file(data = ccr_normalised$logFCs,
                                   outfile = opt$lfc_matrix_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Fold change matrix written to:", outfile))

# Write processed library to file
message("Writing processed library to file...")
outfile <- write_dataframe_to_file(data =get_library_annotations(processed_library_annotation_obj),
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
                                                   library_annotations_object,
                                                   processed_library_annotation,
                                                   ccr_normalised))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")

