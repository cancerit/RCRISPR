suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(rcrispr))
suppressPackageStartupMessages(suppressWarnings(library(CRISPRcleanR)))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))

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
  opt$count_matrix_outfile <- 'count_matrix.CRISPRcleanR_corrected.tsv'
if (is.null(opt$lfc_matrix_outfile))
  opt$lfc_matrix_outfile <- 'fold_change_matrix.sgRNA.CRISPRcleanR_corrected.tsv'
if (is.null(opt$lfc_gene_matrix_outfile))
  opt$lfc_gene_matrix_outfile <- 'fold_change_matrix.gene.CRISPRcleanR_corrected.tsv'

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
  processed = T,
  check.names = FALSE
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


# Compare sgRNA IDs and gene names between fold change matrix and library
message("Comparing fold change matrix to count matrix...")
mat_lib_match <- compare_annotations(
  x = sample_count_matrix,
  x_id_column = 1,
  x_gene_column = 2,
  y = fold_change_matrix,
  y_id_column = 1,
  y_gene_column = 2,
  check.attributes = F,
  check.names = F)

# Prepare CRISPRcleanR inputs as a list
message("Preparing CRISPRcleanR inputs...")
ccr.input <- list()
ccr.input[['logFCs']] <- fold_change_matrix
ccr.input[['norm_counts']] <- sample_count_matrix

# Prepare CRISPRcleanR library
message("Preparing CRISPRcleanR library...")
processed_library_annotation <- get_library_annotations(library_annotations_object, processed = TRUE, crisprcleanr = T)

# Map genome-wide sgRNA log fold changes (averaged across replicates) on the genome
# Sort according to their targeted gene on the chromosomes.
message("CRISPRcleanR logFCs2chromPos...")
gwSortedFCs <-ccr.logFCs2chromPos(ccr.input$logFCs, processed_library_annotation)

# Identify and correct biased sgRNA log fold changes
message("Running CRISPRcleanR GWclean...")
correctedFCs <- ccr.GWclean(gwSortedFCs, display = FALSE, label = 'CRISPRcleanR')
processed_sgrna_fold_change_matrix <- correctedFCs$corrected_logFCs
processed_sgrna_fold_change_matrix$sgRNA <- rownames(processed_sgrna_fold_change_matrix)
processed_sgrna_fold_change_matrix <- processed_sgrna_fold_change_matrix[,c('sgRNA', 'genes', 'CHR', 'startp', 'endp', 'avgFC', 'BP', 'correction', 'correctedFC')]

# Derive corrected sgRNAs treatment counts from CRISPRcleanR corrected log fold-changes
message("Running CRISPRcleanR correctCounts...")
processed_count_matrix <- ccr.correctCounts('CRISPRcleanR',
                                            ccr.input[['norm_counts']],
                                            correctedFCs,
                                            processed_library_annotation,
                                            OutDir = opt$outdir,
                                            minTargetedGenes = 3)

# Collecting outputs
message("Collecting CRISPRcleanR outputs...")
processed_sgrna_avg_fold_change_matrix <- data.frame('sgRNA' = rownames(correctedFCs$corrected_logFCs),
                                                     'gene' = correctedFCs$corrected_logFCs$genes,
                                                     'LFC' = correctedFCs$corrected_logFCs$avgFC)
sgrna_fcs <- correctedFCs$corrected_logFCs$avgFC
names(sgrna_fcs) <- rownames(correctedFCs$corrected_logFCs)
processed_gene_fold_change_matrix <- ccr.geneMeanFCs(sgrna_fcs, processed_library_annotation)
processed_gene_fold_change_matrix <- data.frame('gene' = names(processed_gene_fold_change_matrix),
                                                'LFC' = processed_gene_fold_change_matrix)

# Write processed count matrix to file
message("Writing count matrix to file...")
outfile <- write_dataframe_to_file(data = processed_count_matrix,
                                   outfile = opt$count_matrix_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Count matrix written to:", outfile))

# Write CRISPRcleanR sgRNA fold change matrix to file
message("Writing fold change matrix to file...")
outfile <- write_dataframe_to_file(data = correctedFCs$corrected_logFCs %>% rownames_to_column('id'),
                                   outfile = opt$lfc_matrix_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Fold change matrix written to:", outfile))

# Write processed fold change matrix to file
message("Writing gene-level fold change matrix to file...")
outfile <- write_dataframe_to_file(data = processed_gene_fold_change_matrix,
                                   outfile = opt$lfc_gene_matrix_outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Gene-level fold change matrix written to:", outfile))

# Write Rdata to file
if (!is.null(opt$rdata)) {
  message("Writing R data to file...")
  rdata_outfile <- write_rdata_to_file(prefix = opt$prefix,
                                       outfile = opt$rdata,
                                       outdir = opt$outdir,
                                       data = list(sample_count_matrix,
                                                   library_annotations_object,
                                                   processed_library_annotation,
                                                   gwSortedFCs,
                                                   correctedFCs,
                                                   processed_count_matrix,
                                                   processed_sgrna_fold_change_matrix,
                                                   processed_sgrna_avg_fold_change_matrix,
                                                   processed_gene_fold_change_matrix))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")

