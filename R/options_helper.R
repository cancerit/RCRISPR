###############################################################################
#* --                                                                     -- *#
#* --                        OPTION VALIDATION                            -- *#
#* --                                                                     -- *#
###############################################################################
#' Check optparse option is not null
#'
#' @param option name of option to check
#' @param value value of option to check
#'
#' @return logical
#' @export check_option
check_option <- function(option, value) {
  if (is.null(value)) {
    stop(paste("Please provide a value for option:", option), call. = FALSE)
  }
  return(TRUE)
}

###############################################################################
#* --                                                                     -- *#
#* --                      SHARED OUTPUT OPTIONS                          -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for shared output
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export shared_output_options
shared_output_options <- function() {
  opts <- list(
    make_option(
      c("-r", "--rdata"),
      type = "character",
      default = NULL,
      help = "output file for .Rdata",
      metavar = "character"
    ),
    make_option(
      c("-d", "--outdir"),
      type = "character",
      default = NULL,
      help = "output directory",
      metavar = "character"
    ),
    make_option(
      c("-p", "--prefix"),
      type = "character",
      default = NULL,
      help = "output file prefix",
      metavar = "character"
    ),
    make_option(
      c("-s", "--suffix"),
      type = "character",
      default = NULL,
      help = "output file suffix",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                       BASIC INPUT OPTIONS                           -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for basic input
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export basic_infile_options
basic_infile_options <- function() {
  opts <- list(
    make_option(
      c("--infile"),
      type = "character",
      default = NULL,
      help = "input file",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                       BASIC OUTPUT OPTIONS                          -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for basic output
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export basic_outfile_options
basic_outfile_options <- function() {
  opts <- list(
    make_option(
      c("-o", "--outfile"),
      type = "character",
      default = NULL,
      help = "output file",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                           COUNT OPTIONS                             -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for count paths
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export count_path_options
count_path_options <- function() {
  opts <- list(
    make_option(
      c("-c", "--counts"),
      type = "character",
      help = "path to directory of counts or a count matrix",
      metavar = "character"
    )
  )
  return(opts)
}

#' Optparse options for input count type
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export count_type_options
count_type_options <- function() {
  opts <- list(
    make_option(
      c("--counts_type"),
      type = "character",
      default = "single",
      help = "count type (must be either 'single' or 'matrix') [Default: single]",
      metavar = "character"
    )
  )
  return(opts)
}

#' Optparse options for count formatting
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export count_format_options
count_format_options <- function() {
  opts <- list(
     make_option(
       c("--no_counts_header"),
       default = FALSE,
       action = "store_true",
       help = "count file has no header"
     ),
     make_option(
       c("--counts_delim"),
       type = "character",
       default = "\t",
       help = "count file delimiter [Default: \\t]",
       metavar = "character"
     )
  )
  return(opts)
}

#' Optparse options for count column indices
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export count_column_index_options
count_column_index_options <- function() {
  opts <- list(
    make_option(
      c("--count_id_column_index"),
      type = "integer",
      default = 1,
      help = "index of id column in counts [Default: 1]",
      metavar = "integer"
    ),
    make_option(
      c("--count_gene_column_index"),
      type = "integer",
      default = 2,
      help = "index of gene column in counts [Default: 2]",
      metavar = "integer"
    ),
    make_option(
      c("--count_count_column_index"),
      type = "character",
      default = 3,
      help = "index of columns containing counts (e.g. 3 or 3,4,5 or 3-5 or 3-5,6-8) [Default: 3]",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                         FOLD CHANGE OPTIONS                         -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for fold change paths
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export fold_change_path_options
fold_change_path_options <- function() {
  opts <- list(
    make_option(
      c("--fold_changes"),
      type = "character",
      help = "path to fold change matrix",
      metavar = "character"
    )
  )
  return(opts)
}

#' Optparse options for count formatting
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export fold_change_format_options
fold_change_format_options <- function() {
  opts <- list(
     make_option(
       c("--no_fc_header"),
       default = FALSE,
       action = "store_true",
       help = "fold change file has no header"
     ),
     make_option(
       c("--fc_delim"),
       type = "character",
       default = "\t",
       help = "fold change file delimiter [Default: \\t]",
       metavar = "character"
     )
  )
  return(opts)
}

#' Optparse options for fold change column indices
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export fold_change_column_index_options
fold_change_column_index_options <- function() {
  opts <- list(
    make_option(
      c("--fc_id_column_index"),
      type = "integer",
      default = 1,
      help = "index of id column in fold change matrix [Default: 1]",
      metavar = "integer"
    ),
    make_option(
      c("--fc_gene_column_index"),
      type = "integer",
      default = NULL,
      help = "index of gene column in fold change matrix",
      metavar = "integer"
    ),
    make_option(
      c("--fc_column_index"),
      type = "character",
      default = NULL,
      help = "index of columns containing fold changes (e.g. 3 or 3,4,5 or 3-5 or 3-5,6-8)",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                       GENERIC INPUT COLUMN OPTIONS                  -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for infile formatting
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export infile_format_options
infile_format_options <- function() {
  opts <- list(
     make_option(
       c("--no_infile_header"),
       default = FALSE,
       action = "store_true",
       help = "infile has no header"
     ),
     make_option(
       c("--infile_delim"),
       type = "character",
       default = "\t",
       help = "infile delimiter [Default: \\t]",
       metavar = "character"
     )
  )
  return(opts)
}

#' Optparse options for infile column indices
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export infile_column_index_options
infile_column_index_options <- function() {
  opts <- list(
    make_option(
      c("--infile_id_column_index"),
      type = "integer",
      default = NULL,
      help = "index of id column in infile",
      metavar = "integer"
    ),
    make_option(
      c("--infile_gene_column_index"),
      type = "integer",
      default = NULL,
      help = "index of gene column in infile matrix",
      metavar = "integer"
    ),
    make_option(
      c("--infile_data_column_index"),
      type = "character",
      default = NULL,
      help = "index of columns containing data for processing in infile (e.g. 3 or 3,4,5 or 3-5 or 3-5,6-8)",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                    LIBRARY ANNOTATION OPTIONS                       -- *#
#* --                                                                     -- *#
###############################################################################

#' Main optparse options for library annotations
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export library_annotation_options
library_annotation_options <- function() {
  opts <- list(
    make_option(
      c("-l", "--library"),
      type = "character",
      help = "library file containing guide identifiers and annotations",
      metavar = "character"
    )
  )
  return(opts)
}

#' Optparse options for library formatting
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export library_annotation_format_options
library_annotation_format_options <- function() {
  opts <- list(
    make_option(
      c("--no_library_header"),
      default = FALSE,
      action = "store_true",
      help = "library has no header"
    ),
    make_option(
      c("--library_delim"),
      type = "character",
      default = "\t",
      help = "library delimiter [Default: \\t]",
      metavar = "character"
    )
  )
  return(opts)
}

#' Optparse options for library column indices
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export library_annotation_column_index_options
library_annotation_column_index_options <- function() {
  opts <- list(
    make_option(
      c("--library_id_column_index"),
      type = "integer",
      default = 1,
      help = "index of column containing guide id in library annotations [Default: 1]",
      metavar = "integer"
    ),
    make_option(
      c("--library_gene_column_index"),
      type = "integer",
      default = 2,
      help = "index of column containing gene name(s) in library annotations",
      metavar = "integer"
    )
  )
  return(opts)
}

#' Optparse options for library genomic column indices
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export library_annotation_genomic_column_index_options
library_annotation_genomic_column_index_options <- function() {
  opts <- list(
    make_option(
      c("--library_chr_column_index"),
      type = "character",
      default = NULL,
      help = "index of column containing chromosome name in library annotations",
      metavar = "character"
    ),
    make_option(
      c("--library_start_column_index"),
      type = "character",
      default = NULL,
      help = "index of column containing chromosome start position in library annotations",
      metavar = "integer"
    ),
    make_option(
      c("--library_end_column_index"),
      type = "character",
      default = NULL,
      help = "index of column containing chromosome end position in library annotations",
      metavar = "integer"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                       SAMPLE METADATA OPTIONS                       -- *#
#* --                                                                     -- *#
###############################################################################

#' Main optparse options for sample metadata
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export sample_metadata_options
sample_metadata_options <- function() {
  opts <- list(
    make_option(
      c("-i", "--info"),
      type = "character",
      help = "file mapping sample information to count files",
      metavar = "character"
    )
  )
}

#' Optparse options for sample metadata formatting
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export sample_metadata_format_options
sample_metadata_format_options <- function() {
  opts <- list(
    make_option(
      c("--no_info_header"),
      default = FALSE,
      action = "store_true",
      help = "sample metadata has no header"
    ),
    make_option(
      c("--info_delim"),
      type = "character",
      default = "\t",
      help = "sample metadata delimiter [Default: \\t]",
      metavar = "character"
    )
  )
  return(opts)
}

#' Optparse options for sample metadata count files column indices
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export sample_metadata_sample_filename_column_index_options
sample_metadata_sample_filename_column_index_options <- function() {
  opts <- list(
    make_option(
      c("--info_filename_column_index"),
      type = "integer",
      default = 1,
      help = "index of column containing sample count filename in sample mapping [Default: 1]",
      metavar = "integer"
    )
  )
  return(opts)
}

#' Optparse options for sample metadata sample name column indices
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export sample_metadata_sample_label_column_index_options
sample_metadata_sample_label_column_index_options <- function() {
  opts <- list(
    make_option(
      c("--info_label_column_index"),
      type = "integer",
      default = 2,
      help = "index of column containing sample labels in sample mapping [Default: 2]",
      metavar = "integer"
    )
  )
  return(opts)
}

#' Optparse options for sample metadata sample type column indices
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export sample_metadata_sample_type_column_index_options
sample_metadata_sample_type_column_index_options <- function() {
  opts <- list(
    make_option(
      c("--info_plasmid_column_index"),
      type = "integer",
      help = "index of column indicating whether sample type is plasmid in sample mapping",
      metavar = "integer"
    ),
    make_option(
      c("--info_control_column_index"),
      type = "integer",
      help = "index of column indicating whether sample type is control in sample mapping",
      metavar = "integer"
    ),
    make_option(
      c("--info_treatment_column_index"),
      type = "integer",
      help = "index of column indicating whether sample type is treatment in sample mapping",
      metavar = "integer"
    )
  )
  return(opts)
}

#' Optparse options for sample metadata sample group column index
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export sample_metadata_sample_group_column_index_options
sample_metadata_sample_group_column_index_options <- function() {
  opts <- list(
    make_option(
      c("--info_group_column_index"),
      type = "character",
      default = NULL,
      help = "index of column containing sample group name in sample mapping",
      metavar = "character"
    )
  )
  return(opts)
}

#' Optparse options for sample metadata read count column index
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export sample_metadata_sample_read_count_column_index_options
sample_metadata_sample_read_count_column_index_options <- function() {
  opts <- list(
    make_option(
      c("--info_reads_column_index"),
      type = "character",
      default = NULL,
      help = "index of column containing sample read counts in sample mapping",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                       DUPLICATE GUIDE OPTIONS                       -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for output paths
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export duplicate_guide_options
duplicate_guide_options <- function() {
  opts <- list(
    make_option(
      c("--duplicate_guides_outfile"),
      type = "character",
      default = NULL,
      help = "output file for duplicate guides",
      metavar = "character"
    ),
    make_option(
      c("--count_matrix_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed count matrix",
      metavar = "character"
    ),
    make_option(
      c("--library_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed library",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                       REMOVE GUIDE OPTIONS                       -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for output paths
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export remove_guide_options
remove_guide_options <- function() {
  opts <- list(
    make_option(
      c("--guides_to_remove"),
      type = "character",
      default = NULL,
      help = "file of guides to remove",
      metavar = "character"
    ),
    make_option(
      c("--count_matrix_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed count matrix",
      metavar = "character"
    ),
    make_option(
      c("--library_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed library",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                    MAGECK RRA SUMMARY OPTIONS                       -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for MAGeCK RRA gene summary
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export mageck_rra_summary_options
mageck_rra_summary_options <- function() {
  opts <- list(
    make_option(
      c("--gene_summary"),
      type = "character",
      default = NULL,
      help = "MAGeCK RRA gene summary",
      metavar = "character"
    ),
    make_option(
      c("--sgrna_summary"),
      type = "character",
      default = NULL,
      help = "MAGeCK RRA gene summary",
      metavar = "character"
    ),
    make_option(
      c("--n_genes"),
      type = "character",
      default = 10,
      help = "number of genes to label in plots (in each direction)",
      metavar = "character"
    ),
    make_option(
      c("-f", "--fdr"),
      type = "character",
      default = 0.05,
      help = "maximum FDR value",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                      BAGEL NORMALISATION OPTIONS                    -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for output paths
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export bagel_normalisation_options
bagel_normalisation_options <- function() {
  opts <- list(
    make_option(
      c("--pseudocount"),
      type = "numeric",
      default = 0.05,
      help = "pseudocount to add to sample counts",
      metavar = "numeric"
    ),
    make_option(
      c("--scaling_factor"),
      type = "numeric",
      default = 10000000,
      help = "scaling factor for normalised counts",
      metavar = "numeric"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                         CALCULATE LFC OPTIONS                       -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for output paths
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export calculate_lfc_options
calculate_lfc_options <- function() {
  opts <- list(
    make_option(
      c("--sgrna_outfile"),
      type = "character",
      help = "output file for sgRNA-level fold change matrix",
      metavar = "character"
    ),
    make_option(
      c("--gene_outfile"),
      type = "character",
      help = "output file for gene-level fold change matrix",
      metavar = "character"
    ),
    make_option(
      c("--control_indices"),
      type = "character",
      help = "pseudocount to add to sample counts",
      metavar = "character"
    ),
    make_option(
      c("--treatment_indices"),
      type = "character",
      help = "scaling factor for normalised counts",
      metavar = "character"
    ),
    make_option(
      c("--pseudocount"),
      type = "numeric",
      default = 0.5,
      help = "pseudocount to add to sample counts",
      metavar = "numeric"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                       REMOVE GUIDE OPTIONS                          -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for removing guides with no coordinates
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export remove_no_coordinate_guide_options
remove_no_coordinate_guide_options <- function() {
  opts <- list(
    make_option(
      c("--excluded_guides_outfile"),
      type = "character",
      default = NULL,
      help = "file of guides that were remove",
      metavar = "character"
    ),
    make_option(
      c("--count_matrix_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed count matrix",
      metavar = "character"
    ),
    make_option(
      c("--library_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed library",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                       FILTER BY INDEX OPTIONS                       -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for filtering data by index
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export filter_by_index_options
filter_by_index_options <- function() {
  opts <- list(
    make_option(
      c("--filter_indices"),
      type = "character",
      default = NULL,
      help = "column indices to use for filtering",
      metavar = "character"
    ),
    make_option(
      c("--filter_method"),
      type = "character",
      default = NULL,
      help = "filter method ( all, any, mean or median ) [Default: all]",
      metavar = "character"
    ),
    make_option(
      c("--min_reads"),
      type = "character",
      default = 30,
      help = "minimum number of reads for filtering [Default: 30]",
      metavar = "character"
    ),
    make_option(
      c("--count_matrix_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed count matrix",
      metavar = "character"
    ),
    make_option(
      c("--library_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed library",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                    CRISPRCLEANR OUTPUT OPTIONS                      -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for CRISPRcleanR outputs
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export crisprcleanr_output_options
crisprcleanr_output_options <- function() {
  opts <- list(
    make_option(
      c("--count_matrix_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed count matrix",
      metavar = "character"
    ),
    make_option(
      c("--lfc_matrix_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed log fold change matrix",
      metavar = "character"
    ),
    make_option(
      c("--library_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed library",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                 CRISPRCLEANR NORMALISATION OPTIONS                  -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for CRISPRcleanR normalisation
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export crisprcleanr_normalisation_options
crisprcleanr_normalisation_options <- function() {
  opts <- list(
    make_option(
      c("--min_reads"),
      type = "numeric",
      default = 30,
      help = "minimum number of reads for filtering [Default: 30]",
      metavar = "numeric"
    ),
    make_option(
      c("--n_controls"),
      type = "character",
      default = 30,
      help = "number of control samples",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                 CRISPRCLEANR CORRECTION OPTIONS                     -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for CRISPRcleanR normalisation
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export crisprcleanr_correction_options
crisprcleanr_correction_options <- function() {
  opts <- list(
    make_option(
      c("--lfc_matrix"),
      type = "character",
      default = NULL,
      help = "log fold change matrix file",
      metavar = "character"
    ),
    make_option(
       c("--no_lfc_header"),
       default = FALSE,
       action = "store_true",
       help = "fold change file has no header"
    ),
    make_option(
       c("--lfc_delim"),
       type = "character",
       default = "\t",
       help = "fold change file delimiter [Default: \\t]",
       metavar = "character"
    ),
    make_option(
      c("--lfc_id_column_index"),
      type = "integer",
      default = 1,
      help = "index of id column in fold change matrix [Default: 1]",
      metavar = "integer"
    ),
    make_option(
      c("--lfc_gene_column_index"),
      type = "integer",
      default = 2,
      help = "index of gene column in fold change matrix [Default: 2]",
      metavar = "integer"
    ),
    make_option(
      c("--lfc_lfc_column_index"),
      type = "character",
      default = 3,
      help = "index of columns containing fold change matrix (e.g. 3 or 3,4,5 or 3-5 or 3-5,6-8) [Default: 3]",
      metavar = "character"
    ),
    make_option(
      c("--lfc_gene_matrix_outfile"),
      type = "character",
      default = NULL,
      help = "output file for processed gene-level fold change matrix",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                       SEQUENCING QC OPTIONS                         -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for raw count QC
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export sequencing_qc_options
sequencing_qc_options <- function() {
  opts <- list(
    make_option(
      c("--low_counts"),
      type = "numeric",
      default = 30,
      help = "threshold for low counts [Default: 30]",
      metavar = "numeric"
    ),
    make_option(
      c("--no_plot"),
      default = FALSE,
      action = "store_true",
      help = "do not produce plots"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                         CORRELATION OPTIONS                         -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for sample correlations
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export correlation_options
correlation_options <- function() {
  opts <- list(
    make_option(
      c("--cor_method"),
      type = "character",
      default = 'pearson',
      help = "which correlation coefficient to compute [Default: pearson]",
      metavar = "character"
    )
  )
  return(opts)
}

###############################################################################
#* --                                                                     -- *#
#* --                     INTERMEDIATE QC OPTIONS                         -- *#
#* --                                                                     -- *#
###############################################################################

#' Optparse options for intermediate qc
#'
#' @importFrom optparse make_option
#' @return list of optparse options
#' @export intermediate_qc_options
intermediate_qc_options <- function() {
  opts <- list(
    make_option(
       c("--is_fc"),
       default = FALSE,
       action = "store_true",
       help = "whether input is a fold change matrix"
     ),
    make_option(
       c("--is_gene"),
       default = FALSE,
       action = "store_true",
       help = "whether input is gene-level"
     ),
    make_option(
       c("--no_check_names"),
       default = FALSE,
       action = "store_true",
       help = "whether to check sample names exist in metadata"
     ),
    make_option(
      c("--no_plot"),
      default = FALSE,
      action = "store_true",
      help = "do not produce plots"
    )
  )
  return(opts)
}
