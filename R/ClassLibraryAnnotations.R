#' An S4 class to represent library annotations
#'
#' @slot filepath character.
#' @slot id_column numeric.
#' @slot gene_column numeric
#' @slot chr_column character.
#' @slot chr_start_column numeric
#' @slot chr_end_column numeric
#' @slot file_separator character.
#' @slot file_header logical.
#' @slot annotations data.frame.

setClass(
  "LibraryAnnotations",
  representation(
    filepath = "character",
    id_column = "numeric",
    gene_column = "numeric",
    chr_column = "ANY",
    chr_start_column = "ANY",
    chr_end_column = "ANY",
    file_separator = "character",
    file_header = "logical",
    annotations = "data.frame"
  )
)

setValidity("LibraryAnnotations", function(object) {
  if (is.null(object@filepath))
    return("file must have a value")
  if (!is.numeric(object@id_column))
    return("id_column must be numeric")
  if (!is.numeric(object@gene_column))
    return("gene_column must be numeric")
  if (is.null(object@file_separator))
    return("file_separator must have a value")
  if (!is.logical(object@file_header))
    return("file_header must be TRUE/FALSE")

  check_file(object@filepath)
  check_dataframe(object@annotations,
                  c(object@id_column,
                    object@gene_column))
  if (!is.null(object@chr_column))
    check_dataframe(object@annotations, object@chr_column)
  if (!is.null(object@chr_start_column)) {
    if (!is.numeric(object@chr_start_column))
      stop("chr_start_column must be numeric")
    check_dataframe(object@annotations, object@chr_start_column)
  }
  if (!is.null(object@chr_end_column)) {
    if (!is.numeric(object@chr_end_column))
      stop("chr_end_column must be numeric")
    check_dataframe(object@annotations, object@chr_end_column)
  }
  TRUE
})

#' Generic for get_library_annotations method
#'
#' @param object object
#' @param processed logical
#' @param crisprcleanr logical
#'
#' @export
setGeneric("get_library_annotations", function(object, processed = FALSE, crisprcleanr = FALSE) {
  standardGeneric("get_library_annotations")
})

#' Get library annotations from LibraryAnnotations object
#'
#' @param object LibraryAnnotations
#' @param processed raw (TRUE) or processed annotations (FALSE)
#' @param crisprcleanr format for CRISPRcleanR (TRUE) or default to processed annotations (FALSE)
#' @return dataframe containing library annotations
#' @exportMethod get_library_annotations
setMethod("get_library_annotations",
          signature(object = "LibraryAnnotations"),
          function(object, processed, crisprcleanr) {
            annotations <- object@annotations
            if (crisprcleanr && !processed) {
              stop("Cannot format library for CRISPRcleanR when processed is FALSE.")
            }
            if (processed) {
              if (crisprcleanr && (is.null(object@chr_column) || is.null(object@chr_start_column) || is.null(object@chr_end_column))) {
                stop("Cannot format library for CRISPRcleanR when chr_column, chr_start_column or chr_end_column is null.")
              }
              if (is.null(object@chr_column)) {
                annotations <- annotations[, c(object@id_column,
                                               object@gene_column)]
                colnames(annotations) <- c("sgRNA", "gene")
              } else {
                annotations <- annotations[, c(object@id_column,
                                               object@gene_column,
                                               object@chr_column,
                                               object@chr_start_column,
                                               object@chr_end_column)]
                if (crisprcleanr) {
                  colnames(annotations) <- c('CODE', 'GENES', 'CHRM', 'STARTpos', 'ENDpos')
                  rownames(annotations) <- annotations$CODE
                } else {
                  colnames(annotations) <- c("sgRNA", "gene", "chr", "start", "end")
                }
              }
            }
            return(annotations)
          })

#' Generic for library_has_coordinates method
#'
#' @param object object
#'
#' @export
setGeneric("library_has_coordinates", function(object) {
  standardGeneric("library_has_coordinates")
})

#' Check whether LibraryAnnotations object has coordinates
#'
#' @param object LibraryAnnotations
#' @return logical
#' @exportMethod library_has_coordinates
setMethod("library_has_coordinates",
          signature(object = "LibraryAnnotations"),
          function(object) {
            annotations <- object@annotations
            if (is.null(object@chr_column) || is.null(object@chr_start_column) || is.null(object@chr_end_column)) {
              return(FALSE)
            }
            return(TRUE)
          })
