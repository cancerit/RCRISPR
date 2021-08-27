#' An S4 class to represent sample metadata (mapping)
#'
#' @slot filepath character.
#' @slot filename_column numeric.
#' @slot label_column numeric
#' @slot plasmid_column numeric.
#' @slot control_column numeric.
#' @slot treatment_column numeric.
#' @slot group_column numeric.
#' @slot reads_column numeric.
#' @slot file_separator character.
#' @slot file_header logical.
#' @slot metadata data.frame.

setClass(
  "SampleMetadata",
  representation(
    filepath = "character",
    filename_column = "numeric",
    label_column = "numeric",
    plasmid_column = "numeric",
    control_column = "numeric",
    treatment_column = "numeric",
    group_column = "ANY",
    reads_column = "ANY",
    file_separator = "character",
    file_header = "logical",
    metadata = "data.frame"
  )
)

setValidity("SampleMetadata", function(object) {
  if (is.null(object@filepath))
    return("file must have a value")
  if (!is.numeric(object@filename_column))
    return("filename_column must be numeric")
  if (!is.numeric(object@label_column))
    return("label_column must be numeric")
  if (!is.numeric(object@plasmid_column) & !object@plasmid_column %in% c(0:1))
    return("plasmid_column must be numeric and 0 or 1")
  if (!is.numeric(object@control_column) & !object@control_column %in% c(0:1))
    return("control_column must be numeric and 0 or 1")
  if (!is.numeric(object@treatment_column) & !object@treatment_column %in% c(0:1))
    return("treatment_column must be numeric and 0 or 1")
  if (!is.numeric(object@label_column))
    return("label_column must be numeric")
  if (is.null(object@file_separator))
    return("file_separator must have a value")
  if (!is.logical(object@file_header))
    return("file_header must be TRUE/FALSE")

  check_file(object@filepath)
  check_dataframe(object@metadata,
                  c(object@filename_column,
                    object@label_column,
                    object@plasmid_column,
                    object@control_column,
                    object@treatment_column))
  if (!is.null(object@group_column)) {
    #if (!is.numeric(object@group_column))
      #stop("group_column must be numeric")
    check_dataframe(object@metadata, object@group_column)
  }
  if (!is.null(object@reads_column)) {
    #if (!is.numeric(object@reads_column))
    #  stop("reads_column must be numeric")
    if (any(object@reads_column == 0))
      stop("reads_column contains 0 values")
    if (any(is.na(object@reads_column)))
      stop("reads_column contains NA values")
    check_dataframe(object@metadata, object@reads_column)
  }
  TRUE
})

#' Generic for get_sample_metadata method
#'
#' @param object object
#' @param processed logical
#'
#' @export
setGeneric("get_sample_metadata", function(object, processed = FALSE) {
  standardGeneric("get_sample_metadata")
})

#' Get sample metadata from SampleMetadata object
#'
#' @param object SampleMetadata
#' @param processed raw (TRUE) or processed sample metadata (FALSE)
#' @return dataframe containing library annotations
#' @exportMethod get_sample_metadata
setMethod("get_sample_metadata",
          signature(object = "SampleMetadata"),
          function(object, processed) {
            metadata <- object@metadata
            if (processed) {
              if (!is.null(object@reads_column) && !is.null(object@group_column)) {
                metadata <- metadata[, c(object@filename_column,
                                         object@label_column,
                                         object@plasmid_column,
                                         object@control_column,
                                         object@treatment_column,
                                         object@group_column,
                                         object@reads_column)]
                colnames(metadata) <- c("filename", "label", "plasmid", "control", "treatment", "group", "reads")
                metadata$group <- as.factor(metadata$group)
              } else if (!is.null(object@reads_column) && is.null(object@group_column)) {
                metadata <- metadata[, c(object@filename_column,
                                         object@label_column,
                                         object@plasmid_column,
                                         object@control_column,
                                         object@treatment_column,
                                         object@reads_column)]
                colnames(metadata) <- c("filename", "label", "plasmid", "control", "treatment", "reads")
              } else if (is.null(object@reads_column) && !is.null(object@group_column)) {
                metadata <- metadata[, c(object@filename_column,
                                         object@label_column,
                                         object@plasmid_column,
                                         object@control_column,
                                         object@treatment_column,
                                         object@group_column)]
                colnames(metadata) <- c("filename", "label", "plasmid", "control", "treatment", "group")
                metadata$group <- as.factor(metadata$group)
              } else {
                metadata <- metadata[, c(object@filename_column,
                                         object@label_column,
                                         object@plasmid_column,
                                         object@control_column,
                                         object@treatment_column)]
                colnames(metadata) <- c("filename", "label", "plasmid", "control", "treatment")
              }
              metadata$plasmid <- factor(metadata$plasmid, levels = c(0,1))
              metadata$control <- factor(metadata$control, levels = c(0,1))
              metadata$treatment <- factor(metadata$treatment, levels = c(0,1))
            }
            return(metadata)
})
