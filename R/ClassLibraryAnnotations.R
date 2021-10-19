# Copyright (c) 2021 Genome Research Ltd
#
# Author: CASM/Cancer IT <cgphelp@sanger.ac.uk>
#
# This file is part of RCRISPR.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# 1. The usage of a range of years within a copyright statement contained within
# this distribution should be interpreted as being equivalent to a list of years
# including the first and last year specified and all consecutive years between
# them. For example, a copyright statement that reads ‘Copyright (c) 2005, 2007-
# 2009, 2011-2012’ should be interpreted as being identical to a statement that
# reads ‘Copyright (c) 2005, 2007, 2008, 2009, 2011, 2012’ and a copyright
# statement that reads ‘Copyright (c) 2005-2012’ should be interpreted as being
# identical to a statement that reads ‘Copyright (c) 2005, 2006, 2007, 2008,
# 2009, 2010, 2011, 2012’.
#
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
#' @param sort_ids logical
#' @param crisprcleanr logical
#'
#' @export
setGeneric("get_library_annotations", function(object, processed = FALSE, sort_ids = FALSE, crisprcleanr = FALSE) {
  standardGeneric("get_library_annotations")
})

#' Get library annotations from LibraryAnnotations object
#'
#' @param object LibraryAnnotations
#' @param processed raw (TRUE) or processed annotations (FALSE)
#' @param sort_ids order annotations by guide - only applicable when processed = TRUE (FALSE)
#' @param crisprcleanr format for CRISPRcleanR (TRUE) or default to processed annotations (FALSE)
#' @return dataframe containing library annotations
#' @exportMethod get_library_annotations
setMethod("get_library_annotations",
          signature(object = "LibraryAnnotations"),
          function(object, processed, sort_ids, crisprcleanr) {
            annotations <- object@annotations
            if (sort_ids && !processed)
              stop(paste("Cannot order unprocessed annotations (sort_ids can only be TRUE when processed is TRUE)."))
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
              if (sort_ids) {
                annotations <- annotations[order(annotations$gene, annotations$sgRNA),]
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
