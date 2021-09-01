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
#' An S4 class to represent a single set of sample counts.
#'
#' @slot sample_name character.
#' @slot filepath character.
#' @slot id_column numeric.
#' @slot gene_column numeric.
#' @slot count_column numeric.
#' @slot file_separator character.
#' @slot file_header logical.
#' @slot counts data.frame.

setClass(
  "SampleCounts",
  representation(
    sample_name = "character",
    filepath = "character",
    id_column = "numeric",
    gene_column = "numeric",
    count_column = "numeric",
    file_separator = "character",
    file_header = "logical",
    counts = "data.frame"
  )
)

setValidity("SampleCounts", function(object) {
  if (is.null(object@sample_name))
    return("sample_name must have a value")
  if (is.null(object@filepath))
    return("file must have a value")
  if (!is.numeric(object@id_column))
    return("id_column must be numeric")
  if (!is.numeric(object@gene_column))
    return("gene_column must be numeric")
  if (!is.numeric(object@count_column))
    return("count_column must be numeric")
  if (is.null(object@file_separator))
    return("file_separator must have a value")
  if (!is.logical(object@file_header))
    return("file_header must be TRUE/FALSE")
  check_file(object@filepath)
  check_dataframe(object@counts,
                  c(as.integer(object@id_column),
                    as.integer(object@gene_column),
                    as.integer(object@count_column)))
  TRUE
})

#' Generic for counts method
#'
#' @param object object
#' @param processed logical
#'
#' @export
setGeneric("counts", function(object, processed = FALSE) {
  standardGeneric("counts")
})

#' Get counts from SampleCounts object
#'
#' @param object SampleCounts.
#' @param processed raw (TRUE) or processed counts (FALSE)
#' @return dataframe containing sample counts
#' @exportMethod counts
setMethod("counts",
          signature(object = "SampleCounts"),
          function(object, processed) {
  sample_counts <- object@counts
  if (processed) {
    sample_counts <- sample_counts[, c(object@id_column,
                         object@gene_column,
                         object@count_column)]
    if (nrow(sample_counts) == 0)
      stop(paste("Could not generate processed sample counts:", object@sample_name))
    colnames(sample_counts) <- c("sgRNA", "gene", object@sample_name)
  }
  return(sample_counts)
})
