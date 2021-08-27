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
#' An S4 class to represent MAGeCK RRA gene summary
#'
#' @slot filepath character.
#' @slot gene_summary data.frame.

setClass(
  "MageckGeneRRA",
  representation(
    filepath = "character",
    gene_summary = "data.frame"
  )
)

setValidity("MageckGeneRRA", function(object) {
  # Check we have all data
  if (is.null(object@filepath))
    return("file must have a value")
  if (is.null(object@gene_summary))
    return("gene_summary must have a value")

  # Check gene summary data frame
  if (!is.null(object@gene_summary))
    check_dataframe(object@gene_summary)

  # Check gene summary has the correct column names
  # Assumes summary has been read in with `check.names = T`
  expected_column_names <- c('id', 'num',
                             'neg.score', 'neg.p.value', 'neg.fdr',
                             'neg.rank', 'neg.goodsgrna', 'neg.lfc',
                             'pos.score', 'pos.p.value', 'pos.fdr',
                             'pos.rank', 'pos.goodsgrna', 'pos.lfc')
  if (length(setdiff(expected_column_names, colnames(object@gene_summary))) != 0)
    stop(paste("Expected columns not found:", paste(expected_column_names, sep = ",")))

  TRUE
})

#' Generic for get_mageck_gene_summary method
#'
#' @param object object
#' @param filters character
#'
#' @export
setGeneric("get_mageck_gene_summary", function(object, filters = NULL) {
  standardGeneric("get_mageck_gene_summary")
})

#' Get gene summary from MageckGeneRRA object
#'
#' @param object MageckGeneRRA
#' @param filters filter as string (e.g. 'neg.fdr < 0.05')
#' @import dplyr
#' @importFrom rlang parse_expr
#' @return data frame of MAGeCK gene summary results
#' @exportMethod get_mageck_gene_summary
setMethod("get_mageck_gene_summary",
          signature(object = "MageckGeneRRA"),
          function(object, filters = NULL) {
            gene_summary <- object@gene_summary
            if (!is.null(filters)) {
              # Filter using string
              # TODO: put in better error catching
              gene_summary <- gene_summary %>% filter(eval(rlang::parse_expr(filters)))
              if (nrow(gene_summary) == 0)
                warning(paste("No rows returned from MAGeCK gene summary with filter:", filters))
            }
            return(gene_summary)
          })
