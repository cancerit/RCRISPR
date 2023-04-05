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

###############################################################################
#* --                                                                     -- *#
#* --                       bagel_normalise_counts()                      -- *#
#* --                                                                     -- *#
###############################################################################

#' Normalise counts using BAGEL method.
#'
#' @description
#' Normalise count matrix using BAGEL method.
#'
#' @details
#' Assumes that sgRNA identifiers and gene names are in the first two columns.
#' First a defined `pseudocount` gets added to all guide counts. Then sample counts
#' are total normalised and multiplied by a scaling factor.
#'
#' @param sample_counts a sample count matrix.
#' @param pseudocount a pseudocount (Default: 5).
#' @param scaling_factor a scaling factor (Default: 10000000).
#'
#' @import dplyr
#' @return dataframe
#' @export bagel_normalise_counts
bagel_normalise_counts <-
  function(sample_counts = NULL,
           scaling_factor = 10000000,
           pseudocount = 5) {
    # Validate inputs
    if (is.null(sample_counts))
      stop("Cannot normalise counts with BAGEL method, sample_counts is null.")
    if (is.null(pseudocount))
      stop("Cannot normalise counts with BAGEL method, pseudocount is null.")
    if (is.null(scaling_factor))
      stop("Cannot normalise counts with BAGEL method, scaling_factor is null.")
    if (!is.numeric(scaling_factor))
      stop("Cannot normalise counts with BAGEL method, scaling_factor is not numeric.")
    if (!is.numeric(pseudocount))
      stop("Cannot normalise counts with BAGEL method, pseudocount is not numeric.")
    # Check data frame
    check_dataframe(sample_counts)
    # Add pseudocount to all counts
    sample_counts <- add_pseudocount(data = sample_counts,
                                     pseudocount = pseudocount,
                                     indices = c(3:ncol(sample_counts)))
    # Total normalisation with scaling factor
    sample_counts <- sample_counts %>%
      mutate(across(all_of(3:ncol(sample_counts)), ~ (. / sum(.)) * scaling_factor))
    # Check dataframe
    check_dataframe(sample_counts, check_na = T, check_nan = T)
    return(sample_counts)
  }
