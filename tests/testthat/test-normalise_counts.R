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
###############################################################################
#* --                                                                     -- *#
#* --                       bagel_normalise_counts()                      -- *#
#* --                                                                     -- *#
###############################################################################
test_count_matrix <- data.frame('sgRNA' = c('guide1', 'guide2'),
                                'gene' = c('gene1', 'gene2'),
                                'sample1' = c(1,2),
                                'sample2' = rep(0,2))

testthat::test_that("cannot normalise counts with BAGEL when counts are null", {
  testthat::expect_error(bagel_normalise_counts(sample_counts = NULL),
                         "Cannot normalise counts with BAGEL method, sample_counts is null.")
})

testthat::test_that("cannot normalise counts with BAGEL when pseudocount is null", {
  testthat::expect_error(bagel_normalise_counts(sample_counts = test_count_matrix,
                                                pseudocount = NULL),
                         "Cannot normalise counts with BAGEL method, pseudocount is null.")
})

testthat::test_that("cannot normalise counts with BAGEL when pseudocount is not numeric", {
  testthat::expect_error(bagel_normalise_counts(sample_counts = test_count_matrix,
                                                pseudocount = 'x'),
                         "Cannot normalise counts with BAGEL method, pseudocount is not numeric.")
})

testthat::test_that("cannot normalise counts with BAGEL when scaling factor is not numeric", {
  testthat::expect_error(bagel_normalise_counts(sample_counts = test_count_matrix,
                                                scaling_factor = 'x'),
                         "Cannot normalise counts with BAGEL method, scaling_factor is not numeric.")
})

testthat::test_that("cannot normalise counts with BAGEL when scaling factor is null", {
  testthat::expect_error(bagel_normalise_counts(sample_counts = test_count_matrix,
                                                scaling_factor = NULL),
                         "Cannot normalise counts with BAGEL method, scaling_factor is null.")
})

testthat::test_that("cannot normalise counts with BAGEL when counts are null with pseudocount of 0", {
  testthat::expect_error(bagel_normalise_counts(sample_counts = test_count_matrix,
                                                pseudocount = 0),
                         "Dataframe contains NA values.")
})
testthat::test_that("can normalise counts with BAGEL when counts are null", {
  testthat::expect_snapshot(bagel_normalise_counts(sample_counts = test_count_matrix))
})

