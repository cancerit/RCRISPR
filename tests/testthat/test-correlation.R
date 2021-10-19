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
#* --                               TEST DATA                             -- *#
#* --                                                                     -- *#
###############################################################################

test_count_matrix_file <- system.file("testdata", "test_count_matrix.tsv", package = 'rcrispr')
test_count_matrix <- read_count_matrix_file(filepath = test_count_matrix_file,
                                            id_column = 1,
                                            gene_column = 2,
                                            count_column = 3:9,
                                            processed = T)

###############################################################################
#* --                                                                     -- *#
#* --                        plot_correlation()                           -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot plot correlation, df is null", {
  testthat::expect_error(plot_correlation(df = NULL),
                         "Cannot plot correlation, df is null")
})

testthat::test_that("cannot plot correlation, cor_columns is null", {
  testthat::expect_error(plot_correlation(df = test_count_matrix,
                                          cor_columns = NULL),
                         "Cannot plot correlation, cor_columns is null")
})

testthat::test_that("cannot plot correlation of one column", {
  testthat::expect_error(plot_correlation(df = test_count_matrix, cor_columns = 3),
                         "Cannot plot correlation for only one column")
})

testthat::test_that("can plot correlation", {
  p <- plot_correlation(df = test_count_matrix,
                        cor_columns = 3:9)
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$gg)
})

testthat::test_that("can plot correlation with groups", {
  p <- plot_correlation(df = data.frame('x' = c(1:8), 'y' = c(1:8), 'g' = c(rep('g1', 4), rep('g2', 4))),
                        cor_columns = 1:2,
                        group_column = 3)
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$gg)
})

testthat::test_that("can plot correlation with > 12 groups with message", {
  testthat::expect_message(plot_correlation(df = data.frame('x' = c(1:56),
                                                            'y' = c(1:56),
                                                            'g' = c(rep(1:14,4))),
                                            cor_columns = 1:2,
                                            group_column = 3),
                           "Cannot plot more than 12 groups. Setting groups to null")
})
