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
#* --                  total_counts_per_sample()                          -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can calculate total counts using one columns", {
  result <- data.frame('sample' = c('HELA_T0'),
                       'total_counts' = c(752))
  testthat::expect_equal(total_counts_per_sample(count_matrix = test_count_matrix,
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3), result)
})

testthat::test_that("can calculate total counts using multiple columns", {
  result <- data.frame('sample' = c('HELA_T0', 'HELA_T15A_CTRL', 'HELA_T15B_CTRL'),
                       'total_counts' = c(752, 712, 602))
  testthat::expect_equal(total_counts_per_sample(count_matrix = test_count_matrix,
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3:5), result)
})

testthat::test_that("can calculate total counts using multiple columns with NAs", {
  result <- data.frame('sample' = c('S1', 'S2'),
                       'total_counts' = c(0, 0))
  testthat::expect_equal(total_counts_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                           'gene' = c('g1', 'g2'),
                                                                           'S1' = rep(0, 2),
                                                                           'S2' = rep(NA, 2)),
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3:4), result)
})

testthat::test_that("can calculate total counts using multiple columns with zeros", {
  result <- data.frame('sample' = c('S1', 'S2'),
                       'total_counts' = c(0, 0))
  testthat::expect_equal(total_counts_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                           'gene' = c('g1', 'g2'),
                                                                           'S1' = c(NA, 0),
                                                                           'S2' = rep(0, 2)),
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3:4), result)
})

testthat::test_that("can calculate total counts using one column with zeros", {
  result <- data.frame('sample' = c('S1'),
                       'total_counts' = c(0))
  testthat::expect_equal(total_counts_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                           'gene' = c('g1', 'g2'),
                                                                           'S1' = rep(0, 2),
                                                                           'S2' = rep(0, 2)),
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3), result)
})

testthat::test_that("can calculate total counts using one column with NAs", {
  result <- data.frame('sample' = c('S1'),
                       'total_counts' = c(0))
  testthat::expect_equal(total_counts_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                           'gene' = c('g1', 'g2'),
                                                                           'S1' = rep(NA, 2),
                                                                           'S2' = rep(0, 2)),
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3), result)
})

testthat::test_that("cannot calculate total counts, id_column overlaps count columns", {
  testthat::expect_error(total_counts_per_sample(count_matrix = test_count_matrix,
                                                 id_column = 3,
                                                 gene_column = 2,
                                                 count_column = 3:5),
                         "id_column cannot overlap count columns")
})

testthat::test_that("cannot calculate total counts, gene_column overlaps count columns", {
  testthat::expect_error(total_counts_per_sample(count_matrix = test_count_matrix,
                                                 id_column = 1,
                                                 gene_column = 3,
                                                 count_column = 3:5),
                         "gene_column cannot overlap count columns")
})

###############################################################################
#* --                                                                     -- *#
#* --                    low_counts_per_sample()                          -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can calculate zero counts using one column", {
  result <- data.frame('sample' = c('HELA_T0'),
                       'zero_sgrnas' = c(1))
  testthat::expect_equal(low_counts_per_sample(count_matrix = test_count_matrix,
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3), result)
})

testthat::test_that("can calculate zero counts using multiple columns", {
  result <- data.frame('sample' = c('HELA_T0', 'HELA_T15A_CTRL', 'HELA_T15B_CTRL'),
                       'zero_sgrnas' = rep(1, 3))
  testthat::expect_equal(low_counts_per_sample(count_matrix = test_count_matrix,
                                               id_column = 1,
                                               gene_column = 2,
                                               count_column = 3:5), result)
})

testthat::test_that("can calculate low counts using one column", {
  result <- data.frame('sample' = c('HELA_T0'),
                       'low_sgrnas' = c(1))
  testthat::expect_equal(low_counts_per_sample(count_matrix = test_count_matrix,
                                               id_column = 1,
                                               gene_column = 2,
                                               count_column = 3,
                                               filter_label = 'low_sgrnas',
                                               threshold = 200), result)
})

testthat::test_that("can calculate low counts using multiple columns", {
  result <- data.frame('sample' = c('HELA_T0', 'HELA_T15A_CTRL', 'HELA_T15B_CTRL'),
                       'low_sgrnas' = c(1, 2, 2))
  testthat::expect_equal(low_counts_per_sample(count_matrix = test_count_matrix,
                                               id_column = 1,
                                               gene_column = 2,
                                               count_column = 3:5,
                                               filter_label = 'low_sgrnas',
                                               threshold = 200), result)
})

testthat::test_that("can calculate low counts using multiple columns with NAs", {
  result <- data.frame('sample' = c('S1', 'S2'),
                       'zero_sgrnas' = c(2, 2))
  testthat::expect_equal(low_counts_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                         'gene' = c('g1', 'g2'),
                                                                         'S1' = rep(NA, 2),
                                                                         'S2' = rep(NA, 2)),
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3:4), result)
})

testthat::test_that("can calculate low counts using multiple columns with zeros", {
  result <- data.frame('sample' = c('S1', 'S2'),
                       'zero_sgrnas' = c(1, 2))
  testthat::expect_equal(low_counts_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                         'gene' = c('g1', 'g2'),
                                                                         'S1' = c(0, 2),
                                                                         'S2' = rep(0, 2)),
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3:4), result)
})

testthat::test_that("can calculate low counts using one column with zeros", {
  result <- data.frame('sample' = c('S1'),
                       'zero_sgrnas' = c(2))
  testthat::expect_equal(low_counts_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                         'gene' = c('g1', 'g2'),
                                                                         'S1' = rep(0, 2),
                                                                         'S2' = rep(0, 2)),
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3), result)
})

testthat::test_that("can calculate low counts using one column with NAs", {
  result <- data.frame('sample' = c('S1'),
                       'zero_sgrnas' = c(2))
  testthat::expect_equal(low_counts_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                         'gene' = c('g1', 'g2'),
                                                                         'S1' = rep(NA, 2),
                                                                         'S2' = rep(0, 2)),
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3), result)
})

testthat::test_that("cannot calculate low counts, id_column overlaps count columns", {
  testthat::expect_error(low_counts_per_sample(count_matrix = test_count_matrix,
                                               id_column = 3,
                                               gene_column = 2,
                                               count_column = 3:5),
                         "id_column cannot overlap count columns")
})

testthat::test_that("cannot calculate low counts, gene_column overlaps count columns", {
  testthat::expect_error(low_counts_per_sample(count_matrix = test_count_matrix,
                                               id_column = 1,
                                               gene_column = 3,
                                               count_column = 3:5),
                         "gene_column cannot overlap count columns")
})

###############################################################################
#* --                                                                     -- *#
#* --                    calculate_gini_index()                           -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can calculate gini index", {
  testthat::expect_equal(calculate_gini_index(c(1,1,2,2)), 0.15)
})

testthat::test_that("can calculate gini index, remove NAs", {
  testthat::expect_equal(calculate_gini_index(c(1,NA,2,2)), 0.14)
})

testthat::test_that("cannot calculate gini index, fail NAs", {
  testthat::expect_error(calculate_gini_index(c(1,NA,2,2), fail_na = T),
                         "Cannot calculate gini index, x contains NAs")
})

testthat::test_that("cannot calculate gini index, all NAs", {
  testthat::expect_error(calculate_gini_index(c(NA, NA)),
                         "Cannot calculate gini index, x only contains NAs")
})

testthat::test_that("cannot calculate gini index, x is null", {
  testthat::expect_error(calculate_gini_index(x = NULL),
                         "Cannot calculate gini index on null value.")
})


testthat::test_that("cannot calculate gini index, x is empty", {
  testthat::expect_error(calculate_gini_index(vector()),
                         "Cannot calculate gini index, x is empty")
})

###############################################################################
#* --                                                                     -- *#
#* --                    gini_index_per_sample()                          -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot calculate gini index per sample, count_matrix is null", {
  testthat::expect_error(gini_index_per_sample(count_matrix = NULL),
                         "Dataframe is null.")
})

testthat::test_that("cannot calculate gini index per sample, id_column is null", {
  testthat::expect_error(gini_index_per_sample(count_matrix = test_count_matrix,
                                               id_column = NULL),
                         "Cannot calculate gini index per sample, id_column is null.")
})

testthat::test_that("cannot calculate gini index per sample, gene_column is null", {
  testthat::expect_error(gini_index_per_sample(count_matrix = test_count_matrix,
                                               gene_column = NULL),
                         "Cannot calculate gini index per sample, gene_column is null.")
})

testthat::test_that("cannot calculate gini index per sample, count_column is null", {
  testthat::expect_error(gini_index_per_sample(count_matrix = test_count_matrix,
                                               count_column = NULL),
                         "Cannot calculate gini index per sample, count_column is null.")
})

testthat::test_that("cannot calculate gini index per sample, id_column overlaps counts", {
  testthat::expect_error(gini_index_per_sample(count_matrix = test_count_matrix,
                                               id_column = 3),
                         "id_column cannot overlap count columns")
})

testthat::test_that("cannot calculate gini index per sample, gene_column overlaps counts", {
  testthat::expect_error(gini_index_per_sample(count_matrix = test_count_matrix,
                                               gene_column = 3),
                         "gene_column cannot overlap count columns")
})

testthat::test_that("can calculate gini index per sample with one column", {
  result <- data.frame('sample' = c('HELA_T0'),
                       'gini_index' = c(0.52))
  testthat::expect_equal(gini_index_per_sample(count_matrix = test_count_matrix,
                                               id_column = 1,
                                               gene_column = 2,
                                               count_column = 3), result)
})

testthat::test_that("can calculate gini index per sample with multiple columns", {
  result <- data.frame('sample' = c('HELA_T0', 'HELA_T15A_CTRL'),
                       'gini_index' = c(0.52, 0.54))
  testthat::expect_equal(gini_index_per_sample(count_matrix = test_count_matrix,
                                               id_column = 1,
                                               gene_column = 2,
                                               count_column = 3:4), result)
})

testthat::test_that("cannot calculate gini index using multiple columns all NAs", {
  testthat::expect_error(gini_index_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                         'gene' = c('g1', 'g2'),
                                                                         'S1' = rep(NA, 2),
                                                                         'S2' = rep(NA, 2)),
                                               id_column = 1,
                                               gene_column = 2,
                                               count_column = 3:4),
                         "Cannot calculate gini index, x only contains NAs")
})

testthat::test_that("can calculate gini index per sample using multiple columns with zeros", {
  result <- data.frame('sample' = c('S1', 'S2'),
                       'gini_index' = c(1, -3))
  testthat::expect_equal(gini_index_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                         'gene' = c('g1', 'g2'),
                                                                         'S1' = c(0, 2),
                                                                         'S2' = rep(0, 2)),
                                               id_column = 1,
                                               gene_column = 2,
                                               count_column = 3:4), result)
})

testthat::test_that("can calculate gini index per sample using one column with zeros", {
  result <- data.frame('sample' = c('S1'),
                       'gini_index' = c(-3))
  testthat::expect_equal(gini_index_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                         'gene' = c('g1', 'g2'),
                                                                         'S1' = rep(0, 2),
                                                                         'S2' = rep(0, 2)),
                                               id_column = 1,
                                               gene_column = 2,
                                               count_column = 3), result)
})

testthat::test_that("can calculate gini index per sample using one column with NAs", {
  result <- data.frame('sample' = c('S1'),
                       'gini_index' = c(0))
  testthat::expect_equal(gini_index_per_sample(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                         'gene' = c('g1', 'g2'),
                                                                         'S1' = c(NA, 1),
                                                                         'S2' = rep(0, 2)),
                                               id_column = 1,
                                               gene_column = 2,
                                               count_column = 3), result)
})

###############################################################################
#* --                                                                     -- *#
#* --                       count_matrix_stats()                          -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot generate count matrix stats, count_matrix is null", {
  testthat::expect_error(count_matrix_stats(count_matrix = NULL),
                         "Cannot generate count matrix stats, count_matrix is null")
})

testthat::test_that("cannot generate count matrix stats, id_column is null", {
  testthat::expect_error(count_matrix_stats(count_matrix = test_count_matrix,
                                            id_column = NULL),
                         "Cannot generate count matrix stats, id_column is null")
})

testthat::test_that("cannot generate count matrix stats, gene_column is null", {
  testthat::expect_error(count_matrix_stats(count_matrix = test_count_matrix,
                                            gene_column = NULL),
                         "Cannot generate count matrix stats, gene_column is null")
})

testthat::test_that("cannot generate count matrix stats, count_column is null", {
  testthat::expect_error(count_matrix_stats(count_matrix = test_count_matrix,
                                            count_column = NULL),
                         "Cannot generate count matrix stats, count_column is null")
})

testthat::test_that("cannot generate count matrix stats, low_counts is null", {
  testthat::expect_error(count_matrix_stats(count_matrix = test_count_matrix,
                                            low_counts = NULL),
                         "Cannot generate count matrix stats, low_counts is null")
})

testthat::test_that("cannot generate count matrix stats, id_column overaps counts", {
  testthat::expect_error(count_matrix_stats(count_matrix = test_count_matrix,
                                            id_column = 3),
                         "id_column cannot overlap count columns")
})

testthat::test_that("cannot generate count matrix stats, gene_column overaps counts", {
  testthat::expect_error(count_matrix_stats(count_matrix = test_count_matrix,
                                            gene_column = 3),
                         "gene_column cannot overlap count columns")
})

testthat::test_that("can generate count matrix stats with one column", {
  testthat::expect_snapshot(count_matrix_stats(count_matrix = test_count_matrix))
})

testthat::test_that("can generate count matrix stats with multiple columns", {
  testthat::expect_snapshot(count_matrix_stats(count_matrix = test_count_matrix,
                                               count_column = c(3,5)))
})

testthat::test_that("can generate count matrix stats with total_reads", {
  testthat::expect_snapshot(count_matrix_stats(count_matrix = test_count_matrix,
                                               count_column = c(3,5),
                                               total_reads = c('HELA_T0' = 2000, 'HELA_T15B_CTRL' = 5120)))
})

testthat::test_that("cannot generate count matrix stats, total_reads contain zeros", {
  testthat::expect_error(count_matrix_stats(count_matrix = test_count_matrix,
                                            count_column = c(3,5),
                                            total_reads = c('HELA_T0' = 0, 'HELA_T15B_CTRL' = 5120)),
                         "Cannot calculate total reads as they contain zeros")
})

testthat::test_that("can generate count matrix stats with zeros", {
  testthat::expect_snapshot(count_matrix_stats(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                        'gene' = c('g1', 'g2'),
                                                                        'S1' = rep(0, 2),
                                                                        'S2' = rep(0, 2)),
                                               count_column = 3:4))
})

testthat::test_that("can generate count matrix stats with NAs", {
  testthat::expect_snapshot(count_matrix_stats(count_matrix = data.frame('id' = c('sg1', 'sg2'),
                                                                         'gene' = c('g1', 'g2'),
                                                                         'S1' = c(NA, 2),
                                                                         'S2' = rep(0, 2)),
                                               count_column = 3:4))
})


