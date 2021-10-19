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

test_count_matrix_narrow <- test_count_matrix %>%
                              gather(sample, values, -sgRNA, -gene)

################################################################################
#* --                                                                      -- *#
#* --                     plot_common_violin()                             -- *#
#* --                                                                      -- *#
################################################################################

testthat::test_that("cannot plot common violin, df is null", {
  testthat::expect_error(plot_common_violin(df = NULL),
                         "Dataframe is null")
})

testthat::test_that("cannot plot common violin, ycol is null", {
  testthat::expect_error(plot_common_violin(df = test_count_matrix,
                                            ycol = NULL),
                         "Cannot generate violin plot, ycol is null")
})

testthat::test_that("cannot plot common violin, ycol is not in data frame", {
  testthat::expect_error(plot_common_violin(df = test_count_matrix,
                                            ycol = 'bad'),
                         "Cannot generate violin plot, ycol is not in data frame")
})

testthat::test_that("cannot plot common violin, ylab is null", {
  testthat::expect_error(plot_common_violin(df = test_count_matrix_narrow,
                                            ycol = 'values'),
                         "Cannot generate violin plot, ylab is null")
})

testthat::test_that("can plot common violin, all samples", {
  p <- plot_common_violin(df = test_count_matrix_narrow,
                          ycol = 'values',
                          ylab = 'Test counts')
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot common violin, with groups", {
  p <- plot_common_violin(df = test_count_matrix_narrow %>% mutate(group = sample),
                          ycol = 'values',
                          ylab = 'Test counts')
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot common violin, one sample", {
  p <- plot_common_violin(df = test_count_matrix_narrow %>% filter(sample == "HELA_T15A_CTRL"),
                          ycol = 'values',
                          ylab = 'Test counts')
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})


testthat::test_that("can plot common violin, single NA", {
  test_single_na_count_matrix_narrow <- test_count_matrix_narrow
  test_single_na_count_matrix_narrow$values[1] <- NA
  testthat::expect_warning(plot_common_violin(df = test_single_na_count_matrix_narrow,
                                              ycol = 'values',
                                              ylab = 'Test counts'),
                           "Removed 1 rows containing missing values")
})

testthat::test_that("can plot common violin, single NA", {
  test_all_na_count_matrix_narrow <- test_count_matrix_narrow
  test_all_na_count_matrix_narrow$values <- NA
  testthat::expect_error(plot_common_violin(df = test_all_na_count_matrix_narrow,
                                            ycol = 'values',
                                            ylab = 'Test counts'),
                           "Cannot plot common violin, all rows contain NAs")
})

testthat::test_that("can plot common violin, too many groups", {
  testthat::expect_message(plot_common_violin(df = test_count_matrix_narrow %>% mutate(group = 1:21),
                                              ycol = 'values',
                                              ylab = 'Test counts'),
                          "Cannot plot more than 12 groups. Setting groups to null.")
})


################################################################################
#* --                                                                      -- *#
#* --                     plot_common_density_ridges()                     -- *#
#* --                                                                      -- *#
################################################################################

testthat::test_that("cannot plot common density ridges, df is null", {
  testthat::expect_error(plot_common_density_ridges(df = NULL),
                         "Dataframe is null")
})

testthat::test_that("cannot plot common density ridges, xcol is null", {
  testthat::expect_error(plot_common_density_ridges(df = test_count_matrix,
                                                    xcol = NULL),
                         "Cannot generate ridgeline denisty plot, xcol is null")
})

testthat::test_that("cannot plot common density ridges, xcol is not in data frame", {
  testthat::expect_error(plot_common_density_ridges(df = test_count_matrix,
                                                    xcol = 'bad'),
                         "Cannot generate ridgeline denisty plot, xcol is not in data frame")
})

testthat::test_that("cannot plot common density ridges, ylab is null", {
  testthat::expect_error(plot_common_density_ridges(df = test_count_matrix_narrow,
                                                    xcol = 'values'),
                         "Cannot generate ridgeline denisty plot, xlab is null")
})

testthat::test_that("can plot density ridges, all samples", {
  p <- plot_common_density_ridges(df = test_count_matrix_narrow,
                                  xcol = 'values',
                                  xlab = 'Test counts')
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot common density ridges, with groups", {
  p <- plot_common_density_ridges(df = test_count_matrix_narrow %>% mutate(group = sample),
                                  xcol = 'values',
                                  xlab = 'Test counts')
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot common density ridges, one sample", {
  p <- plot_common_density_ridges(df = test_count_matrix_narrow %>% filter(sample == "HELA_T15A_CTRL"),
                                  xcol = 'values',
                                  xlab = 'Test counts')
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot common density ridges, single NA", {
  test_single_na_count_matrix_narrow <- test_count_matrix_narrow
  test_single_na_count_matrix_narrow$values[1] <- NA
  testthat::expect_warning(plot_common_density_ridges(df = test_single_na_count_matrix_narrow,
                                                      xcol = 'values',
                                                      xlab = 'Test counts'),
                           "Removed 1 rows containing missing values")
})

testthat::test_that("can plot common density ridges, multiple NA", {
  test_all_na_count_matrix_narrow <- test_count_matrix_narrow
  test_all_na_count_matrix_narrow$values <- NA
  testthat::expect_error(plot_common_density_ridges(df = test_all_na_count_matrix_narrow,
                                                    xcol = 'values',
                                                    xlab = 'Test counts'),
                         "Cannot plot ridgeline denisty plot , all rows contain NAs")
})

testthat::test_that("can plot common density ridges, too many groups", {
  testthat::expect_message(plot_common_density_ridges(df = test_count_matrix_narrow %>% mutate(group = 1:21),
                                                      xcol = 'values',
                                                      xlab = 'Test counts'),
                           "Cannot plot more than 12 groups. Setting groups to null.")
})
