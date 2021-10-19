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

test_total_reads <- c(1050,1934,1995,1673,1558,1338,1022)
names(test_total_reads) <-  colnames(test_count_matrix)[3:9]
test_raw_stats <- count_matrix_stats(count_matrix = test_count_matrix,
                                     count_column = 3:9,
                                     total_reads = test_total_reads)

###############################################################################
#* --                                                                     -- *#
#* --                       plot_mapping_statistics()                     -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot plot mapping statistics, df is null", {
  testthat::expect_error(plot_mapping_statistics(df = NULL),
                         "Dataframe is null")
})

testthat::test_that("cannot plot mapping statistics, total_reads does not exist", {
  testthat::expect_error(plot_mapping_statistics(df = test_raw_stats %>% select(-total_reads)),
                         "Column total_reads does not exist in data frame")
})

testthat::test_that("cannot plot mapping statistics, total_counts does not exist", {
  testthat::expect_error(plot_mapping_statistics(df = test_raw_stats %>% select(-total_counts)),
                         "Column total_counts does not exist in data frame")
})

testthat::test_that("cannot plot mapping statistics, pct_mapped_reads does not exist", {
  testthat::expect_error(plot_mapping_statistics(df = test_raw_stats %>% select(-pct_mapped_reads)),
                         "Column pct_mapped_reads does not exist in data frame")
})

testthat::test_that("can plot mapping statistics, warning for NA total_reads", {
  test_single_na_total_reads <- test_raw_stats
  test_single_na_total_reads$total_reads[1] <- NA
  testthat::expect_warning(plot_mapping_statistics(df = test_single_na_total_reads),
                         "Removed 1 rows containing missing values")
})

testthat::test_that("cannot plot mapping statistics, all total_reads are NA", {
  test_single_na_total_reads <- test_raw_stats
  test_single_na_total_reads$total_reads[1] <- NA
  testthat::expect_warning(plot_mapping_statistics(df = test_single_na_total_reads),
                           "Removed 1 rows containing missing values")
})

testthat::test_that("cannot plot mapping statistics, all total_reads are NA", {
  test_all_na_total_reads <- test_raw_stats
  test_all_na_total_reads$total_reads <- NA
  testthat::expect_error(plot_mapping_statistics(df = test_all_na_total_reads),
                           "Cannot plot mapping statistics, all total_reads are NA")
})

testthat::test_that("can plot mapping statistics, all samples", {
  p <- plot_mapping_statistics(df = test_raw_stats)
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})


testthat::test_that("can plot mapping statistics, one sample", {
  p <- plot_mapping_statistics(df = test_raw_stats %>% filter(sample == 'HELA_T0'))
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

###############################################################################
#* --                                                                     -- *#
#* --                         plot_common_barplot()                       -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can plot common barplot, one sample", {
  p <- plot_common_barplot(df = test_raw_stats %>% filter(sample == 'HELA_T0'),
                           xcol = 'sample',
                           ycol = 'pct_zero_sgrnas')
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot common barplot, multiple samples", {
  p <- plot_common_barplot(df = test_raw_stats,
                           xcol = 'sample',
                           ycol = 'pct_zero_sgrnas')
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot common barplot, remove NA", {
  test_single_na_raw_stats <- test_raw_stats
  test_single_na_raw_stats$sample[1] <- NA
  test_single_na_raw_stats$pct_zero_sgrnas[2] <- NA
  testthat::expect_warning(plot_common_barplot(df = test_single_na_raw_stats,
                                               xcol = 'sample',
                                               ycol = 'pct_zero_sgrnas'),
                           "Removed 2 rows containing missing values")
})

testthat::test_that("can plot common barplot, remove NA", {
  test_all_na_raw_stats <- test_raw_stats
  test_all_na_raw_stats$sample <- NA
  testthat::expect_error(plot_common_barplot(df = test_all_na_raw_stats,
                                               xcol = 'sample',
                                               ycol = 'pct_zero_sgrnas'),
                           "Cannot plot mapping statistics, all rows contain NAs")
})

testthat::test_that("can plot common barplot, with xlab and ylab", {
  p <- plot_common_barplot(df = test_raw_stats,
                           xcol = 'sample',
                           ycol = 'pct_zero_sgrnas',
                           ylab = 'Guides with no reads assigned (%)',
                           xlab = 'Sample')
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("cannot plot common barplot, df is null", {
  testthat::expect_error(plot_common_barplot(df = NULL),
                         "Dataframe is null")
})

testthat::test_that("cannot plot common barplot, ycol is null", {
  testthat::expect_error(plot_common_barplot(df = test_raw_stats,
                                             ycol = NULL),
                         "Cannot generate bar plot, ycol is null")
})

testthat::test_that("cannot plot common barplot, ycol not in data frame", {
  testthat::expect_error(plot_common_barplot(df = test_raw_stats,
                                             ycol = 'bad'),
                         "Cannot generate bar plot, ycol is not in data frame")
})

testthat::test_that("cannot plot common barplot, xcol is null", {
  testthat::expect_error(plot_common_barplot(df = test_raw_stats,
                                             ycol = 'pct_zero_sgrnas',
                                             xcol = NULL),
                         "Cannot generate bar plot, xcol is null")
})

testthat::test_that("cannot plot common barplot, xcol not in data frame", {
  testthat::expect_error(plot_common_barplot(df = test_raw_stats,
                                             ycol = 'pct_zero_sgrnas',
                                             xcol = 'bad'),
                         "Cannot generate bar plot, xcol is not in data frame")
})

testthat::test_that("can plot common barplot, with groups", {
  p <- plot_common_barplot(df = test_raw_stats %>% mutate(group = sample),
                          ycol = 'pct_zero_sgrnas',
                          xcol = 'sample')
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot common barplot, with groups", {
  testthat::expect_message(plot_common_barplot(df = rbind(test_raw_stats, test_raw_stats) %>%
                                                      mutate(group = c(1:14)),
                           ycol = 'pct_zero_sgrnas',
                           xcol = 'sample'),
                           "Cannot plot more than 12 groups. Setting groups to null.")
})



