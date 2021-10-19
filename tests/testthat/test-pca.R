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

test_count_matrix_file <- system.file("testdata", "BAGEL_HAP1-TKOv3-EXAMPLE_4d0fc08.tsv", package = 'rcrispr')
test_count_matrix <- read_count_matrix_file(filepath = test_count_matrix_file,
                                            id_column = 1,
                                            gene_column = 2,
                                            count_column = 3:12,
                                            processed = T)
test_count_matrix <- test_count_matrix[1:500,]

###############################################################################
#* --                                                                     -- *#
#* --                             prepare_pca()                           -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot prepare pca, df is null", {
  testthat::expect_error(prepare_pca(df = NULL),
                         "Dataframe is null")
})

testthat::test_that("can prepare pca", {
  testthat::expect_snapshot(prepare_pca(df = test_count_matrix[3:6]))
})

testthat::test_that("can prepare pca with scaling", {
  testthat::expect_snapshot(prepare_pca(df = test_count_matrix[3:6],
                                        scaled = T))
})

testthat::test_that("can prepare pca with transform", {
  testthat::expect_snapshot(prepare_pca(df = test_count_matrix[3:6],
                                        transform = T))
})

testthat::test_that("can prepare pca with log transform", {
  testthat::expect_snapshot(prepare_pca(df = test_count_matrix[3:6],
                                        log_transform = T))
})

###############################################################################
#* --                                                                     -- *#
#* --                               plot_pca()                            -- *#
#* --                                                                     -- *#
###############################################################################

test_pca <- prepare_pca(df = test_count_matrix[,3:12],
                        scaled = T,
                        transform = T,
                        log_transform = T)
test_pca <- as.data.frame(test_pca[['data']]$x)

testthat::test_that("cannot plot pca, pca_x not in df", {
  testthat::expect_error(plot_pca(df = test_pca, pc_x = 'bad'),
                         "Cannot generate PCA plot, pc_x is not in data frame")
})

testthat::test_that("cannot plot pca, pca_y not in df", {
  testthat::expect_error(plot_pca(df = test_pca, pc_y = 'bad'),
                         "Cannot generate PCA plot, pc_y is not in data frame")
})

testthat::test_that("can plot pca", {
  p <- plot_pca(df = test_pca)
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot pca with color", {
  test_pca_with_color <- test_pca %>%
    mutate(color = c('T0', rep('T3', 3), rep('T18', 3), rep('T18_Starved', 3)))
  p <- plot_pca(df = test_pca_with_color)
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot pca, message if more than 8 colors", {
  test_pca_with_too_many_color <- test_pca %>% mutate(color = c(1:10))
  testthat::expect_message(plot_pca(df = test_pca_with_too_many_color),
                         "Cannot plot more than 8 unique items in color. Setting color to null.")
})

testthat::test_that("can plot pca with shape", {
  test_pca_with_shape <- test_pca %>%
    mutate(shape = c('T0', rep('T3', 3), rep('T18', 3), rep('T18_Starved', 3)))
  p <- plot_pca(df = test_pca_with_shape)
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("can plot pca with color and shape", {
  test_pca_with_color_and_shape <- test_pca %>%
    mutate(color = c('T0', rep('T3', 3), rep('T18', 3), rep('T18_Starved', 3))) %>%
    mutate(shape = c('T0', rep('T3', 3), rep('T18', 6)))
  p <- plot_pca(df = test_pca_with_color_and_shape)
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})
