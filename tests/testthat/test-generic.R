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
#* --                          check_dataframe()                          -- *#
#* --                                                                     -- *#
###############################################################################

mock_df <- data.frame(a = 1:2, b = 1:2)

testthat::test_that("data frame is valid", {
  testthat::expect_true(check_dataframe(data = mock_df))
})

testthat::test_that("data frame is not valid when data is null", {
  testthat::expect_error(check_dataframe(data = NULL),
                         "Dataframe is null.")
})

testthat::test_that("data frame is not valid when empty", {
  testthat::expect_error(check_dataframe(data = data.frame()),
                         "Dataframe has no rows.")
})

testthat::test_that("data frame is not valid when index exceeds limits", {
  testthat::expect_error(
      check_dataframe(data = mock_df, indices = as.integer(3)),
                      "Index exceeds dataframe limits: 3, 2"
    )
})
testthat::test_that("data frame is not valid when index is not an integer", {
  testthat::expect_error(
    check_dataframe(data = mock_df, indices = 'D'),
                    "Cannot convert index to integer:")
})

testthat::test_that("data frame is not valid when it contains NA values", {
  testthat::expect_error(
    check_dataframe(data = data.frame(a = c(1,NA)), check_na = TRUE),
    "Dataframe contains NA values.")
})

testthat::test_that("data frame is not valid when it contains NaN values", {
  testthat::expect_error(
    check_dataframe(data = data.frame(a = c(1,NaN)), check_nan = TRUE),
    "Dataframe contains NaN values.")
})

###############################################################################
#* --                                                                     -- *#
#* --                   check_is_numeric_and_is_integer()                 -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("returns TRUE if value is numeric and an integer", {
  testthat::expect_true(check_is_numeric_and_is_integer(1))
})

testthat::test_that("returns FLASE if value is NULL", {
  testthat::expect_false(check_is_numeric_and_is_integer(NULL))
})

testthat::test_that("returns FLASE if value is NA", {
  testthat::expect_false(check_is_numeric_and_is_integer(NA))
})


testthat::test_that("returns FLASE if value is numeric and not an integer", {
  testthat::expect_false(check_is_numeric_and_is_integer(1.2))
})

testthat::test_that("returns FLASE if value is not numeric and not an integer", {
  testthat::expect_false(check_is_numeric_and_is_integer('x'))
})


###############################################################################
#* --                                                                     -- *#
#* --                        process_column_indices()                     -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can process column indices", {
  testthat::expect_equal(process_column_indices('1,3:5,7-9'), c(1,3,4,5,7,8,9))
})

testthat::test_that("can't process column indices when null", {
  x <- NULL
  testthat::expect_error(process_column_indices(x),
                         "Cannot process columns")
})

testthat::test_that("can't process column indices with non-integers", {
  testthat::expect_error(process_column_indices('1,x'),
                         "Column indices contain a non-integer value:")
})

testthat::test_that("can't process column indices with non-integers in range", {
  testthat::expect_error(process_column_indices('1,x-2'),
                         "Column indices contain a non-integer value in range")
})

###############################################################################
#* --                                                                     -- *#
#* --                          add_pseudocount()                          -- *#
#* --                                                                     -- *#
###############################################################################

test_pseudocount_data <- data.frame('id' = 'sg1', 'gene' = 'g1', 's1' = 1, 's2' = 0)

testthat::test_that("cannot add pseudocount, data is null", {
  testthat::expect_error(add_pseudocount(data = NULL),
                         "Cannot add pseudocount, data is null")
})

testthat::test_that("cannot add pseudocount, pseudocount is null", {
  testthat::expect_error(add_pseudocount(data = test_pseudocount_data,
                                         pseudocount = NULL),
                         "Cannot add pseudocount, pseudocount is null")
})

testthat::test_that("can add pseudocount, indices is null", {
  testthat::expect_warning(add_pseudocount(data = test_pseudocount_data,
                                           indices = NULL),
                           "No indices given, adding pseudocount to all indices")
})

testthat::test_that("cannot add pseudocount, data is null", {
  testthat::expect_error(add_pseudocount(data = test_pseudocount_data,
                                         pseudocount = 'x'),
                         "Cannot add pseudocount, pseudocount is not numeric")
})

###############################################################################
#* --                                                                     -- *#
#* --                          compare_annotations()                      -- *#
#* --                                                                     -- *#
###############################################################################

annotations_x <- data.frame('gene' = rep('g1', 3), 'guide' = c('sg1', 'SG10', '12mg'))
annotations_y <- data.frame('guide' = c('sg1', 'SG10', '12mg'), 'gene' = c(rep('g1', 2), 'g2'))

testthat::test_that("can compare annorations", {
  testthat::expect_true(compare_annotations(x = annotations_x,
                                            y = annotations_x,
                                            x_id_column = 2,
                                            x_gene_column = 1,
                                            y_id_column = 2,
                                            y_gene_column = 1))
})

testthat::test_that("cannot compare annorations, mismatch", {
  testthat::expect_error(compare_annotations(x = annotations_x,
                                             y = annotations_y,
                                             x_id_column = 2,
                                             x_gene_column = 1),
                         "sgRNA IDs and gene names in data frames do not match")
})

testthat::test_that("cannot compare annorations, x is null", {
  testthat::expect_error(compare_annotations(x = NULL,
                                             y = annotations_y),
                         "Cannot compare data frames, x is null")
})

testthat::test_that("cannot compare annorations, x is null", {
  testthat::expect_error(compare_annotations(x = annotations_x,
                                             y = NULL),
                         "Cannot compare data frames, y is null")
})

testthat::test_that("cannot compare annorations, number of rows differ", {
  testthat::expect_error(compare_annotations(x = annotations_x,
                                             y = annotations_x[1:2,],
                                             x_id_column = 2,
                                             x_gene_column = 1,
                                             y_id_column = 2,
                                             y_gene_column = 1),
                         "Cannot compare compare data frames, number of guides differ")
})

###############################################################################
#* --                                                                     -- *#
#* --                      convert_variable_to_integer()                  -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can convert variable to integer", {
  testthat::expect_equal(convert_variable_to_integer('1'), 1)
})

testthat::test_that("cannot convert variable to integer, x is null", {
  testthat::expect_error(convert_variable_to_integer(NULL),
                         "Cannot convert variable to integer, x is null")
})

testthat::test_that("cannot convert variable to integer", {
  testthat::expect_error(convert_variable_to_integer('1.3'),
                         "Could not convert value to integer")
})

###############################################################################
#* --                                                                     -- *#
#* --                      convert_variable_to_numeric()                  -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can convert variable to numeric", {
  testthat::expect_equal(convert_variable_to_numeric('1.2'), 1.2)
})

testthat::test_that("cannot convert variable to numeric, x is null", {
  testthat::expect_error(convert_variable_to_numeric(NULL),
                         "Cannot convert variable to numeric, x is null")
})

testthat::test_that("cannot convert variable to numeric", {
  testthat::expect_error(convert_variable_to_numeric('xyz'),
                         "Could not convert value to numeric")
})

###############################################################################
#* --                                                                     -- *#
#* --                         get_column_indices()                        -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can get unsorted column indices", {
  testthat::expect_equal(get_column_indices(n = c('guide', 'gene'),
                                            df = annotations_x), c(2, 1))
})

testthat::test_that("can get sorted column indices", {
  testthat::expect_equal(get_column_indices(n = c('guide', 'gene'),
                                            df = annotations_x,
                                            sorted = T), c(1, 2))
})

testthat::test_that("cannot get column indices, column names null", {
  testthat::expect_error(get_column_indices(n = NULL,
                                            df = annotations_x),
                         "Cannot get column indices, list of column names is null")
})

testthat::test_that("cannot get column indices, column names empty", {
  testthat::expect_error(get_column_indices(n = vector(),
                                            df = annotations_x),
                         "Cannot get column indices, list of column names is empty")
})

testthat::test_that("cannot get column indices, column does not exist", {
  testthat::expect_error(get_column_indices(n = c('no'),
                                            df = annotations_x),
                         "Could not get column index for")
})

testthat::test_that("cannot get column indices, indices not sample length as column names", {
  testthat::expect_error(get_column_indices(n = c('gene', 'gene'),
                                            df = annotations_x),
                         "Column indices is not the same length as column names")
})

###############################################################################
#* --                                                                     -- *#
#* --                         average_replicates()                        -- *#
#* --                                                                     -- *#
###############################################################################

test_count_matrix_file <- system.file("testdata", "test_count_matrix.tsv", package = 'rcrispr')
test_count_matrix <- read_count_matrix_file(filepath = test_count_matrix_file,
                                            id_column = 1,
                                            gene_column = 2,
                                            count_column = 3:9,
                                            processed = T)

testthat::test_that("cannot average replicates, gene_column null", {
  testthat::expect_error(average_replicates(data = data.frame(),
                                            gene_column = NULL),
                         "Cannot average replicates, gene_column is null")
})

testthat::test_that("cannot average replicates, data_columns null", {
  testthat::expect_error(average_replicates(data = data.frame(),
                                            gene_column = 1,
                                            data_columns = NULL),
                         "Cannot average replicates, data_columns is null")
})

testthat::test_that("cannot average replicates, data_columns null", {
  testthat::expect_error(average_replicates(data = test_count_matrix,
                                            gene_column = 4,
                                            data_columns = 3:9),
                         "Cannot average replicates, gene_column is within data_columns")
})

testthat::test_that("can average replicates", {
  testthat::expect_snapshot(average_replicates(data = test_count_matrix,
                                               gene_column = 2,
                                               data_columns = 3:9))
})
