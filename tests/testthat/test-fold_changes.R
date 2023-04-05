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
#* --                           calculate_lfc()                           -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can calculate lfc using all columns", {
  result <- data.frame('sgRNA' = test_count_matrix$sgRNA,
                       'gene' = test_count_matrix$gene,
                       'HELA_T15A_OLA' = c(-0.4043491, -5.1565045, -1.1023208),
                       'HELA_T15B_OLA' = c(0.19840574, -0.11211037, 1.18698215),
                       'HELA_T15C_OLA' = c(-0.9091663, -5.1565045, -1.9254430))
  testthat::expect_equal(calculate_lfc(data = test_count_matrix,
                                       gene_column = 2,
                                       control_indices = 4:6,
                                       treatment_indices = 7:9), result)
})

testthat::test_that("can calculate lfc using one treatment", {
  result <- data.frame('sgRNA' = test_count_matrix$sgRNA,
                       'gene' = test_count_matrix$gene,
                       'HELA_T15B_OLA' = c(0.19840574, -0.11211037, 1.18698215))
  testthat::expect_equal(calculate_lfc(data = test_count_matrix,
                                       gene_column = 2,
                                       control_indices = 4:6,
                                       treatment_indices = 8), result)
})

testthat::test_that("can calculate lfc using one control", {
  result <- data.frame('sgRNA' = test_count_matrix$sgRNA,
                       'gene' = test_count_matrix$gene,
                       'HELA_T15A_OLA' = c(-0.41550042, 0.00000000, -1.26527290),
                       'HELA_T15B_OLA' = c(0.18725442, 5.04439412, 1.02403007),
                       'HELA_T15C_OLA' = c(-0.92031760, 0.00000000, -2.08839512))
  testthat::expect_equal(calculate_lfc(data = test_count_matrix,
                                       gene_column = 2,
                                       control_indices = 4,
                                       treatment_indices = 7:9), result)
})

testthat::test_that("can calculate lfc using one control and one treatment", {
  result <- data.frame('sgRNA' = test_count_matrix$sgRNA,
                       'gene' = test_count_matrix$gene,
                       'HELA_T15A_OLA' = c(-0.41550042, 0.00000000, -1.26527290))
  testthat::expect_equal(calculate_lfc(data = test_count_matrix,
                                       gene_column = 2,
                                       control_indices = 4,
                                       treatment_indices = 7), result)
})

testthat::test_that("cannot calculate lfc, data is null", {
  testthat::expect_error(calculate_lfc(data = NULL),
                         "Cannot calculate LFCs, data is null")
})

testthat::test_that("cannot calculate lfc, control_indices is null", {
  testthat::expect_error(calculate_lfc(data = test_count_matrix,
                                       control_indices = NULL),
                         "Cannot calculate LFCs, control_indices is null")
})

testthat::test_that("cannot calculate lfc, treatment_indices is null", {
  testthat::expect_error(calculate_lfc(data = test_count_matrix,
                                       control_indices = 4:6,
                                       treatment_indices = NULL),
                         "Cannot calculate LFCs, treatment_indices is null")
})

testthat::test_that("cannot calculate lfc, pseudocount is null", {
  testthat::expect_error(calculate_lfc(data = test_count_matrix,
                                       control_indices = 4:6,
                                       treatment_indices = 7:9,
                                       pseudocount = NULL),
                         "Cannot calculate LFCs, pseudocount is null")
})

testthat::test_that("cannot calculate lfc, pseudocount is null", {
  testthat::expect_error(calculate_lfc(data = test_count_matrix,
                                       control_indices = 4:6,
                                       treatment_indices = 7:9,
                                       pseudocount = 'x'),
                         "Cannot calculate LFCs, pseudocount is not numeric")
})

testthat::test_that("cannot calculate lfc, control and treatment indices overlap", {
  testthat::expect_error(calculate_lfc(data = test_count_matrix,
                                       control_indices = 4:6,
                                       treatment_indices = 6:9),
                         "Cannot calculate LFC, duplicated indices")
})

###############################################################################
#* --                                                                     -- *#
#* --                         calculate_gene_lfc()                        -- *#
#* --                                                                     -- *#
###############################################################################

test_gene_count_matrix <- data.frame('sgRNA' = c('sg1', 'sg2', 'sg3'),
                                     'gene' = c('g2', 'g1', 'g2'),
                                     test_count_matrix[,7:9])

testthat::test_that("can calculate gene lfc using all columns", {
  result <- data.frame('gene' = c('g1', 'g2'),
                       'HELA_T15A_OLA' = c(0, 234.5),
                       'HELA_T15B_OLA' = c(16, 492),
                       'HELA_T15C_OLA' = c(0, 159.5))
  testthat::expect_equal(calculate_gene_lfc(data = test_gene_count_matrix,
                                            id_column = 1,
                                            gene_column = 2), result)
})

testthat::test_that("can calculate gene lfc using one column", {
  result <- data.frame('gene' = c('g1', 'g2'),
                       'HELA_T15B_OLA' = c(16, 492))
  testthat::expect_equal(calculate_gene_lfc(data = test_gene_count_matrix,
                                            id_column = 1,
                                            gene_column = 2,
                                            sample_columns = 4), result)
})

testthat::test_that("can calculate gene lfc using multiple columns", {
  result <- data.frame('gene' = c('g1', 'g2'),
                       'HELA_T15A_OLA' = c(0, 234.5),
                       'HELA_T15C_OLA' = c(0, 159.5))
  testthat::expect_equal(calculate_gene_lfc(data = test_gene_count_matrix,
                                            id_column = 1,
                                            gene_column = 2,
                                            sample_columns = c(3,5)), result)
})

testthat::test_that("cannot calculate gene lfc, data is null", {
  testthat::expect_error(calculate_gene_lfc(data = NULL),
                         "Cannot calculate gene LFCs, data is null")
})

###############################################################################
#* --                                                                     -- *#
#* --                read_fold_change_matrix_file()                       -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can read sgRNA-level fold change matrix file", {
  testthat::expect_snapshot(read_fold_change_matrix_file(test_count_matrix_file,
                                                         id_column = 1,
                                                         gene_column = 2,
                                                         fc_column = 3:5))
})

testthat::test_that("can read gene-level fold change matrix file", {
  testthat::expect_snapshot(read_fold_change_matrix_file(test_count_matrix_file,
                                                         id_column = NULL,
                                                         gene_column = 2,
                                                         fc_column = 3:5,
                                                         is_gene = T))
})

testthat::test_that("can read sgRNA-level fold change matrix file and return processed data", {
  testthat::expect_snapshot(read_fold_change_matrix_file(test_count_matrix_file,
                                                         id_column = 1,
                                                         gene_column = 2,
                                                         fc_column = 3:5,
                                                         processed = T))
})

testthat::test_that("can read gene-level fold change matrix fileand return processed data", {
  testthat::expect_snapshot(read_fold_change_matrix_file(test_count_matrix_file,
                                                         id_column = NULL,
                                                         gene_column = 2,
                                                         fc_column = 3:5,
                                                         is_gene = T,
                                                         processed = T))
})

testthat::test_that("cannot calculate gene lfc, id_column is null with sgRNA fcs", {
  testthat::expect_error(read_fold_change_matrix_file(test_count_matrix_file,
                                                      id_column = NULL,
                                                      gene_column = 2,
                                                      fc_column = 3:5,
                                                      is_gene = F),
                         "Cannot read in a fold change matrix, is_gene is FALSE with NULL id_column")
})

testthat::test_that("cannot calculate gene lfc, gene_column is null with sgRNA fcs", {
  testthat::expect_error(read_fold_change_matrix_file(test_count_matrix_file,
                                                      id_column = 1,
                                                      gene_column = NULL,
                                                      fc_column = 3:5,
                                                      is_gene = F),
                         "Cannot read in a fold change matrix, is_gene is FALSE with NULL gene_column")
})

testthat::test_that("cannot calculate gene lfc, gene_column is null with gene fcs", {
  testthat::expect_error(read_fold_change_matrix_file(test_count_matrix_file,
                                                      id_column = 1,
                                                      gene_column = NULL,
                                                      fc_column = 3:5,
                                                      is_gene = T),
                         "Cannot read in a fold change matrix, is_gene is TRUE with NULL gene_column")
})


testthat::test_that("cannot calculate gene lfc, no header", {
  testthat::expect_error(read_fold_change_matrix_file(test_count_matrix_file,
                                                      id_column = 1,
                                                      gene_column = 2,
                                                      fc_column = 3:5,
                                                      file_header = F),
                         "Cannot read in a fold change matrix file without a header")
})

testthat::test_that("cannot calculate gene lfc, sgrna-level indices overlap", {
  testthat::expect_error(read_fold_change_matrix_file(test_count_matrix_file,
                                                      id_column = 1,
                                                      gene_column = 2,
                                                      fc_column = 2:5),
                         "Cannot read fold change matrix, duplicate column indices")
})

testthat::test_that("cannot calculate gene lfc, gene-level indices overlap", {
  testthat::expect_error(read_fold_change_matrix_file(test_count_matrix_file,
                                                      id_column = NULL,
                                                      gene_column = 2,
                                                      fc_column = 2:5,
                                                      is_gene = T),
                         "Cannot read fold change matrix, duplicate column indices")
})

testthat::test_that("can read gene-level fold change matrix file with numeric sample name", {
  test_count_matrix_file_with_numeric_sample <- system.file("testdata", "test_count_matrix_with_numeric_sample.tsv", package = 'rcrispr')
  testthat::expect_snapshot(read_fold_change_matrix_file(test_count_matrix_file_with_numeric_sample,
                                                         id_column = 1,
                                                         gene_column = 2,
                                                         fc_column = 3:5,
                                                         is_gene = T,
                                                         processed = T))
})
