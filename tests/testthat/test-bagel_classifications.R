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

ess <- scan(system.file("testdata", "BAGEL_CEG50_5aea1eb.tsv", package = 'rcrispr'), character())
noness <- scan(system.file("testdata", "BAGEL_NEG50_5aea1eb.tsv", package = 'rcrispr'), character())
test_count_matrix_classified <- test_count_matrix[test_count_matrix$gene %in% ess | test_count_matrix$gene %in% noness, ]
test_count_matrix_unclassified <- test_count_matrix[which(!test_count_matrix$gene %in% ess & !test_count_matrix$gene %in% noness), ]
test_count_matrix <- rbind(test_count_matrix_classified, test_count_matrix_unclassified[1:60,])

test_classified_df <- add_bagel_classifications(data = test_count_matrix,
                                                gene_column = 2,
                                                ess = ess,
                                                noness = noness)
test_classified_sgrna <-  test_classified_df %>% gather(sample, values, -sgRNA, -gene, -classification)
test_classified_gene <-  test_classified_df %>% gather(sample, values, -sgRNA, -gene, -classification)

test_sgrna_fc_matrix <- calculate_lfc(test_count_matrix, control_indices = 3, treatment_indices = 4:6)
test_gene_fc_matrix <- calculate_gene_lfc(test_sgrna_fc_matrix, sample_columns = 3:5)
test_classified_fc_gene <-  add_bagel_classifications(data = test_gene_fc_matrix,
                                                      gene_column = 1,
                                                      ess = ess,
                                                      noness = noness)

test_classified_fc_gene_narrow <- test_classified_fc_gene %>% gather(sample, values, -gene, -classification)
test_avg_classified_fc_gene <- average_replicates(data = test_classified_fc_gene,
                                             gene_column = 1,
                                             data_columns = 2:4)
test_avg_classified_fc_gene <- add_bagel_classifications(data = test_avg_classified_fc_gene,
                                                gene_column = 1,
                                                ess = ess,
                                                noness = noness)

test_essentiality_data <- prepare_essentiality_data(data = test_avg_classified_fc_gene)
test_screen_roc <- pROC::roc(test_essentiality_data[['essentiality']],
                             test_essentiality_data[['modified_predictor']])

###############################################################################
#* --                                                                     -- *#
#* --                       add_bagel_classifications()                   -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can add bagel_classification", {
  testthat::expect_snapshot(add_bagel_classifications(data = test_count_matrix,
                                                   gene_column = 2,
                                                   ess = ess,
                                                   noness = noness))
})

testthat::test_that("cannot add bagel_classification, data is null", {
  testthat::expect_error(add_bagel_classifications(data = NULL),
                         "Cannot add BAGEL classification, data is null.")
})

testthat::test_that("cannot add bagel_classification, gene_column is null", {
  testthat::expect_error(add_bagel_classifications(data = test_count_matrix,
                                                   gene_column = NULL),
                         "Cannot add BAGEL classification, gene_column is null.")
})

testthat::test_that("cannot add bagel_classification, ess is null", {
  testthat::expect_error(add_bagel_classifications(data = test_count_matrix,
                                                   ess = NULL),
                         "Cannot add BAGEL classification, ess is null.")
})

testthat::test_that("cannot add bagel_classification, noness is null", {
  testthat::expect_error(add_bagel_classifications(data = test_count_matrix,
                                                   ess = ess,
                                                   noness = NULL),
                         "Cannot add BAGEL classification, noness is null.")
})

###############################################################################
#* --                                                                     -- *#
#* --                       get_bagel_statistics()                        -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can get bagel statistics sgrna-level", {
  testthat::expect_snapshot(get_bagel_statistics(data = test_classified_sgrna))
})

testthat::test_that("can get bagel statistics gene-level", {
  testthat::expect_snapshot(get_bagel_statistics(data = test_classified_gene, is_gene = T))
})

testthat::test_that("can get bagel statistics gene-level fold changes", {
  testthat::expect_snapshot(get_bagel_statistics(data = test_classified_fc_gene_narrow, is_gene = T, is_fc = T))
})

testthat::test_that("cannot get bagel statistics, data is null", {
  testthat::expect_error(get_bagel_statistics(data = NULL),
                         "Cannot add BAGEL classification, data is null")
})

testthat::test_that("cannot get bagel statistics, missing required columns", {
  testthat::expect_error(get_bagel_statistics(data = test_classified_sgrna %>% select(-classification)),
                         "Cannot add BAGEL classification, required columns")
})

testthat::test_that("cannot get bagel statistics gene-level fold changes, no essentials", {
  testthat::expect_error(get_bagel_statistics(data = test_classified_fc_gene_narrow %>% filter(classification != 'essential'),
                                              is_gene = T, is_fc = T),
                         "Cannot calculate NNMD and Glass' delta as no essential genes were found in")
})

testthat::test_that("cannot get bagel statistics gene-level fold changes, no essentials", {
  testthat::expect_error(get_bagel_statistics(data = test_classified_fc_gene_narrow %>% filter(classification != 'nonessential'),
                                              is_gene = T, is_fc = T),
                         "Cannot calculate Glass' delta as no non-essential genes were found in")
})

###############################################################################
#* --                                                                     -- *#
#* --                       prepare_essentiality_data()                   -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can prepare essentiality data", {
  testthat::expect_snapshot(prepare_essentiality_data(data = test_avg_classified_fc_gene))
})


testthat::test_that("cannot prepare essentiality data, data is null", {
  testthat::expect_error(prepare_essentiality_data(data = NULL),
                         "Dataframe is null")
})

testthat::test_that("cannot prepare essentiality data, threshold is null", {
  testthat::expect_error(prepare_essentiality_data(data = test_avg_classified_fc_gene,
                                                   threshold = NULL),
                         "Cannot prepare essentiality data, threshold is null")
})

testthat::test_that("cannot prepare essentiality data, threshold is null", {
  testthat::expect_error(prepare_essentiality_data(data = test_avg_classified_fc_gene,
                                                   threshold = 'a'),
                         "Cannot prepare essentiality data, threshold is not numeric")
})

testthat::test_that("cannot prepare essentiality data, threshold is null", {
  testthat::expect_error(prepare_essentiality_data(data = test_avg_classified_fc_gene %>% filter(classification == 'unknown')),
                         "Cannot scale data, no essential or non-essential genes found")
})

testthat::test_that("cannot prepare essentiality data, threshold is null", {
  testthat::expect_error(prepare_essentiality_data(data = test_avg_classified_fc_gene %>% filter(classification != 'essential')),
                         "Cannot scale data, no essential genes found")
})

###############################################################################
#* --                                                                     -- *#
#* --                           process_roc()                             -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can process ROC", {
  testthat::expect_snapshot(process_roc(test_screen_roc,
                                        test_essentiality_data[['min']]))
})

testthat::test_that("cannot process ROC, ROC object is null", {
  testthat::expect_error(process_roc(roc_obj = NULL),
                         "Cannot process ROC, roc_obj is null")
})

testthat::test_that("cannot process ROC, predictor_min is null", {
  testthat::expect_error(process_roc(roc_obj = test_screen_roc,
                                     predictor_min = NULL),
                         "Cannot process ROC, predictor_min is null")
})

testthat::test_that("cannot process ROC, predictor_min is not numeric", {
  testthat::expect_error(process_roc(roc_obj = test_screen_roc,
                                     predictor_min = 'a'),
                         "Cannot process ROC, predictor_min is not numeric")
})

###############################################################################
#* --                                                                     -- *#
#* --                             plot_roc()                              -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can plot ROC", {
  p <- plot_roc(roc_obj = test_screen_roc)
  testthat::expect_snapshot(p$data)
  testthat::expect_snapshot(p$layers)
})

testthat::test_that("cannot plot ROC, roc_obj is null", {
  testthat::expect_error(plot_roc(roc_obj = NULL),
                         "Cannot plot ROC, roc_obj is null")
})

###############################################################################
#* --                                                                     -- *#
#* --                         average_guide_bfs()                         -- *#
#* --                                                                     -- *#
###############################################################################

test_bf_matrix_file <- system.file("testdata", "test_bf_gene.tsv", package = 'rcrispr')
test_bf_matrix <- read.delim(test_bf_matrix_file, check.names =F, header = T, sep = "\t")

testthat::test_that("cannot average guide BFs, gene_column null", {
  testthat::expect_error(average_guide_bfs(data = data.frame(),
                                           gene_column = NULL),
                         "Cannot average guide BFs, gene_column is null")
})

testthat::test_that("cannot average guide BFs, data_columns null", {
  testthat::expect_error(average_guide_bfs(data = data.frame(),
                                           gene_column = 1,
                                           data_columns = NULL),
                         "Cannot average guide BFs, data_columns is null")
})

testthat::test_that("cannot average guide BFs, data_columns null", {
  testthat::expect_error(average_guide_bfs(data = test_count_matrix,
                                           gene_column = 1,
                                           data_columns = 1),
                         "Cannot average guide BFs, gene_column is within data_columns")
})

testthat::test_that("can average guide BFs", {
  testthat::expect_snapshot(average_guide_bfs(data = test_bf_matrix,
                                              gene_column = 1,
                                              data_columns = 2))
})

