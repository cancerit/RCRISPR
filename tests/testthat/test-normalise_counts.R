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

