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
