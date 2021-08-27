testthat::test_that("read sample metadata", {
  test_sample_metadata <- read_sample_metadata_file(
    filepath = system.file("testdata", "test_sample_metadata.tsv", package = 'rcrispr'),
    filename_column = 5,
    label_column = 1,
    plasmid_column = 2,
    control_column = 3,
    treatment_column = 4,
    reads_column = 7,
    group_column = 6)
  slot(test_sample_metadata, 'filepath', check = TRUE) <- 'test'
  testthat::expect_snapshot(test_sample_metadata)
})

testthat::test_that("get processed sample metadata with reads and group", {
  test_sample_metadata <- read_sample_metadata_file(
    filepath = system.file("testdata", "test_sample_metadata.tsv", package = 'rcrispr'),
    filename_column = 5,
    label_column = 1,
    plasmid_column = 2,
    control_column = 3,
    treatment_column = 4,
    group_column = 6,
    reads_column = 7)
  slot(test_sample_metadata, 'filepath', check = TRUE) <- 'test'
  testthat::expect_snapshot(get_sample_metadata(test_sample_metadata, processed = T))
})

testthat::test_that("get processed sample metadata with reads only", {
  test_sample_metadata <- read_sample_metadata_file(
    filepath = system.file("testdata", "test_sample_metadata.tsv", package = 'rcrispr'),
    filename_column = 5,
    label_column = 1,
    plasmid_column = 2,
    control_column = 3,
    treatment_column = 4,
    group_column = NULL,
    reads_column = 7)
  slot(test_sample_metadata, 'filepath', check = TRUE) <- 'test'
  testthat::expect_snapshot(get_sample_metadata(test_sample_metadata, processed = T))
})

testthat::test_that("get processed sample metadata with group only", {
  test_sample_metadata <- read_sample_metadata_file(
    filepath = system.file("testdata", "test_sample_metadata.tsv", package = 'rcrispr'),
    filename_column = 5,
    label_column = 1,
    plasmid_column = 2,
    control_column = 3,
    treatment_column = 4,
    group_column = 6,
    reads_column = NULL)
  slot(test_sample_metadata, 'filepath', check = TRUE) <- 'test'
  testthat::expect_snapshot(get_sample_metadata(test_sample_metadata, processed = T))
})

testthat::test_that("get unprocessed sample metadata with reads and group", {
  test_sample_metadata <- read_sample_metadata_file(
    filepath = system.file("testdata", "test_sample_metadata.tsv", package = 'rcrispr'),
    filename_column = 5,
    label_column = 1,
    plasmid_column = 2,
    control_column = 3,
    treatment_column = 4,
    group_column = 6,
    reads_column = 7)
  slot(test_sample_metadata, 'filepath', check = TRUE) <- 'test'
  testthat::expect_snapshot(get_sample_metadata(test_sample_metadata, processed = F))
})

testthat::test_that("get unprocessed sample metadata with reads only", {
  test_sample_metadata <- read_sample_metadata_file(
    filepath = system.file("testdata", "test_sample_metadata.tsv", package = 'rcrispr'),
    filename_column = 5,
    label_column = 1,
    plasmid_column = 2,
    control_column = 3,
    treatment_column = 4,
    group_column = NULL,
    reads_column = 7)
  slot(test_sample_metadata, 'filepath', check = TRUE) <- 'test'
  testthat::expect_snapshot(get_sample_metadata(test_sample_metadata, processed = F))
})

testthat::test_that("get unprocessed sample metadata with group only", {
  test_sample_metadata <- read_sample_metadata_file(
    filepath = system.file("testdata", "test_sample_metadata.tsv", package = 'rcrispr'),
    filename_column = 5,
    label_column = 1,
    plasmid_column = 2,
    control_column = 3,
    treatment_column = 4,
    group_column = 6,
    reads_column = NULL)
  slot(test_sample_metadata, 'filepath', check = TRUE) <- 'test'
  testthat::expect_snapshot(get_sample_metadata(test_sample_metadata, processed = F))
})
