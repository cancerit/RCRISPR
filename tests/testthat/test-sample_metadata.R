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
