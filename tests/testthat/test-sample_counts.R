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

# Read an uncompressed sample count file to test with
test_counts <-  read_sample_count_file(
  sample_name = 'HELA_T0',
  filepath = system.file("testdata", "test_counts.tsv", package = 'rcrispr'),
  id_column = 1,
  gene_column = 2,
  count_column = 3)
slot(test_counts, 'filepath', check = TRUE) <- 'test'

# Read a compressed sample count file to test with
gzipped_test_counts <-  read_sample_count_file(
  sample_name = 'CTRL_HELA_T15A',
  filepath = system.file("testdata", "test_counts.tsv.gz", package = 'rcrispr'),
  id_column = 1,
  gene_column = 2,
  count_column = 3)
slot(gzipped_test_counts, 'filepath', check = TRUE) <- 'test.gz'

# Read an unordered sample count file to test with
test_unordered_counts <-  read_sample_count_file(
  sample_name = 'HELA_T0',
  filepath = system.file("testdata", "test_unsorted_counts.tsv", package = 'rcrispr'),
  id_column = 1,
  gene_column = 2,
  count_column = 3)
slot(test_unordered_counts, 'filepath', check = TRUE) <- 'test'

# Add path for sample test counts
sample_count_dir <- system.file("testdata", package = 'rcrispr')
# Read test sample metadata to go with test counts
test_metadata_file <- system.file("testdata", "test_sample_metadata.tsv", package = 'rcrispr')
test_metadata_obj <- read_sample_metadata_file(test_metadata_file,
                                               filename_column = 1,
                                               label_column = 2,
                                               plasmid_column = 3,
                                               control_column = 4,
                                               treatment_column = 5)

# Read test library annotation to go with test counts
test_library_file <- system.file("testdata", "test_library_annotation.tsv", package = 'rcrispr')
test_library_obj <- read_library_annotation_file(test_library_file,
                                                 id_column = 1,
                                                 gene_column = 7)

###############################################################################
#* --                                                                     -- *#
#* --                              counts()                               -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("get original counts", {
  testthat::expect_snapshot(counts(test_unordered_counts))
})

testthat::test_that("get unsorted processed counts", {
  testthat::expect_snapshot(counts(test_unordered_counts, processed = T))
})

testthat::test_that("get sorted processed counts", {
  testthat::expect_snapshot(counts(test_unordered_counts, processed = T, sort_ids = T))
})

testthat::test_that("sort_ids cannot be true without processed being true", {
  testthat::expect_error(counts(test_unordered_counts, processed = F, sort_ids = T),
                         "Cannot order unprocessed count matrix")
})

###############################################################################
#* --                                                                     -- *#
#* --                     read_sample_count_file()                        -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("read uncompressed sample count file", {
  testthat::expect_snapshot(test_counts)
})

testthat::test_that("read gzipped sample count file", {
  testthat::expect_snapshot(gzipped_test_counts)
})

testthat::test_that("read sample count file and strip ids", {
  # Read an uncompressed sample count file to test with
  test_counts_stripped <-  read_sample_count_file(
    sample_name = 'HELA_T0',
    filepath = system.file("testdata", "test_counts_strip_ids.tsv", package = 'rcrispr'),
    strip_ids = T)
  slot(test_counts_stripped, 'filepath', check = TRUE) <- 'test'
  testthat::expect_snapshot(test_counts_stripped)
})

testthat::test_that("read sample count file with numeric sample names", {
  test_counts_numeric_sample <-  read_sample_count_file(
    sample_name = '1HELA_T0',
    filepath = system.file("testdata", "test_counts_numeric_sample.tsv", package = 'rcrispr'),
    id_column = 1,
    gene_column = 2,
    count_column = 3,
    check.names = F)
  slot(test_counts_numeric_sample, 'filepath', check = TRUE) <- 'test'
  testthat::expect_snapshot(test_counts_numeric_sample)
})

###############################################################################
#* --                                                                     -- *#
#* --           convert_sample_counts_objects_to_count_matrix()           -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("combine SampleCounts objects into count matrix", {
  test_count_matrix <- convert_sample_counts_objects_to_count_matrix(list(test_counts,gzipped_test_counts))
  testthat::expect_snapshot(test_count_matrix)
})

testthat::test_that("combine SampleCounts objects into count matrix and sort ids", {
  test_count_matrix <- convert_sample_counts_objects_to_count_matrix(list(test_counts,gzipped_test_counts), sort_ids = T)
  testthat::expect_snapshot(test_count_matrix)
})

testthat::test_that("list of SampleCounts objects provided to function", {
  testthat::expect_error(convert_sample_counts_objects_to_count_matrix(data.frame(x = c(1,2,3))),
                         "Not a SampleCounts object:")
})

testthat::test_that("cannot combine empty dataframe into count matrix", {
  test_counts_empty <- test_counts
  slot(test_counts_empty, 'counts', check = FALSE) <- data.frame('sgRNA' = character(), 'gene' = character(), 'test_counts' = numeric())
  testthat::expect_error(convert_sample_counts_objects_to_count_matrix(list(test_counts,test_counts_empty)),
                         "Could not generate processed sample counts:")
  rm(test_counts_empty)
})

testthat::test_that("cannot combine SampleCounts with different sgRNA IDs", {
  test_counts_wrong <- test_counts
  slot(test_counts_wrong, 'counts', check = FALSE) <- counts(test_counts_wrong, processed = T)[,c(2,1,3)]
  testthat::expect_error(convert_sample_counts_objects_to_count_matrix(list(test_counts,test_counts_wrong)),
                         "Could not add sample to count matrix, sgRNA ids don't match:")
  rm(test_counts_wrong)
})

###############################################################################
#* --                                                                     -- *#
#* --                        read_count_matrix_file()                     -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can read count matrix file", {
  testthat::expect_snapshot(read_count_matrix_file(test_count_matrix_file,
                                                   id_column = 1,
                                                   gene_column = 2,
                                                   count_column = '3'))
})

testthat::test_that("can read count matrix file returning processed matrix", {
  testthat::expect_snapshot(read_count_matrix_file(test_count_matrix_file,
                                                   id_column = 1,
                                                   gene_column = 2,
                                                   count_column = '3',
                                                   processed = T))
})

testthat::test_that("can read count matrix file and strip ids", {
  testthat::expect_snapshot(read_count_matrix_file(filepath = system.file("testdata", "test_counts_strip_ids.tsv", package = 'rcrispr'),
                                                   id_column = 1,
                                                   gene_column = 2,
                                                   count_column = '3',
                                                   processed = T,
                                                   strip_ids = T))
})

testthat::test_that("can read count matrix file and sort ids", {
  testthat::expect_snapshot(read_count_matrix_file(filepath = test_count_matrix_file,
                                                   id_column = 1,
                                                   gene_column = 2,
                                                   count_column = '3',
                                                   processed = T,
                                                   sort_ids = T))
})

testthat::test_that("cannot read count matrix file without header", {
  testthat::expect_error(read_count_matrix_file(test_count_matrix_file,
                                                file_header = F,
                                                count_column = '3'),
                         "Cannot read in a count matrix file without a header.")
})

testthat::test_that("cannot read count matrix file with character id_column", {
  testthat::expect_error(read_count_matrix_file(test_count_matrix_file,
                                                id_column = 'x',
                                                gene_column = 2,
                                                count_column = '3'),
                         "Could not convert value to integer:")
})

testthat::test_that("cannot read count matrix file with character gene_column", {
  testthat::expect_error(read_count_matrix_file(test_count_matrix_file,
                                                id_column = 1,
                                                gene_column = 'x',
                                                count_column = '3'),
                         "Could not convert value to integer:")
})

testthat::test_that("cannot read count matrix file with duplicate column indices provided", {
  testthat::expect_error(read_count_matrix_file(test_count_matrix_file,
                                                id_column = 1,
                                                gene_column = 2,
                                                count_column = '2'),
                         "Cannot read count matrix, duplicate column indices:")
})

###############################################################################
#* --                                                                     -- *#
#* --                       read_sample_count_files()                     -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot read sample count files when directory is null", {
  testthat::expect_error(read_sample_count_files(count_directory = NULL,
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3,
                                                 file_separator = "\t",
                                                 file_header = TRUE,
                                                 sample_metadata_object = NULL),
                         "Directory is null:")
})

testthat::test_that("cannot read sample count files when metadata is null", {
  testthat::expect_error(read_sample_count_files(count_directory = sample_count_dir,
                                                 id_column = 1,
                                                 gene_column = 2,
                                                 count_column = 3,
                                                 file_separator = "\t",
                                                 file_header = TRUE,
                                                 sample_metadata_object = NULL),
                         "Cannot read sample counts, sample metadata is NULL.")
})

testthat::test_that("can read sample count files", {
  count_objects <- read_sample_count_files(
                      count_directory = sample_count_dir,
                      id_column = 1,
                      gene_column = 2,
                      count_column = 3,
                      file_separator = "\t",
                      file_header = TRUE,
                      sample_metadata_object = test_metadata_obj)
  for(i in 1:length(count_objects)) {
    slot(count_objects[[i]], 'filepath', check = TRUE) <- 'test'
  }
  testthat::expect_snapshot(count_objects)
})

###############################################################################
#* --                                                                     -- *#
#* --                     compare_counts_to_library()                     -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot compare counts to library when sample counts are null", {
  testthat::expect_error(compare_counts_to_library(sample_counts_object = NULL,
                                                   library_annotation_object = NULL),
                         "Cannot compare counts to library, sample counts object is null.")
})

testthat::test_that("cannot compare counts to library when library is null", {
  testthat::expect_error(compare_counts_to_library(sample_counts_object = test_counts,
                                                   library_annotation_object = NULL),
                         "Cannot compare counts to library, library annotation object is null.")
})

testthat::test_that("cannot compare counts to library when ids don't match", {
  bad_test_library_obj <- test_library_obj
  bad_annotation <- bad_test_library_obj@annotations
  bad_annotation[1,1] <- 'ERROR'
  slot(bad_test_library_obj, 'annotations', check = TRUE) <- bad_annotation
  testthat::expect_error(compare_counts_to_library(sample_counts_object = test_counts,
                                                  library_annotation_object = bad_test_library_obj),
                         "sgRNA IDs and gene names in sample counts do not match:")
})

testthat::test_that("can compare counts to library when ids match", {
  testthat::expect_true(compare_counts_to_library(sample_counts_object = test_counts,
                                                  library_annotation_object = test_library_obj))
})


###############################################################################
#* --                                                                     -- *#
#* --                 compare_count_matrix_to_library()                   -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot compare count matrix to library when count_matrix is null", {
  testthat::expect_error(compare_count_matrix_to_library(count_matrix = NULL,
                                                         library_annotation_object = NULL),
                         "Cannot compare count matrix to library, sample count matrix is null.")
})

testthat::test_that("cannot compare count matrix to library when library is null", {
  testthat::expect_error(compare_count_matrix_to_library(count_matrix = test_count_matrix,
                                                         library_annotation_object = NULL),
                         "Cannot compare count matrix to library, library annotation object is null.")
})

testthat::test_that("cannot compare count matrix to library when ids don't match", {
  bad_test_count_matrix <- test_count_matrix
  bad_test_count_matrix[1,1] <- 'ERROR'
  testthat::expect_error(compare_count_matrix_to_library(count_matrix = bad_test_count_matrix,
                                                         library_annotation_object = test_library_obj),
                         "sgRNA IDs and gene names in sample count matrix and library do not match.")
})

testthat::test_that("can compare count matrix to library when ids match", {
  testthat::expect_true(compare_count_matrix_to_library(count_matrix = test_count_matrix,
                                                        id_column = 1,
                                                        gene_column = 2,
                                                        library_annotation_object = test_library_obj))
})

###############################################################################
#* --                                                                     -- *#
#* --               reorder_count_matrix_by_sample_type()                 -- *#
#* --                                                                     -- *#
###############################################################################

# Add path for test sample count matrix
sample_count_matrix <- convert_sample_counts_objects_to_count_matrix(c(test_counts, gzipped_test_counts))

testthat::test_that("cannot reorder count matrix when count matrix is null", {
  testthat::expect_error(reorder_count_matrix_by_sample_type(count_matrix = NULL,
                                                             sample_metadata_object = NULL),
                         "Cannot reorder count matrix, count matrix is null.")
})

testthat::test_that("cannot reorder count matrix when sample metadata is null", {
  testthat::expect_error(reorder_count_matrix_by_sample_type(count_matrix = sample_count_matrix,
                                                             sample_metadata_object = NULL),
                         "Cannot reorder count matrix, sample metadata object is null.")
})

testthat::test_that("cannot reorder count matrix when sample name is missing in metadata", {
  bad_sample_count_matrix <- sample_count_matrix
  colnames(bad_sample_count_matrix)[3] <- 'ERROR'
  testthat::expect_error(
    reorder_count_matrix_by_sample_type(count_matrix = bad_sample_count_matrix,
                                        sample_metadata_object = test_metadata_obj),
    "Cannot reorder count matrix, sample name not in metadata:")
})

testthat::test_that("cannot reorder count matrix when sample type is not set in metadata", {
  # Note: this shouldn't occur as metadata is typically validated
  bad_test_metadata_obj <- test_metadata_obj
  bad_metadata <- get_sample_metadata(bad_test_metadata_obj)
  bad_metadata$plasmid[1] <- 0
  slot(bad_test_metadata_obj, 'metadata', check = TRUE) <- bad_metadata
  testthat::expect_error(
    reorder_count_matrix_by_sample_type(count_matrix = sample_count_matrix,
                                        sample_metadata_object = bad_test_metadata_obj),
    "Number of columns in reordered count matrix doesn't match:")
})

testthat::test_that("can reorder count matrix", {
  testthat::expect_snapshot(
    reorder_count_matrix_by_sample_type(count_matrix = sample_count_matrix,
                                        sample_metadata_object = test_metadata_obj))
})

###############################################################################
#* --                                                                     -- *#
#* --                 remove_guides_from_sample_counts()                  -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot remove guides from sample counts when count object is null", {
  testthat::expect_error(remove_guides_from_sample_counts(sample_counts_object = NULL,
                                                          guides_to_remove = c('A1BG_CACCTTCGAGCTGCTGCGCG')),
                         "Cannot remove guides from sample counts, sample counts object is null.")
})

testthat::test_that("cannot remove guides from sample counts when guides to remove is null", {
  testthat::expect_error(remove_guides_from_sample_counts(sample_counts_object = test_counts,
                                                          guides_to_remove = NULL),
                         "Cannot remove guides from sample counts, guides to remove is null.")
})


testthat::test_that("cannot remove guides from sample counts when guide not in counts", {
  testthat::expect_error(suppressWarnings(remove_guides_from_sample_counts(
    sample_counts_object = test_counts,
    guides_to_remove = c('xxx'))),
    "Guides not found in counts:")
})

testthat::test_that("can remove guides from sample counts", {
  testthat::expect_snapshot(
    remove_guides_from_sample_counts(sample_counts_object = test_counts,
                                     guides_to_remove = c('A1BG_CACCTTCGAGCTGCTGCGCG')))
})

testthat::test_that("can remove guides as dataframe from sample counts", {
  testthat::expect_snapshot(
    remove_guides_from_sample_counts(sample_counts_object = test_counts,
                                     guides_to_remove = data.frame(
                                       'guide' = 'A1BG_CACCTTCGAGCTGCTGCGCG',
                                       'extra' = 'test')))
})

###############################################################################
#* --                                                                     -- *#
#* --                 remove_guides_from_count_matrix()                   -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot remove guides from count matrix when count object is null", {
  testthat::expect_error(remove_guides_from_count_matrix(count_matrix = NULL,
                                                         id_column = 1,
                                                         guides_to_remove = c('A1BG_CACCTTCGAGCTGCTGCGCG')),
                         "Cannot remove guides from count matrix, count matrix is null.")
})

testthat::test_that("cannot remove guides from count matrix when guides to remove is null", {
  testthat::expect_error(remove_guides_from_count_matrix(count_matrix = test_count_matrix,
                                                         id_column = 1,
                                                         guides_to_remove = NULL),
                         "Cannot remove guides from count matrix, guides to remove is null.")
})


testthat::test_that("cannot remove guides from count matrix when guide not in counts", {
  testthat::expect_error(suppressWarnings(remove_guides_from_count_matrix(
    count_matrix = test_count_matrix,
    id_column = 1,
    guides_to_remove = c('xxx'))),
    "Guides not found in counts:")
})

testthat::test_that("can remove guides from count matrix", {
  testthat::expect_snapshot(
    remove_guides_from_count_matrix(count_matrix = test_count_matrix,
                                    id_column = 1,
                                    guides_to_remove = c('A1BG_CACCTTCGAGCTGCTGCGCG')))
})

testthat::test_that("can remove guides as dataframe from count matrix", {
  testthat::expect_snapshot(
    remove_guides_from_count_matrix(count_matrix = test_count_matrix,
                                    id_column = 1,
                                    guides_to_remove = data.frame(
                                      'guide' = 'A1BG_CACCTTCGAGCTGCTGCGCG',
                                      'extra' = 'test')))
})

###############################################################################
#* --                                                                     -- *#
#* --                 compare_matrix_to_sample_metadata()                 -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can compare sample metadata to count matrix", {
  testthat::expect_snapshot(
    compare_matrix_to_sample_metadata(data = test_count_matrix,
                                      sample_metadata_object = test_metadata_obj))
})

testthat::test_that("cannot compare sample metadata to count matrix, data is null", {
  testthat::expect_error(
    compare_matrix_to_sample_metadata(data = NULL,
                                      sample_metadata_object = test_metadata_obj),
    "Cannot compare sample column names to sample metadata, data is null")
})

testthat::test_that("cannot compare sample metadata to count matrix, data is null", {
  testthat::expect_error(
    compare_matrix_to_sample_metadata(data = test_count_matrix,
                                      sample_metadata_object = NULL),
    "Cannot compare sample column names to sample metadata, sample metadata object is null")
})

testthat::test_that("cannot compare sample metadata to count matrix, sample name not present in metadata", {
  bad_test_count_matrix <- test_count_matrix
  colnames(bad_test_count_matrix)[3] <- "BAD"
  testthat::expect_error(
    compare_matrix_to_sample_metadata(data = bad_test_count_matrix,
                                      sample_metadata_object = test_metadata_obj),
    "Cannot compare sample column names to sample metadata, sample name not in metadata: BAD")
})

###############################################################################
#* --                                                                     -- *#
#* --                    get_guides_failing_filter()                      -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can get guides failing filter", {
  testthat::expect_snapshot(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = 3:4))
})

testthat::test_that("get no guides failing filter", {
  testthat::expect_silent(
    get_guides_failing_filter(count_matrix = test_count_matrix[c(1,3),],
                              count_column = 3:4,
                              filter_indices = 3:4))
})

testthat::test_that("cannot get guides failing filter, count_matrix null", {
  testthat::expect_error(
    get_guides_failing_filter(count_matrix = NULL,
                              count_column = 3:4,
                              filter_indices = 3:4),
    "Cannot get guides to filter from count matrix, count matrix is null")
})

testthat::test_that("cannot get guides failing filter, count_column null", {
  testthat::expect_error(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = NULL,
                              filter_indices = 3:4),
    "Cannot get guides to filter from count matrix, count_column is null")
})

testthat::test_that("cannot get guides failing filter, filter_indices null", {
  testthat::expect_error(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = NULL),
    "Cannot get guides to filter from count matrix, filter_indices is null")
})

testthat::test_that("cannot get guides failing filter, min_reads null", {
  testthat::expect_error(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = 3:4,
                              min_reads = NULL),
    "Cannot get guides to filter from count matrix, min_reads is null")
})

testthat::test_that("cannot get guides failing filter, min_reads < 0", {
  testthat::expect_error(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = 3:4,
                              min_reads = -1),
    "Cannot get guides to filter from count matrix, min_reads is < 0")
})

testthat::test_that("cannot get guides failing filter, filter_method null", {
  testthat::expect_error(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = 3:4,
                              filter_method = NULL),
    "Cannot get guides to filter from count matrix, filter_method is null")
})

testthat::test_that("cannot get guides failing filter, filter_method invalid", {
  testthat::expect_error(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = 3:4,
                              filter_method = 'bad'),
    "Cannot get guides to filter from count matrix, filter_method is not valid")
})

testthat::test_that("cannot get guides failing filter, filter_indices not in count columns", {
  testthat::expect_error(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = 5),
    "Cannot get guides to filter from count matrix, filter indices not in count columns")
})

testthat::test_that("get guides failing filter with filter_method any", {
  testthat::expect_snapshot(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = 3:4,
                              min_reads = 200,
                              filter_method = 'any'))
})

testthat::test_that("get guides failing filter with filter_method all", {
  testthat::expect_snapshot(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = 3:4,
                              min_reads = 200,
                              filter_method = 'all'))
})

testthat::test_that("get guides failing filter with filter_method mean", {
  testthat::expect_snapshot(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = 3:4,
                              min_reads = 200,
                              filter_method = 'mean'))
})

testthat::test_that("get guides failing filter with filter_method median", {
  testthat::expect_snapshot(
    get_guides_failing_filter(count_matrix = test_count_matrix,
                              count_column = 3:4,
                              filter_indices = 3:4,
                              min_reads = 200,
                              filter_method = 'median'))
})
