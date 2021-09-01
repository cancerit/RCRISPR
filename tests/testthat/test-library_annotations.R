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
#* --                              TEST DATA                              -- *#
#* --                                                                     -- *#
###############################################################################

test_lib_ann_obj <- read_library_annotation_file(
  filepath = system.file("testdata", "test_library_annotation.tsv", package = 'rcrispr'),
  id_column = 1,
  gene_column = 7,
  chr_column = 4,
  chr_start_column = 5,
  chr_end_column = 6)
slot(test_lib_ann_obj, 'filepath', check = TRUE) <- 'test'

###############################################################################
#* --                                                                     -- *#
#* --                   read_library_annotation_file()                    -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("read library annotation", {
  testthat::expect_snapshot(test_lib_ann_obj)
})

testthat::test_that("get unprocessed library annotations", {
  testthat::expect_snapshot(get_library_annotations(test_lib_ann_obj, processed = F))
})

testthat::test_that("get processed library annotations", {
  testthat::expect_snapshot(get_library_annotations(test_lib_ann_obj, processed = T))
})

testthat::test_that("return true when checking library annotation has coordinates", {
  testthat::expect_true(library_has_coordinates(test_lib_ann_obj))
})

testthat::test_that("return false when checking library annotation has coordinates", {
  test_lib_ann_obj_without_chr <- test_lib_ann_obj
slot(test_lib_ann_obj_without_chr, 'chr_column', check = TRUE) <- NULL
  testthat::expect_false(library_has_coordinates(test_lib_ann_obj_without_chr))
})

###############################################################################
#* --                                                                     -- *#
#* --          remove_guides_from_library_annotations_object()            -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot remove guides from library object when library object is null", {
  testthat::expect_error(remove_guides_from_library_annotations_object(
                          library_annotations_object = NULL,
                          guides_to_remove = c('A1BG_CACCTTCGAGCTGCTGCGCG')),
                         "Cannot remove guides from library, library annotations object is null.")
})

testthat::test_that("cannot remove guides from library object when guides to remove is null", {
  testthat::expect_error(remove_guides_from_library_annotations_object(
                          library_annotations_object = test_lib_ann_obj,
                          guides_to_remove = NULL),
                         "Cannot remove guides from library, guides to remove is null.")
})

testthat::test_that("cannot remove guides from library object when guide not in library", {
  testthat::expect_error(suppressWarnings(remove_guides_from_library_annotations_object(
                                            library_annotations_object = test_lib_ann_obj,
                                            guides_to_remove = c('xxx'))),
                         "Guides not found in library:")
})

testthat::test_that("can remove guides from library object", {
  testthat::expect_snapshot(remove_guides_from_library_annotations_object(
                              library_annotations_object = test_lib_ann_obj,
                              guides_to_remove = c('A1BG_CACCTTCGAGCTGCTGCGCG')))
})

testthat::test_that("can remove guides as dataframe from library object", {
  testthat::expect_snapshot(remove_guides_from_library_annotations_object(
                              library_annotations_object = test_lib_ann_obj,
                              guides_to_remove = data.frame(
                                'guide' = 'A1BG_CACCTTCGAGCTGCTGCGCG',
                                'extra' = 'test')))
})
