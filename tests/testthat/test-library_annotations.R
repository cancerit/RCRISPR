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

test_lib_ann_obj_no_coords <- new("LibraryAnnotations",
  filepath = system.file("testdata", "test_library_annotation.tsv", package = 'rcrispr'),
  id_column = 1,
  gene_column = 2,
  chr_column = 3,
  chr_start_column = 4,
  chr_end_column = 5,
  annotations = data.frame( 'id' = c('sg1', 'sg2', 'sg3', 'sg4'),
                            'gene' = rep('g1', 4),
                            'chr' = c(NA, rep(1, 3)),
                            'start' = c(1, NA, rep(1, 2)),
                            'end' =  c(rep(1, 2), NA, 1)))
slot(test_lib_ann_obj_no_coords, 'filepath', check = TRUE) <- 'test'

###############################################################################
#* --                                                                     -- *#
#* --                    read_library_annotation_file()                   -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("read library annotation and strip ids", {
  test_strip_lib_ann_obj <- read_library_annotation_file(
    filepath = system.file("testdata", "test_library_annotation_with_strip_ids.tsv", package = 'rcrispr'),
    id_column = 1,
    gene_column = 7,
    chr_column = 4,
    chr_start_column = 5,
    chr_end_column = 6,
    strip_ids = T)
  testthat::expect_snapshot(get_library_annotations(test_strip_lib_ann_obj))
})

###############################################################################
#* --                                                                     -- *#
#* --                    get_library_annotations()                        -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("read library annotation", {
  testthat::expect_snapshot(test_lib_ann_obj)
})

testthat::test_that("error when trying to sort unprocessed library annotations", {
  testthat::expect_error(get_library_annotations(test_lib_ann_obj, processed = F, sort_ids = T),
                         "Cannot order unprocessed annotations")
})

testthat::test_that("error when trying to format unprocessed library annotations for crisprcleanr", {
  testthat::expect_error(get_library_annotations(test_lib_ann_obj, processed = F, crisprcleanr = T),
                         "Cannot format library for CRISPRcleanR when processed is FALSE")
})

testthat::test_that("get unprocessed library annotations", {
  testthat::expect_snapshot(get_library_annotations(test_lib_ann_obj, processed = F))
})

testthat::test_that("get processed library annotations", {
  testthat::expect_snapshot(get_library_annotations(test_lib_ann_obj, processed = T))
})

testthat::test_that("error when formatting library annotations for crisprcleanr and chr is null", {
  bad_test_lib_ann_obj <- test_lib_ann_obj
  slot(bad_test_lib_ann_obj, 'chr_column', check = TRUE) <- NULL
  testthat::expect_error(get_library_annotations(bad_test_lib_ann_obj, processed = T, crisprcleanr = T),
                         "Cannot format library for CRISPRcleanR when chr_column, chr_start_column or chr_end_column is null")
})

testthat::test_that("error when formatting library annotations for crisprcleanr and chr is null", {
  bad_test_lib_ann_obj <- test_lib_ann_obj
  slot(bad_test_lib_ann_obj, 'chr_start_column', check = TRUE) <- NULL
  testthat::expect_error(get_library_annotations(bad_test_lib_ann_obj, processed = T, crisprcleanr = T),
                         "Cannot format library for CRISPRcleanR when chr_column, chr_start_column or chr_end_column is null")
})

testthat::test_that("error when formatting library annotations for crisprcleanr and chr is null", {
  bad_test_lib_ann_obj <- test_lib_ann_obj
  slot(bad_test_lib_ann_obj, 'chr_end_column', check = TRUE) <- NULL
  testthat::expect_error(get_library_annotations(bad_test_lib_ann_obj, processed = T, crisprcleanr = T),
                         "Cannot format library for CRISPRcleanR when chr_column, chr_start_column or chr_end_column is null")
})

testthat::test_that("get processed library annotations for crisprcleanr", {
  testthat::expect_snapshot(get_library_annotations(test_lib_ann_obj, processed = T, crisprcleanr = T))
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

###############################################################################
#* --                                                                     -- *#
#* --               get_guides_with_no_coordinates()                      -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can identify guides in library object with no coordinates", {
  testthat::expect_snapshot(get_guides_with_no_coordinates(test_lib_ann_obj_no_coords))
})

testthat::test_that("error when all guides in library have no coordinates", {
  test_lib_ann_obj_all_no_coords <- test_lib_ann_obj_no_coords
  slot(test_lib_ann_obj_all_no_coords, 'annotations', check = TRUE) <-
    data.frame( 'id' = c('sg1'), 'gene' = 'g1', 'chr' = NA, 'start' = 1, 'end' = 1)
  testthat::expect_warning(get_guides_with_no_coordinates(test_lib_ann_obj_all_no_coords),
                         "All guides have no coordinates")
})

testthat::test_that("error when all guides in library have no coordinates", {
  test_lib_ann_obj_all_no_coords <- test_lib_ann_obj_no_coords
  slot(test_lib_ann_obj_all_no_coords, 'chr_column', check = TRUE) <- NULL
  testthat::expect_error(get_guides_with_no_coordinates(test_lib_ann_obj_all_no_coords),
                           "Cannot remove guides with no coordinates")
})
