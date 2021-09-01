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
#* --                           CHECK OPTION                              -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("check option", {
  testthat::expect_true(check_option('test', 1))
})

testthat::test_that("check option", {
  testthat::expect_error(check_option('test', NULL),
                         "Please provide a value for option:")
})

###############################################################################
#* --                                                                     -- *#
#* --                        SHARED OUTPUT OPTIONS                        -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("shared output options", {
  testthat::expect_snapshot(shared_output_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                         BASIC OUTPUT OPTIONS                        -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("basic outfile options", {
  testthat::expect_snapshot(basic_outfile_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                           COUNT OPTIONS                             -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("count path options", {
  testthat::expect_snapshot(count_path_options())
})

testthat::test_that("count type options", {
  testthat::expect_snapshot(count_type_options())
})

testthat::test_that("count format options", {
  testthat::expect_snapshot(count_format_options())
})

testthat::test_that("count column index options", {
  testthat::expect_snapshot(count_column_index_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                    LIBRARY ANNOTATION OPTIONS                       -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("library annotation options", {
  testthat::expect_snapshot(library_annotation_options())
})

testthat::test_that("library annotation format options", {
  testthat::expect_snapshot(library_annotation_format_options())
})

testthat::test_that("library annotation column index options", {
  testthat::expect_snapshot(library_annotation_column_index_options())
})

testthat::test_that("library annotation genomic column index options", {
  testthat::expect_snapshot(library_annotation_genomic_column_index_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                       SAMPLE METADATA OPTIONS                       -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("sample metadata options", {
  testthat::expect_snapshot(sample_metadata_options())
})

testthat::test_that("sample metadata format options", {
  testthat::expect_snapshot(sample_metadata_format_options())
})

testthat::test_that("sample metadata sample filename options", {
  testthat::expect_snapshot(sample_metadata_sample_filename_column_index_options())
})

testthat::test_that("sample metadata sample label options", {
  testthat::expect_snapshot(sample_metadata_sample_label_column_index_options())
})

testthat::test_that("sample metadata sample type options", {
  testthat::expect_snapshot(sample_metadata_sample_type_column_index_options())
})

testthat::test_that("sample metadata sample group options", {
  testthat::expect_snapshot(sample_metadata_sample_group_column_index_options())
})

testthat::test_that("sample metadata sample read count options", {
  testthat::expect_snapshot(sample_metadata_sample_read_count_column_index_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                       DUPLICATE GUIDE OPTIONS                       -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("duplicate guide options", {
  testthat::expect_snapshot(duplicate_guide_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                        REMOVE GUIDE OPTIONS                         -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("remove guide options", {
  testthat::expect_snapshot(remove_guide_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                         MAGECK RRA OPTIONS                          -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("MAGeCK RRA summary options", {
  testthat::expect_snapshot(mageck_rra_summary_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                      BAGEL NORMALISATION OPTIONS                    -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("BAGEL normalisation options", {
  testthat::expect_snapshot(bagel_normalisation_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                           CALCULATE LFC OPTIONS                     -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("Calculate LFC options", {
  testthat::expect_snapshot(calculate_lfc_options())
})

###############################################################################
#* --                                                                     -- *#
#* --             REMOVE GUIDES WITH NO COORDINATES OPTIONS               -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("Remove guides with no coordinates options", {
  testthat::expect_snapshot(remove_no_coordinate_guide_options())
})

###############################################################################
#* --                                                                     -- *#
#* --             FILTER COUNTS AND LIBRARY BY INDEX OPTIONS              -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("Filter counts and library by index options", {
  testthat::expect_snapshot(filter_by_index_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                    CRISPRCLEANR OUTPUT OPTIONS                      -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("CRISPRcleanR output options", {
  testthat::expect_snapshot(crisprcleanr_output_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                 CRISPRCLEANR NORMALISATION OPTIONS                  -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("CRISPRcleanR normalisation options", {
  testthat::expect_snapshot(crisprcleanr_normalisation_options())
})

###############################################################################
#* --                                                                     -- *#
#* --                   CRISPRCLEANR CORRECTION OPTIONS                   -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("CRISPRcleanR correction options", {
  testthat::expect_snapshot(crisprcleanr_correction_options())
})
