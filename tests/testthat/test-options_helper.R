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
