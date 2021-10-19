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
tmpfile <- tempfile()
file.create(tmpfile)
tmpdir <- tempdir(check = TRUE)

tmp_sample <- tempfile()
file.create(tmpfile)
#write.table(read.table(file = system.file("extdata/HeLa_raw_sample_counts", "HELA_T0.tsv", package = 'rcrispr'), header = F, nrows = 5), tmp_sample, sep = "\t", row.names = F, quote = F)

if (!file.exists(tmpfile))
  stop(paste("File not created:", tmpfile))

###############################################################################
#* --                                                                     -- *#
#* --                            check_file()                             -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("error when file is null", {
  testthat::expect_error(check_file(file = NULL),
                         "File is null.")
})

testthat::test_that("error when not a file", {
  testthat::expect_error(check_file(file = tmpdir),
                         "Not a file.")
})

testthat::test_that("error when file does not exist", {
  testthat::expect_error(check_file(file = "this_is_not_a_file"),
                         "File does not exist.")
})

testthat::test_that("error when file is empty", {
  testthat::expect_error(check_file(file = tmpfile),
                         "File is empty.")
})

testthat::test_that("class of uncompressed file is file", {
  testthat::expect_equal(get_file_class(file = system.file("testdata", "test_counts.tsv",
                                                           package = 'rcrispr')), "file")
})

testthat::test_that("can validate file", {
  testthat::expect_true(check_file(file = system.file("testdata", "test_counts.tsv", package = 'rcrispr')))
})

# SKIP as failing on CI
#`check_file(file = tmpfile, ignore_empty = TRUE)` did not throw the expected error.
#Backtrace:
# 1. testthat::expect_error(...) test-file_helper.R:50:2
# 2. testthat:::expect_condition_matching(...)
#testthat::test_that("error when file is not readable", {
#  cat('Test', file = tmpfile)
#  Sys.chmod(tmpfile, mode="333", use_umask = FALSE)
#  testthat::expect_error(check_file(file = tmpfile, ignore_empty = TRUE),
#                         "File is not readable.")
#  Sys.chmod(tmpfile, mode="0755", use_umask = FALSE)
#})

###############################################################################
#* --                                                                     -- *#
#* --                          check_directory()                          -- *#
#* --                                                                     -- *#
###############################################################################


testthat::test_that("error when directory is null", {
  testthat::expect_error(check_directory(directory = NULL),
                         "Directory is null.")
})

testthat::test_that("error when directory does not exist", {
  testthat::expect_error(check_directory(directory = "this_is_not_a_directory"),
                         "Directory does not exist.")
})

testthat::test_that("error when directory is empty", {
  if (!dir.exists(file.path(tmpdir, 'empty_dir')))
    dir.create(file.path(tmpdir, 'empty_dir'))
  testthat::expect_error(check_directory(directory = file.path(tmpdir, 'empty_dir')),
                         "Directory is empty.")
})

# SKIP as failing in CI
#Error (test-file_helper.R:81:3): error when directory is not readable
#Error: Directory is empty: /tmp/unreadable_dir
#Backtrace:
# 1. testthat::expect_error(...) test-file_helper.R:81:2
# 7. rcrispr::check_directory(directory = file.path(tmpdir, "unreadable_dir"))
#testthat::test_that("error when directory is not readable", {
#  dir.create(file.path(tmpdir, 'unreadable_dir'))
#  Sys.chmod(file.path(tmpdir, 'unreadable_dir'), mode="333", use_umask = FALSE)
#  testthat::expect_error(check_directory(directory = file.path(tmpdir, 'unreadable_dir')),
#                         "Directory is not readable.")
#  Sys.chmod(file.path(tmpdir, 'unreadable_dir'), mode="0755", use_umask = FALSE)
#})

testthat::test_that("can validate directory", {
  file.create(file.path(tmpdir, 'empty.txt'))
  testthat::expect_true(check_directory(directory = tmpdir))
})

###############################################################################
#* --                                                                     -- *#
#* --                       read_file_to_dataframe()                      -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("read uncompressed sample count to dataframe", {
  testthat::expect_snapshot(read_file_to_dataframe(filepath = system.file("testdata", "test_counts.tsv", package = 'rcrispr'),
                                                   file_separator = "\t",
                                                   file_header = TRUE))
})

testthat::test_that("read gzipped sample count to dataframe", {
  testthat::expect_snapshot(read_file_to_dataframe(filepath = system.file("testdata", "test_counts.tsv.gz", package = 'rcrispr'),
                                                   file_separator = "\t",
                                                   file_header = TRUE))
})

testthat::test_that("read uncompressed sample count to dataframe with column indices", {
  testthat::expect_snapshot(read_file_to_dataframe(filepath = system.file("testdata", "test_counts.tsv", package = 'rcrispr'),
                                                   file_separator = "\t",
                                                   file_header = TRUE,
                                                   column_indices = 3))
})

#testthat::test_that("error reading sample count file which doesn't exist to dataframe", {
#  testthat::expect_error(read_file_to_dataframe(filepath = "does_not_exist"), 'File does not exist')
#})

###############################################################################
#* --                                                                     -- *#
#* --                         prepare_filepath()                          -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("error creating file path when outfile is null", {
  testthat::expect_error(prepare_filepath(outfile = NULL),
                         "Cannot write data to file, outfile is NULL.")
})

#testthat::test_that("error creating file path when outdir does not exist", {
#  testthat::expect_error(prepare_filepath(outfile = 'test.txt',
#                                          outdir = 'does_not_exist'),
#                         "Directory does not exist:")
#})

testthat::test_that("can create file path and get warning when no outdir set", {
  testthat::expect_warning(prepare_filepath(outfile = 'test.txt'),
                           "outdir was not set, using working directory")
})

testthat::test_that("can create file path with prefix", {
  testthat::expect_equal(prepare_filepath(outfile = 'test.txt',
                                          outdir = tmpdir,
                                          prefix = 'prefix'),
                         file.path(tmpdir, 'prefix.test.txt'))
})

testthat::test_that("can create file path with suffix", {
  testthat::expect_equal(prepare_filepath(outfile = 'test.txt',
                                          outdir = tmpdir,
                                          suffix = 'suffix'),
                         file.path(tmpdir, 'test.suffix.txt'))
})

testthat::test_that("can create file path with suffix (compressed)", {
  testthat::expect_equal(prepare_filepath(outfile = 'test.gz',
                                          outdir = tmpdir,
                                          suffix = 'suffix'),
                         file.path(tmpdir, 'test.suffix.gz'))
})

###############################################################################
#* --                                                                     -- *#
#* --                      write_dataframe_to_file()                      -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot write data frame to file when data is null", {
  testthat::expect_error(write_dataframe_to_file(outfile = tmpfile,
                                                 data = NULL),
                         "Cannot write data to file, data is NULL.")
})

testthat::test_that("cannot write data frame to file when file is null", {
  testthat::expect_error(write_dataframe_to_file(outfile = NULL,
                                                 data = c(1,2)),
                                                 "Cannot write data to file, outfile is NULL.")
})

# SKIP as failing on CI
#`actual`:   "/tmp/file1aa4ff02d39"
#`expected`: "/private/tmp/file1aa4ff02d39"
#testthat::test_that("can write data frame to file", {
#  testthat::expect_equal(write_dataframe_to_file(outfile = basename(tmpfile), outdir = dirname(tmpfile), data = c(1,2)),
#                           paste0('/private', gsub("//", "/", dirname(tmpfile)), '/', basename(tmpfile)))
#})


###############################################################################
#* --                                                                     -- *#
#* --                        write_rdata_to_file()                        -- *#
#* --                                                                     -- *#
###############################################################################
testthat::test_that("cannot write data to file when data is null", {
  testthat::expect_error(write_rdata_to_file(outfile = tmpfile,
                                             data = NULL),
                         "Cannot write data to file, data is NULL.")
})

testthat::test_that("cannot write data to file when outfile is null", {
  test <- c(1,2)
  testthat::expect_error(write_rdata_to_file(outfile = NULL,
                                             data = test),
                         "Cannot write data to file, outfile is NULL.")
})

# SKIP as failing on CI
#`actual`:   "/tmp/file1aa4ff02d39"
#`expected`: "/private/tmp/file1aa4ff02d39"
#testthat::test_that("can write rdata to file", {
#  testthat::expect_equal(write_rdata_to_file(outfile = basename(tmpfile), outdir = dirname(tmpfile), data = c(1,2)),
#                           paste0('/private', gsub("//", "/", dirname(tmpfile)), '/', basename(tmpfile)))
#})


###############################################################################
#* --                                                                     -- *#
#* --                      save_plot_with_ggsave()                        -- *#
#* --                                                                     -- *#
###############################################################################
tmpdir <- tempdir(check = TRUE)
test_plot <- ggplot(data.frame('x' = c(1:2), 'y' = c(1:2)),
                    aes( x = x, y = y)) + geom_point()

testthat::test_that("can save plot with ggsave", {
  testthat::expect_equal(save_plot_with_ggsave(data = test_plot,
                                               outfile = 'test.png',
                                               outdir = tmpdir),
                         file.path(tmpdir, 'test.png'))
})

testthat::test_that("cannot save plot with ggsave when data is null", {
  testthat::expect_error(save_plot_with_ggsave(data = NULL,
                                               outfile = 'test.png',
                                               outdir = tmpdir),
                         "Cannot save plot with ggsave, data is null.")
})

testthat::test_that("cannot save plot with ggsave with bad data", {
  testthat::expect_error(save_plot_with_ggsave(data = 'x',
                                               outfile = 'test.png',
                                               outdir = tmpdir),
                         "Could not save plot with ggsave.")
})

###############################################################################
#* --                                                                     -- *#
#* --                         save_plot_list()                            -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can save plot list", {
  testthat::expect_equal(suppressMessages(
                           save_plot_list(plot_list = list('x' = test_plot,
                                                           'y' = test_plot),
                                          outdir = tmpdir)),
                           c(file.path(tmpdir, 'x.png'), file.path(tmpdir, 'y.png')))
})

testthat::test_that("cannot save plot list, plot_list null", {
  testthat::expect_error(
    save_plot_list(plot_list = NULL, outdir = tmpdir),
    "Cannot save plot list, plot_list is null")
})

testthat::test_that("cannot save plot list, item is not a plot", {
  testthat::expect_error(
    suppressMessages(save_plot_list(plot_list = list('x' = '123'), outdir = tmpdir)),
    "Could not save plot from list, item is not a plot")
})

