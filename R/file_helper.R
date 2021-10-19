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
################################################################################
#* --                                                                      -- *#
#* --                         check_directory()                            -- *#
#* --                                                                      -- *#
################################################################################

#' Run basic checks on directory
#'
#' @description Checks that path is a directory, that the directory exists,
#' the directory can be read and that the directory is not empty.
#'
#' @export check_directory
#' @param directory the path of the directory which is being checked.
#' @param ignore_empty whether to skip check for empty directory
#' @return Boolean
check_directory <- function(directory = NULL,
                            ignore_empty = FALSE) {
  # Check whether directory is null
  if (is.null(directory))
    stop(paste("Directory is null:", directory, sep = ": "), call. = F)
  # Check whether directory exists
  if (!dir.exists(directory))
    stop(paste("Directory does not exist", directory, sep = ": "), call. = F)
  # Check whether directory is readable
  if (file.access(directory, mode = 4)[1] != 0)
    stop(paste("Directory is not readable", directory, sep = ": "), call. = F)
  # Check whether directory is empty
  if (length(list.files(directory)) == 0 && !ignore_empty)
    stop(paste("Directory is empty", directory, sep = ": "), call. = F)
  return(TRUE)
}

################################################################################
#* --                                                                      -- *#
#* --                              check_file()                            -- *#
#* --                                                                      -- *#
################################################################################

#' Run basic checks on input file
#'
#' @description Checks that path is a file, that the file exists,
#' the file can be read and that the file is not empty.
#' @importFrom utils file_test
#' @export check_file
#' @param file the path of the file which is being checked.
#' @param ignore_empty whether to ignore empty file check.
#' @return Boolean
check_file <- function(file = NULL, ignore_empty = FALSE) {
  # Check if file is null
  if (is.null(file))
    stop(paste("File is null", file, sep = ": "), call. = F)
  # Check file exists
  if (!file.exists(file))
    stop(paste("File does not exist", file, sep = ": "), call. = F)
  if(!ignore_empty) {
    # Check file isn't empty
    if (is.na(file.size(file)) || file.size(file) == 0)
      stop(paste("File is empty", file, sep = ": "), call. = F)
  }
  # Check it is a file
  if (!file_test("-f", file))
    stop(paste("Not a file", file, sep = ": "), call. = F)
  # Check file is readable
  if (file.access(file, mode = 4)[1] != 0)
    stop(paste("File is not readable", file, sep = ": "), call. = F)
  return(TRUE)
}

#' Get file class
#'
#' @description Returns the class of a given file.
#' @export get_file_class
#' @param file the path of the file whose class is being determined.
#' @return The class of \code{file}.
get_file_class <- function(file = NULL) {
  # Validate file
  check_file(file)
  # Open connection
  con <- file(file)
  # Get file information
  file_summary <- summary(con)
  # Close connection
  close(con)
  # Return file class
  return(file_summary$class)
}

################################################################################
#* --                                                                      -- *#
#* --                       read_file_to_dataframe()                       -- *#
#* --                                                                      -- *#
################################################################################

#' Read delimited file
#'
#' @param filepath delimited file to be read.
#' @param file_separator field separator.
#' @param file_header logical stating whether file has header.
#' @param column_indices columns to include
#' @param ... additional read.delim parameters.
#'
#' @importFrom utils read.delim
#'
#' @return dataframe
#' @export
#'
read_file_to_dataframe <-
  function(filepath = NULL,
           file_separator = "\t",
           file_header = T,
           column_indices = NULL,
           ...) {
    # Get file class
    file_class <-  get_file_class(filepath)
    # Validate file
    check_file(filepath)
    # Read file into data frame
    if (file_class == "file") {
      # Read uncompressed file
      df <- read.delim( file = filepath,
                        sep = file_separator,
                        header = file_header,
                        ...)
    } else if (file_class == "gzfile") {
      # Read compressed file
      gz <- gzfile(filepath, "rt")
      df <- read.delim(file = gz,
                       sep = file_separator,
                       header = file_header,
                       ...)
      close(gz)
    } else {
      # Error out if file type is unknown
      stop(paste("Unknown file type:", filepath))
    }
    # Validate data frame
    check_dataframe(df)
    # If column indices is not null
    if (!is.null(column_indices)) {
      column_indices_to_include <- process_column_indices(column_indices)
      # Check indices are within data frame
      check_dataframe(df, indices = column_indices_to_include)
      # Subset data frame
      colnames_to_include <- colnames(df)[column_indices_to_include]
      df <- as.data.frame(df[,column_indices_to_include], check.names =  FALSE)
      colnames(df) <- colnames_to_include
      # Validate data frame
      check_dataframe(df)
    }
    # Return data frame
    return(df)
  }

################################################################################
#* --                                                                      -- *#
#* --                             prepare_filepath()                       -- *#
#* --                                                                      -- *#
################################################################################

#' Prepare file path
#'
#' @description Prepares a file path by setting the output directory and modifying with a prefix and/or suffix.
#' @param outfile the name of the output file.
#' @param outdir the name of the output directory (defaults to current directory).
#' @param prefix a prefix to be added to the file name.
#' @param suffix a suffix to be added to the file name (before file extension).
#' @param create_dir whether to create directory if it does not exist
#' @importFrom tools file_path_sans_ext
#' @importFrom tools file_ext
#' @return A file path.
#' @export prepare_filepath
prepare_filepath <-
  function(outfile = NULL,
           outdir = NULL,
           prefix = NULL,
           suffix = NULL,
           create_dir = TRUE) {
    if (is.null(outfile))
      stop("Cannot write data to file, outfile is NULL.")
    # If outdir is null, default to current working directory
    if (is.null(outdir)) {
      outdir <- getwd()
      warning(paste("outdir was not set, using working directory:", getwd()))
    } else {
      if (create_dir) {
        # Check that directory exists, if not, create it
        if (!dir.exists(outdir)) {
          dir.create(outdir, recursive = TRUE)
        }
      }
      # Check preset directory is valid
      check_directory(outdir, ignore_empty = TRUE)
    }
    # Add prefix to file name if required
    if (!is.null(prefix))
      outfile <- paste(prefix, basename(outfile), sep = '.')
    # Add suffix to file name if required
    # TODO: will need to consider compressed files like: counts.tsv.gz
    if (!is.null(suffix))
      if (grepl("\\.gz|zip|bz2|tar$", outfile)) {
        outfile <- paste(file_path_sans_ext(outfile, compression = T),
                         suffix, file_ext(outfile), sep = '.')
      } else {
        outfile <- paste(file_path_sans_ext(outfile), suffix, file_ext(outfile), sep = '.')
      }
    # Add output directory
    outfile <- file.path(outdir, outfile)
    return(outfile)
  }

################################################################################
#* --                                                                      -- *#
#* --                          write_dataframe_to_file()                   -- *#
#* --                                                                      -- *#
################################################################################

#' Write data frame to file.
#'
#' @description Writes a data frame to file.
#' @param data a data frame.
#' @param outfile the name of the output file.
#' @param outdir the name of the output directory (defaults to current directory).
#' @param prefix a prefix to be added to the file name.
#' @param suffix a suffix to be added to the file name (before file extension).
#' @param ignore_empty whether to ignore empty file check.
#' @param ... parameters for write.table
#' @importFrom utils write.table
#' @return The class of \code{file}.
#' @export write_dataframe_to_file
write_dataframe_to_file <-
  function(data = NULL,
           outfile = NULL,
           outdir = NULL,
           prefix = NULL,
           suffix = NULL,
           ignore_empty = FALSE,
           ...) {
  # Check whether data or file are null
  if (is.null(data))
    stop("Cannot write data to file, data is NULL.")
  # Prepare output file name
  outfile <- prepare_filepath(outfile = outfile,
                              outdir = outdir,
                              prefix = prefix,
                              suffix = suffix,
                              create_dir = TRUE)
  # Write data to file
  write.table(x = data, file = outfile, ...)
  # Check file
  check_file(outfile, ignore_empty = ignore_empty)
  # Get full file paths
  outfile <- normalizePath(outfile)
  # Return output filename
  return(outfile)
  }

################################################################################
#* --                                                                      -- *#
#* --                          write_rdata_to_file()                       -- *#
#* --                                                                      -- *#
################################################################################

#' Write R data to file.
#'
#' @description Writes R data objects to file.
#' @param data list of objects to save.
#' @param outfile the name of the output file.
#' @param outdir the name of the output directory (defaults to current directory).
#' @param prefix a prefix to be added to the file name.
#' @param suffix a suffix to be added to the file name (before file extension).
#' @importFrom utils write.table
#' @return The class of \code{file}.
#' @export write_rdata_to_file
write_rdata_to_file <-
  function(data = NULL,
           outfile = NULL,
           outdir = NULL,
           prefix = NULL,
           suffix = NULL) {
    # Check whether data or file are null
    if (is.null(data))
      stop("Cannot write data to file, data is NULL.")
    # Prepare output file name
    outfile <- prepare_filepath(outfile = outfile,
                                outdir = outdir,
                                prefix = prefix,
                                suffix = suffix,
                                create_dir = TRUE)
    # Write data to file
    save(data, file = outfile)
    # Check file
    check_file(outfile)
    # Get full file paths
    outfile <- normalizePath(outfile)
    # Return output filename
    return(outfile)
  }

################################################################################
#* --                                                                      -- *#
#* --                          save_plot_with_ggsave()                     -- *#
#* --                                                                      -- *#
################################################################################

#' Save plot with ggsave
#'
#' @description
#' Save a plot using ggsave
#'
#' @param data plot to save.
#' @param outfile the name of the output file.
#' @param outdir the name of the output directory (defaults to current directory).
#' @param prefix a prefix to be added to the file name.
#' @param suffix a suffix to be added to the file name (before file extension).
#' @param ... parameters for ggsave.
#'
#' @return a file path.
#' @importFrom ggplot2 ggsave
#' @export save_plot_with_ggsave
save_plot_with_ggsave <-
  function(data = NULL,
           outfile = NULL,
           outdir = NULL,
           prefix = NULL,
           suffix = NULL,
           ...) {
    # Check data is valid
    if (is.null(data))
      stop("Cannot save plot with ggsave, data is null.")
    # Get plot file path
    filepath <- prepare_filepath(outfile = outfile,
                                 outdir = outdir,
                                 prefix = prefix,
                                 suffix = suffix,
                                 create_dir = TRUE)
    # Save plot
    tryCatch({
      suppressMessages(
        ggsave(filename = basename(filepath),
               path = dirname(filepath),
               plot = data,
               ...))
    }, error = function(e) {
      # Stop if there is an error
      stop(paste("Could not save plot with ggsave:", e))
    })
    # Check output file exists
    check_file(filepath)
    # Return file path
    return(filepath)
  }

################################################################################
#* --                                                                      -- *#
#* --                             save_plot_list()                         -- *#
#* --                                                                      -- *#
################################################################################

#' Save list of plots
#'
#' @description
#' Converts variable value to numeric (where necessary)
#'
#' @param plot_list list of plots to save.
#' @param outdir the name of the output directory (defaults to current directory).
#' @param prefix a prefix to be added to the file name.
#' @param suffix a suffix to be added to the file name (before file extension).
#' @param ... parameters for ggsave.
#'
#' @export save_plot_list
save_plot_list <-
  function(plot_list = NULL,
           outdir = NULL,
           prefix = NULL,
           suffix = NULL,
           ...) {
    # Check data is valid
    if (is.null(plot_list))
      stop("Cannot save plot list, plot_list is null.")
    # Create list of filepaths
    plot_filepaths <- vector()
    # Check plot_list is valid
    if (length(plot_list) == 0) {
       message("Cannot save plot list, plot_list is empty.")
    } else {
      # Loop over plot list
      for (pn in names(plot_list)) {
        message(paste("Saving plot:", pn))
        # Check list item is a plot
        if(!is.ggplot(plot_list[[pn]]))
          stop(paste("Could not save plot from list, item is not a plot:", pn))
        # Get plot file path
        outfile <- paste0(gsub(" ", "_", pn), '.png')
        # Save plot
        filepath <- tryCatch({
          save_plot_with_ggsave(
            data = plot_list[[pn]],
            outfile = outfile,
            outdir = outdir,
            prefix = prefix,
            suffix = suffix,
            ...)
        }, error = function(e) {
          # Stop if there is an error
          stop(paste("Could not save plot from list with ggsave:", e))
        })
        plot_filepaths <- c(plot_filepaths, filepath)
      }
    }
    return(plot_filepaths)
  }
