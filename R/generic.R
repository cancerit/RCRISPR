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
#* --                           check_dataframe()                         -- *#
#* --                                                                     -- *#
###############################################################################

#' Check dataframe is not empty and indices are valid
#'
#' @description Check dataframe is not empty and indices exist.
#' @export check_dataframe
#' @param data a dataframe.
#' @param indices vector of indices to check.
#' @param check_na whether to check for NA values.
#' @param check_nan whether to check for NaN values.
check_dataframe <-
  function(data = NULL, indices = NULL, check_na = FALSE, check_nan = FALSE) {
    if (is.null(data))
      stop("Dataframe is null.")
    if (nrow(data) == 0) # Assumes ncol also by default
      stop("Dataframe has no rows.")
    # Check indices don't exceed dataframe columns
    if (!is.null(indices)) {
      for (i in 1:length(indices)) {
        indices[i] <- tryCatch({
          convert_variable_to_integer(indices[i])
        }, error = function(e) {
          # Stop if there is an error
          stop(paste("Cannot convert index to integer:", indices[i]))
        })
        if (indices[i] > ncol(data))
          stop(paste0("Index exceeds dataframe limits: ", indices[i], ", ", ncol(data)))
      }
    }
    # Check whether data frame contains NaN values
    if (check_nan) {
      if (sum(apply(data, 2, function(x) any(is.nan(x)))) != 0)
        stop("Dataframe contains NaN values.")
    }
    # If required, check whether data frame contains NA values
    if (check_na) {
      if (sum(apply(data, 2, function(x) any(is.na(x)))) != 0)
        stop("Dataframe contains NA values.")
    }
    return(TRUE)
  }

###############################################################################
#* --                                                                     -- *#
#* --                 check_is_numeric_and_is_integer()                   -- *#
#* --                                                                     -- *#
###############################################################################

#' Check value is numeric and is an integer.
#'
#' @description Check value is numeric and is an integer.
#' @export check_is_numeric_and_is_integer
#' @param x a value.
#' @returns logical.
check_is_numeric_and_is_integer <-
  function(x) {
    if (is.null(x)) {
      return(FALSE)
    } else if (is.na(x)) {
      return(FALSE)
    } else if (!is.numeric(x)) {
      return(FALSE)
    } else if (x %% 1 != 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

###############################################################################
#* --                                                                     -- *#
#* --                       process_column_indices()                      -- *#
#* --                                                                     -- *#
###############################################################################

#' Process column indices into vector.
#'
#' @description
#' Convert a character string of column indices into a numeric vector.
#'
#' @examples
#' process_column_indices('2,3,4,5')
#' process_column_indices('2:5')
#' process_column_indices('2-5')
#' process_column_indices('2:4,5')
#'
#' @param columns character string of column indices.
#'
#' @return a numeric vector of column indices.
#' @import dplyr
#' @importFrom stringr str_split
#' @export process_column_indices
process_column_indices <-
  function(columns = NULL) {
    # Error if there are no values or input is null
    if (length(columns) < 1 || is.null(columns))
      stop("Cannot process columns (<1 or null).")
    # Split character into a vector
    columns <- columns %>% paste(collapse = ",") %>% str_split(',', simplify = T)
    # Loop through values and expand ranges
    processed_columns <- vector()
    for (i in 1:length(columns)) {
      # If the input is a range, expand the range
      if (grepl("[:-]", columns[i])) {
        # Split the start and end values
        numeric_column_range <- suppressWarnings(
          as.numeric(
            str_split(columns[i], "[\\:\\-]", n = 2, simplify = T)))
        # Check the range values are integers
        if (!check_is_numeric_and_is_integer(numeric_column_range[1]) ||
            !check_is_numeric_and_is_integer(numeric_column_range[2]))
          stop(paste("Column indices contain a non-integer value in range:", columns[i]))
        # Add expanded range into the column vector
        processed_columns <- c(processed_columns,
                               c(numeric_column_range[1]:numeric_column_range[2]))
      } else {
        # If not a range, check it's an integer and return the value
        numeric_column_index <- suppressWarnings(as.numeric(columns[i]))
        # Check value is an integer
        if (!check_is_numeric_and_is_integer(numeric_column_index)) {
          stop(paste("Column indices contain a non-integer value:", columns[i]))
        }
        # Add value into column vector
        processed_columns <- c(processed_columns, numeric_column_index)
      }
    }
    return(processed_columns)
  }

###############################################################################
#* --                                                                     -- *#
#* --                         add_pseudocount()                           -- *#
#* --                                                                     -- *#
###############################################################################

#' Add pseudocount
#'
#' @description Add pseudocount to selected dataframe columns
#' @export add_pseudocount
#' @param data a data frame.
#' @param pseudocount a pseudocount (Default: 5).
#' @param indices column indices.
#' @param ... parameters for `check_dataframe`.
#'
#' @import dplyr
#' @return a data frame.
#' @export add_pseudocount
add_pseudocount <-
  function(data = NULL,
           pseudocount = 5,
           indices = NULL,
           ...) {
    # Validate inputs
    if (is.null(data))
      stop("Cannot add pseudocount, data is null.")
    if (is.null(pseudocount))
      stop("Cannot add pseudocount, pseudocount is null.")
    if (!is.numeric(pseudocount))
      stop("Cannot add pseudocount, pseudocount is not numeric.")
    # Check dataframe
    check_dataframe(data)
    if (is.null(indices)) {
      # If no indices given, apply to full data frame
      # Add pseudocount to all data frame columns
      warning("No indices given, adding pseudocount to all indices")
      data <- data %>% mutate(across(indices, ~ . + pseudocount))
    } else {
      # Process column indices
      indices <- process_column_indices(indices)
      # Add pseudocount to selected data frame columns
      data <- tryCatch({
        data %>% mutate(across(indices, ~ . + pseudocount))
      }, error = function(e) {
        # Stop if there is an error
        stop("Cannot add pseudocount to dataframe.")
      })
    }
    # Check data frame
    check_dataframe(data, ...)
    # Return data frame
    return(data)
  }

###############################################################################
#* --                                                                     -- *#
#* --                       compare_annotations()                         -- *#
#* --                                                                     -- *#
###############################################################################

#' Compare annotations between two dataframes.
#'
#' @description
#' Compares guide identifier and gene symbols between two dataframes
#'
#' @param x data frame to be compared.
#' @param x_id_column the index of column containing unique sgRNA identifiers.
#' @param x_gene_column the index of column containing gene symbols.
#' @param y data frame to be compared.
#' @param y_id_column the index of column containing unique sgRNA identifiers.
#' @param y_gene_column the index of column containing gene symbols.
#' @param ... options for all.equal.character function.
#'
#' @import dplyr
#' @return logical.
#' @export compare_annotations
compare_annotations <-
  function (x = NULL,
            y = NULL,
            x_id_column = 1,
            x_gene_column = 2,
            y_id_column = 1,
            y_gene_column = 2,
            ...) {
    # Check input data is not null
    if (is.null(x) )
      stop("Cannot compare data frames, x is null.")
    if (is.null(y) )
      stop("Cannot compare data frames, y is null.")
    # Check id and gene column indices
    check_is_numeric_and_is_integer(x_id_column)
    check_is_numeric_and_is_integer(x_gene_column)
    check_is_numeric_and_is_integer(y_id_column)
    check_is_numeric_and_is_integer(y_gene_column)
    # Check indices are within data frame
    check_dataframe(x, indices = c(x_id_column, x_gene_column))
    # Check indices are within data frame
    check_dataframe(y, indices = c(y_id_column, y_gene_column))
    # Check number of guides is equal
    if (nrow(x) != nrow(y))
      stop(paste("Cannot compare compare data frames, number of guides differ:", nrow(x), nrow(y)))
    # Compare sgRNA and gene columns between counts and library
    if (!isTRUE( all.equal.character(x[,c(x_id_column, x_gene_column)],
                                     y[,c(y_id_column, y_gene_column)],
                                     ...))) {
      stop("sgRNA IDs and gene names in data frames do not match.")
    }
    return(TRUE)
  }


################################################################################
#* --                                                                      -- *#
#* --                     convert_variable_to_integer()                    -- *#
#* --                                                                      -- *#
################################################################################

#' Converts variable value to integers
#'
#' @description
#' Converts variable value to integer (where necessary)
#'
#' @param x input value
#'
#' @export convert_variable_to_integer
convert_variable_to_integer <-
  function(x = NULL) {
    y <- x
    # Validate input
    if (is.null(y) || length(y) == 0)
      stop("Cannot convert variable to integer, x is null.")
    # Only assign variable if it's not null
    if (!is.null(y)) {
    # Skip if the variable is already an integer
      if (!is.integer(y)) {
        y <- tryCatch({
          strtoi(x)
        }, error = function(e) {
          # Stop if there is an error
          stop(paste("Cannot make variable an integer:", x))
        })
      }
      if (is.na(y))
        stop(paste("Could not convert value to integer:", x))
    }
    return(y)
  }

################################################################################
#* --                                                                      -- *#
#* --                     convert_variable_to_numeric()                    -- *#
#* --                                                                      -- *#
################################################################################

#' Converts variable value to numerics
#'
#' @description
#' Converts variable value to numeric (where necessary)
#'
#' @param x input value
#'
#' @export convert_variable_to_numeric
convert_variable_to_numeric <-
  function(x = NULL) {
    y <- x
    # Validate input
    if (is.null(y) || length(y) == 0)
      stop("Cannot convert variable to numeric, x is null.")
    # Only assign variable if it's not null
    if (!is.null(y)) {
    # Skip if the variable is already an numeric
      if (!is.numeric(y)) {
        y <- tryCatch({
          suppressWarnings(as.numeric(x))
        }, error = function(e) {
          # Stop if there is an error
          stop(paste("Cannot make variable an numeric:", x))
        })
      }
      if (is.na(y))
        stop(paste("Could not convert value to numeric:", x))
    }
    return(y)
  }


################################################################################
#* --                                                                      -- *#
#* --                     get_column_indices()                             -- *#
#* --                                                                      -- *#
################################################################################

#' Converts variable value to numerics
#'
#' @description
#' Converts variable value to numeric (where necessary)
#'
#' @param n column names
#' @param df data frame
#' @param sorted sort output list of columns
#'
#' @export get_column_indices
get_column_indices <-
  function(n = NULL, df = NULL, sorted = FALSE) {
    tryCatch({
      check_dataframe(df)
    }, error = function(e) {
      stop(paste("Cannot get column indices:", e))
    })
    if (is.null(n))
      stop("Cannot get column indices, list of column names is null.")
    if (length(n) == 0)
      stop("Cannot get column indices, list of column names is empty.")
    column_indices <- vector()
    for (cn in n) {
      grep_pattern <- paste0('^', cn, '$')
      column_index <- grep(grep_pattern, colnames(df))
      if (length(column_index) == 0)
        stop(paste("Could not get column index for:", cn))
      column_indices <- c(column_indices, column_index)
    }
    column_indices <- unique(column_indices)
    if (length(column_indices) != length(n))
      stop("Column indices is not the same length as column names.")
    if (sorted)
      column_indices <- sort(column_indices)
    return(column_indices)
}

################################################################################
#* --                                                                      -- *#
#* --                     average_replicates()                             -- *#
#* --                                                                      -- *#
################################################################################

#' Average replicates
#'
#' @description
#' Calculate average of replicates
#'
#' @param data data frame
#' @param gene_column index of column for rownames
#' @param data_columns index of columns to average
#'
#' @export average_replicates
average_replicates <-
  function(data = NULL,
           gene_column = NULL,
           data_columns = NULL) {
    # Error if gene_column is null
    if (is.null(gene_column))
      stop("Cannot average replicates, gene_column is null.")
    # Error if data_columns is null
    if (is.null(data_columns))
      stop("Cannot average replicates, data_columns is null.")
    # Check data frame
    check_dataframe(data, check_na = TRUE, check_nan = TRUE)
    # Process column indices
    column_indices <- process_column_indices(data_columns)
    # Convert gene column to integer
    gene_column <- convert_variable_to_integer(gene_column)
    # Check gene column not in data_columns
    if (gene_column %in% column_indices)
      stop("Cannot average replicates, gene_column is within data_columns.")
    # Process data frame
    processed_data <- tryCatch({
      data.frame('gene' = data[, gene_column],
                 'mean' = rowMeans(data[, column_indices]),
                 check.names = FALSE)
    }, error = function(e) {
      stop(paste("Cannot average replicates:", e))
    })
    # Check data frame
    check_dataframe(processed_data)
    # Return data frame
    return(processed_data)
}
