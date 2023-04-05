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
#* --                 read_fold_change_matrix_file()                      -- *#
#* --                                                                     -- *#
###############################################################################

#' Read in sample fold change matrix.
#'
#' @description Read file containing fold change matrix into a data frame.
#'
#' @details
#' Reads a fold change matrix file, as defined by `filepath`, into a data frame.
#' Requires annotation column indices be defined. For sgRNA-level fold changes:
#' \itemize{
#' \item `id_column` - column containing guide (sgRNA) identifiers (Default = 1).
#' \item `gene_column` - column containing gene symbols/identifiers (Default = 2).
#' \item `fc_columns` - indices of columns containing sample fold changes.
#' }
#' For gene-level fold changes:
#' \itemize{
#' \item `id_column` - column containing guide (sgRNA) identifiers (Default = 1).
#' \item `fc_columns` - indices of columns containing sample fold changes.
#' }
#' There must also be one or more columns containing sample fold changes whose indices are
#' indicated using `fc_columns`. If `fc_columns` is `NULL` then it is assumed
#' that all columns except `id_column` and/or `gene_column` contain fold change data.
#'
#' Assumes by default that the fold changes file has a header, as defined by `file_header`,
#' and that it is tab-delimited, as defined by `file_separator`.
#'
#' @param filepath Character string specifying a file path.
#' @param file_separator fold changes file separator.
#' @param file_header fold changes file header.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param fc_column vector indices of columns containing sample fold changes.
#' @param is_gene logical of whether fold changes are at sgRNA or gene level.
#' @param processed logical of whether to apply predefined column names.
#' @param ... additional read.delim parameters.
#'
#' @return a data frame containing sample fold changes
#' @export read_fold_change_matrix_file
read_fold_change_matrix_file <-
  function(filepath = NULL,
           file_separator = "\t",
           file_header = TRUE,
           id_column = 1,
           gene_column = NULL,
           fc_column = NULL,
           is_gene = FALSE,
           processed = FALSE,
           ...) {
    # Error if level is gene with null gene_column
    if (is_gene && is.null(gene_column)) {
      stop("Cannot read in a fold change matrix, is_gene is TRUE with NULL gene_column.", call. = F)
    }
    # Error if level is sgRNA with null gene_column
    if (!is_gene && is.null(gene_column)) {
      stop("Cannot read in a fold change matrix, is_gene is FALSE with NULL gene_column.", call. = F)
    }
    # Error if level is sgRNA with null gene_column
    if (!is_gene && is.null(id_column)) {
      stop("Cannot read in a fold change matrix, is_gene is FALSE with NULL id_column.", call. = F)
    }
    # Error out if fold change matrix has no header as we can't determine the sample names
    if (file_header == FALSE) {
      stop("Cannot read in a fold change matrix file without a header.", call. = F)
    }
    # Try reading fold change matrix file into data frame
    df <- tryCatch({
      # Read fold change matrix file into a data frame
      df <- read_file_to_dataframe(
        filepath = filepath,
        file_separator = file_separator,
        file_header = file_header,
        check.names = F,
        ...
      )
    }, error = function(e) {
      # Stop if there is an error
      stop(e)
    })
    # Validate data frame
    check_dataframe(df, check_na = TRUE)
    # Try to make each column an integer if it isn't already
    assign('gene_column', convert_variable_to_integer(get('gene_column')))
    if (!is_gene) {
      assign('id_column', convert_variable_to_integer(get('id_column')))
    }
    # Process indices of fold changes columns
    fc_column_indices <- process_column_indices(fc_column)
    if (!is_gene) {
      # Check none of the column indexes overlap
      if (anyDuplicated(c(id_column, gene_column, fc_column_indices)) != 0)
        stop(paste("Cannot read fold change matrix, duplicate column indices:",
                   c(id_column, gene_column, fc_column_indices)))
      # Check indices are within dataframe
      check_dataframe(df, indices = fc_column_indices)
      # Remove unwanted columns
      df <- df[,c(id_column, gene_column, fc_column_indices)]
      # If processed, set the column names for id_column and gene_column
      if (processed) {
        colnames(df)[1] <- 'sgRNA'
        colnames(df)[2] <- 'gene'
      }
    } else {
      # Check none of the column indexes overlap
      if (anyDuplicated(c(gene_column, fc_column_indices)) != 0)
        stop(paste("Cannot read fold change matrix, duplicate column indices:",
                   c(gene_column, fc_column_indices)))
      # Check indices are within dataframe
      check_dataframe(df, indices = fc_column_indices)
      # Remove unwanted columns
      df <- df[,c(gene_column, fc_column_indices)]
      # If processed, set the column names for id_column
      if (processed) {
        colnames(df)[1] <- 'gene'
      }
    }
    # Validate data frame
    check_dataframe(df, check_na = TRUE)
    # Return fold change matrix
    return(df)
  }

###############################################################################
#* --                                                                     -- *#
#* --                            calculate_lfc()                          -- *#
#* --                                                                     -- *#
###############################################################################

#' Calculate log fold changes.
#'
#' @description Calculate log fold changes.
#'
#' @details
#' Takes the per-guide mean of control samples, adds a pseduocount to
#' all counts (including control mean) and calculates the log2 fold change.
#' Requires column indices be defined:
#' \itemize{
#' \item `id_column` - column containing guide (sgRNA) identifiers (Default = 1).
#' \item `gene_column` - column containing gene symbols/identifiers (Default = 2).
#' \item `control_indices` - indices of columns containing control sample counts.
#' \item `treatment_indices` - indices of columns containing treatment sample counts.
#' }
#'
#' @param data sample count matrix.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param control_indices vector indices of columns containing control sample counts.
#' @param treatment_indices vector indices of columns containing treatment sample counts.
#' @param pseudocount a pseudocount (Default: 0.5).
#'
#' @return a data frame containing sample log fold changes.
#' @export calculate_lfc
calculate_lfc <-
  function(data = NULL,
           id_column = 1,
           gene_column = 2,
           control_indices = NULL,
           treatment_indices = NULL,
           pseudocount = 0.5) {
    # Validate input
    if (is.null(data))
      stop("Cannot calculate LFCs, data is null.")
    if (is.null(control_indices))
      stop("Cannot calculate LFCs, control_indices is null.")
    if (is.null(treatment_indices))
      stop("Cannot calculate LFCs, treatment_indices is null.")
    if (is.null(pseudocount))
      stop("Cannot calculate LFCs, pseudocount is null.")
    if (!is.numeric(pseudocount))
      stop("Cannot calculate LFCs, pseudocount is not numeric.")
    check_is_numeric_and_is_integer(id_column)
    check_is_numeric_and_is_integer(gene_column)
    # Check data
    check_dataframe(data, check_na = T, check_nan = T)
    # Process control and treatment indices
    control_indices <- process_column_indices(control_indices)
    treatment_indices <- process_column_indices(treatment_indices)
    count_indices <- c(control_indices, treatment_indices)
    # Check that control and treatment indices don't overlap
    if (sum(duplicated(count_indices)) > 0)
      stop(paste("Cannot calculate LFC, duplicated indices:",
                 paste(control_indices, sep = ','),
                 paste(treatment_indices, sep = ',')))
    # Get only the data we need
    data <- data[,c(id_column, gene_column, count_indices)]
    # Reformat control indices and get number of samples
    ncontrol <- length(process_column_indices(control_indices))
    control_indices <- c(3:(3 + (ncontrol - 1)))
    ntreatment <- length(process_column_indices(treatment_indices))
    treatment_indices <- c((2 + ncontrol + 1):(2 + ncontrol + ntreatment))
    nsamples <- ncontrol + ntreatment
    # Get mean of control indices
    data <- data.frame(data[1:2],
                       'control_means' = apply(data[3:(4 + (ncontrol - 2))], 1, function(x) mean(x)),
                       data[treatment_indices], check.names = FALSE)
    # Add pseudocount to counts
    data <- add_pseudocount(data,
                            pseudocount = pseudocount,
                            indices = c(3:(3 + ntreatment)))
    # Calculate log fold changes
    lfc <- data.frame(data[1:2],
                      log2(data[,4:(3 + ntreatment)] / data[,3]), check.names = FALSE)
    # Preserve column names when only one treatment sample
    if (length(treatment_indices) == 1) {
      colnames(lfc)[3] <- colnames(data)[4]
    }
    return(lfc)
  }

###############################################################################
#* --                                                                     -- *#
#* --                         calculate_gene_lfc()                        -- *#
#* --                                                                     -- *#
###############################################################################

#' Calculate gene log fold changes.
#'
#' @description Calculate gene log fold changes.
#'
#' @details
#' Takes an sgRNA fold change matrix and averages by gene.
#'
#' Requires column indices be defined:
#' \itemize{
#' \item `id_column` - column containing guide (sgRNA) identifiers (Default = 1).
#' \item `gene_column` - column containing gene symbols/identifiers (Default = 2).
#' }
#'
#' @param data sample sgRNA fold change matrix.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param sample_columns the index of columns containing sample data to process.
#'
#' @import dplyr
#' @importFrom tidyr spread gather
#' @return a data frame containing sample gene-level log fold changes.
#' @export calculate_gene_lfc
calculate_gene_lfc <-
  function(data = NULL,
           id_column = 1,
           gene_column = 2,
           sample_columns = NULL) {
    # Validate input
    if (is.null(data))
      stop("Cannot calculate gene LFCs, data is null.")
    check_is_numeric_and_is_integer(id_column)
    check_is_numeric_and_is_integer(gene_column)
    # Check data
    check_dataframe(data, check_na = T, check_nan = T)
    # If sample indices are not null then check them and subset the data
    if (!is.null(sample_columns)) {
      # Process control and treatment indices
      sample_columns <- process_column_indices(sample_columns)
      # Get subset of data
      data <- data[, c(id_column, gene_column, sample_columns)]
      # Check data
      check_dataframe(data, check_na = T, check_nan = T)
    }
    # Calculate gene level log fold changes
    id_column <- colnames(data)[id_column]
    gene_column <- colnames(data)[gene_column]
    data <- data %>%
              dplyr::select( -!!id_column ) %>%
              tidyr::gather( sample, sgrna_lfc, -!!gene_column, factor_key = TRUE ) %>%
              dplyr::group_by( sample, !!!syms( gene_column ) ) %>%
              dplyr::summarise( lfc = mean( sgrna_lfc ), .groups = 'keep' ) %>%
              tidyr::spread( sample, lfc ) %>%
              dplyr::ungroup() %>%
              as.data.frame()
    # Check data
    check_dataframe(data)
    return(data)
  }
