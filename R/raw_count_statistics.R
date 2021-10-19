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
#' Build count matrix statistics
#'
#' @description Calculate common CRISPR statistics from a count matrix
#' @details Optionally, total read counts can be given as a vector to `total_reads` with sample names as names for adding total reads (`total_reads`), proportion of mapped reads (`prop_mapped_reads`) and percentage of mapped reads (`pct_mapped_reads`).
#'
#' @param count_matrix a count matrix data frame.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param count_column the index of column containing counts.
#' @param low_counts a threshold below which counts are considered low (default: 30).
#' @param total_reads a vector of read totals with samples as names.
#'
#' @export count_matrix_stats
count_matrix_stats <-
  function(count_matrix = NULL,
           id_column = 1,
           gene_column = 2,
           count_column = 3,
           low_counts = 30,
           total_reads = NULL) {
    # Exit if count_matrix is null
    if(is.null(count_matrix))
      stop("Cannot generate count matrix stats, count_matrix is null.")
    # Exit if id_column is null
    if(is.null(id_column))
      stop("Cannot generate count matrix stats, id_column is null.")
    # Exit if gene_column is null
    if(is.null(gene_column))
      stop("Cannot generate count matrix stats, gene_column is null.")
    # Exit if count_column is null
    if(is.null(count_column))
      stop("Cannot generate count matrix stats, count_column is null.")
    # Exit if low_counts is null
    if(is.null(low_counts))
      stop("Cannot generate count matrix stats, low_counts is null.")
    # Try to convert column indices to integers
    for (i in c('id_column', 'gene_column')) {
      if (!is.null(get(i)))
        assign(i, convert_variable_to_integer(get(i)))
    }
    # Process count columns
    count_column <- process_column_indices(count_column)
    # Validate count matrix
    check_dataframe(count_matrix, indices = c(id_column, gene_column, count_column))
    # Check that id column and gene column aren't in count columns
    if (id_column %in% count_column)
      stop(paste("id_column cannot overlap count columns:", id_column, paste(count_column, sep = ",")))
    if (gene_column %in% count_column)
      stop(paste("gene_column cannot overlap count columns:", gene_column, paste(count_column, sep = ",")))
    # Get number of guides
    total_sgrnas <- data.frame('sample' = colnames(count_matrix)[count_column],
                               'total_sgrnas' = rep(nrow(count_matrix), length(count_column)))
    # Get zero count summary per sample
    zero_sgrnas <- low_counts_per_sample(count_matrix, id_column, gene_column, count_column)
    # Get low count summary per sample
    low_sgrnas <- low_counts_per_sample(count_matrix, id_column, gene_column, count_column,
                                        threshold = low_counts, filter_label = 'low_sgrnas')
    # Get gini index per sample
    gini_index <- gini_index_per_sample(count_matrix, id_column, gene_column, count_column)
    # Get total couints per sample
    total_counts <- total_counts_per_sample(count_matrix, id_column, gene_column, count_column)
    # Build count summary
    count_stats <- total_sgrnas %>%
      dplyr::left_join(total_counts, by = 'sample') %>%
      dplyr::left_join(zero_sgrnas, by = 'sample') %>%
      dplyr::left_join(low_sgrnas, by = 'sample') %>%
      dplyr::left_join(gini_index, by = 'sample') %>%
      dplyr::mutate('pct_zero_sgrnas' = round((zero_sgrnas / total_sgrnas) * 100, 2),
                    'pct_low_sgrnas' = round((low_sgrnas / total_sgrnas) * 100, 2 )) %>%
      select(sample, total_counts, total_sgrnas, zero_sgrnas, pct_zero_sgrnas, low_sgrnas, pct_low_sgrnas, gini_index)
    # If total reads isn't null, then add the read mapping proportions to the count stats
    if (!is.null(total_reads)) {
      if (any(total_reads == 0))
        stop("Cannot calculate total reads as they contain zeros.")
      read_df <- data.frame('sample' = names(total_reads), 'total_reads' = total_reads)
      # Check there are the same sample in stats and total reads
      if (length(intersect(read_df$sample, count_stats$sample)) != length(read_df$sample))
        stop("Cannot calculate total reads, number of samples in total reads and stats do not match.")
      count_stats <- read_df %>%
        right_join(count_stats, by = 'sample') %>%
        rowwise() %>%
        mutate('prop_mapped_reads' = round(total_counts / total_reads, 5),
               'pct_mapped_reads' = round(prop_mapped_reads * 100, 2 ), .after = total_reads)
    }
    count_stats <- as.data.frame(count_stats)
    # Replace NAs with 0s
    count_stats[is.na(count_stats)] <- 0
    # Check data frame
    check_dataframe(count_stats)
    # Check data frame
    check_dataframe(count_stats)
    return(count_stats)
  }

#' Low counts per sample
#'
#' @description Calculate number of low count guides per sample.
#'
#' @param count_matrix a count matrix data frame.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param count_column the index of column containing counts.
#' @param filter_label name of low count column being returned.
#' @param threshold value below which counts are considered low (default: 1).
#'
#' @export low_counts_per_sample
low_counts_per_sample <-
  function(count_matrix = NULL,
           id_column = 1,
           gene_column = 2,
           count_column = 3,
           filter_label = 'zero_sgrnas',
           threshold = 1) {
    # Try to convert column indices to integers
    for (i in c('id_column', 'gene_column')) {
      if (!is.null(get(i)))
        assign(i, convert_variable_to_integer(get(i)))
    }
    # Process count columns
    count_column <- process_column_indices(count_column)
    # Validate count matrix
    check_dataframe(count_matrix, indices = c(id_column, gene_column, count_column))
    # Check that id column and gene column aren't in count columns
    if (id_column %in% count_column)
      stop(paste("id_column cannot overlap count columns:", id_column, paste(count_column, sep = ",")))
    if (gene_column %in% count_column)
      stop(paste("gene_column cannot overlap count columns:", gene_column, paste(count_column, sep = ",")))
    # Get zero count guides
    sample_order <- colnames(count_matrix)[count_column]
    count_matrix_subset <- count_matrix[,c(count_column)]
    # Replace NAs with 0s
    count_matrix_subset[is.na(count_matrix_subset)] <- 0
    if (length(count_column) > 1) {
      check_dataframe(count_matrix_subset)
      low_counts <- count_matrix_subset %>%
        tidyr::gather(sample, counts) %>%
        dplyr::filter(counts < threshold) %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(!!filter_label := n(), .groups = 'keep')
    } else {
      low_counts <- data.frame('sample' = colnames(count_matrix)[count_column], 'counts' = count_matrix_subset)
      low_counts <- low_counts %>%
        dplyr::group_by(sample) %>%
        dplyr::filter(counts < threshold) %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(!!filter_label := n(), .groups = 'keep')
    }
    low_counts <- as.data.frame(low_counts)
    low_counts <- low_counts[match(sample_order, low_counts$sample),]
    # Return number of zero guides per sample
    return(low_counts)
  }

#' Gini index per sample
#'
#' @description Calculate gini index per sample.
#'
#' @param count_matrix a count matrix data frame.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param count_column the index of column containing counts.
#'
#' @export gini_index_per_sample
gini_index_per_sample <-
  function( count_matrix = NULL,
            id_column = 1,
            gene_column = 2,
            count_column = 3) {
    # Exit if id_column is NULL
    if (is.null(id_column))
      stop("Cannot calculate gini index per sample, id_column is null.")
    # Exit if gene_column is NULL
    if (is.null(gene_column))
      stop("Cannot calculate gini index per sample, gene_column is null.")
    # Exit if count_column is NULL
    if (is.null(count_column))
      stop("Cannot calculate gini index per sample, count_column is null.")
    # Process count columns
    count_column <- process_column_indices(count_column)
    # Validate count matrix
    check_dataframe(count_matrix, indices = c(id_column, gene_column, count_column))
    # Check that id column and gene column aren't in count columns
    if (id_column %in% count_column)
      stop(paste("id_column cannot overlap count columns:", id_column, paste(count_column, sep = ",")))
    if (gene_column %in% count_column)
      stop(paste("gene_column cannot overlap count columns:", gene_column, paste(count_column, sep = ",")))
    # Get zero count guides
    sample_order <- colnames(count_matrix)[count_column]
    count_matrix_subset <- count_matrix[,c(count_column)]
    if (length(count_column) > 1) {
      gini_index <- count_matrix_subset %>%
        tidyr::gather(sample, counts) %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(gini_index = calculate_gini_index(counts))
    } else {
      gini_index <- data.frame('sample' = colnames(count_matrix)[count_column], 'counts' = count_matrix_subset)
      gini_index <- gini_index %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(gini_index = calculate_gini_index(counts))
    }
    gini_index <- as.data.frame(gini_index)
    check_dataframe(gini_index)
    return(gini_index)
  }

#' Total counts per sample
#'
#' @description Calculate total counts per sample.
#'
#' @param count_matrix a count matrix data frame.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param count_column the index of column containing counts.
#'
#' @export total_counts_per_sample
total_counts_per_sample <-
  function( count_matrix = NULL,
            id_column = 1,
            gene_column = 2,
            count_column = 3) {
    # Process count columns
    count_column <- process_column_indices(count_column)
    # Validate count matrix
    check_dataframe(count_matrix, indices = c(id_column, gene_column, count_column))
    # Check that id column and gene column aren't in count columns
    if (id_column %in% count_column)
      stop(paste("id_column cannot overlap count columns:", id_column, paste(count_column, sep = ",")))
    if (gene_column %in% count_column)
      stop(paste("gene_column cannot overlap count columns:", gene_column, paste(count_column, sep = ",")))
    # Get zero count guides
    sample_order <- colnames(count_matrix)[count_column]
    count_matrix_subset <- count_matrix[,c(count_column)]
    # Replace NAs with 0s
    count_matrix_subset[is.na(count_matrix_subset)] <- 0
    if (length(count_column) > 1) {
      total_counts <- count_matrix_subset %>%
        tidyr::gather(sample, counts) %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(total_counts = sum(counts))
    } else {
      total_counts <- data.frame('sample' = colnames(count_matrix)[count_column], 'counts' = count_matrix_subset)
      total_counts <- total_counts %>%
        dplyr::group_by(sample) %>%
        dplyr::summarise(total_counts = sum(counts))
    }
    total_counts <- as.data.frame(total_counts)
    # Check data frame
    check_dataframe(total_counts)
    return(total_counts)
  }

#' Calculate gini index
#'
#' @description Calculate gini index.
#'
#' @param x a vector of counts.
#' @param fail_na stop if NAs are found.
#'
#' @export calculate_gini_index
calculate_gini_index <-
  function(x = NULL, fail_na = F) {
    # Function derived from:
    # https://github.com/ebartom/NGSbartom/blob/master/lib/python2.7/site-packages/mageck/mageckCountIO.py
    # Stop if x is null
    if (is.null(x))
      stop("Cannot calculate gini index on null value.")
    # Check vector has values
    if (length(x) == 0)
      stop("Cannot calculate gini index, x is empty.")
    # Process NAs
    nas <- sum(is.na(x))
    # If x is all NAs
    if (nas == length(x))
      stop("Cannot calculate gini index, x only contains NAs.")
    if (nas > 0 && fail_na)
      stop("Cannot calculate gini index, x contains NAs.")
    x <- na.omit(x)
    # If length x is 1, return 0
    if (length(x) == 1)
      return(0)
    df <- data.frame('count' = x) %>%
      mutate(nrdcnt = log(count + 1))
    xs <- sort(df$nrdcnt)
    n <- length(xs)
    gssum <- 0
    for (i in 1:n) {
      gssum <- gssum + (i * xs[i])
    }
    ysum <- sum(xs)
    if (ysum == 0) {
      ysum = 1
    }
    gs <- 1 - (2 * ((n - (gssum / ysum)) / (n - 1)))
    gs <- round(gs, 2)
    return(gs)
  }
