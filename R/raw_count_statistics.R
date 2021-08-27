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
      stop(paste("id_column cannot overlap count columns": id_column, paste(count_column, sep = ",")))
    if (gene_column %in% count_column)
      stop(paste("gene_column cannot overlap count columns": gene_column, paste(count_column, sep = ",")))
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
    # Check data frame
    tryCatch({
      check_dataframe(count_stats)
    }, error = function(e) {
      stop(paste("Cannot calculate count stats:", e))
    })
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
      stop(paste("id_column cannot overlap count columns": id_column, paste(count_column, sep = ",")))
    if (gene_column %in% count_column)
      stop(paste("gene_column cannot overlap count columns": gene_column, paste(count_column, sep = ",")))
    # Get zero count guides
    sample_order <- colnames(count_matrix)[count_column]
    count_matrix <- count_matrix[,c(count_column)]
    check_dataframe(count_matrix)
    low_counts <- count_matrix %>%
      tidyr::gather(sample, counts) %>%
      dplyr::filter(counts < threshold) %>%
      dplyr::group_by(sample) %>%
      dplyr::summarise(!!filter_label := n(), .groups = 'keep')
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
    # Process count columns
    count_column <- process_column_indices(count_column)
    # Validate count matrix
    check_dataframe(count_matrix, indices = c(id_column, gene_column, count_column))
    # Check that id column and gene column aren't in count columns
    if (id_column %in% count_column)
      stop(paste("id_column cannot overlap count columns": id_column, paste(count_column, sep = ",")))
    if (gene_column %in% count_column)
      stop(paste("gene_column cannot overlap count columns": gene_column, paste(count_column, sep = ",")))
    # Get zero count guides
    sample_order <- colnames(count_matrix)[count_column]
    count_matrix <- count_matrix[,c(count_column)]
    gini_index <- count_matrix %>%
      tidyr::gather(sample, counts) %>%
      dplyr::group_by(sample) %>%
      dplyr::summarise(gini_index = calculate_gini_index(counts))
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
      stop(paste("id_column cannot overlap count columns": id_column, paste(count_column, sep = ",")))
    if (gene_column %in% count_column)
      stop(paste("gene_column cannot overlap count columns": gene_column, paste(count_column, sep = ",")))
    # Get zero count guides
    sample_order <- colnames(count_matrix)[count_column]
    count_matrix <- count_matrix[,c(count_column)]
    total_counts <- count_matrix %>%
      tidyr::gather(sample, counts) %>%
      dplyr::group_by(sample) %>%
      dplyr::summarise(total_counts = sum(counts))
    return(total_counts)
  }

#' Calculate gini index
#'
#' @description Calculate gini index.
#'
#' @param x a vector of counts.
#'
#' @export calculate_gini_index
calculate_gini_index <-
  function(x = NULL) {
    # Function derived from:
    # https://github.com/ebartom/NGSbartom/blob/master/lib/python2.7/site-packages/mageck/mageckCountIO.py
    if (is.null(x))
      stop("Cannot calculate gini index on null value.")
    df <- data.frame('count' = x) %>%
      mutate(nrdcnt = case_when(
        count > 0 ~ log(count + 1),
        count == 0 ~ log(1)
      ) )
    xs <- sort(df$nrdcnt)
    n <- length(xs)
    gssum <- 0
    for (i in 1:n) {
      gssum <- gssum + ((i + 1) * xs[i])
    }
    ysum <- sum(xs)
    if (ysum == 0) {
      ysum = 1
    }
    gs <- 1 - (2 * ((n - (gssum / ysum)) / (n - 1)))
    gs <- round(gs, 2)
    return(gs)
  }
