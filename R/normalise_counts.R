#' Normalise counts using BAGEL method.
#'
#' @description
#' Normalise count matrix using BAGEL method.
#'
#' @details
#' Assumes that sgRNA identifiers and gene names are in the first two columns.
#' First a defined `pseudocount` gets added to all guide counts. Then sample counts
#' are total normalised and multiplied by a scaling factor.
#'
#' @param sample_counts a sample count matrix.
#' @param pseudocount a pseudocount (Default: 5).
#' @param scaling_factor a scaling factor (Default: 10000000).
#'
#' @return dataframe
#' @export bagel_normalise_counts
bagel_normalise_counts <-
  function(sample_counts = NULL,
           scaling_factor = 10000000,
           pseudocount = 5) {
    # Validate inputs
    if (is.null(sample_counts))
      stop("Cannot normalise counts with BAGEL method, sample_counts is null.")
    if (is.null(pseudocount))
      stop("Cannot normalise counts with BAGEL method, pseudocount is null.")
    if (is.null(scaling_factor))
      stop("Cannot normalise counts with BAGEL method, scaling_factor is null.")
    if (!is.numeric(scaling_factor))
      stop("Cannot normalise counts with BAGEL method, scaling_factor is not numeric.")
    if (!is.numeric(pseudocount))
      stop("Cannot normalise counts with BAGEL method, pseudocount is not numeric.")
    # Check data frame
    check_dataframe(sample_counts)
    # Add pseudocount to all counts
    sample_counts <- add_pseudocount(data = sample_counts,
                                     pseudocount = pseudocount,
                                     indices = c(3:ncol(sample_counts)))
    # Total normalisation with scaling factor
    sample_counts <- data.frame(sample_counts[1:2],
                                lapply(sample_counts[3:ncol(sample_counts)],
                                       function(x) (x / sum(x)) * scaling_factor))
    # Check dataframe
    check_dataframe(sample_counts, check_na = T, check_nan = T)
    return(sample_counts)
  }
