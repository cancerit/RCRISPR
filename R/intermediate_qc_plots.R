#' Generate violin plot
#'
#' @description Generate violin plot.
#'
#' @param df a data frame.
#' @param ycol column from data to plot
#' @param ylab label for y axis
#'
#' @import dplyr
#' @import tidyr
#' @importFrom ggpubr theme_pubr
#' @importFrom scales pretty_breaks unit_format
#' @export plot_common_violin
plot_common_violin <-
  function(df = NULL,
           ycol = NULL,
           ylab = NULL
           ) {
    # Check data frame
    check_dataframe(df)
    # Check ycol exists and is a column in the dataframe
    if (is.null(ycol))
      stop("Cannot generate violin plot, ycol is null.")
    if (!ycol %in% colnames(df))
      stop(paste("Cannot generate violin plot, ycol is not in data frame:", ycol))
    # Check ylab exists
    if (is.null(ylab))
      stop("Cannot generate violin plot, ylab is null.")
    # Set use_groups to FALSE by default
    groups <- FALSE
    # If group is in column names, set it to TRUE
    if ('group' %in% colnames(df)) {
      if (length(unique(df$group)) > 12) {
        # If there are more than 12 groups, message and don't use groups
        message('Cannot plot more than 12 groups. Setting groups to null.')
      } else {
        groups <- TRUE
      }
    }
    # Build common violin plot
    if (groups == TRUE) {
      p <- tryCatch({
        ggplot(df, aes_string(x = 'sample', y = ycol, fill = 'group')) +
          geom_violin(colour="gray30") +
          labs(x = "", y = ylab, fill = "") +
          scale_fill_brewer(palette = 'Set3') +
          scale_y_continuous(breaks = pretty_breaks(10)) +
          theme_pubr(base_size = 16) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      }, error = function(e) {
        # Stop if there is an error
        stop(paste("Cannot plot read mapping statistics:", e))
      })
    } else {
      p <- tryCatch({
        ggplot(df, aes_string(x = 'sample', y = ycol)) +
          geom_violin(colour="gray30") +
          labs(x = "", y = ylab) +
          scale_fill_brewer(palette = 'Set3') +
          scale_y_continuous(breaks = pretty_breaks(10)) +
          theme_pubr(base_size = 16) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      }, error = function(e) {
        # Stop if there is an error
        stop(paste("Cannot plot read mapping statistics:", e))
      })
    }
    return(p)
  }
