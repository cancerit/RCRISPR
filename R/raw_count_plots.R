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
## TODO: move this to a config driven graph set

#' Plot read mapping statistics
#'
#' @description Plot read mapping statistics from count statistic data frame.
#'
#' @param df a count statisitic summary produced by `count_matrix_stats` with read mapping stats.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom ggpubr theme_pubr
#' @importFrom scales pretty_breaks unit_format
#' @export plot_mapping_statistics
plot_mapping_statistics <-
  function(df = NULL) {
    # Check data frame
    check_dataframe(df)
    # Process data frame for plot
    processed_df <- tryCatch({
      df %>%
        mutate('unmapped' = total_reads - total_counts,
               'pct_unmapped_reads' = 100 - pct_mapped_reads) %>%
        select(sample, 'mapped' = total_counts, unmapped, pct_unmapped_reads, pct_mapped_reads) %>%
        gather(category, num_reads, -sample, -pct_mapped_reads, -pct_unmapped_reads) %>%
        mutate(pct = ifelse(category == 'mapped', pct_mapped_reads, pct_unmapped_reads)) %>%
        select(-pct_unmapped_reads, -pct_mapped_reads) %>%
        mutate('category' = factor(category, levels = c('unmapped', 'mapped')))
    }, error = function(e) {
      # Stop if there is an error
      stop(paste("Cannot reshape data frame to plot read mapping statistics:", e))
    })
    # Build mapping stats plot
    p <- tryCatch({
      ggplot(processed_df, aes(x = sample, y = num_reads, group = category)) +
        geom_col(aes(fill = category), colour="gray30") +
        #geom_text(aes(x = sample, label = paste0(round(pct, 2), '%'), group = category),
        #          color = "gray20", fontface = 'bold') +
        scale_fill_manual(values = c('unmapped' = "#DEEBF7", 'mapped' = "#9ECAE1")) +
        scale_y_continuous(breaks = pretty_breaks(10), labels = unit_format(unit = "M", scale = 1e-6)) +
        labs(x = "", y = "Number of reads (millions)", fill = "") +
        theme_pubr(base_size = 16) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }, error = function(e) {
      # Stop if there is an error
      stop(paste("Cannot plot read mapping statistics:", e))
    })
    return(p)
  }

#' Generate bar plot
#'
#' @description Generate bar plot.
#'
#' @param df a count statisitic summary produced by `count_matrix_stats` with read mapping stats.
#' @param ycol column from count statistics to plot
#' @param ylab label for y axis
#'
#' @import dplyr
#' @import tidyr
#' @importFrom ggpubr theme_pubr
#' @importFrom scales pretty_breaks unit_format
#' @export plot_common_barplot
plot_common_barplot <-
  function(df = NULL,
           ycol = NULL,
           ylab = NULL
           ) {
    # Check data frame
    check_dataframe(df)
    # Check ycol exists and is a column in the dataframe
    if (is.null(ycol))
      stop("Cannot generate bar plot, ycol is null.")
    if (!ycol %in% colnames(df))
      stop(paste("Cannot generate bar plot, ycol is not in data frame:", ycol))
    # Check ylab exists
    if (is.null(ylab))
      stop("Cannot generate bar plot, ylab is null.")
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
    # Build common bar plot
    if (groups == TRUE) {
      p <- tryCatch({
        ggplot(df, aes_string(x = 'sample', y = ycol, fill = 'group')) +
          geom_col(colour="gray30") +
          labs(x = "", y = ylab, fill = "") +
          scale_fill_brewer(palette = 'Set3') +
          scale_y_continuous(breaks = pretty_breaks(10)) +
          theme_pubr(base_size = 16) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      }, error = function(e) {
        # Stop if there is an error
        stop(paste("Cannot generate bar plot:", e))
      })
    } else {
      p <- tryCatch({
        ggplot(df, aes_string(x = 'sample', y = ycol)) +
          geom_col(colour="gray30") +
          labs(x = "", y = ylab) +
          scale_fill_brewer(palette = 'Set3') +
          scale_y_continuous(breaks = pretty_breaks(10)) +
          theme_pubr(base_size = 16) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      }, error = function(e) {
        # Stop if there is an error
        stop(paste("Cannot generate bar plot:", e))
      })
    }
    return(p)
  }
