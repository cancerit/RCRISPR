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
#* --                     plot_common_violin()                             -- *#
#* --                                                                      -- *#
################################################################################

#' Generate violin plot
#'
#' @description Generate violin plot.
#'
#' @param df a data frame.
#' @param ycol column from data to plot
#' @param ylab label for y axis
#'
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
          geom_violin(colour = "gray30") +
          labs(x = "", y = ylab, fill = "") +
          scale_fill_brewer(palette = 'Set3') +
          scale_y_continuous(breaks = pretty_breaks(10)) +
          theme_pubr(base_size = 16) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      }, error = function(e) {
        # Stop if there is an error
        stop(paste("Cannot generate violin plot:", e))
      })
    } else {
      p <- tryCatch({
        ggplot(df, aes_string(x = 'sample', y = ycol)) +
          geom_violin(colour="gray30") +
          labs(x = "", y = ylab) +
          scale_y_continuous(breaks = pretty_breaks(10)) +
          theme_pubr(base_size = 16) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      }, error = function(e) {
        # Stop if there is an error
        stop(paste("Cannot generate violin plot:", e))
      })
    }
    return(p)
  }

################################################################################
#* --                                                                      -- *#
#* --                     plot_common_density_ridges()                     -- *#
#* --                                                                      -- *#
################################################################################

#' Generate ridgeline density plot
#'
#' @description Generate ridgeline density plot.
#'
#' @param df a data frame.
#' @param xcol column from data to plot
#' @param xlab label for x axis
#'
#' @importFrom ggridges geom_density_ridges
#' @importFrom ggpubr theme_pubr
#' @importFrom scales pretty_breaks unit_format
#' @export plot_common_density_ridges
plot_common_density_ridges <-
  function(df = NULL,
           xcol = NULL,
           xlab = NULL
           ) {
    # Check data frame
    check_dataframe(df)
    # Check xcol exists and is a column in the data frame
    if (is.null(xcol))
      stop("Cannot generate ridgeline denisty plot, xcol is null.")
    if (!xcol %in% colnames(df))
      stop(paste("Cannot generate ridgeline denisty plot, ycol is not in data frame:", xcol))
    # Check xlab exists
    if (is.null(xlab))
      stop("Cannot generate ridgeline denisty plot, xlab is null.")
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
        ggplot(df, aes_string(y = 'sample', x = xcol, fill = 'group')) +
          geom_density_ridges(colour = "gray30", alpha = 0.5) +
          labs(x = xlab, y = "", fill = "") +
          scale_fill_brewer(palette = 'Set3') +
          scale_x_continuous(breaks = pretty_breaks(16)) +
          theme_pubr(base_size = 16) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
      }, error = function(e) {
        # Stop if there is an error
        stop(paste("Cannot generate ridgeline denisty plot:", e))
      })
    } else {
      p <- tryCatch({
        ggplot(df, aes_string(y = 'sample', x = xcol)) +
          geom_density_ridges(colour = "gray30", alpha = 0.5) +
          labs(x = xlab, y = "", fill = "") +
          scale_x_continuous(breaks = pretty_breaks(16)) +
          theme_pubr(base_size = 16) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
      }, error = function(e) {
        # Stop if there is an error
        stop(paste("Cannot generate ridgeline denisty plot:", e))
      })
    }
    return(p)
  }

