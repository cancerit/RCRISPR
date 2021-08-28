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
#* --                      plot_correlation()                              -- *#
#* --                                                                      -- *#
################################################################################
#' Generate correlation plot
#'
#' @description Generate correlation plot.
#'
#' @param df a data frame.
#' @param cor_columns list of columns to correlate.
#' @param group_column name of column containing groups (maximum of 3 groups).
#' @param method correlation method
#'
#' @import ggplot2
#' @importFrom GGally ggpairs wrap
#' @export plot_correlation
plot_correlation <-
  function(df = NULL,
           cor_columns = NULL,
           group_column = NULL,
           method = 'pearson'
           ) {
  # Check columns to correlate are not null
  if (is.null(cor_columns))
    stop("Cannot plot correlation, cor_columns is null.")
  # Process indices of columns to use for correlation
  cor_column_indices <- process_column_indices(cor_columns)
  # Check indexes are within data frame
  if (!is.null(group_column)) {
    check_dataframe(df, indices = c(cor_column_indices, group_column))
  } else {
    check_dataframe(df, indices = cor_column_indices)
  }
  # Convert group column to integer
  if (!is.null(group_column))
    group_column <- convert_variable_to_integer(group_column)
  # Set groups to FALSE by default
  groups <- FALSE
  # If group_column given set to TRUE
  if (!is.null(group_column)) {
    if (length(unique(df[,group_column])) > 12) {
      # If there are more than 12 groups, message and don't use groups
      message('Cannot plot more than 12 groups. Setting groups to null.')
    } else {
      groups <- TRUE
    }
  }
  # Plot correlations
  # TODO: reduce code duplication
  p <-
    tryCatch({
      if (groups) {
        group_column <- colnames(df)[group_column]
        ggpairs(
          data = df,
          columns = cor_column_indices,
          mapping = ggplot2::aes(color = group_column),
          progress = FALSE,
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
          upper = list(continuous = wrap('cor', fontface = 'bold', size = 2, method = method)),
          lower = list(continuous = wrap("points", alpha = 0.7, size = 0.8, position = position_jitter(height = 3, width = 3)),
                       combo = wrap("dot", alpha = 0.7, size = 0.8))
        ) +
        scale_fill_brewer(palette = 'Set3') +
        scale_color_brewer(palette = 'Set3') +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              strip.text.y = element_text(angle = 0, hjust = 0, face = 'bold', size = 10, color = 'black'),
              strip.text.x = element_text(angle = 90, hjust = 0, face = 'bold', size = 10, color = 'black'),
              strip.background = element_blank())
      } else {
        ggpairs(
          data = df,
          columns = cor_column_indices,
          progress = FALSE,
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
          upper = list(continuous = wrap('cor', fontface = 'bold', size = 2, method = method)),
          lower = list(continuous = wrap("points", alpha = 0.7, size = 0.8, position = position_jitter(height = 3, width = 3)),
                       combo = wrap("dot", alpha = 0.7, size = 0.8))
        ) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              strip.text.y = element_text(angle = 0, hjust = 0, face = 'bold', size = 10, color = 'black'),
              strip.text.x = element_text(angle = 90, hjust = 0, face = 'bold', size = 10, color = 'black'),
              strip.background = element_blank())
      }
    }, error = function(e) {
      # Stop if there is an error
      stop(paste("Cannot plot correlation:", e))
    })
  return(p)
}






