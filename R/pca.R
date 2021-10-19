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
#* --                          prepare_pca()                               -- *#
#* --                                                                      -- *#
################################################################################

#' Prepare data for PCA plot
#'
#' @description Prepare data for PCA plot.
#'
#' @param df a data frame.
#' @param scaled whether to scale data.
#' @param transform whether to transform data.
#' @param log_transform whether to log transform data.
#'
#' @importFrom stats prcomp
#' @export prepare_pca
prepare_pca <-
  function(df = NULL,
           scaled = FALSE,
           transform = FALSE,
           log_transform = FALSE
           ) {
    # Check data frame
    check_dataframe(df)
    # Remove zeros from data frame
    df <- filter_if(df, is.numeric, all_vars((.) != 0))
    # Check data frame
    check_dataframe(df)
    # Log transform if required
    if (log_transform)
      df <- log2(df)
    # Check data frame
    check_dataframe(df)
    # Transform data frame
    if (transform)
      df <- t(df)
    # Check data frame
    check_dataframe(df)
    # Create PCA list
    pca_list <- list()
    # Prepare data
    pca_list[['data']] <- prcomp(df, scale. = scaled)
    pca_list[['variance_explained']] <- data.frame(PC = colnames(pca_list[['data']]$x),
                                                   variance_explained = (pca_list[['data']]$sdev) ^ 2 / sum((pca_list[['data']]$sdev) ^ 2) * 100)
    return(pca_list)
}

################################################################################
#* --                                                                      -- *#
#* --                          plot_pca()                               -- *#
#* --                                                                      -- *#
################################################################################

#' Generate PCA plot.
#'
#' @description Generate PCA plot.
#'
#' @param df a data frame.
#' @param pc_x principal component to plot on x axis
#' @param pc_y principal component to plot on y axis
#'
#' @import ggplot2
#' @export plot_pca
plot_pca <-
  function(df = NULL,
           pc_x = 'PC1',
           pc_y = 'PC2'
           ) {
    # Check data frame
    check_dataframe(df)
    # Check pc_x and pc_y exist in data frame
    if (!pc_x %in% colnames(df))
      stop(paste("Cannot generate PCA plot, pc_x is not in data frame:", pc_x))
    if (!pc_y %in% colnames(df))
      stop(paste("Cannot generate PCA plot, pc_y is not in data frame:", pc_y))
    # Set color to FALSE by default
    color <- FALSE
    # If group is in column names, set it to TRUE
    if ('color' %in% colnames(df)) {
      if (length(unique(df$color)) > 8) {
        # If there are more than 8 unqiue items in color, message and don't use color
        message('Cannot plot more than 8 unique items in color. Setting color to null.')
      } else {
        color <- TRUE
      }
    }
    # Set shape to FALSE by default
    shape <- FALSE
    # If shape is in column names, set it to TRUE
    if ('shape' %in% colnames(df)) {
      if (length(unique(df$shape)) > 25) {
        # If there are more than 25 unique items in shape, message and don't use shape
        message('Cannot plot more than 25 unique items in shape Setting shape to null.')
      } else {
        shape <- TRUE
      }
    }
    pca <- tryCatch({
      if (shape && color) {
        ggplot(df) +
        scale_color_brewer(palette = 'Dark2') +
        scale_x_continuous(breaks = pretty_breaks(10)) +
        scale_y_continuous(breaks = pretty_breaks(10)) +
        theme_pubr(base_size = 16) +
        geom_point(aes_string(x = pc_x, y = pc_y, color = 'color', shape = 'shape'), size = 4) +
        theme(legend.box="vertical")
      } else if (shape) {
        ggplot(df) +
        scale_color_brewer(palette = 'Dark2') +
        scale_x_continuous(breaks = pretty_breaks(10)) +
        scale_y_continuous(breaks = pretty_breaks(10)) +
        theme_pubr(base_size = 16) +
        geom_point(aes_string(x = pc_x, y = pc_y, shape = 'shape'), size = 4)
      } else if (color) {
        ggplot(df) +
        scale_color_brewer(palette = 'Dark2') +
        scale_x_continuous(breaks = pretty_breaks(10)) +
        scale_y_continuous(breaks = pretty_breaks(10)) +
        theme_pubr(base_size = 16) +
        geom_point(aes_string(x = pc_x, y = pc_y, color = 'color'), size = 4)
      } else {
        ggplot(df) +
        scale_color_brewer(palette = 'Dark2') +
        scale_x_continuous(breaks = pretty_breaks(10)) +
        scale_y_continuous(breaks = pretty_breaks(10)) +
        theme_pubr(base_size = 16) +
        geom_point(aes_string(x = pc_x, y = pc_y), size = 4)
      }
    }, error = function(e) {
      # Stop if there is an error
      stop(paste("Cannot generate pca plot:", e))
    })
    return(pca)
}

