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
#* --                       add_bagel_classifications()                   -- *#
#* --                                                                     -- *#
###############################################################################
#' Add BAGEL classifications
#'
#' @description
#' Add BAGEL classifications to data frame.
#'
#' @param data data frame.
#' @param gene_column the index of column containing gene symbols.
#' @param ess vector of essential gene symbols.
#' @param noness vector of non-essential gene symbols.
#'
#' @import dplyr
#' @return logical.
#' @export add_bagel_classifications
add_bagel_classifications <-
  function (data = NULL,
            gene_column = 1,
            ess = NULL,
            noness = NULL) {
    # Check input data is not null
    if (is.null(data))
      stop("Cannot add BAGEL classification, data is null.")
    if (is.null(gene_column))
      stop("Cannot add BAGEL classification, gene_column is null.")
    if (is.null(ess))
      stop("Cannot add BAGEL classification, ess is null.")
    if (is.null(noness))
      stop("Cannot add BAGEL classification, noness is null.")
    # Try to make each column an integer if it isn't already
    assign('gene_column', convert_variable_to_integer(get('gene_column')))
    # Check indices are within data frame
    check_dataframe(data, indices = gene_column)
    # Add bagel classification into data
    gene_colname <- colnames(data)[gene_column]
    processed_data <- data %>%
      mutate('classification' = case_when(!!sym(gene_colname) %in% ess ~ 'essential',
                                          !!sym(gene_colname) %in% noness ~ 'nonessential',
                                          TRUE ~ 'unknown'))
    # Check data frame
    check_dataframe(processed_data)
    # Return processed data
    return(processed_data)
  }

###############################################################################
#* --                                                                     -- *#
#* --                       get_bagel_statistics()                        -- *#
#* --                                                                     -- *#
###############################################################################
#' Prepare BAGEL statistics
#'
#' @description
#' Prepare BAGEL statistics
#'
#' @param data data frame.
#' @param is_gene whether input data is at gene-level.
#' @param is_fc whether input data is at fc-level.
#'
#' @import dplyr
#' @importFrom stats median sd setNames
#' @return dataframe.
#' @export get_bagel_statistics
get_bagel_statistics <-
  function (data = NULL,
            is_gene = FALSE,
            is_fc = FALSE) {
    # Check input data is not null
    if (is.null(data))
      stop("Cannot add BAGEL classification, data is null.")
    # Check required column names are present
    if(length(intersect(c('sample', 'values', 'classification'), colnames(data))) != 3)
      stop("Cannot add BAGEL classification, required columns (sample, values, classification) not present.")
    # Check data
    check_dataframe(data)
    # Summarise sample data
    message("Building BAGEL classification statistics...")
    sample_stats <- data %>%
      filter(classification != 'unknown') %>%
      group_by(sample, classification) %>%
      summarise('n' = n(),
                'mean' = round(mean(values), 2),
                'median' = median(values),
                'min' = min(values),
                'max' = max(values), .groups = 'keep') %>%
      gather(stat, val, -sample, -classification) %>%
      mutate(stat = paste0(stat, "_", classification)) %>%
      ungroup() %>%
      select(-classification) %>%
      unique() %>%
      spread(stat, val) %>%
      mutate('total_essential' = length(ess[,1]),
             'prop_bagel_essential' = round(n_essential / total_essential, 2)) %>%
      relocate('n_essential', .after = sample) %>%
      relocate('total_essential', .before = n_essential) %>%
      relocate('prop_bagel_essential', .after = n_essential) %>%
      mutate('total_nonessential' = length(ess[,1]),
             'prop_bagel_nonessential' = round(n_nonessential / total_nonessential, 2)) %>%
      relocate('n_nonessential', .after = prop_bagel_essential) %>%
      relocate('total_nonessential', .before = n_nonessential) %>%
      relocate('prop_bagel_nonessential', .after = n_nonessential)
    # If is gene-level and fold changes, calculate NNMD and Glass' delta
    if (is_fc && is_gene) {
      nnmd_and_glass_delta <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('sample', 'NNMD', 'Essential Glass Delta'))
      for (sample_name in unique(data$sample)) {
        essentials <- data %>% filter(sample == sample_name & classification == 'essential') %>% pull(values)
        nonessentials <- data %>% filter(sample == sample_name & classification == 'nonessential') %>% pull(values)
        mean_fc_essentials <- mean(essentials)
        mean_fc_nonessentials <- mean(nonessentials)
        sd_fc_essentials <- sd(essentials)
        sd_fc_nonessentials <- sd(nonessentials)
        nnmd_mean_diff <- mean_fc_essentials - mean_fc_nonessentials
        essential_glass_delta_mean_diff <- abs(mean_fc_essentials) - mean_fc_nonessentials
        nnmd <- nnmd_mean_diff / sd_fc_nonessentials
        glass_delta <- essential_glass_delta_mean_diff / sd_fc_essentials
        nnmd <- formatC(round(nnmd, 3), 3, format = "f")
        glass_delta <- formatC(round(glass_delta, 3), 3, format = "f")
        sample_nnmd_and_glass_delta <- data.frame('sample' = sample_name,
                                                  'NNMD' = nnmd,
                                                  'Essential.Glass.Delta' = glass_delta)
        nnmd_and_glass_delta <- rbind(nnmd_and_glass_delta, sample_nnmd_and_glass_delta)
      }
      sample_stats <- sample_stats %>%
        left_join(nnmd_and_glass_delta, by = 'sample') %>%
        relocate(NNMD, .after = sample) %>%
        relocate(Essential.Glass.Delta, .after = NNMD)
    }
  # Check dataframe
  check_dataframe(sample_stats)
  # Return sample stats
  return(sample_stats)
}

###############################################################################
#* --                                                                     -- *#
#* --                       get_bagel_statistics()                        -- *#
#* --                                                                     -- *#
###############################################################################
#' Prepare BAGEL statistics
#'
#' @description
#' Prepare BAGEL statistics
#'
#' @param data data frame.
#' @param gene_column index of column containing gene names.
#' @param data_column index of column containing values.
#' @param classification_column index of column containing BAGEL classifications.
#'
#' @import dplyr
#' @return dataframe
#' @export prepare_essentiality_data
# Based on code from Clare Pacini
prepare_essentiality_data <-
  function (data = NULL,
            gene_column = 1,
            data_column = 2,
            classification_column = 3,
            threshold = 0.05) {
    # Check data frame
    check_dataframe(data)
    # Check threshold is not null
    if (is.null(threshold))
      stop("Cannot prepare essentiality data, threshold is null.")
    # Check threshold is numeric
    if (!is.numeric(threshold))
      stop(paste("Cannot prepare essentiality data, threshold is not numeric:", threshold))
    # Subset data
    data <- data[,c(gene_column, data_column, classification_column)]
    colnames(data) <- c('gene', 'values', 'classification')
    # Get genes that are essential
    all_genes <- data %>% filter(classification != 'unknown') %>% pull(gene) %>% unique()
    if (length(all_genes) == 0)
      stop("Cannot scale data, no essential or non-essential genes found.")
    # Get essential genes
    essential_genes <- data %>% filter(classification == 'essential') %>% pull(gene) %>% unique()
    if (length(essential_genes) == 0)
      stop("Cannot scale data, no essential genes found.")
    # Prepare the response dataframe for pROC
    # Essential = 1, Non-essential = 0
    essentiality <- rep(0, length(all_genes))
    names (essentiality) <- all_genes
    essentiality[essential_genes] <- 1
    # Get predicted values for each observation
    essentiality_data <- list()
    essentiality_data[['essentiality']] <- essentiality
    essentiality_data[['predictor']] <- data %>% filter(gene %in% all_genes) %>% pull(values)
    essentiality_data[['min']] <- min(essentiality_data[['predictor']], na.rm = TRUE)
    essentiality_data[['modified_predictor']] <-  essentiality_data[['predictor']] - essentiality_data[['min']]
    # Return essentiality data
    return(essentiality_data)
}

###############################################################################
#* --                                                                     -- *#
#* --                           process_roc()                             -- *#
#* --                                                                     -- *#
###############################################################################

#' Process ROC
#'
#' @description
#' Process ROC
#'
#' @param roc_obj pROC ROC object.
#' @param predictor_min minimum value of predictor.
#' @param threshold threshold value.
#'
#' @return dataframe
#' @export process_roc
# Based on code from Clare Pacini
process_roc <-
  function(roc_obj = NULL,
           predictor_min = NULL,
           threshold = 0.05 ) {
    # Check roc_obj is not null
    if (is.null(roc_obj))
      stop("Cannot process ROC, roc_obj is null.")
    # Check predictor min is not null
    if (is.null(predictor_min))
      stop("Cannot process ROC, predictor_min is null.")
    # Check predictor_min is numeric
    if (!is.numeric(predictor_min))
      stop(paste("Cannot process ROC, predictor_min is not numeric:", predictor_min))
    # Get coordinates
    roc_coords <- coords(roc_obj, ret = c('all'), transpose = T)
    roc_coords['threshold',] <- roc_coords['threshold',] + predictor_min
    # Add id
    id <- min(which(roc_coords['ppv',] > (1 - threshold)))
    if( id == "Inf" ){
      id <- min(which(roc_coords['ppv',] >= (1 - threshold)))
    }
    if( id == "Inf" ){
      id <- max(which(round(roc_coords['ppv',]) >= (1 - threshold)))
    }
    # Get best precision
    best_prec <- c(roc_coords['threshold',id],
                   roc_coords['specificity',id],
                   roc_coords['sensitivity',id],
                   roc_coords['ppv',id])
    # Get thresholds
    bestPrecisionTh <- rbind(best_prec)
    colnames(bestPrecisionTh) <- c('thresholds', 'specificity', 'sensitivity', 'ppv')
    # Return threshold data
    return(bestPrecisionTh)
}

###############################################################################
#* --                                                                     -- *#
#* --                             plot_roc()                              -- *#
#* --                                                                     -- *#
###############################################################################

#' Plot ROC
#'
#' @description
#' Plot ROC
#'
#' @param roc_obj pROC `roc` object.
#'
#' @import ggplot2
#' @importFrom ggpubr theme_pubr
#'
#' @return ggplot
#' @export plot_roc
# Based on code from Clare Pacini
plot_roc <-
  function( roc_obj ) {
    roc_obj.coords <- data.frame('FPR' = (1 - roc_obj$specificities),
                                 'TPR' = roc_obj$sensitivities)
    roc_obj.plot <- ggplot(roc_obj.coords, aes(x = FPR, y = TPR)) +
                      geom_line(size = 0.8, color = 'dodgerblue4') +
                      xlab('FPR (1 - Specificity)') +
                      ylab('TPR (Sensitivity)') +
                      theme_pubr() +
                      theme(text = element_text(size = 14)) +
                      annotate("text",
                               x = 0.9,
                               y = 1.05,
                               vjust = 0,
                               size = 5,
                               label = paste("AUC =", sprintf("%.3f", roc_obj$auc)))
    return( roc_obj.plot )
}

