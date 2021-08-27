#' Reads MAGeCK RRA gene summary
#'
#' @description
#' Reads in a MAGeCK RRA gene summary file to a `MageckGeneRRA` object.
#'
#' @param filepath character string specifying a file path.
#' @param ... additional read.delim parameters.
#'
#' @return a data frame containing MAGeCK RRA gene summary.
#' @importFrom methods new
#' @importFrom methods validObject
#' @export read_mageck_rra_gene_summary
read_mageck_rra_gene_summary <-
  function(filepath = NULL,
           ...) {
    # Check if filepath is null
    if (is.null(filepath))
      stop("Cannot read MAGeCK RRA gene summary, filepath is null.")
    # Validate input file
    check_file(filepath)
    # Read MAGeCK RRA gene summary file
    gene_summary <- read_file_to_dataframe(filepath = filepath, ...)
    # Validate data frame
    check_dataframe(gene_summary)
    # Try reading gene summary into sobject
    gene_summary_object <- tryCatch({
      # Create new MAGeCK RRA gene summary object
      new(
        "MageckGeneRRA",
        filepath = filepath,
        gene_summary = gene_summary
      )
    }, error = function(e) {
      # Stop if there is an error
      stop(e)
    })
    # Validate MageckGeneRRA object
    validObject(gene_summary_object)
    # Return MageckGeneRRA object
    return(gene_summary_object)
  }

#' Reads MAGeCK RRA sgRNA summary
#'
#' @description
#' Reads in a MAGeCK RRA sgRNA summary file to a `MageckSgrnaRRA` object.
#'
#' @param filepath character string specifying a file path.
#' @param ... additional read.delim parameters.
#'
#' @return a data frame containing MAGeCK RRA sgRNA summary.
#' @importFrom methods new
#' @importFrom methods validObject
#' @export read_mageck_rra_sgrna_summary
read_mageck_rra_sgrna_summary <-
  function(filepath = NULL,
           ...) {
    # Check if filepath is null
    if (is.null(filepath))
      stop("Cannot read MAGeCK RRA sgRNA summary, filepath is null.")
    # Validate input file
    check_file(filepath)
    # Read MAGeCK RRA sgRNA summary file
    sgrna_summary <- read_file_to_dataframe(filepath = filepath, ...)
    # Validate data frame
    check_dataframe(sgrna_summary)
    # Try reading sgRNA summary into object
    sgrna_summary_object <- tryCatch({
      # Create new MAGeCK RRA sgRNA summary object
      new(
        "MageckSgrnaRRA",
        filepath = filepath,
        sgrna_summary = sgrna_summary
      )
    }, error = function(e) {
      # Stop if there is an error
      stop(e)
    })
    # Validate MageckSgrnaRRA object
    validObject(sgrna_summary_object)
    # Return MageckSgrnaRRA object
    return(sgrna_summary_object)
  }

#' Retrieve top n genes from MAGeCK gene summary
#'
#' @description
#' Collects top n genes from a `MageckGeneRRA` object.
#'
#' @param gene_summary_object a `MAGeCKGeneRRA` object.
#' @param n maximum number of genes to retrieve
#' @param fdr maximum FDR threshold
#' @param direction direction of results to retrieve (pos or neg)
#'
#' @return a data frame containing MAGeCK RRA gene results.
#' @import dplyr
#' @export gene_summary_top_n_genes
gene_summary_top_n_genes <-
  function(gene_summary_object = NULL,
           n = 10,
           fdr = 0.05,
           direction = 'neg') {
    # Validate inputs
    if (is.null(gene_summary_object))
      stop("Cannot get top n genes, gene_summary is null.")
    if (!is.numeric(fdr))
      stop(paste("Cannot get top n genes, fdr is not numeric:", fdr))
    if (!direction %in% c('pos', 'neg'))
      stop(paste("Cannot get top n genes, direction is not 'pos' or 'neg':", direction))
    check_is_numeric_and_is_integer(n)
    # Get top n genes by rank and filter by fdr
    filter_string <- paste0(direction, '.rank <= ', n, ' & ', direction, '.fdr < ', fdr)
    top_genes <- get_mageck_gene_summary(gene_summary_object, filter_string)
    # Warn if number of rows returned is less than n
    if (nrow(top_genes) < n)
      warning("Fewer than n top genes returned:", nrow(top_genes))
    return(top_genes)
  }

#' Plot MAGeCK RRA gene volcano
#'
#' @description
#' Volcano plot of MAGeCK RRA gene summary results.
#'
#' @param gene_summary_object a `MAGeCKGeneRRA` object.
#' @param n maximum number of genes to label (in each direction)
#' @param color_enriched color for enriched genes
#' @param color_depleted color for depleted genes
#' @param fdr maximum FDR threshold
#'
#' @return a ggplot2 object.
#' @import dplyr
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
#' @importFrom grDevices col2rgb
#' @export plot_mageck_rra_gene_volcano
plot_mageck_rra_gene_volcano <-
  function( gene_summary_object = NULL,
            n = 10,
            color_enriched = 'royalblue4',
            color_depleted = 'orangered4',
            fdr = 0.05 ) {
    # Validate inputs
    if (is.null(gene_summary_object))
      stop("Cannot plot MAGeCK gene volcano, gene_summary_object is null.")
    if (!is.numeric(fdr))
      stop(paste("Cannot plot MAGeCK gene volcano, fdr is not numeric:", fdr))
    check_is_numeric_and_is_integer(n)
    # Check color_enriched is valid
    tryCatch({
      col2rgb(color_enriched)
    }, error = function(e) {
      # Stop if there is an error
      stop(paste("Cannot plot MAGeCK gene volcano, color_enriched is invalid:", color_enriched))
    })
    # Check color_depleted is valid
    tryCatch({
      col2rgb(color_depleted)
    }, error = function(e) {
      # Stop if there is an error
      stop(paste("Cannot plot MAGeCK gene volcano, color_depleted is invalid:", color_depleted))
    })
    # Get gene_summary data frame
    gene_summary <- get_mageck_gene_summary(gene_summary_object)
    # Set log fold change (LFC)
    # Note: neg.lfc and pos.lfc are identical
    # Set FDR (combined from each direction)
    gene_summary <- gene_summary %>%
      mutate('LFC' = neg.lfc) %>%
      mutate('FDR'= ifelse((max(c(neg.goodsgrna, pos.goodsgrna))>1) &
                             (neg.goodsgrna > pos.goodsgrna), neg.fdr, pos.fdr) )
    # Label enriched and depleted genes
    gene_summary <- gene_summary %>%
      mutate('is_depleted' = ifelse(neg.fdr < fdr, 1, 0)) %>%
      mutate('is_enriched' = ifelse(pos.fdr < fdr, 1, 0))
    # Get top n enriched genes
    filter_enriched_string <- paste0('pos.rank <= ', n, ' & pos.fdr < ', fdr)
    top_n_enriched_genes <- get_mageck_gene_summary(gene_summary_object, filter_enriched_string)
    # Get top n depleted genes
    filter_depleted_string <- paste0('neg.rank <= ', n, ' & neg.fdr < ', fdr)
    top_n_depleted_genes <- get_mageck_gene_summary(gene_summary_object, filter_depleted_string)
    # Validate gene summary
    check_dataframe(gene_summary)
    # Build basic volcano
    mageck_rra_gene_volcano <-
      ggplot(gene_summary, aes(x=LFC, y=-log10(FDR))) +
      geom_point(data = subset(gene_summary, is_enriched == 0 & is_depleted == 0 ),
                 color="grey50", alpha=0.2, size = 0.7) +
      geom_point(data = subset(gene_summary, is_depleted == 1 ),
                 color=color_depleted, alpha=0.8, size = 0.7) +
      geom_point(data = subset(gene_summary, is_enriched == 1 ),
                 color=color_enriched, alpha=0.8, size = 0.7) +
      geom_hline(yintercept = -log10(fdr), linetype = "dotted") +
      geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted") +
      ylab('-log10 FDR') +
      xlab('Log fold change (LFC)') +
      theme_classic()
    # Add labels for enriched genes
    if (nrow(top_n_enriched_genes) > 0) {
      mageck_rra_gene_volcano <-
        mageck_rra_gene_volcano +
        geom_label_repel(data = subset(gene_summary, gene_summary$id %in% top_n_enriched_genes$id),
                         aes(label = id),
                         fontface = 'bold', fill=color_enriched, label.size = 0.2,
                         alpha = 0.75, force = 3 )
    }
    # Add labels for depleted genes
    if (nrow(top_n_depleted_genes) > 0) {
      mageck_rra_gene_volcano <-
        mageck_rra_gene_volcano +
        geom_label_repel(data = subset(gene_summary, gene_summary$id %in% top_n_depleted_genes$id),
                         aes(label = id),
                         fontface = 'bold', fill=color_depleted, label.size = 0.2,
                         alpha = 0.75, force = 3 )
    }
    return(mageck_rra_gene_volcano)
}

#' Plot MAGeCK RRA sgRNA LFC barplot
#'
#' @description
#' Barplot of MAGeCK RRA sgRNA log fold changes (LFC) for a given gene.
#'
#' @param sgrna_summary_object a `MAGeCKGeneRRA` object.
#' @param gene name of gene to plot
#'
#' @return a ggplot2 object.
#' @import dplyr
#' @import ggplot2
#' @export plot_mageck_rra_sgrna_barplot
plot_mageck_rra_sgrna_barplot <-
  function(sgrna_summary_object = NULL,
           gene = NULL) {
    # Validate inputs
    if (is.null(sgrna_summary_object))
      stop("Cannot plot MAGeCK RRA sgRNA LFC barplot, sgrna_summary_object is null.")
    if (is.null(gene))
      stop("Cannot plot MAGeCK RRA sgRNA LFC barplot, gene is null.")
    # Get sgRNA summary result for a given gene
    sgrna_summary <- get_mageck_sgrna_gene_results(
                      object = sgrna_summary_object,
                      gene = gene)
    # Plot sgRNA LFCs
    mageck_rra_sgrna_barplot <-
      ggplot(sgrna_summary, aes(x = sgrna, y = LFC)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      coord_flip() +
      xlab('') +
      ylab('Log fold change') +
      theme_classic() +
      theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
            strip.background = element_rect(color = "black", size = 1))
    # Return plot
    return(mageck_rra_sgrna_barplot)
  }
