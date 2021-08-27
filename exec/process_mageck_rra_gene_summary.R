suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(rcrispr))

###############################################################################
#* --                                                                     -- *#
#* --                             OPTIONS                                 -- *#
#* --                                                                     -- *#
###############################################################################

option_list = c(
  mageck_rra_summary_options(),
  shared_output_options()
)
opt_parser <- OptionParser(option_list = option_list)
opt <- tryCatch({
          parse_args(opt_parser)
        }, error = function(e) {
          # Stop if there is an error
          stop(paste("Cannot parse arguments (error):", e, sep = "\n"))
        }, warning = function(w) {
          # Stop if there is a warning
          stop(paste("Cannot parse arguments (warning):", w, sep = "\n"))
        })

###############################################################################
#* --                                                                     -- *#
#* --                          OPTION VALIDATION                          -- *#
#* --                                                                     -- *#
###############################################################################

# Check whether required fields are provided
for (n in c('gene_summary', 'sgrna_summary', 'n_genes', 'fdr')) {
  check_option(n, opt[[n]])
}

# Check FDR can be numeric
opt$fdr <- tryCatch({
  as.numeric(opt$fdr)
}, error = function(e) {
  # Stop if there is an error
  stop(paste("FDR is not numeric:", opt$fdr))
})

###############################################################################
#* --                                                                     -- *#
#* --                          MAIN SCRIPT                                -- *#
#* --                                                                     -- *#
###############################################################################

# Read in gene summary
message("Reading MAGeCK RRA gene summary...")
gene_summary_object <-
  read_mageck_rra_gene_summary(
    filepath = opt$gene_summary
  )

# Read in sgRNA summary
message("Reading MAGeCK RRA sgRNA summary...")
sgrna_summary_object <-
  read_mageck_rra_sgrna_summary(
    filepath = opt$sgrna_summary
  )

# Get significantly enriched genes
message(paste0("Finding significantly enriched genes (pos.fdr < ", opt$fdr, ")..."))
enriched_genes <- get_mageck_gene_summary(object = gene_summary_object,
                                          filters = paste('pos.fdr <', opt$fdr))
enriched_genes <- enriched_genes[order(enriched_genes$pos.rank),]
message(paste("Number of significantly enriched genes:", nrow(enriched_genes)))

# If there are any, write significantly enriched genes to file
if (nrow(enriched_genes) > 0) {
  message("Writing enriched genes to file...")
  outfile <- write_dataframe_to_file(data = enriched_genes$id,
                                     outfile = 'mageck_rra_enriched_genes.tsv',
                                     outdir = opt$outdir,
                                     prefix = opt$prefix,
                                     suffix = opt$suffix,
                                     row.names = FALSE,
                                     quote = FALSE,
                                     sep = "\t")
  message(paste("Enriched genes written to:", outfile))
}

# Get significantly depleted genes
message(paste0("Finding significantly depleted genes (neg.fdr < ", opt$fdr, ")..."))
depleted_genes <- get_mageck_gene_summary(object = gene_summary_object,
                                          filters = paste('neg.fdr <', opt$fdr))
depleted_genes <- depleted_genes[order(depleted_genes$neg.rank),]
message(paste("Number of significantly depleted genes:", nrow(depleted_genes)))

# If there are any, write significantly depleted genes to file
if (nrow(depleted_genes) > 0) {
  message("Writing depleted genes to file...")
  outfile <- write_dataframe_to_file(data = depleted_genes$id,
                                     outfile = 'mageck_rra_depleted_genes.tsv',
                                     outdir = opt$outdir,
                                     prefix = opt$prefix,
                                     suffix = opt$suffix,
                                     row.names = FALSE,
                                     quote = FALSE,
                                     sep = "\t")
  message(paste("depleted genes written to:", outfile))
}

# Plot MAGeCK gene summary volcano
message("Generating MAGeCK gene summary volcano plot...")
mageck_rra_gene_volcano <- tryCatch({
  plot_mageck_rra_gene_volcano(gene_summary_object,
                               n = opt$n_genes,
                               fdr = opt$fdr)
}, error = function(e) {
  # Stop if there is an error
  stop("Could not generate MAGeCK gene volcano plot.", call. = FALSE)
})

# Save MAGeCK gene summary volcano
message("Saving MAGeCK gene summary volcano plot...")
mageck_rra_gene_volcano.fn <- save_plot_with_ggsave(
                                data = mageck_rra_gene_volcano,
                                outfile = 'MAGeCK_gene_volcano.png',
                                outdir = opt$outdir,
                                prefix = opt$prefix,
                                suffix = opt$suffix,
                                device = 'png', dpi = 200,
                                width = 11, height = 11)
message(paste("MAGeCK gene summary volcano saved to:", mageck_rra_gene_volcano.fn))

# Plot sgRNAs if there are enriched genes
if (nrow(enriched_genes) > 0) {
  # Generate MAGeCK sgRNA LFC barplot for top enriched gene
  message(paste("Generating MAGeCK sgRNA LFC barplot for top enriched gene:", enriched_genes$id[1]))
  mageck_rra_sgrna_enriched_barplot <- tryCatch({
    plot_mageck_rra_sgrna_barplot(sgrna_summary_object,
                                  gene = enriched_genes$id[1])
  }, error = function(e) {
    # Stop if there is an error
    stop(paste("Could not generate MAGeCK sgRNA LFC barplot for:",
               enriched_genes$id[1]), call. = FALSE)
  })
  # Save MAGeCK sgRNA LFC barplot for top enriched gene
  message("Saving MAGeCK gene summary volcano plot...")
  mageck_rra_sgrna_enriched_barplot.fn <- save_plot_with_ggsave(
    data = mageck_rra_sgrna_enriched_barplot,
    outfile = paste0('MAGeCK_sgRNA_LFC_barplot_', enriched_genes$id[1], '.png'),
    outdir = opt$outdir,
    prefix = opt$prefix,
    suffix = opt$suffix,
    device = 'png', dpi = 200,
    width = 11, height = 8)
  message(paste0("MAGeCK sgRNA LFC barplot (",
                 enriched_genes$id[1], ") saved to:",
                 mageck_rra_sgrna_enriched_barplot.fn))
}

# Plot sgRNAs if there are depleted genes
if (nrow(depleted_genes) > 0) {
  # Generate MAGeCK sgRNA LFC barplot for top depleted gene
  message(paste("Generating MAGeCK sgRNA LFC barplot for top depleted gene:", depleted_genes$id[1]))
  mageck_rra_sgrna_depleted_barplot <- tryCatch({
    plot_mageck_rra_sgrna_barplot(sgrna_summary_object = sgrna_summary_object,
                                  gene = depleted_genes$id[1])
  }, error = function(e) {
    # Stop if there is an error
    stop(paste("Could not generate MAGeCK sgRNA LFC barplot for:", depleted_genes$id[1]))
  })
  # Save MAGeCK sgRNA LFC barplot for top depleted gene
  message("Saving MAGeCK gene summary volcano plot...")
  mageck_rra_sgrna_depleted_barplot.fn <- save_plot_with_ggsave(
    data = mageck_rra_sgrna_depleted_barplot,
    outfile = paste0('MAGeCK_sgRNA_LFC_barplot_', depleted_genes$id[1], '.png'),
    outdir = opt$outdir,
    prefix = opt$prefix,
    suffix = opt$suffix,
    device = 'png', dpi = 200,
    width = 11, height = 8)
  message(paste0("MAGeCK sgRNA LFC barplot (",
                 depleted_genes$id[1], ") saved to:",
                 mageck_rra_sgrna_depleted_barplot.fn))
}

# Write processed data to .Rdata
if (!is.null(opt$rdata)) {
  message("Writing R data to file...")
  data_list <- list(gene_summary_object,
                    enriched_genes,
                    depleted_genes,
                    mageck_rra_gene_volcano)
  if (nrow(enriched_genes) > 0)
    data_list <- c(data_list, mageck_rra_sgrna_enriched_barplot)
  if (nrow(depleted_genes) > 0)
    data_list <- c(data_list, mageck_rra_sgrna_depleted_barplot)
  rdata_outfile <- write_rdata_to_file(prefix = opt$prefix,
                                       suffix = opt$suffix,
                                       outdir = opt$outdir,
                                       outfile = opt$rdata,
                                       data = data_list)
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")
