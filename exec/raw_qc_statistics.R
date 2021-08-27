suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rcrispr))

###############################################################################
#* --                                                                     -- *#
#* --                             OPTIONS                                 -- *#
#* --                                                                     -- *#
###############################################################################

option_list = c(
  count_path_options(),
  sample_metadata_options(),
  sequencing_qc_options(),
  basic_outfile_options(),
  shared_output_options(),
  count_format_options(),
  sample_metadata_format_options(),
  count_column_index_options(),
  sample_metadata_sample_filename_column_index_options(),
  sample_metadata_sample_label_column_index_options(),
  sample_metadata_sample_type_column_index_options(),
  sample_metadata_sample_group_column_index_options(),
  sample_metadata_sample_read_count_column_index_options()
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
for (n in c('counts')) {
  check_option(n, opt[[n]])
}

if (is.null(opt$outfile))
  opt$outfile <- 'raw_count_qc_summary.tsv'

# Convert column indices to integers
low_counts <- opt$low_counts
for (i in c('low_counts')) {
  if (!is.null(get(i)))
    assign(i, convert_variable_to_integer(get(i)))
}

###############################################################################
#* --                                                                     -- *#
#* --                       MAIN SCRIPT - STATISTICS                      -- *#
#* --                                                                     -- *#
###############################################################################

# Read in sample count matrix
message("Reading count matrix...")
sample_count_matrix <- read_count_matrix_file(
  filepath = opt$counts,
  id_column = opt$count_id_column_index,
  gene_column = opt$count_gene_column_index,
  count_column = opt$count_count_column_index,
  file_separator = opt$counts_delim,
  file_header = ifelse(opt$no_counts_header,FALSE,TRUE),
  processed = T
)

# Read in sample metadata (optional)
message("Reading sample metadata...")
sample_metadata_object <-
  read_sample_metadata_file(
    filepath = opt$info,
    filename_column = opt$info_filename_column_index,
    label_column = opt$info_label_column_index,
    plasmid_column = opt$info_plasmid_column_index,
    control_column = opt$info_control_column_index,
    treatment_column = opt$info_treatment_column_index,
    reads_column = opt$info_reads_column_index,
    group_column =  opt$info_group_column_index,
    file_separator = opt$info_delim,
    file_header = ifelse(opt$no_info_header,FALSE,TRUE),
    check.names = FALSE
  )
sample_metadata <- get_sample_metadata(sample_metadata_object, processed = T)

# If read counts then get them in their own vector
total_reads <- NULL
if (!is.null(opt$info_reads_column_index)) {
  message("Extracting read counts...")
  total_reads <- sample_metadata$reads
  names(total_reads) <- sample_metadata$label
}

# Get raw QC statistics
message("Building raw count QC statistics...")
raw_qc_stats <- count_matrix_stats(count_matrix = sample_count_matrix,
                                   id_column = 1,
                                   gene_column = 2,
                                   count_column = 3:ncol(sample_count_matrix),
                                   low_counts = low_counts,
                                   total_reads = total_reads)

# Write raw count statistics to output file
message("Writing raw count statistics to file...")
outfile <- write_dataframe_to_file(data = raw_qc_stats,
                                   outfile = opt$outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Raw count statistics written to:", outfile))

###############################################################################
#* --                                                                     -- *#
#* --                       MAIN SCRIPT - PLOTS                           -- *#
#* --                                                                     -- *#
###############################################################################
# Set up empty plot filename vector
plot_files <- NULL

if (opt$no_plot) {
  # Message when --no_plot
  message("Plots are disabled.")
} else {
  message("Generating plots...")
  # Set groups for remaining plots
  if (!is.null(opt$info_group_column_index)) {
    message("Adding groups to statistics...")
    # Check samples match between metadata and stats
    if (!identical(sort(sample_metadata$label), sort(raw_qc_stats$sample)))
      stop("Cannot add groups to stats, sample names do not match between stats and sample metadata.")
    raw_qc_stats <- raw_qc_stats %>%
      left_join(sample_metadata %>%
                  select('sample' = label, group), by = 'sample')
  }
  # Generate list of plots for saving
  plot_list <- list()
  if (is.null(opt$info_reads_column_index)) {
    # Don't generate mapping plot if no column index given (info_reads_column_index)
    message("No info_reads_column_index provided, skipping mapping statistics plot.")
  } else {
    # Plot mapping statistics
    message("Generating mapping statistics plot...")
    plot_list[['proportion_mapped_reads_per_sample']] <- plot_mapping_statistics(raw_qc_stats)
  }
  # Plot percent zero sgRNAs
  message("Generating zero sgRNA statistics plot...")
  plot_list[['percentage_zero_guides_per_sample']] <- plot_common_barplot(
                                                        raw_qc_stats,
                                                        ycol = 'pct_zero_sgrnas',
                                                        ylab = 'Guides with no reads assigned (%)')
  # Plot percent low sgRNAs
  message("Generating low sgRNA statistics plot...")
  plot_list[['percentage_low_guides_per_sample']] <- plot_common_barplot(
                                                      raw_qc_stats,
                                                      ycol = 'pct_low_sgrnas',
                                                      ylab = paste0("Guides with <", low_counts, " reads assigned (%)"))
  # Plot percent low sgRNAs
  message("Generating gini index statistics plot...")
  plot_list[['gini_index_per_sample']] <- plot_common_barplot(
                                                      raw_qc_stats,
                                                      ycol = 'gini_index',
                                                      ylab = paste0("Gini index"))
  # Save plots
  message("Saving plots...")
  plot_files <- save_plot_list(plot_list = plot_list,
                               outdir = opt$outdir,
                               prefix = opt$prefix,
                               suffix = opt$suffix,
                               dpi = 300)
}

# Write processed data to .Rdata
if (!is.null(opt$rdata)) {
  message("Writing R data to file...")
  rdata_outfile <- write_rdata_to_file(prefix = opt$prefix,
                                       outfile = opt$rdata,
                                       outdir = opt$outdir,
                                       data = list(sample_count_matrix,
                                                   sample_metadata_object,
                                                   sample_metadata,
                                                   total_reads,
                                                   raw_qc_stats,
                                                   plot_list,
                                                   plot_files))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")
