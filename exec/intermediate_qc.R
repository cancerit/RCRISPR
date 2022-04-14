suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(rcrispr))

###############################################################################
#* --                                                                     -- *#
#* --                             OPTIONS                                 -- *#
#* --                                                                     -- *#
###############################################################################

option_list = c(
  basic_infile_options(),
  sample_metadata_options(),
  intermediate_qc_options(),
  basic_outfile_options(),
  shared_output_options(),
  infile_format_options(),
  sample_metadata_format_options(),
  infile_column_index_options(),
  sample_metadata_sample_filename_column_index_options(),
  sample_metadata_sample_label_column_index_options(),
  sample_metadata_sample_type_column_index_options(),
  sample_metadata_sample_group_column_index_options()
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
for (n in c('infile', 'info', 'infile_id_column_index')) {
  check_option(n, opt[[n]])
}

# Set is_lfc to true if option used
is_fc = FALSE
if (opt$is_fc) {
  is_fc = TRUE
}

# Set is_gene to true if option used
is_gene = FALSE
if (opt$is_gene) {
  is_gene = TRUE
}

# If is_gene is FALSE expect the infile_gene_column_index to be set
if (!is_gene && is.null(opt$infile_gene_column_index))
  stop("When is_gene option is used, infile_gene_column_index is required.")

# If is_gene is TRUE and is_lfc is FALSE
if (is_gene && !is_fc)
  stop("When is_gene option is used, is_fc option must also be used.")

# Set default stats file name
if (is.null(opt$outfile))
  opt$outfile <- 'intermediate_summary.tsv'

###############################################################################
#* --                                                                     -- *#
#* --                       MAIN SCRIPT - STATISTICS                      -- *#
#* --                                                                     -- *#
###############################################################################

# Read in sample data matrix
# NOTE: although options say counts, this can be either a count or lfc matrix (where count column should be set to lfc columns)
if (is_fc) {
  message("Reading sample fold change matrix...")
  sample_data <- read_fold_change_matrix_file(
    filepath = opt$infile,
    id_column = opt$infile_id_column_index,
    gene_column = opt$infile_gene_column_index,
    fc_column = opt$infile_data_column_index,
    file_separator = opt$infile_delim,
    file_header = ifelse(opt$no_infile_header, FALSE, TRUE),
    is_gene = is_gene,
    processed = TRUE,
    check.names = FALSE
  )
} else {
  message("Reading sample count matrix...")
    sample_data <- read_count_matrix_file(
      filepath = opt$infile,
      id_column = opt$infile_id_column_index,
      gene_column = opt$infile_gene_column_index,
      count_column = opt$infile_data_column_index,
      file_separator = opt$infile_delim,
      file_header = ifelse(opt$no_infile_header, FALSE, TRUE),
      processed = TRUE,
      check.names = FALSE
    )
}

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
    group_column =  opt$info_group_column_index,
    file_separator = opt$info_delim,
    file_header = ifelse(opt$no_info_header,FALSE,TRUE),
    check.names = FALSE
  )
sample_metadata <- get_sample_metadata(sample_metadata_object, processed = TRUE)

# Get ordered sample labels
ordered_sample_metadata <- sample_metadata %>%
  arrange(desc(plasmid), desc(control), desc(treatment), desc(label))

# Set sample columns
if (!is_gene) {
  sample_columns = 3:ncol(sample_data)
} else {
  sample_columns = 2:ncol(sample_data)
}

# If --check_names is used, check that count/lfc sample names exist in the sample metadata
if (!opt$no_check_names) {
  message("Comparing matrix column names to sample metadata labels...")
  if (!is_gene) {
    sample_name_check <- compare_matrix_to_sample_metadata(
      data = sample_data,
      id_column = 1,
      gene_column = 2,
      sample_columns = sample_columns,
      sample_metadata_object = sample_metadata_object
    )
  } else {
    sample_name_check <- compare_matrix_to_sample_metadata(
      data = sample_data,
      id_column = NULL,
      gene_column = 1,
      sample_columns = sample_columns,
      sample_metadata_object = sample_metadata_object
    )
  }
  # No need to add another check here, if the names mismatch the function will error out
}

# Collapse data for stats and plots
message("Collapsing data...")
  if (!is_gene) {
    sample_data_narrow <- sample_data %>%
      gather(sample, values, -sgRNA, -gene)
  } else {
    sample_data_narrow <- sample_data %>%
      gather(sample, values, -gene)
  }

# Summarise sample data
message("Building generic intermediate statistics...")
sample_data_statistics <- sample_data_narrow %>%
  group_by(sample) %>%
  summarise('n' = n(),
            'total' = sum(values),
            'mean' = mean(values),
            'median' = median(values),
            'min' = min(values),
            'max' = max(values),
            .groups = 'keep')

# Write raw count statistics to output file
message("Writing generic intermediate to file...")
outfile <- write_dataframe_to_file(data = sample_data_statistics,
                                   outfile = opt$outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   suffix = opt$suffix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Intermediate statistics written to:", outfile))

###############################################################################
#* --                                                                     -- *#
#* --                       MAIN SCRIPT - PLOTS                           -- *#
#* --                                                                     -- *#
###############################################################################

# Get number of samples
num_sample_columns <- length(process_column_indices(sample_columns))

# Set up empty plot filename vector
plot_files <- NULL

if (opt$no_plot) {
  # Message when --no_plot
  message("Plots are disabled.")
} else {
  message("Generating plots...")
  # Generate list of plots for saving
  plot_list <- list()

  # Set groups for remaining plots
  if (!is.null(opt$info_group_column_index)) {
    message("Adding groups...")
    sample_data_narrow <- tryCatch({
       sample_data_narrow %>%
        left_join(sample_metadata %>% select('sample' = label, group), by = 'sample')
      }, error = function(e) {
        # Stop if there is an error
        stop(paste("Cannot add groups to collapsed data:", e))
      })
  }

  # Check that the union of sample_order and sample is the right length
  message("Ordering sample names...")
  if (length(intersect(ordered_sample_metadata$label, sample_data_narrow$sample)) == 0) {
    message("Will not apply sample order, sample names do not exist in metadata.")
  } else {
    sample_data_narrow$sample <- factor(sample_data_narrow$sample, levels = intersect(ordered_sample_metadata$label, sample_data_narrow$sample))
  }

  # Violin plot of count or lfc densities
  message("Generating violin plot...")
  violin_plot_name <- ifelse(is_fc, 'fold_change.violin', 'count_matrix.violin')
  violin_plot_name <- ifelse(is_gene, paste0('gene.', violin_plot_name), paste0('sgrna.', violin_plot_name))
  plot_list[[violin_plot_name]] <- plot_common_violin(sample_data_narrow,
                                                      ycol = 'values',
                                                      ylab = case_when(is_fc & is_gene ~ 'Gene fold changes',
                                                                       is_fc & !is_gene ~ 'sgRNA fold changes',
                                                                       !is_fc & is_gene ~ 'Gene counts',
                                                                       !is_fc & !is_gene ~ 'sgRNA counts',
                                                                       TRUE ~ 'Density'))

  # Ridgeline density plot of count or lfc densities
  message("Generating ridgeline density plot...")
  density_plot_name <- ifelse(is_fc, 'fold_change.density', 'count_matrix.density')
  density_plot_name <- ifelse(is_gene, paste0('gene.', density_plot_name), paste0('sgrna.', density_plot_name))
  plot_list[[density_plot_name]] <- plot_common_density_ridges(sample_data_narrow,
                                                                xcol = 'values',
                                                                xlab = case_when(is_fc & is_gene ~ 'Gene fold changes',
                                                                                 is_fc & !is_gene ~ 'sgRNA fold changes',
                                                                                 !is_fc & is_gene ~ 'Gene counts',
                                                                                 !is_fc & !is_gene ~ 'sgRNA counts',
                                                                                 TRUE ~ 'Density'))

  # Only prepare and plot PCA if there are 2 or more samples
  if (num_sample_columns < 2) {
    message("Cannot generate PCA plot as there are less than 2 sample columns.")
    pca_data <- NULL
  } else {
    message("Preparing data for PCA plots...")
    # Get only sample columns
    if (is_gene) {
      sample_data_pca <- sample_data_narrow %>%
        select(gene, sample, values) %>%
        spread(sample, values) %>%
        select(-gene)
    } else {
      sample_data_pca <- sample_data_narrow %>%
        select(sgRNA, sample, values) %>%
        spread(sample, values) %>%
        select(-sgRNA)
    }
    # Prepare data for PCA plots
    pca_data <- prepare_pca(df = sample_data_pca, transform = T, log_transform = ifelse(is_fc, FALSE, TRUE))

    # Set groups for PCA plots
    pca_data[['processed_data']] <- pca_data[['data']]$x %>% as.data.frame()
    pca_data[['processed_data']] <- pca_data[['processed_data']] %>% rownames_to_column('sample')

    if (!is.null(opt$info_group_column_index)) {
      message("Adding groups to PCA data...")
       pca_data[['processed_data']] <- pca_data[['processed_data']] %>%
        left_join(sample_metadata %>% select('sample' = label, 'color' = group), by = 'sample')
    }
    message("Adding plasmid, control and treatment to PCA data...")
    pca_data[['processed_data']] <- pca_data[['processed_data']] %>%
      left_join(sample_metadata %>% select('sample' = label, plasmid, control, treatment), by = 'sample') %>%
      mutate('shape' = case_when(plasmid == 1 ~ 'plasmid',
                                control == 1 ~ 'control',
                                treatment == 1 ~ 'treatment')) %>%
      select(-control, -plasmid, -treatment)
    if (length(unique(pca_data[['processed_data']]$color)) == 1 && length(unique(pca_data[['processed_data']]$shape)) == 1) {
      message("Samples are all in one group and of one type, skipping PCA plot.")
    } else {
      # Plot PC scree
      message("Generating PCA scree plot...")
      pca_scree_plot_name <- ifelse(is_fc, 'fold_change.PCA_scree', 'count_matrix.PCA_scree')
      pca_scree_plot_name <- ifelse(is_gene, paste0('gene.', pca_scree_plot_name), paste0('sgrna.', pca_scree_plot_name))
      plot_list[[pca_scree_plot_name]] <- plot_common_barplot(pca_data[['variance_explained']],
                                                      xcol = 'PC',
                                                      ycol = 'variance_explained',
                                                      ylab = 'Variance explained (%)')
      # Plot PCA
      message("Plot PCA...")
      pca_plot_name <- ifelse(is_fc, 'fold_change.PCA', 'count_matrix.PCA')
      pca_plot_name <- ifelse(is_gene, paste0('gene.', pca_plot_name), paste0('sgrna.', pca_plot_name))
      plot_list[[pca_plot_name]] <- plot_pca(pca_data[['processed_data']])
    }
  }

  # Save plots
  message("Saving plots...")
  plot_files <- save_plot_list(plot_list = plot_list,
                               outdir = opt$outdir,
                               prefix = opt$prefix,
                               suffix = opt$suffix,
                               dpi = 300)

  # Plot and save correlation plot separately
  if (num_sample_columns < 2) {
    message("Cannot generate correlation plot as there are less than 2 sample columns.")
    correlation_plot <- NULL
    correlation_plot_file <- NULL
  } else {
    # Correlation plot
    message("Generating correlation plot...")
    correlation_plot_name <- ifelse(is_fc, 'fold_change.correlation', 'count_matrix.correlation')
    correlation_plot_name <- ifelse(is_gene, paste0('gene.', correlation_plot_name), paste0('sgrna.', correlation_plot_name))
    correlation_plot <- plot_correlation(sample_data, sample_columns)
    message("Saving correlation plot...")
    correlation_plot_file <- save_plot_with_ggsave(
                                data = correlation_plot,
                                outfile = paste0(correlation_plot_name, '.png'),
                                outdir = opt$outdir,
                                prefix = opt$prefix,
                                suffix = opt$suffix,
                                dpi = 300)
  }
}

# Write processed data to .Rdata
if (!is.null(opt$rdata)) {
  message("Writing R data to file...")
  rdata_outfile <- write_rdata_to_file(prefix = opt$prefix,
                                       suffix = opt$suffix,
                                       outfile = opt$rdata,
                                       outdir = opt$outdir,
                                       data = list(sample_data,
                                                   sample_metadata_object,
                                                   sample_metadata,
                                                   sample_data_narrow,
                                                   sample_data_statistics,
                                                   pca_data,
                                                   correlation_plot,
                                                   correlation_plot_file,
                                                   plot_list,
                                                   plot_files))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")
