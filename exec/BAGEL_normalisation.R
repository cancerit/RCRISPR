suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(rcrispr))

###############################################################################
#* --                                                                     -- *#
#* --                             OPTIONS                                 -- *#
#* --                                                                     -- *#
###############################################################################

option_list = c(
  count_path_options(),
  bagel_normalisation_options(),
  basic_outfile_options(),
  shared_output_options(),
  count_format_options(),
  count_column_index_options()
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
for (n in c('counts', 'pseudocount', 'scaling_factor')) {
  check_option(n, opt[[n]])
}

# Set default output filenames
if (is.null(opt$outfile))
  opt$outfile <- 'count_matrix.BAGEL.normalised.tsv'

###############################################################################
#* --                                                                     -- *#
#* --                          MAIN SCRIPT                                -- *#
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

# Normalise counts
normalised_count_matrix <- bagel_normalise_counts(sample_count_matrix,
                                                  pseudocount = opt$pseudocount,
                                                  scaling_factor = opt$scaling_factor)

# Write processed count matrix to file
message("Writing normalised count matrix to file...")
outfile <- write_dataframe_to_file(data = normalised_count_matrix,
                                   outfile = opt$outfile,
                                   outdir = opt$outdir,
                                   prefix = opt$prefix,
                                   row.names = FALSE,
                                   quote = FALSE,
                                   sep = "\t")
message(paste("Normalised count matrix written to:", outfile))

# Write Rdata to file
if (!is.null(opt$rdata)) {
  message("Writing R data to file...")
  rdata_outfile <- write_rdata_to_file(prefix = opt$prefix,
                                       outfile = opt$rdata,
                                       outdir = opt$outdir,
                                       data = list(sample_count_matrix,
                                                   normalised_count_matrix))
  message(paste("R data written to:", rdata_outfile))
}

# Script exited without error
message("DONE")
