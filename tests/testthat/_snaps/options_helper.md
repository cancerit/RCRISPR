# shared output options

    Code
      shared_output_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] "-r"
      
      Slot "long_flag":
      [1] "--rdata"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "rdata"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file for .Rdata"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] "-d"
      
      Slot "long_flag":
      [1] "--outdir"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "outdir"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output directory"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[3]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] "-p"
      
      Slot "long_flag":
      [1] "--prefix"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "prefix"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file prefix"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[4]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] "-s"
      
      Slot "long_flag":
      [1] "--suffix"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "suffix"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file suffix"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# basic outfile options

    Code
      basic_outfile_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] "-o"
      
      Slot "long_flag":
      [1] "--outfile"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "outfile"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# count path options

    Code
      count_path_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] "-c"
      
      Slot "long_flag":
      [1] "--counts"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "counts"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "path to directory of counts or a count matrix"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# count type options

    Code
      count_type_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--counts_type"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "counts_type"
      
      Slot "default":
      [1] "single"
      
      Slot "help":
      [1] "count type (must be either 'single' or 'matrix') [Default: single]"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# count format options

    Code
      count_format_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--no_counts_header"
      
      Slot "action":
      [1] "store_true"
      
      Slot "type":
      [1] "logical"
      
      Slot "dest":
      [1] "no_counts_header"
      
      Slot "default":
      [1] FALSE
      
      Slot "help":
      [1] "count file has no header"
      
      Slot "metavar":
      character(0)
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--counts_delim"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "counts_delim"
      
      Slot "default":
      [1] "\t"
      
      Slot "help":
      [1] "count file delimiter [Default: \\t]"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# count column index options

    Code
      count_column_index_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--count_id_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "count_id_column_index"
      
      Slot "default":
      [1] 1
      
      Slot "help":
      [1] "index of id column in counts [Default: 1]"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--count_gene_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "count_gene_column_index"
      
      Slot "default":
      [1] 2
      
      Slot "help":
      [1] "index of gene column in counts [Default: 2]"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[3]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--count_count_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "count_count_column_index"
      
      Slot "default":
      [1] "3"
      
      Slot "help":
      [1] "index of columns containing counts (e.g. 3 or 3,4,5 or 3-5 or 3-5,6-8) [Default: 3]"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# library annotation options

    Code
      library_annotation_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] "-l"
      
      Slot "long_flag":
      [1] "--library"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "library"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "library file containing guide identifiers and annotations"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# library annotation format options

    Code
      library_annotation_format_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--no_library_header"
      
      Slot "action":
      [1] "store_true"
      
      Slot "type":
      [1] "logical"
      
      Slot "dest":
      [1] "no_library_header"
      
      Slot "default":
      [1] FALSE
      
      Slot "help":
      [1] "library has no header"
      
      Slot "metavar":
      character(0)
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--library_delim"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "library_delim"
      
      Slot "default":
      [1] "\t"
      
      Slot "help":
      [1] "library delimiter [Default: \\t]"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# library annotation column index options

    Code
      library_annotation_column_index_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--library_id_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "library_id_column_index"
      
      Slot "default":
      [1] 1
      
      Slot "help":
      [1] "index of column containing guide id in library annotations [Default: 1]"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--library_gene_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "library_gene_column_index"
      
      Slot "default":
      [1] 2
      
      Slot "help":
      [1] "index of column containing gene name(s) in library annotations"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# library annotation genomic column index options

    Code
      library_annotation_genomic_column_index_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--library_chr_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "library_chr_column_index"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "index of column containing chromosome name in library annotations"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--library_start_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "library_start_column_index"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "index of column containing chromosome start position in library annotations"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[3]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--library_end_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "library_end_column_index"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "index of column containing chromosome end position in library annotations"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# sample metadata options

    Code
      sample_metadata_options()

# sample metadata format options

    Code
      sample_metadata_format_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--no_info_header"
      
      Slot "action":
      [1] "store_true"
      
      Slot "type":
      [1] "logical"
      
      Slot "dest":
      [1] "no_info_header"
      
      Slot "default":
      [1] FALSE
      
      Slot "help":
      [1] "sample metadata has no header"
      
      Slot "metavar":
      character(0)
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--info_delim"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "info_delim"
      
      Slot "default":
      [1] "\t"
      
      Slot "help":
      [1] "sample metadata delimiter [Default: \\t]"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# sample metadata sample filename options

    Code
      sample_metadata_sample_filename_column_index_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--info_filename_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "info_filename_column_index"
      
      Slot "default":
      [1] 1
      
      Slot "help":
      [1] "index of column containing sample count filename in sample mapping [Default: 1]"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# sample metadata sample label options

    Code
      sample_metadata_sample_label_column_index_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--info_label_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "info_label_column_index"
      
      Slot "default":
      [1] 2
      
      Slot "help":
      [1] "index of column containing sample labels in sample mapping [Default: 2]"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# sample metadata sample type options

    Code
      sample_metadata_sample_type_column_index_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--info_plasmid_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "info_plasmid_column_index"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "index of column indicating whether sample type is plasmid in sample mapping"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--info_control_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "info_control_column_index"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "index of column indicating whether sample type is control in sample mapping"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[3]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--info_treatment_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "info_treatment_column_index"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "index of column indicating whether sample type is treatment in sample mapping"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# sample metadata sample group options

    Code
      sample_metadata_sample_group_column_index_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--info_group_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "info_group_column_index"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "index of column containing sample group name in sample mapping"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# sample metadata sample read count options

    Code
      sample_metadata_sample_read_count_column_index_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--info_reads_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "info_reads_column_index"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "index of column containing sample read counts in sample mapping"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# duplicate guide options

    Code
      duplicate_guide_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--duplicate_guides_outfile"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "duplicate_guides_outfile"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file for duplicate guides"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# remove guide options

    Code
      remove_guide_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--guides_to_remove"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "guides_to_remove"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "file of guides to remove"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# MAGeCK RRA summary options

    Code
      mageck_rra_summary_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--gene_summary"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "gene_summary"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "MAGeCK RRA gene summary"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--sgrna_summary"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "sgrna_summary"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "MAGeCK RRA gene summary"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[3]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--n_genes"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "n_genes"
      
      Slot "default":
      [1] "10"
      
      Slot "help":
      [1] "number of genes to label in plots (in each direction)"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[4]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] "-f"
      
      Slot "long_flag":
      [1] "--fdr"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "fdr"
      
      Slot "default":
      [1] "0.05"
      
      Slot "help":
      [1] "maximum FDR value"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# BAGEL normalisation options

    Code
      bagel_normalisation_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--pseudocount"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "double"
      
      Slot "dest":
      [1] "pseudocount"
      
      Slot "default":
      [1] 5
      
      Slot "help":
      [1] "pseudocount to add to sample counts"
      
      Slot "metavar":
      [1] "numeric"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--scaling_factor"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "double"
      
      Slot "dest":
      [1] "scaling_factor"
      
      Slot "default":
      [1] 1e+07
      
      Slot "help":
      [1] "scaling factor for normalised counts"
      
      Slot "metavar":
      [1] "numeric"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# Calculate LFC options

    Code
      calculate_lfc_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--sgrna_outfile"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "sgrna_outfile"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file for sgRNA-level fold change matrix"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--gene_outfile"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "gene_outfile"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file for gene-level fold change matrix"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[3]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--count_matrix_outfile"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "count_matrix_outfile"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file for count matrix"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[4]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--control_indices"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "control_indices"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "pseudocount to add to sample counts"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[5]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--treatment_indices"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "treatment_indices"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "scaling factor for normalised counts"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[6]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--pseudocount"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "double"
      
      Slot "dest":
      [1] "pseudocount"
      
      Slot "default":
      [1] 0.5
      
      Slot "help":
      [1] "pseudocount to add to sample counts"
      
      Slot "metavar":
      [1] "numeric"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# Remove guides with no coordinates options

    Code
      remove_no_coordinate_guide_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--excluded_guides_outfile"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "excluded_guides_outfile"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "file of guides that were remove"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# Filter counts and library by index options

    Code
      filter_by_index_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--filter_indices"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "filter_indices"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "column indices to use for filtering"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--filter_method"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "filter_method"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "filter method ( all, any, mean or median ) [Default: all]"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[3]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--min_reads"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "min_reads"
      
      Slot "default":
      [1] "30"
      
      Slot "help":
      [1] "minimum number of reads for filtering [Default: 30]"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# CRISPRcleanR output options

    Code
      crisprcleanr_output_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--count_matrix_outfile"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "count_matrix_outfile"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file for processed count matrix"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--lfc_matrix_outfile"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "lfc_matrix_outfile"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file for processed log fold change matrix"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[3]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--library_outfile"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "library_outfile"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file for processed library"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# CRISPRcleanR normalisation options

    Code
      crisprcleanr_normalisation_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--min_reads"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "double"
      
      Slot "dest":
      [1] "min_reads"
      
      Slot "default":
      [1] 30
      
      Slot "help":
      [1] "minimum number of reads for filtering [Default: 30]"
      
      Slot "metavar":
      [1] "numeric"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--n_controls"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "n_controls"
      
      Slot "default":
      [1] "30"
      
      Slot "help":
      [1] "number of control samples"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

# CRISPRcleanR correction options

    Code
      crisprcleanr_correction_options()
    Output
      [[1]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--lfc_matrix"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "lfc_matrix"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "log fold change matrix file"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[2]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--no_lfc_header"
      
      Slot "action":
      [1] "store_true"
      
      Slot "type":
      [1] "logical"
      
      Slot "dest":
      [1] "no_lfc_header"
      
      Slot "default":
      [1] FALSE
      
      Slot "help":
      [1] "fold change file has no header"
      
      Slot "metavar":
      character(0)
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[3]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--lfc_delim"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "lfc_delim"
      
      Slot "default":
      [1] "\t"
      
      Slot "help":
      [1] "fold change file delimiter [Default: \\t]"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[4]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--lfc_id_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "lfc_id_column_index"
      
      Slot "default":
      [1] 1
      
      Slot "help":
      [1] "index of id column in fold change matrix [Default: 1]"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[5]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--lfc_gene_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "integer"
      
      Slot "dest":
      [1] "lfc_gene_column_index"
      
      Slot "default":
      [1] 2
      
      Slot "help":
      [1] "index of gene column in fold change matrix [Default: 2]"
      
      Slot "metavar":
      [1] "integer"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[6]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--lfc_lfc_column_index"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "lfc_lfc_column_index"
      
      Slot "default":
      [1] "3"
      
      Slot "help":
      [1] "index of columns containing fold change matrix (e.g. 3 or 3,4,5 or 3-5 or 3-5,6-8) [Default: 3]"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      
      [[7]]
      An object of class "OptionParserOption"
      Slot "short_flag":
      [1] NA
      
      Slot "long_flag":
      [1] "--lfc_gene_matrix_outfile"
      
      Slot "action":
      [1] "store"
      
      Slot "type":
      [1] "character"
      
      Slot "dest":
      [1] "lfc_gene_matrix_outfile"
      
      Slot "default":
      NULL
      
      Slot "help":
      [1] "output file for processed gene-level fold change matrix"
      
      Slot "metavar":
      [1] "character"
      
      Slot "callback":
      NULL
      
      Slot "callback_args":
      list()
      
      

