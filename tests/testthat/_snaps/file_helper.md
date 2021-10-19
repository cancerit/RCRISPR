# read uncompressed sample count to dataframe

    Code
      read_file_to_dataframe(filepath = system.file("testdata", "test_counts.tsv",
        package = "rcrispr"), file_separator = "\t", file_header = TRUE)
    Output
                            sgRNA GENE HELA_T0
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274

# read gzipped sample count to dataframe

    Code
      read_file_to_dataframe(filepath = system.file("testdata", "test_counts.tsv.gz",
        package = "rcrispr"), file_separator = "\t", file_header = TRUE)
    Output
                            sgRNA GENE HELA_T15A_CTRL
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG            519
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG              0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG            193

# read uncompressed sample count to dataframe with column indices

    Code
      read_file_to_dataframe(filepath = system.file("testdata", "test_counts.tsv",
        package = "rcrispr"), file_separator = "\t", file_header = TRUE,
      column_indices = 3)
    Output
        HELA_T0
      1     478
      2       0
      3     274

