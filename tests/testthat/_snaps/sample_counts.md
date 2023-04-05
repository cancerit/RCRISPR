# get original counts

    Code
      counts(test_unordered_counts)
    Output
                            sgRNA GENE HELA_T0
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478
      2 1000_AAGAGCGCCTCGGTCCCAGC 1000       0
      3 0001_AAGAGCGCCTCGGTCCCAGC 0001       3
      4 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274
      5 AABC_TGGACTTCCAGCTACGGCGC AABC      12

# get unsorted processed counts

    Code
      counts(test_unordered_counts, processed = T)
    Output
                            sgRNA gene HELA_T0
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478
      2 1000_AAGAGCGCCTCGGTCCCAGC 1000       0
      3 0001_AAGAGCGCCTCGGTCCCAGC 0001       3
      4 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274
      5 AABC_TGGACTTCCAGCTACGGCGC AABC      12

# get sorted processed counts

    Code
      counts(test_unordered_counts, processed = T, sort_ids = T)
    Output
                            sgRNA gene HELA_T0
      3 0001_AAGAGCGCCTCGGTCCCAGC 0001       3
      2 1000_AAGAGCGCCTCGGTCCCAGC 1000       0
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478
      4 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274
      5 AABC_TGGACTTCCAGCTACGGCGC AABC      12

# read uncompressed sample count file

    Code
      test_counts
    Output
      An object of class "SampleCounts"
      Slot "sample_name":
      [1] "HELA_T0"
      
      Slot "filepath":
      [1] "test"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 2
      
      Slot "count_column":
      [1] 3
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "counts":
                            sgRNA GENE HELA_T0
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274
      

# read gzipped sample count file

    Code
      gzipped_test_counts
    Output
      An object of class "SampleCounts"
      Slot "sample_name":
      [1] "CTRL_HELA_T15A"
      
      Slot "filepath":
      [1] "test.gz"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 2
      
      Slot "count_column":
      [1] 3
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "counts":
                            sgRNA GENE HELA_T15A_CTRL
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG            519
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG              0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG            193
      

# read sample count file and strip ids

    Code
      test_counts_stripped
    Output
      An object of class "SampleCounts"
      Slot "sample_name":
      [1] "HELA_T0"
      
      Slot "filepath":
      [1] "test"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 2
      
      Slot "count_column":
      [1] 3
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "counts":
          id gene s1
      1 xy12   g1  1
      

# read sample count file with numeric sample names

    Code
      test_counts_numeric_sample
    Output
      An object of class "SampleCounts"
      Slot "sample_name":
      [1] "1HELA_T0"
      
      Slot "filepath":
      [1] "test"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 2
      
      Slot "count_column":
      [1] 3
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "counts":
                            sgRNA GENE 1HELA_T0
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG      478
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG        0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG      274
      

# combine SampleCounts objects into count matrix

    Code
      test_count_matrix
    Output
                            sgRNA gene HELA_T0 CTRL_HELA_T15A
      1 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0              0
      2 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478            519
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274            193

# combine SampleCounts objects into count matrix and sort ids

    Code
      test_count_matrix
    Output
                            sgRNA gene HELA_T0 CTRL_HELA_T15A
      1 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0              0
      2 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478            519
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274            193

# can read count matrix file

    Code
      read_count_matrix_file(test_count_matrix_file, id_column = 1, gene_column = 2,
        count_column = "3")
    Output
                            sgRNA GENE HELA_T0
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274

# can read count matrix file returning processed matrix

    Code
      read_count_matrix_file(test_count_matrix_file, id_column = 1, gene_column = 2,
        count_column = "3", processed = T)
    Output
                            sgRNA gene HELA_T0
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274

# can read count matrix file and strip ids

    Code
      read_count_matrix_file(filepath = system.file("testdata",
        "test_counts_strip_ids.tsv", package = "rcrispr"), id_column = 1,
      gene_column = 2, count_column = "3", processed = T, strip_ids = T)
    Output
        sgRNA gene s1
      1  xy12   g1  1

# can read count matrix file and sort ids

    Code
      read_count_matrix_file(filepath = test_count_matrix_file, id_column = 1,
        gene_column = 2, count_column = "3", processed = T, sort_ids = T)
    Output
                            sgRNA gene HELA_T0
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274

# can read sample count files

    Code
      count_objects
    Output
      [[1]]
      An object of class "SampleCounts"
      Slot "sample_name":
      [1] "HELA_T0"
      
      Slot "filepath":
      [1] "test"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 2
      
      Slot "count_column":
      [1] 3
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "counts":
                            sgRNA GENE HELA_T0
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274
      
      
      [[2]]
      An object of class "SampleCounts"
      Slot "sample_name":
      [1] "CTRL_HELA_T15A"
      
      Slot "filepath":
      [1] "test"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 2
      
      Slot "count_column":
      [1] 3
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "counts":
                            sgRNA GENE HELA_T15A_CTRL
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG            519
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG              0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG            193
      
      

# can reorder count matrix

    Code
      reorder_count_matrix_by_sample_type(count_matrix = sample_count_matrix,
        sample_metadata_object = test_metadata_obj)
    Output
                            sgRNA gene HELA_T0 CTRL_HELA_T15A
      1 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0              0
      2 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478            519
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274            193

# can remove guides from sample counts

    Code
      remove_guides_from_sample_counts(sample_counts_object = test_counts,
        guides_to_remove = c("A1BG_CACCTTCGAGCTGCTGCGCG"))
    Output
      An object of class "SampleCounts"
      Slot "sample_name":
      [1] "HELA_T0"
      
      Slot "filepath":
      [1] "test"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 2
      
      Slot "count_column":
      [1] 3
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "counts":
                            sgRNA GENE HELA_T0
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274
      

# can remove guides as dataframe from sample counts

    Code
      remove_guides_from_sample_counts(sample_counts_object = test_counts,
        guides_to_remove = data.frame(guide = "A1BG_CACCTTCGAGCTGCTGCGCG", extra = "test"))
    Output
      An object of class "SampleCounts"
      Slot "sample_name":
      [1] "HELA_T0"
      
      Slot "filepath":
      [1] "test"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 2
      
      Slot "count_column":
      [1] 3
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "counts":
                            sgRNA GENE HELA_T0
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274
      

# can remove guides from count matrix

    Code
      remove_guides_from_count_matrix(count_matrix = test_count_matrix, id_column = 1,
        guides_to_remove = c("A1BG_CACCTTCGAGCTGCTGCGCG"))
    Output
                            sgRNA gene HELA_T0 HELA_T15A_CTRL HELA_T15B_CTRL
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0              0              0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274            193            163
        HELA_T15C_CTRL HELA_T15A_OLA HELA_T15B_OLA HELA_T15C_OLA
      2             52             0            16             0
      3            161            80           393            45

# can remove guides as dataframe from count matrix

    Code
      remove_guides_from_count_matrix(count_matrix = test_count_matrix, id_column = 1,
        guides_to_remove = data.frame(guide = "A1BG_CACCTTCGAGCTGCTGCGCG", extra = "test"))
    Output
                            sgRNA gene HELA_T0 HELA_T15A_CTRL HELA_T15B_CTRL
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0              0              0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274            193            163
        HELA_T15C_CTRL HELA_T15A_OLA HELA_T15B_OLA HELA_T15C_OLA
      2             52             0            16             0
      3            161            80           393            45

# can compare sample metadata to count matrix

    Code
      compare_matrix_to_sample_metadata(data = test_count_matrix,
        sample_metadata_object = test_metadata_obj)
    Output
      [1] TRUE

# can get guides failing filter

    Code
      get_guides_failing_filter(count_matrix = test_count_matrix, count_column = 3:4,
      filter_indices = 3:4)
    Output
      [1] "A1BG_AAGAGCGCCTCGGTCCCAGC"

# get guides failing filter with filter_method any

    Code
      get_guides_failing_filter(count_matrix = test_count_matrix, count_column = 3:4,
      filter_indices = 3:4, min_reads = 200, filter_method = "any")
    Output
      [1] "A1BG_AAGAGCGCCTCGGTCCCAGC" "A1BG_TGGACTTCCAGCTACGGCGC"

# get guides failing filter with filter_method all

    Code
      get_guides_failing_filter(count_matrix = test_count_matrix, count_column = 3:4,
      filter_indices = 3:4, min_reads = 200, filter_method = "all")
    Output
      [1] "A1BG_AAGAGCGCCTCGGTCCCAGC"

# get guides failing filter with filter_method mean

    Code
      get_guides_failing_filter(count_matrix = test_count_matrix, count_column = 3:4,
      filter_indices = 3:4, min_reads = 200, filter_method = "mean")
    Output
      [1] "A1BG_AAGAGCGCCTCGGTCCCAGC"

# get guides failing filter with filter_method median

    Code
      get_guides_failing_filter(count_matrix = test_count_matrix, count_column = 3:4,
      filter_indices = 3:4, min_reads = 200, filter_method = "median")
    Output
      [1] "A1BG_AAGAGCGCCTCGGTCCCAGC"

