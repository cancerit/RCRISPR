# read sample metadata

    Code
      test_sample_metadata
    Output
      An object of class "SampleMetadata"
      Slot "filepath":
      [1] "test"
      
      Slot "filename_column":
      [1] 5
      
      Slot "label_column":
      [1] 1
      
      Slot "plasmid_column":
      [1] 2
      
      Slot "control_column":
      [1] 3
      
      Slot "treatment_column":
      [1] 4
      
      Slot "group_column":
      [1] 6
      
      Slot "reads_column":
      [1] 7
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "metadata":
                  filename          label plasmid control treatment     group
      1    test_counts.tsv        HELA_T0       1       0         0        T0
      2 test_counts.tsv.gz CTRL_HELA_T15A       0       1         0 T15A_CTRL
           reads  X
      1 30000000 NA
      2 30000000 NA
      

# get processed sample metadata with reads and group

    Code
      get_sample_metadata(test_sample_metadata, processed = T)
    Output
        filename              label plasmid control treatment     group    reads
      1        0    test_counts.tsv    <NA>       1         0        T0 30000000
      2        0 test_counts.tsv.gz    <NA>       0         1 T15A_CTRL 30000000

# get processed sample metadata with reads only

    Code
      get_sample_metadata(test_sample_metadata, processed = T)
    Output
        filename              label plasmid control treatment    reads
      1        0    test_counts.tsv    <NA>       1         0 30000000
      2        0 test_counts.tsv.gz    <NA>       0         1 30000000

# get processed sample metadata with group only

    Code
      get_sample_metadata(test_sample_metadata, processed = T)
    Output
        filename              label plasmid control treatment     group
      1        0    test_counts.tsv    <NA>       1         0        T0
      2        0 test_counts.tsv.gz    <NA>       0         1 T15A_CTRL

# get unprocessed sample metadata with reads and group

    Code
      get_sample_metadata(test_sample_metadata, processed = F)
    Output
                  filename          label plasmid control treatment     group
      1    test_counts.tsv        HELA_T0       1       0         0        T0
      2 test_counts.tsv.gz CTRL_HELA_T15A       0       1         0 T15A_CTRL
           reads  X
      1 30000000 NA
      2 30000000 NA

# get unprocessed sample metadata with reads only

    Code
      get_sample_metadata(test_sample_metadata, processed = F)
    Output
                  filename          label plasmid control treatment     group
      1    test_counts.tsv        HELA_T0       1       0         0        T0
      2 test_counts.tsv.gz CTRL_HELA_T15A       0       1         0 T15A_CTRL
           reads  X
      1 30000000 NA
      2 30000000 NA

# get unprocessed sample metadata with group only

    Code
      get_sample_metadata(test_sample_metadata, processed = F)
    Output
                  filename          label plasmid control treatment     group
      1    test_counts.tsv        HELA_T0       1       0         0        T0
      2 test_counts.tsv.gz CTRL_HELA_T15A       0       1         0 T15A_CTRL
           reads  X
      1 30000000 NA
      2 30000000 NA

