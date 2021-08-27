# read library annotation

    Code
      test_lib_ann_obj
    Output
      An object of class "LibraryAnnotations"
      Slot "filepath":
      [1] "test"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 7
      
      Slot "chr_column":
      [1] 4
      
      Slot "chr_start_column":
      [1] 5
      
      Slot "chr_end_column":
      [1] 6
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "annotations":
                            sgRNA                  SEQ                          LOCUS
      1 A1BG_CACCTTCGAGCTGCTGCGCG CACCTTCGAGCTGCTGCGCG chr19:58858890-58858909_A1BG_-
      2 A1BG_AAGAGCGCCTCGGTCCCAGC AAGAGCGCCTCGGTCCCAGC chr19:58861852-58861871_A1BG_+
      3 A1BG_TGGACTTCCAGCTACGGCGC TGGACTTCCAGCTACGGCGC chr19:58862932-58862951_A1BG_-
          CHR    START      END GENE STRAND
      1 chr19 58858890 58858909 A1BG      -
      2 chr19 58861852 58861871 A1BG      +
      3 chr19 58862932 58862951 A1BG      -
                                                       TARGET
      1 A1BG:ENST00000263100.3:exon_7:chr19:58858700-58859024
      2 A1BG:ENST00000263100.3:exon_6:chr19:58861717-58862035
      3 A1BG:ENST00000263100.3:exon_5:chr19:58862738-58863071
      

# get unprocessed library annotations

    Code
      get_library_annotations(test_lib_ann_obj, processed = F)
    Output
                            sgRNA                  SEQ                          LOCUS
      1 A1BG_CACCTTCGAGCTGCTGCGCG CACCTTCGAGCTGCTGCGCG chr19:58858890-58858909_A1BG_-
      2 A1BG_AAGAGCGCCTCGGTCCCAGC AAGAGCGCCTCGGTCCCAGC chr19:58861852-58861871_A1BG_+
      3 A1BG_TGGACTTCCAGCTACGGCGC TGGACTTCCAGCTACGGCGC chr19:58862932-58862951_A1BG_-
          CHR    START      END GENE STRAND
      1 chr19 58858890 58858909 A1BG      -
      2 chr19 58861852 58861871 A1BG      +
      3 chr19 58862932 58862951 A1BG      -
                                                       TARGET
      1 A1BG:ENST00000263100.3:exon_7:chr19:58858700-58859024
      2 A1BG:ENST00000263100.3:exon_6:chr19:58861717-58862035
      3 A1BG:ENST00000263100.3:exon_5:chr19:58862738-58863071

# get processed library annotations

    Code
      get_library_annotations(test_lib_ann_obj, processed = T)
    Output
                            sgRNA gene   chr    start      end
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG chr19 58858890 58858909
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG chr19 58861852 58861871
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG chr19 58862932 58862951

# can remove guides from library object

    Code
      remove_guides_from_library_annotations_object(library_annotations_object = test_lib_ann_obj,
        guides_to_remove = c("A1BG_CACCTTCGAGCTGCTGCGCG"))
    Output
      An object of class "LibraryAnnotations"
      Slot "filepath":
      [1] "test"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 7
      
      Slot "chr_column":
      [1] 4
      
      Slot "chr_start_column":
      [1] 5
      
      Slot "chr_end_column":
      [1] 6
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "annotations":
                            sgRNA                  SEQ                          LOCUS
      2 A1BG_AAGAGCGCCTCGGTCCCAGC AAGAGCGCCTCGGTCCCAGC chr19:58861852-58861871_A1BG_+
      3 A1BG_TGGACTTCCAGCTACGGCGC TGGACTTCCAGCTACGGCGC chr19:58862932-58862951_A1BG_-
          CHR    START      END GENE STRAND
      2 chr19 58861852 58861871 A1BG      +
      3 chr19 58862932 58862951 A1BG      -
                                                       TARGET
      2 A1BG:ENST00000263100.3:exon_6:chr19:58861717-58862035
      3 A1BG:ENST00000263100.3:exon_5:chr19:58862738-58863071
      

# can remove guides as dataframe from library object

    Code
      remove_guides_from_library_annotations_object(library_annotations_object = test_lib_ann_obj,
        guides_to_remove = data.frame(guide = "A1BG_CACCTTCGAGCTGCTGCGCG", extra = "test"))
    Output
      An object of class "LibraryAnnotations"
      Slot "filepath":
      [1] "test"
      
      Slot "id_column":
      [1] 1
      
      Slot "gene_column":
      [1] 7
      
      Slot "chr_column":
      [1] 4
      
      Slot "chr_start_column":
      [1] 5
      
      Slot "chr_end_column":
      [1] 6
      
      Slot "file_separator":
      [1] "\t"
      
      Slot "file_header":
      [1] TRUE
      
      Slot "annotations":
                            sgRNA                  SEQ                          LOCUS
      2 A1BG_AAGAGCGCCTCGGTCCCAGC AAGAGCGCCTCGGTCCCAGC chr19:58861852-58861871_A1BG_+
      3 A1BG_TGGACTTCCAGCTACGGCGC TGGACTTCCAGCTACGGCGC chr19:58862932-58862951_A1BG_-
          CHR    START      END GENE STRAND
      2 chr19 58861852 58861871 A1BG      +
      3 chr19 58862932 58862951 A1BG      -
                                                       TARGET
      2 A1BG:ENST00000263100.3:exon_6:chr19:58861717-58862035
      3 A1BG:ENST00000263100.3:exon_5:chr19:58862738-58863071
      

