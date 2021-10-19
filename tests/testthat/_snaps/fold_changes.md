# can read sgRNA-level fold change matrix file

    Code
      read_fold_change_matrix_file(test_count_matrix_file, id_column = 1,
        gene_column = 2, fc_column = 3:5)
    Output
                            sgRNA GENE HELA_T0 HELA_T15A_CTRL HELA_T15B_CTRL
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478            519            439
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0              0              0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274            193            163

# can read gene-level fold change matrix file

    Code
      read_fold_change_matrix_file(test_count_matrix_file, id_column = NULL,
        gene_column = 2, fc_column = 3:5, is_gene = T)
    Output
        GENE HELA_T0 HELA_T15A_CTRL HELA_T15B_CTRL
      1 A1BG     478            519            439
      2 A1BG       0              0              0
      3 A1BG     274            193            163

# can read sgRNA-level fold change matrix file and return processed data

    Code
      read_fold_change_matrix_file(test_count_matrix_file, id_column = 1,
        gene_column = 2, fc_column = 3:5, processed = T)
    Output
                            sgRNA gene HELA_T0 HELA_T15A_CTRL HELA_T15B_CTRL
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478            519            439
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0              0              0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274            193            163

# can read gene-level fold change matrix fileand return processed data

    Code
      read_fold_change_matrix_file(test_count_matrix_file, id_column = NULL,
        gene_column = 2, fc_column = 3:5, is_gene = T, processed = T)
    Output
        gene HELA_T0 HELA_T15A_CTRL HELA_T15B_CTRL
      1 A1BG     478            519            439
      2 A1BG       0              0              0
      3 A1BG     274            193            163

