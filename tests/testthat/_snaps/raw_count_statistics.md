# can generate count matrix stats with one column

    Code
      count_matrix_stats(count_matrix = test_count_matrix)
    Output
         sample total_counts total_sgrnas zero_sgrnas pct_zero_sgrnas low_sgrnas
      1 HELA_T0          752            3           1           33.33          1
        pct_low_sgrnas gini_index
      1          33.33       0.52

# can generate count matrix stats with multiple columns

    Code
      count_matrix_stats(count_matrix = test_count_matrix, count_column = c(3, 5))
    Output
                sample total_counts total_sgrnas zero_sgrnas pct_zero_sgrnas
      1        HELA_T0          752            3           1           33.33
      2 HELA_T15B_CTRL          602            3           1           33.33
        low_sgrnas pct_low_sgrnas gini_index
      1          1          33.33       0.52
      2          1          33.33       0.54

# can generate count matrix stats with total_reads

    Code
      count_matrix_stats(count_matrix = test_count_matrix, count_column = c(3, 5),
      total_reads = c(HELA_T0 = 2000, HELA_T15B_CTRL = 5120))
    Output
                sample total_reads prop_mapped_reads pct_mapped_reads total_counts
      1        HELA_T0        2000           0.37600            37.60          752
      2 HELA_T15B_CTRL        5120           0.11758            11.76          602
        total_sgrnas zero_sgrnas pct_zero_sgrnas low_sgrnas pct_low_sgrnas gini_index
      1            3           1           33.33          1          33.33       0.52
      2            3           1           33.33          1          33.33       0.54

# can generate count matrix stats with zeros

    Code
      count_matrix_stats(count_matrix = data.frame(id = c("sg1", "sg2"), gene = c(
        "g1", "g2"), S1 = rep(0, 2), S2 = rep(0, 2)), count_column = 3:4)
    Output
        sample total_counts total_sgrnas zero_sgrnas pct_zero_sgrnas low_sgrnas
      1     S1            0            2           2             100          2
      2     S2            0            2           2             100          2
        pct_low_sgrnas gini_index
      1            100         -3
      2            100         -3

# can generate count matrix stats with NAs

    Code
      count_matrix_stats(count_matrix = data.frame(id = c("sg1", "sg2"), gene = c(
        "g1", "g2"), S1 = c(NA, 2), S2 = rep(0, 2)), count_column = 3:4)
    Output
        sample total_counts total_sgrnas zero_sgrnas pct_zero_sgrnas low_sgrnas
      1     S1            2            2           1              50          2
      2     S2            0            2           2             100          2
        pct_low_sgrnas gini_index
      1            100          0
      2            100         -3

