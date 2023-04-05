# can plot mapping statistics, all samples

    Code
      p$data
    Output
                 sample category num_reads   pct
      1         HELA_T0   mapped       752 71.62
      2  HELA_T15A_CTRL   mapped       712 36.81
      3  HELA_T15B_CTRL   mapped       602 30.18
      4  HELA_T15C_CTRL   mapped       800 47.82
      5   HELA_T15A_OLA   mapped       469 30.10
      6   HELA_T15B_OLA   mapped      1000 74.74
      7   HELA_T15C_OLA   mapped       319 31.21
      8         HELA_T0 unmapped       298 28.38
      9  HELA_T15A_CTRL unmapped      1222 63.19
      10 HELA_T15B_CTRL unmapped      1393 69.82
      11 HELA_T15C_CTRL unmapped       873 52.18
      12  HELA_T15A_OLA unmapped      1089 69.90
      13  HELA_T15B_OLA unmapped       338 25.26
      14  HELA_T15C_OLA unmapped       703 68.79

---

    Code
      p$layers
    Output
      [[1]]
      mapping: fill = ~category 
      geom_col: just = 0.5, width = NULL, na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_stack 
      

# can plot mapping statistics, one sample

    Code
      p$data
    Output
         sample category num_reads   pct
      1 HELA_T0   mapped       752 71.62
      2 HELA_T0 unmapped       298 28.38

---

    Code
      p$layers
    Output
      [[1]]
      mapping: fill = ~category 
      geom_col: just = 0.5, width = NULL, na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_stack 
      

# can plot common barplot, one sample

    Code
      p$data
    Output
         sample total_reads prop_mapped_reads pct_mapped_reads total_counts
      1 HELA_T0        1050           0.71619            71.62          752
        total_sgrnas zero_sgrnas pct_zero_sgrnas low_sgrnas pct_low_sgrnas gini_index
      1            3           1           33.33          1          33.33       0.52

---

    Code
      p$layers
    Output
      [[1]]
      geom_col: just = 0.5, width = NULL, na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_stack 
      

# can plot common barplot, multiple samples

    Code
      p$data
    Output
                sample total_reads prop_mapped_reads pct_mapped_reads total_counts
      1        HELA_T0        1050           0.71619            71.62          752
      2 HELA_T15A_CTRL        1934           0.36815            36.81          712
      3 HELA_T15B_CTRL        1995           0.30175            30.18          602
      4 HELA_T15C_CTRL        1673           0.47818            47.82          800
      5  HELA_T15A_OLA        1558           0.30103            30.10          469
      6  HELA_T15B_OLA        1338           0.74738            74.74         1000
      7  HELA_T15C_OLA        1022           0.31213            31.21          319
        total_sgrnas zero_sgrnas pct_zero_sgrnas low_sgrnas pct_low_sgrnas gini_index
      1            3           1           33.33          1          33.33       0.52
      2            3           1           33.33          1          33.33       0.54
      3            3           1           33.33          1          33.33       0.54
      4            3           0            0.00          0           0.00       0.16
      5            3           1           33.33          1          33.33       0.58
      6            3           0            0.00          1          33.33       0.23
      7            3           1           33.33          1          33.33       0.59

---

    Code
      p$layers
    Output
      [[1]]
      geom_col: just = 0.5, width = NULL, na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_stack 
      

# can plot common barplot, with xlab and ylab

    Code
      p$data
    Output
                sample total_reads prop_mapped_reads pct_mapped_reads total_counts
      1        HELA_T0        1050           0.71619            71.62          752
      2 HELA_T15A_CTRL        1934           0.36815            36.81          712
      3 HELA_T15B_CTRL        1995           0.30175            30.18          602
      4 HELA_T15C_CTRL        1673           0.47818            47.82          800
      5  HELA_T15A_OLA        1558           0.30103            30.10          469
      6  HELA_T15B_OLA        1338           0.74738            74.74         1000
      7  HELA_T15C_OLA        1022           0.31213            31.21          319
        total_sgrnas zero_sgrnas pct_zero_sgrnas low_sgrnas pct_low_sgrnas gini_index
      1            3           1           33.33          1          33.33       0.52
      2            3           1           33.33          1          33.33       0.54
      3            3           1           33.33          1          33.33       0.54
      4            3           0            0.00          0           0.00       0.16
      5            3           1           33.33          1          33.33       0.58
      6            3           0            0.00          1          33.33       0.23
      7            3           1           33.33          1          33.33       0.59

---

    Code
      p$layers
    Output
      [[1]]
      geom_col: just = 0.5, width = NULL, na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_stack 
      

# can plot common barplot, with groups

    Code
      p$data
    Output
                sample total_reads prop_mapped_reads pct_mapped_reads total_counts
      1        HELA_T0        1050           0.71619            71.62          752
      2 HELA_T15A_CTRL        1934           0.36815            36.81          712
      3 HELA_T15B_CTRL        1995           0.30175            30.18          602
      4 HELA_T15C_CTRL        1673           0.47818            47.82          800
      5  HELA_T15A_OLA        1558           0.30103            30.10          469
      6  HELA_T15B_OLA        1338           0.74738            74.74         1000
      7  HELA_T15C_OLA        1022           0.31213            31.21          319
        total_sgrnas zero_sgrnas pct_zero_sgrnas low_sgrnas pct_low_sgrnas gini_index
      1            3           1           33.33          1          33.33       0.52
      2            3           1           33.33          1          33.33       0.54
      3            3           1           33.33          1          33.33       0.54
      4            3           0            0.00          0           0.00       0.16
      5            3           1           33.33          1          33.33       0.58
      6            3           0            0.00          1          33.33       0.23
      7            3           1           33.33          1          33.33       0.59
                 group
      1        HELA_T0
      2 HELA_T15A_CTRL
      3 HELA_T15B_CTRL
      4 HELA_T15C_CTRL
      5  HELA_T15A_OLA
      6  HELA_T15B_OLA
      7  HELA_T15C_OLA

---

    Code
      p$layers
    Output
      [[1]]
      geom_col: just = 0.5, width = NULL, na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_stack 
      

