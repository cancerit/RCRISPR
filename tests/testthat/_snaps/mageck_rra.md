# MageckGeneRRA filter returns results

    Code
      get_mageck_gene_summary(test_gene_summary_obj, filters = "neg.fdr < 0.05")
    Output
          id num  neg.score neg.p.value neg.fdr neg.rank neg.goodsgrna neg.lfc
      1 NEG1   9 7.1884e-13  2.6873e-07 6.6e-05        1             9 -2.7520
      2 NEG2   5 4.9934e-12  2.6873e-07 6.6e-05        2             5 -2.7056
      3 NEG3   5 1.3418e-11  2.6873e-07 6.6e-05        3             5 -3.7000
        pos.score pos.p.value pos.fdr pos.rank pos.goodsgrna pos.lfc
      1         1           1       1    18422             0 -2.7520
      2         1           1       1    18421             0 -2.7056
      3         1           1       1    18420             0 -3.7000

# read MAGeCK RRA gene summary

    Code
      test_gene_summary_obj
    Output
      An object of class "MageckGeneRRA"
      Slot "filepath":
      [1] "test"
      
      Slot "gene_summary":
             id num  neg.score neg.p.value  neg.fdr neg.rank neg.goodsgrna neg.lfc
      1    NEG1   9 7.1884e-13  2.6873e-07 0.000066        1             9 -2.7520
      2    NEG2   5 4.9934e-12  2.6873e-07 0.000066        2             5 -2.7056
      3    NEG3   5 1.3418e-11  2.6873e-07 0.000066        3             5 -3.7000
      4 NONSIG1   5 3.2729e-06  1.4243e-03 0.105000      239             5 -1.4231
      5 NONSIG2   5 3.2846e-06  1.4243e-03 0.105000      240             4 -1.9873
      6 NONSIG3   5 3.3205e-06  1.4243e-03 0.105000      241             5 -2.3140
      7    POS3   5 1.0000e+00  1.0000e+00 1.000000    18420             0  1.1215
      8    POS2   5 1.0000e+00  1.0000e+00 1.000000    18421             0  1.2517
      9    POS1   5 1.0000e+00  1.0000e+00 1.000000    18422             0  1.2391
         pos.score pos.p.value  pos.fdr pos.rank pos.goodsgrna pos.lfc
      1 1.0000e+00  1.0000e+00 1.000000    18422             0 -2.7520
      2 1.0000e+00  1.0000e+00 1.000000    18421             0 -2.7056
      3 1.0000e+00  1.0000e+00 1.000000    18420             0 -3.7000
      4 1.0000e+00  9.9999e-01 1.000000    18214             0 -1.4231
      5 9.7746e-01  9.7759e-01 1.000000    16700             0 -1.9873
      6 9.9998e-01  9.9998e-01 1.000000    18093             0 -2.3100
      7 1.0721e-08  2.6873e-07 0.000309        9             5  1.1215
      8 3.2063e-09  2.6873e-07 0.000309        5             5  1.2517
      9 2.0448e-10  2.6873e-07 0.000309        2             5  1.2391
      

# MageckSgrnaRRA filter returns results

    Code
      get_mageck_sgrna_summary(test_sgrna_summary_obj, filters = "FDR < 0.05")
    Output
         sgrna  Gene control_count        treatment_count control_mean treat_mean
      1 sgRNA1  NEG1       142.690   961.11/850.43/454.83      142.690  850.43000
      2 sgRNA2  NEG1        90.208   630.66/592.44/243.95       90.208  592.44000
      3 sgRNA3  NEG1       167.950   792.56/323.78/1074.7      167.950  792.56000
      4 sgRNA4 gene2       535.670   135.08/105.16/90.989      535.670  105.16000
      5 sgRNA5 gene2       187.630   34.896/4.6038/3.3691      187.630    4.60380
      6 sgRNA6 gene2        87.091 0.33972/0.33972/4.9179       87.091    0.33972
            LFC control_var  adj_var   score      p.low     p.high p.twosided
      1  2.5669    84374.00  1915.10 16.1730 1.0000e+00 3.9430e-59 7.8860e-59
      2  2.7019    61451.00  1044.60 15.5390 1.0000e+00 9.4344e-55 1.8869e-54
      3  2.2317   149670.00  2376.50 12.8120 1.0000e+00 6.9816e-38 1.3963e-37
      4 -2.3377      548.00 11106.00  4.0870 2.1849e-05 9.9998e-01 4.3697e-05
      5 -5.0730      459.58  2752.60  3.8132 6.8588e-05 9.9993e-01 1.3718e-04
      6 -6.0390       10.48   997.24  3.7256 9.7440e-05 9.9990e-01 1.9488e-04
               FDR high_in_treatment
      1 7.0296e-54              True
      2 8.4099e-50              True
      3 4.1489e-33              True
      4 6.4597e-03             False
      5 1.4613e-02             False
      6 1.9111e-02             False

# read MAGeCK RRA sgRNA summary

    Code
      test_sgrna_summary_obj
    Output
      An object of class "MageckSgrnaRRA"
      Slot "filepath":
      [1] "test"
      
      Slot "sgrna_summary":
           sgrna  Gene control_count        treatment_count control_mean treat_mean
      1   sgRNA1  NEG1       142.690   961.11/850.43/454.83      142.690  850.43000
      2   sgRNA2  NEG1        90.208   630.66/592.44/243.95       90.208  592.44000
      3   sgRNA3  NEG1       167.950   792.56/323.78/1074.7      167.950  792.56000
      4   sgRNA4 gene2       535.670   135.08/105.16/90.989      535.670  105.16000
      5   sgRNA5 gene2       187.630   34.896/4.6038/3.3691      187.630    4.60380
      6   sgRNA6 gene2        87.091 0.33972/0.33972/4.9179       87.091    0.33972
      7   sgRNA7 gene3       105.950   221.97/94.709/219.35      105.950  219.35000
      8   sgRNA8 gene4       143.680   377.05/282.36/266.08      143.680  282.36000
      9   sgRNA9 gene4       113.010    231.32/177.98/231.7      113.010  231.32000
      10 sgRNA10 gene5        86.927     16.801/2.53/1.0429       86.927    2.53000
      11 sgRNA11 gene5       183.530   62.401/7.7788/23.724      183.530   23.72400
      12 sgRNA12 gene5       208.300   33.177/18.574/43.939      208.300   33.17700
             LFC control_var  adj_var   score      p.low     p.high p.twosided
      1   2.5669    84374.00  1915.10 16.1730 1.0000e+00 3.9430e-59 7.8860e-59
      2   2.7019    61451.00  1044.60 15.5390 1.0000e+00 9.4344e-55 1.8869e-54
      3   2.2317   149670.00  2376.50 12.8120 1.0000e+00 6.9816e-38 1.3963e-37
      4  -2.3377      548.00 11106.00  4.0870 2.1849e-05 9.9998e-01 4.3697e-05
      5  -5.0730      459.58  2752.60  3.8132 6.8588e-05 9.9993e-01 1.3718e-04
      6  -6.0390       10.48   997.24  3.7256 9.7440e-05 9.9990e-01 1.9488e-04
      7   1.0428     7771.20  1291.80  3.1550 9.9920e-01 8.0378e-04 1.6076e-03
      8   0.9698     4615.40  1932.60  3.1546 9.9920e-01 8.0396e-04 1.6079e-03
      9   1.0270     1422.40  1406.70  3.1545 9.9920e-01 8.0496e-04 1.6099e-03
      10 -4.6386      102.94   994.76  3.1544 8.0419e-04 9.9920e-01 1.6084e-03
      11 -2.8999      875.06  2673.10  3.1541 8.0510e-04 9.9919e-01 1.6102e-03
      12 -2.6144      164.54  3161.80  3.1502 8.1586e-04 9.9918e-01 1.6317e-03
                FDR high_in_treatment
      1  7.0296e-54              True
      2  8.4099e-50              True
      3  4.1489e-33              True
      4  6.4597e-03             False
      5  1.4613e-02             False
      6  1.9111e-02             False
      7  7.8177e-02              True
      8  7.8177e-02              True
      9  7.8177e-02              True
      10 7.8177e-02             False
      11 7.8177e-02             False
      12 7.8844e-02             False
      

# get MAGeCK RRA sgRNA summary per gene

    Code
      get_mageck_sgrna_gene_results(object = test_sgrna_summary_obj, gene = "NEG1")
    Output
         sgrna Gene control_count      treatment_count control_mean treat_mean    LFC
      1 sgRNA1 NEG1       142.690 961.11/850.43/454.83      142.690     850.43 2.5669
      2 sgRNA2 NEG1        90.208 630.66/592.44/243.95       90.208     592.44 2.7019
      3 sgRNA3 NEG1       167.950 792.56/323.78/1074.7      167.950     792.56 2.2317
        control_var adj_var  score p.low     p.high p.twosided        FDR
      1       84374  1915.1 16.173     1 3.9430e-59 7.8860e-59 7.0296e-54
      2       61451  1044.6 15.539     1 9.4344e-55 1.8869e-54 8.4099e-50
      3      149670  2376.5 12.812     1 6.9816e-38 1.3963e-37 4.1489e-33
        high_in_treatment
      1              True
      2              True
      3              True

# can get top n depleted genes from MAGeCK RRA gene summary

    Code
      gene_summary_top_n_genes(gene_summary_object = test_gene_summary_obj, n = 3)
    Output
          id num  neg.score neg.p.value neg.fdr neg.rank neg.goodsgrna neg.lfc
      1 NEG1   9 7.1884e-13  2.6873e-07 6.6e-05        1             9 -2.7520
      2 NEG2   5 4.9934e-12  2.6873e-07 6.6e-05        2             5 -2.7056
      3 NEG3   5 1.3418e-11  2.6873e-07 6.6e-05        3             5 -3.7000
        pos.score pos.p.value pos.fdr pos.rank pos.goodsgrna pos.lfc
      1         1           1       1    18422             0 -2.7520
      2         1           1       1    18421             0 -2.7056
      3         1           1       1    18420             0 -3.7000

# can get top n enriched genes from MAGeCK RRA gene summary

    Code
      gene_summary_top_n_genes(gene_summary_object = test_gene_summary_obj,
        direction = "pos", n = 3)
    Warning <simpleWarning>
      Fewer than n top genes returned:1
    Output
          id num neg.score neg.p.value neg.fdr neg.rank neg.goodsgrna neg.lfc
      1 POS1   5         1           1       1    18422             0  1.2391
         pos.score pos.p.value  pos.fdr pos.rank pos.goodsgrna pos.lfc
      1 2.0448e-10  2.6873e-07 0.000309        2             5  1.2391

