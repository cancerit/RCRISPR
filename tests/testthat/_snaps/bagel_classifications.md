# can add bagel_classification

    Code
      add_bagel_classifications(data = test_count_matrix, gene_column = 2, ess = ess,
        noness = noness)
    Output
                                   sgRNA    gene T0_. T3_A T3_B T3_C T18_A T18_B
      2007     PPA1_TTTAGACTTGGGAAGACCCA    PPA1  277   92   64   67    10    27
      2008     PPA1_GTTGCATGAAGACATACCTG    PPA1  267  143  108   46    62     2
      2009     PPA1_TGGTAGTTGAAGTACCACGC    PPA1   98   45   85   60    72    75
      2010     PPA1_TATGAGCGGCTTCAGCACCG    PPA1  388  261  179  159    20     0
      3396   CCDC84_AAGAAGGTCTGGCGGCACAG  CCDC84  321  238  108  181     0     9
      3397   CCDC84_GTGCGGGAACACCTGAGCCA  CCDC84  321  114   70   66     8    31
      3398   CCDC84_CTGGTGGGAGAACAAAGCTG  CCDC84  440  236  107  164    31    51
      3399   CCDC84_GGAATCCAAACCTTTCACCA  CCDC84  116   83   83  111    55    52
      3464     MFRP_AGACGAAGACCACCAGGAGG    MFRP  214   38   30   76    61    75
      3465     MFRP_GGGACAGAGGGACCTACCTG    MFRP  285  115  106  147   129    95
      3466     MFRP_CATAGCCCACTGCCTGCCGG    MFRP  618  492  312  352   398   287
      3467     MFRP_GGAGGCAACAGAATCGAGCA    MFRP  166   72   60   51   280    78
      3599   OR10S1_GTTGCGCATCCGCACAGCCC  OR10S1  185  122  104  134   268   164
      3600   OR10S1_AGACAAACAGGCATCCAGGA  OR10S1  526  218  165  143   377   326
      3601   OR10S1_AGTGGTACATGGGTAAGCTG  OR10S1  178  206  153  150   218   162
      3602   OR10S1_TGAGAAGATGACCATGACAA  OR10S1  592  197  187  176   209   404
      3655    PANX3_CCAGACGCAGTCCTTTGAGG   PANX3  528  233  173  204   250   233
      3656    PANX3_CCAGCAGGAGCTGTCCACGT   PANX3  698  321  225  250   317   239
      3657    PANX3_TGAACAGCAGATCGGAGCTG   PANX3  373  329  140  193   444   206
      3658    PANX3_AAGGTATAGGTAAGTGGCGG   PANX3  276  378  275  409   111   338
      3951    COPB1_AGAAGGAATCCTCTCAAGGG   COPB1  164   18   31   23    20    12
      3952    COPB1_TATTGTGCTAGGATTTACCG   COPB1  317   24   35   15    20    41
      3953    COPB1_AGAAGGATGCAAGTTGCAAA   COPB1  140   58   63   89    66   122
      3954    COPB1_ATAACCAGAAACCATGACGG   COPB1  416   49   40   24    13    39
      6674   OR10A2_GACATTTCTAACCATCTACC  OR10A2  195   71   69   56    93   173
      6675   OR10A2_CATCTACCTGGTCACCCTGA  OR10A2  544  265  265  286   326   440
      6676   OR10A2_CATCAGGGTGACCAGGTAGA  OR10A2  829  387  328  320   402   636
      6677   OR10A2_GATGAGGCAGTTTCCCATCA  OR10A2  206  103  100  111    93   296
      11161   UBE2N_TTCTGTCTACAGCTAGAGCA   UBE2N  342  235  136  203   314    90
      11162   UBE2N_AGATACCTGTTTCTATGGCT   UBE2N  221  215  171  189    80    84
      12503   WDR25_ACAGCGGCAGGCATTAAGCA   WDR25  583  183  115  159    20    56
      12504   WDR25_CGAAGTCAAAGCCACCACTG   WDR25 1182  335  246  207    27    34
      12505   WDR25_GGAACTCGGAGCCTTCCCGG   WDR25   83   63   63   77     1    65
      12506   WDR25_CATCCGGTAGGGCCACACAG   WDR25  527  262  174  199   161    75
      13759  ACTR10_ACTACCATGCCGCTCTACGA  ACTR10  320   86   40  123     0    18
      13760  ACTR10_ATTCGATAATCACAACTCGG  ACTR10  182   54   15   32     6    16
      13761  ACTR10_ATATCCACAATCTAGGACCA  ACTR10  353   72   53   57    35     1
      13762  ACTR10_ACAGAGCCTTCCCTCAGTGA  ACTR10  424  127  146  152    47   102
      15352    HYPK_AAGCCACGGAAACATGACAG    HYPK  276   43   43   90     8    21
      15353    HYPK_GAAACATGACAGCGGTGCGG    HYPK  291  120  109  150   128    34
      15354    HYPK_GCGGGTCACCGACTATGCAG    HYPK  192   43   31    6     0     0
      15355    HYPK_GCGGGAACACATGGGCAACG    HYPK  314  150   75   82    21    17
      17223   PDILT_ATGACTCCAAACGTTAGCTC   PDILT  199   99   58   27   116    91
      17224   PDILT_GAAGAAGCCAACGATGACCA   PDILT  310  200  132  150   239   236
      17225   PDILT_AGTGGACATTACCATAGAGA   PDILT  119   48   38   36    49    29
      17228   PDILT_CAAGCCTGTGCACATCCTGG   PDILT  518  286  203  240   145   166
      27722   ERCC2_ATCCAGGTTGTAGATGCCAG   ERCC2  491  360  205  250   182   222
      27723   ERCC2_AAGGAACAGGTGCTCACCTC   ERCC2  426   98   85   54    13     3
      27724   ERCC2_TTATCGGCAGGCATATCCGC   ERCC2  532  151  101  115    30     4
      27725   ERCC2_GCGGGAGCTCAAACGCACGC   ERCC2  280   94   58   66    52     5
      27770  SNRPD2_TCTCCAAGATGTTCCTGCGC  SNRPD2    2    2    8   19     0     0
      27771  SNRPD2_CATGGTGCTGGAGAACGTGA  SNRPD2  215   25   12   18    58     8
      27772  SNRPD2_ACCCAGCACTCACCTATCGA  SNRPD2  208   39   24   31    56   112
      27773  SNRPD2_GAGGAGGAGGAATTTAACAC  SNRPD2  257   33   22   45    35    35
      28014     CRX_TAGGAATCTGAGATGCCCAG     CRX   90  183  128  161   247    72
      28015     CRX_GCTCGGAGACCCATAGGCGG     CRX  330  253  217  301   279   202
      28016     CRX_GGCCAGGGAAGGTCCCACGG     CRX  245  127   87  137    42    85
      28017     CRX_GAAATTCACCTACAATCCCA     CRX  137   30   18   30    49    36
      28118   RPL18_TCGAGCGTGCCAGAGGCCGA   RPL18  523   68   60   74    47    49
      28119   RPL18_CCTTCCCTAGGTCCTCGCAA   RPL18  241   36   39   30     2    34
      28120   RPL18_CAGCCGCATCCTCAGGGCAG   RPL18  458   82   50   58    12    18
      28121   RPL18_AGTGGACATCCGCCATAACA   RPL18  298   95   49   79    74    52
      30909    GJA8_TGGGCGACTGGAGTTTCCTG    GJA8  523  250  117  178   267   260
      30910    GJA8_AGAGGAGGGAGCCGAACCCG    GJA8  308  239  109  149   187    61
      30911    GJA8_AGCAGAGAGGCTGACCACGG    GJA8  408  162  188  176   137   193
      30912    GJA8_GGAGAAGGTGGCCGTGCCAG    GJA8  315  181  144  188   249   162
      32765   NPHS2_GCAAAGACAAGCCAAAGTGC   NPHS2  354  128  121  174   300   264
      32766   NPHS2_CAGGAAGCAGATGTCCCAGT   NPHS2  538  273  206  235   542   404
      32767   NPHS2_GAGGACAAGAAGCCACTCAC   NPHS2  185   91   93   82   172   107
      32768   NPHS2_GTTGGAGAGCGAGCGGCCCG   NPHS2  174  205  115  126   248    69
      33931  CELA3A_GAGGGATCTGACCTACCAGG  CELA3A  133  166  140  156    79   171
      33932  CELA3A_TGAGTACAACCTTGCTGTGA  CELA3A  476  414  285  296   500   351
      33933  CELA3A_GCAGTGCTTATAGTCCACCA  CELA3A  423  215  146  199   321   259
      36313   CDCP2_CAGACCGCAGCACCACCCGC   CDCP2  530  396  209  183   360   298
      36314   CDCP2_GGTGTTCGTGGACTTCCAGG   CDCP2  128  220  125  128   141    52
      36315   CDCP2_ATGGAAGGTGAGCAGCACCG   CDCP2  155  143  127  145   119   131
      36316   CDCP2_AGGGCTCCAGGCCCAAGCCA   CDCP2  459   99   90  105   250   169
      37544    CST8_GGACCATGCCCAGGTGCCGG    CST8  179  110   52   43   144    36
      37545    CST8_CCACAGACACTGCTTCACGT    CST8  915  249  252  297   283   348
      37546    CST8_AAGGCTTTCTGCAATCGCTG    CST8  276  202  118  147    80   231
      37547    CST8_GAATGGTGAATTCACTGTGA    CST8  132  147   66   60    51   265
      37648   NOP56_GCTGGCGCTGAAGGAAGTGG   NOP56  294  108   44   75    76    51
      37649   NOP56_AATGCCAACGCCGTGTCTGA   NOP56  528  147   95  190    28    64
      37650   NOP56_GATTGGTGCCGCAATACAGG   NOP56  181   70   35   82    23     4
      37651   NOP56_CAGCTTGTAAAGCACAGCTG   NOP56  167   60    8   24    48     1
      37708 DEFB123_GCACAGTCAAAGTCAGCAAA DEFB123  492  141  103   76   179   270
      37709 DEFB123_CAGGTGGCACCCAAAGATGC DEFB123  556  264  143  118   247   249
      37710 DEFB123_CCGTTACAGATGCTCCAAGA DEFB123  493  151   94   90   180   121
      37711 DEFB123_AATGGCCACCACCTTTCTTT DEFB123  644  333  157  207   379   209
      47639    GFM1_TTAATTGGAAGGCCTGCCGA    GFM1  435  258  159  155   143    22
      47640    GFM1_AATGGGCTCCAACCCAGCCA    GFM1  509  136   78  110   109   110
      47641    GFM1_AGCGTTTATGCAGATACCCA    GFM1  166  111   98  111    16    44
      47642    GFM1_ATTCAATTAGCTCCTGCCGG    GFM1  664  265  246  240    32    30
      48225 TMEM207_TGAAGGTGAATTCCAACAGT TMEM207  572  196  115  202   404   239
      48226 TMEM207_TGATTCTCACAGGCGCACCA TMEM207  264  103  107  112   149   160
      48227 TMEM207_TTGGTGGCAGCTCTTCTCTG TMEM207  404  381  228  324   275   205
      48228 TMEM207_ATGACCAACACCCTAATGGC TMEM207  325  197  175  130    99   177
      48401   NCBP2_ACTGGCACAGAACCAGTGAG   NCBP2  299  130   60   87   128   231
      48402   NCBP2_GAGGGCAGGCAATACGGCCG   NCBP2  244   99   71   77   141    69
      48403   NCBP2_GTACTGGCTCAGCTCCACGT   NCBP2  449   51   53   82    22     0
      48404   NCBP2_GCTGCGCAGCGACTCCTACG   NCBP2  397   36   50   58     0     0
      53441     IL9_TGTGCTCCGTGGCAGGCCAG     IL9  504  346  184  242   329   219
      53442     IL9_TGCCACGGAGCACAGGAGCA     IL9  649  312  133  196   497   214
      53443     IL9_CAGGGCAGAGGTAAGGACCA     IL9  834  225  166  211   376   156
      53444     IL9_TGTCAAGATGCTTCTGGCCA     IL9  725  361  308  379   793   569
      58856    BYSL_TCGCGGGACAGGAGAAGCGG    BYSL  682  129   79   71    33    46
      58857    BYSL_GGAGTGGCCCACCCTGGAGA    BYSL  332   83   72   73   149    31
      58858    BYSL_GGTCCTAGAAGTGTACAGGG    BYSL  340  139   88   96    24    18
      58859    BYSL_GTATTATCTAAGTACCGCAG    BYSL   31   13   20   34     1    15
      59238    MCM3_TCAGCCCTCTTTCTCTGCTG    MCM3  481  196  161  226   182   294
      59239    MCM3_AAGAAGGGAGGCTACACCTC    MCM3  108   74   86   90    68    32
      59240    MCM3_GGTAGGATAGACAGAGCTGG    MCM3  220  110   80   76    71    68
      59241    MCM3_GAAGGCAACCAGCTCCTCAA    MCM3  185   52   46   79    49    34
      62425    RFC2_TGTACCGGAGGACTGCACAG    RFC2  409   34   19   27     0     0
      62426    RFC2_TTTCTTCTCAGCATGACCGA    RFC2  420  157   68  127   102   117
      62427    RFC2_TGTGCCCAACATCATCATTG    RFC2  615  134   89  110    77    91
      62428    RFC2_GAGGTGGAGGCCGTCTGTGG    RFC2  440   93   52   61    27    10
      66194   RPL12_TCTTCTGCAGTTAAACACAG   RPL12  208    6   15    9    78    79
      66195   RPL12_GGGATAACGTACCAGACCCA   RPL12  218   11    2    5     1     0
      66196   RPL12_GGATAACGTACCAGACCCAG   RPL12  329    5   20   19     7    22
      66197   RPL12_GATAACGTACCAGACCCAGG   RPL12  148    2    7   10     1     0
      66374    DOLK_GTGGCGGCTACATAGAGCAG    DOLK  531  249  125  138     6    29
      66375    DOLK_GCCAGGGTAGGACCACACCA    DOLK  384  143   96  156    25    12
      66376    DOLK_ATACCAGCAGTGCCTCACCA    DOLK  625  185  208  166   103     0
      66377    DOLK_TACCAGCAGTGCCTCACCAG    DOLK  625  272  249  225    95    23
      67127   IFNA5_TATGATGCAGGAGGTTGGAG   IFNA5  314  321  176  224   229   362
      67578  EXOSC3_TAAAGACATGGAACCAGAGA  EXOSC3  141   26    9   34     1     5
      67579  EXOSC3_TAGGTTGGAGATCTCATCTA  EXOSC3  209  102   58   74    27    23
      67580  EXOSC3_TGCCGGGCTCCTTGTGACGG  EXOSC3  614  170  146  218    27    28
      67581  EXOSC3_GTCGCGGCTGAATCTCTCGC  EXOSC3  237   72   35   63     6     8
      68502  GUCY2F_AAGCTTCCAGATTACCTCAG  GUCY2F  272  111  130  133   151   289
      68503  GUCY2F_ACTCCATAGCACCTACACTG  GUCY2F  348  192  123  193   164   152
      68504  GUCY2F_TAAGGAACAACCCAAAGCTC  GUCY2F  359  101   95   63   129   310
      68505  GUCY2F_GAAGGGACATGACAGACAGA  GUCY2F  334   96   68  100   326   145
      69790  FAM47B_GGCCCGGGAGAAGACAACCG  FAM47B  453  210  118  114   242   250
      69791  FAM47B_GGGCCAGGAGATGACAACCG  FAM47B  183   87  122  102   178   166
      69792  FAM47B_GAGTTATGGGAAGCACCAGG  FAM47B  226  187  190  207   269   157
      69793  FAM47B_GGAGGATAAGGTATGGACCA  FAM47B  245  146  132  163   150   221
      1        EGFP_TGGTTGTCGGGCAGCAGCAC    EGFP  537  222  150  216   243   115
      2        EGFP_GGTTGTCGGGCAGCAGCACG    EGFP  210  197   92  125   195   177
      3        EGFP_TTCAAGTCCGCCATGCCCGA    EGFP  517  423  255  282   329   309
      4        EGFP_CGGCGGTCACGAACTCCAGC    EGFP  278  167   54  120   165   184
      5        EGFP_GGTGCCCATCCTGGTCGAGC    EGFP   83   99   41   19   107   103
      6        EGFP_GAAGTTCGAGGGCGACACCC    EGFP  268  145  153  204   133   295
      7        EGFP_CATGCCGAGAGTGATCCCGG    EGFP  759  327  273  219   371   289
      8        EGFP_TCAGCTCGATGCGGTTCACC    EGFP  470  201  197  265   461   338
      9        EGFP_GCCGTCGTCCTTGAAGAAGA    EGFP  258   83   61   88   261   245
      10       EGFP_CAGCTCGATGCGGTTCACCA    EGFP  613  169  124  144   205   311
      11       EGFP_GAAGGGCATCGACTTCAAGG    EGFP  218  193  108   98   145   141
      12       EGFP_CCTCGAACTTCACCTCGGCG    EGFP  167  216  179  122   170   146
      13       EGFP_CTGGACGTAGCCTTCGGGCA    EGFP  442  285  222  273   369   440
      14       EGFP_ATCCGCCACAACATCGAGGA    EGFP   86  120  115   72   226   185
      15       EGFP_CGTAGGTGAAGGTGGTCACG    EGFP  547  405  354  384   379   761
      16       EGFP_CTGCACGCCGTAGGTGAAGG    EGFP  323  232  193  178   314   362
      17       EGFP_CAACGAGAAGCGCGATCACA    EGFP  494  254  134  167   367   370
      18       EGFP_TGGAGTTCGTGACCGCCGCC    EGFP  514  395  322  271   296   263
      19       EGFP_GCCGTCCAGCTCGACCAGGA    EGFP  357  110  133  148   160   262
      20       EGFP_CAAGTTCAGCGTGTCCGGCG    EGFP  450  277  118  201   362   155
      21       EGFP_AAGTTCAGCGTGTCCGGCGA    EGFP  305  418  247  314   335   298
      22       EGFP_GGTCTTTGCTCAGGGCGGAC    EGFP  285  114  131   94    53    71
      23       LacZ_CAGCTGGCGTAATAGCGAAG    LacZ   73   37   33   37   195   243
      24       LacZ_CCTTGTGGAGCGACATCCAG    LacZ  452  266  164  173   522   398
      25       LacZ_GGCACTTCACCGCTTGCCAG    LacZ  101  157   69   84    98    94
      26       LacZ_CGTTATCGCTATGACGGAAC    LacZ  172  107   77   79   138   134
      27       LacZ_TTTGCCCGGATAAACGGAAC    LacZ  203   68   45   78   143   207
      28       LacZ_CCAGACCGTTCATACAGAAC    LacZ  313  170   98   82   142   341
      29       LacZ_CAATATTGAAACCCACGGCA    LacZ  189  209  133  138   166   217
      30       LacZ_TCACCGCCGTAAGCCGACCA    LacZ  931  320  266  388   925   570
      31       LacZ_GAGCGAACGCGTAACGCGAA    LacZ  173   17   30   25    29   106
      32       LacZ_CACCGCCGTAAGCCGACCAC    LacZ  617  276  232  194   271   245
      33       LacZ_CAAGACTGTTACCCATCGCG    LacZ  280  208  189  179   347   147
      34       LacZ_GACGAAGCCGCCCTGTAAAC    LacZ  205  141  111  126   293   223
      35       LacZ_ACGAAGCCGCCCTGTAAACG    LacZ  479  159  145  191   217    59
      36       LacZ_CGCCCGGTGCAGTATGAAGG    LacZ  423  275  196  239   429   435
      37       LacZ_CCGGTGCAGTATGAAGGCGG    LacZ  129  104   47   81    42   133
      38       LacZ_CGTATTCGCAAAGGATCAGC    LacZ   62   52   28   21    15    87
      39       LacZ_TTGCCCGATGTACGCGCGCG    LacZ  304  155  129  203   146   210
      40       LacZ_CCGCGTGCAGCAGATGGCGA    LacZ  200  115   77   79    66   170
      41       LacZ_CCTTCCCGGCTGTGCCGAAA    LacZ  389  125   89  102   161   208
      42       LacZ_CCATTTCGGCACAGCCGGGA    LacZ  304  166  140  159   278   231
      43       LacZ_TGGACCATTTCGGCACAGCC    LacZ  377  130  105  100   257    97
      44       LacZ_TGCGCAGCCTGAATGGCGAA    LacZ  279   89   50   84   111   232
      45       LacZ_CATTTCGGCACAGCCGGGAA    LacZ  367  109   82  108   297   383
      46       LacZ_CCGCCGCCTTCATACTGCAC    LacZ  565  241  172  298   491   290
      47       LacZ_CATCGGGCAAATAATATCGG    LacZ  307  217  101  141   450   256
      48       LacZ_GCGCGTCGTGATTAGCGCCG    LacZ   39   17    2    0    16    56
      49       LacZ_CGCCTTCATACTGCACCGGG    LacZ  642  431  301  311   319   321
      50       LacZ_CGCCGCCTTCATACTGCACC    LacZ  774  314  220  208   384   310
      51       LacZ_TCATACTGCACCGGGCGGGA    LacZ  422  289  269  195   275   284
      52       LacZ_GCCTTCATACTGCACCGGGC    LacZ  230  159  124  162   190   152
      53       LacZ_AGGGCGGCTTCGTCTGGGAC    LacZ  383  229  183  235   237   244
      54       LacZ_AACCCGTGGTCGGCTTACGG    LacZ  106  104   55   82   114   110
      55       LacZ_CTCATCGCCGGTAGCCAGCG    LacZ  265  143  108   85   155   242
      56       LacZ_GATTCATTGGCACCATGCCG    LacZ  131  114   98   81   141   143
      57       LacZ_TGTAGCGGCTGATGTTGAAC    LacZ  202  145   97  110    97   135
      58       LacZ_TCGCACAGCGTGTACCACAG    LacZ  579  195  158  196   326   271
      59       LacZ_CTGGTTTCCGGCACCAGAAG    LacZ  271  162  131  150   205   330
      60       LacZ_CGGATAATGCGAACAGCGCA    LacZ  400  148  118  149   358   240
            T18_C T18_A_Starved T18_B_Starved T18_C_Starved classification
      2007     36            42            47            17      essential
      2008      7             5            40            43      essential
      2009     97            50            82            50      essential
      2010     22             3            23            15      essential
      3396     11             5             0             0      essential
      3397      2            26            42             7      essential
      3398      0             0             0            22      essential
      3399     35            10            33           159      essential
      3464     49            60            42            44   nonessential
      3465    152           126           159           127   nonessential
      3466    384           304           190           459   nonessential
      3467    181            85            86            62   nonessential
      3599    302            96           190           140   nonessential
      3600    476           318           156           267   nonessential
      3601    468           237           210           191   nonessential
      3602    361           287           262           445   nonessential
      3655    386           170           184           146   nonessential
      3656    334           356           661           462   nonessential
      3657    428           245           266           210   nonessential
      3658    287            89           456           411   nonessential
      3951      8            11            55             2      essential
      3952     12            23            14             9      essential
      3953    104           165           174           155      essential
      3954    100            45            98            42      essential
      6674    224           106            69            65   nonessential
      6675    512           425           362           283   nonessential
      6676    410           545           638           511   nonessential
      6677    211           584           125           162   nonessential
      11161   107           240           175            90      essential
      11162   109           153           207           120      essential
      12503     3            33             2            17      essential
      12504    53            51            16            40      essential
      12505   101            94            13             5      essential
      12506    41            43            59            97      essential
      13759    18            58            11            33      essential
      13760     9             8            41            24      essential
      13761    14            32            16             0      essential
      13762    80            82            59            46      essential
      15352    30            26            38            45      essential
      15353    16            28            40            37      essential
      15354     5             0             0            13      essential
      15355     9            19             2             5      essential
      17223    54            54            76            64   nonessential
      17224   255           164           413           159   nonessential
      17225    76            46           115            28   nonessential
      17228   306           421           254           130   nonessential
      27722   123            93           363           105      essential
      27723    17             4            40             1      essential
      27724    14            11            19            32      essential
      27725     9            15            29            52      essential
      27770    11             2             0             0      essential
      27771     0             0            24             0      essential
      27772    31            13            40            76      essential
      27773    30            23             0             1      essential
      28014    97           115           151           168   nonessential
      28015   219           256           284           229   nonessential
      28016   134            97           104           122   nonessential
      28017    32            41            60            48   nonessential
      28118    85           110            17            10      essential
      28119    17            51             1            61      essential
      28120     0            26            12            10      essential
      28121    66            35            28            66      essential
      30909   182           135           146           162   nonessential
      30910    92           128           129           116   nonessential
      30911   186           140           201           213   nonessential
      30912   130            76           247            79   nonessential
      32765   267           211           127            99   nonessential
      32766   389           170           391           401   nonessential
      32767   268           104            82           147   nonessential
      32768   238            47            67           276   nonessential
      33931   236           144           142            77   nonessential
      33932   357           503           294           225   nonessential
      33933   349           472           554           173   nonessential
      36313   170           217           392           479   nonessential
      36314   110            67           102           210   nonessential
      36315   200           108           208           137   nonessential
      36316   378           215           140            97   nonessential
      37544    92            21            50            55   nonessential
      37545   177           257           488           520   nonessential
      37546   137           110           235           136   nonessential
      37547   119           184           246           129   nonessential
      37648    28            63           133            32      essential
      37649    84            51            30            42      essential
      37650    26            25             2             5      essential
      37651    17             0             0            15      essential
      37708   210           236           229           119   nonessential
      37709   249           134           222           233   nonessential
      37710   158            98           348            98   nonessential
      37711   284           336           622           317   nonessential
      47639     7            31            87            62      essential
      47640   101            43           109            19      essential
      47641    73            84            48            20      essential
      47642    40            22            27            27      essential
      48225   360           260           598           256   nonessential
      48226   221           155           134           167   nonessential
      48227   516           219           361           319   nonessential
      48228   172           168           138           300   nonessential
      48401   205           212           142            79      essential
      48402   158           160            58            89      essential
      48403     0            12             2             0      essential
      48404     0            11             0            15      essential
      53441   295           231           177           155   nonessential
      53442   387           312           303           337   nonessential
      53443   224           179           197           164   nonessential
      53444   665           368           652           557   nonessential
      58856   102           268            85           114      essential
      58857    42            54            44            49      essential
      58858     0            30            42             5      essential
      58859    27            25            10             4      essential
      59238   205           211           240           142      essential
      59239    16            12            54            20      essential
      59240    80            50            52            95      essential
      59241    19            11             5            55      essential
      62425    11            27            13             2      essential
      62426   141           143           220            69      essential
      62427    76           304            53            73      essential
      62428    38             0            57            19      essential
      66194    14             2             5             0      essential
      66195     5             1             0             0      essential
      66196     2             5             0             8      essential
      66197     4             0             2             0      essential
      66374    23            10            17            26      essential
      66375    35            20            45            43      essential
      66376    35            21             0             8      essential
      66377     4             5            26            77      essential
      67127   252           402           272           284   nonessential
      67578     2             0            12            17      essential
      67579    43            36            28            10      essential
      67580    54            24           127           120      essential
      67581    20            16           119            62      essential
      68502   215           164           325           196   nonessential
      68503   170           170           240            46   nonessential
      68504    74           100           112           123   nonessential
      68505   129           128           184           193   nonessential
      69790   146           142           187           175   nonessential
      69791    73           154            80           168   nonessential
      69792   326           149           174           267   nonessential
      69793   149           149           181           197   nonessential
      1       304           277           319           240        unknown
      2       115            96           139           235        unknown
      3       433           371           565           428        unknown
      4        65           169           147            68        unknown
      5        44            19            69            73        unknown
      6       228           339           131           173        unknown
      7       430           268           428           278        unknown
      8       268           272           278           221        unknown
      9       143            87           163            93        unknown
      10      218           200           209           189        unknown
      11      289            57           172           219        unknown
      12      158           158            98           236        unknown
      13      308           302           342           290        unknown
      14      195           152           357            43        unknown
      15      357           378           366           529        unknown
      16      571           179           464           130        unknown
      17      387           105           360           384        unknown
      18      364           379           511           341        unknown
      19      269           102            67            90        unknown
      20      241           184           342           343        unknown
      21      482           542           472           411        unknown
      22      229           192           165           287        unknown
      23      123           100            43            53        unknown
      24      268           291           333           127        unknown
      25      172           138           143           159        unknown
      26      103           220           180            35        unknown
      27      157           170           149            51        unknown
      28      126           156           109           206        unknown
      29      140           286           175           182        unknown
      30      648           431           481           286        unknown
      31       52            50           136            13        unknown
      32      231           161           358           120        unknown
      33      199           120           231           211        unknown
      34      264           144           244           497        unknown
      35      287           259           178           172        unknown
      36      324           254           397           351        unknown
      37       81            66            76            49        unknown
      38      114            12            25            17        unknown
      39      276           127           274           252        unknown
      40      190           137           211            84        unknown
      41      190           187           175            94        unknown
      42      289           239           523           473        unknown
      43      231           180           133           145        unknown
      44      191           150           185            60        unknown
      45      338           181           203           180        unknown
      46      394           402           313           258        unknown
      47      278           121           241           253        unknown
      48       16            52            69            11        unknown
      49      521           306           556           369        unknown
      50      471           504           301           420        unknown
      51      251           264           255           330        unknown
      52      122           173           398           243        unknown
      53      233           334           314           354        unknown
      54      192           162           225            76        unknown
      55      194           216           165            84        unknown
      56      182           131           314           151        unknown
      57      515           170           457           166        unknown
      58      249           150           133           319        unknown
      59      144           128           298           197        unknown
      60      150           207           448           105        unknown

# can get bagel statistics sgrna-level

    Code
      get_bagel_statistics(data = test_classified_sgrna)
    Message <simpleMessage>
      Building BAGEL classification statistics...
    Output
      # A tibble: 10 x 17
         sample        max_essential max_nonessential max_unknown mean_essential
         <chr>                 <dbl>            <dbl>       <dbl>          <dbl>
       1 T0_.                   1182              915         931          343. 
       2 T18_A                   314              793         925           50.2
       3 T18_A_Starved           304              584         542           49.0
       4 T18_B                   294              636         761           43.6
       5 T18_B_Starved           363              661         565           50.7
       6 T18_C                   205              665         648           42.8
       7 T18_C_Starved           159              557         529           39.6
       8 T3_A                    360              492         431          113. 
       9 T3_B                    249              328         354           80.1
      10 T3_C                    250              409         388           95.2
      # ... with 12 more variables: mean_nonessential <dbl>, mean_unknown <dbl>,
      #   median_essential <dbl>, median_nonessential <dbl>, median_unknown <dbl>,
      #   min_essential <dbl>, min_nonessential <dbl>, min_unknown <dbl>,
      #   n_genes_essential <dbl>, n_genes_nonessential <dbl>, n_genes_unknown <dbl>,
      #   total_genes <dbl>

# can get bagel statistics gene-level

    Code
      get_bagel_statistics(data = test_classified_gene, is_gene = T)
    Message <simpleMessage>
      Building BAGEL classification statistics...
    Output
      # A tibble: 10 x 17
         sample        max_essential max_nonessential max_unknown mean_essential
         <chr>                 <dbl>            <dbl>       <dbl>          <dbl>
       1 T0_.                   1182              915         931          343. 
       2 T18_A                   314              793         925           50.2
       3 T18_A_Starved           304              584         542           49.0
       4 T18_B                   294              636         761           43.6
       5 T18_B_Starved           363              661         565           50.7
       6 T18_C                   205              665         648           42.8
       7 T18_C_Starved           159              557         529           39.6
       8 T3_A                    360              492         431          113. 
       9 T3_B                    249              328         354           80.1
      10 T3_C                    250              409         388           95.2
      # ... with 12 more variables: mean_nonessential <dbl>, mean_unknown <dbl>,
      #   median_essential <dbl>, median_nonessential <dbl>, median_unknown <dbl>,
      #   min_essential <dbl>, min_nonessential <dbl>, min_unknown <dbl>,
      #   n_genes_essential <dbl>, n_genes_nonessential <dbl>, n_genes_unknown <dbl>,
      #   total_genes <dbl>

# can get bagel statistics gene-level fold changes

    Code
      get_bagel_statistics(data = test_classified_fc_gene_narrow, is_gene = T, is_fc = T)
    Message <simpleMessage>
      Building BAGEL classification statistics...
    Output
      # A tibble: 3 x 19
        sample NNMD   Essential.Glass.Delta max_essential max_nonessential max_unknown
        <chr>  <chr>  <chr>                         <dbl>            <dbl>       <dbl>
      1 T3_A   -2.502 2.553                        -0.290           0.0318      -0.655
      2 T3_B   -2.430 4.084                        -0.848          -0.732       -1.16 
      3 T3_C   -1.807 3.606                        -0.488          -0.486       -1.06 
      # ... with 13 more variables: mean_essential <dbl>, mean_nonessential <dbl>,
      #   mean_unknown <dbl>, median_essential <dbl>, median_nonessential <dbl>,
      #   median_unknown <dbl>, min_essential <dbl>, min_nonessential <dbl>,
      #   min_unknown <dbl>, n_genes_essential <dbl>, n_genes_nonessential <dbl>,
      #   n_genes_unknown <dbl>, total_genes <dbl>

# can prepare essentiality data

    Code
      prepare_essentiality_data(data = test_avg_classified_fc_gene)
    Output
      $essentiality
       ACTR10    BYSL  CCDC84   CDCP2  CELA3A   COPB1     CRX    CST8 DEFB123    DOLK 
            1       1       1       0       0       1       0       0       0       1 
        ERCC2  EXOSC3  FAM47B    GFM1    GJA8  GUCY2F    HYPK   IFNA5     IL9    MCM3 
            1       1       0       1       0       0       1       0       0       1 
         MFRP   NCBP2   NOP56   NPHS2  OR10A2  OR10S1   PANX3   PDILT    PPA1    RFC2 
            0       1       1       0       0       0       0       0       1       1 
        RPL12   RPL18  SNRPD2 TMEM207   UBE2N   WDR25 
            1       1       1       0       1       1 
      
      $predictor
       [1] -2.2014271 -1.8236720 -1.1793501 -0.8099540 -0.5105452 -2.7617258
       [7] -0.7722082 -1.2019199 -1.9291828 -1.5892816 -1.8663399 -2.0399960
      [13] -0.8658798 -1.3854727 -1.1666279 -1.5530624 -2.1071931 -0.4293259
      [19] -1.4566546 -1.1754400 -1.4115151 -2.3198319 -2.1092655 -0.9654381
      [25] -1.2053156 -0.9958179 -0.8259206 -1.3746929 -1.2871011 -2.7677641
      [31] -4.7692111 -2.6629914 -1.9312815 -1.1161930 -0.5420802 -1.4475384
      
      $min
      [1] -4.769211
      
      $modified_predictor
       [1] 2.567784 2.945539 3.589861 3.959257 4.258666 2.007485 3.997003 3.567291
       [9] 2.840028 3.179929 2.902871 2.729215 3.903331 3.383738 3.602583 3.216149
      [17] 2.662018 4.339885 3.312557 3.593771 3.357696 2.449379 2.659946 3.803773
      [25] 3.563895 3.773393 3.943291 3.394518 3.482110 2.001447 0.000000 2.106220
      [33] 2.837930 3.653018 4.227131 3.321673
      

# can process ROC

    Code
      process_roc(test_screen_roc, test_essentiality_data[["min"]])
    Output
                thresholds specificity sensitivity ppv
      best_prec  -1.930232           1   0.5263158   1

# can plot ROC

    Code
      p$data
    Output
                FPR        TPR
      1  1.00000000 1.00000000
      2  0.94117647 1.00000000
      3  0.88235294 1.00000000
      4  0.88235294 0.94736842
      5  0.82352941 0.94736842
      6  0.76470588 0.94736842
      7  0.70588235 0.94736842
      8  0.64705882 0.94736842
      9  0.58823529 0.94736842
      10 0.52941176 0.94736842
      11 0.47058824 0.94736842
      12 0.41176471 0.94736842
      13 0.41176471 0.89473684
      14 0.41176471 0.84210526
      15 0.35294118 0.84210526
      16 0.29411765 0.84210526
      17 0.29411765 0.78947368
      18 0.23529412 0.78947368
      19 0.23529412 0.73684211
      20 0.17647059 0.73684211
      21 0.17647059 0.68421053
      22 0.11764706 0.68421053
      23 0.05882353 0.68421053
      24 0.05882353 0.63157895
      25 0.05882353 0.57894737
      26 0.05882353 0.52631579
      27 0.00000000 0.52631579
      28 0.00000000 0.47368421
      29 0.00000000 0.42105263
      30 0.00000000 0.36842105
      31 0.00000000 0.31578947
      32 0.00000000 0.26315789
      33 0.00000000 0.21052632
      34 0.00000000 0.15789474
      35 0.00000000 0.10526316
      36 0.00000000 0.05263158
      37 0.00000000 0.00000000

---

    Code
      p$layers
    Output
      [[1]]
      geom_line: na.rm = FALSE, orientation = NA
      stat_identity: na.rm = FALSE
      position_identity 
      
      [[2]]
      mapping: x = ~x, y = ~y 
      geom_text: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_identity 
      

