###############################################################################
#* --                                                                     -- *#
#* --                       get_mageck_gene_summary()                     -- *#
#* --                                                                     -- *#
###############################################################################

test_gene_summary_obj <- read_mageck_rra_gene_summary(
  filepath = system.file("testdata", "test_mageck_rra_gene_summary.txt", package = 'rcrispr'))
slot(test_gene_summary_obj, 'filepath', check = TRUE) <- 'test'

testthat::test_that("MageckGeneRRA filter warns if returning empty data frame", {
  testthat::expect_warning(get_mageck_gene_summary(test_gene_summary_obj,
                                                   filters = 'neg.fdr > 10'),
                         "No rows returned from MAGeCK gene summary with filter:")
})

testthat::test_that("MageckGeneRRA filter returns results", {
  testthat::expect_snapshot(get_mageck_gene_summary(test_gene_summary_obj,
                                                   filters = 'neg.fdr < 0.05'))
})

###############################################################################
#* --                                                                     -- *#
#* --                   read_mageck_rra_gene_summary()                    -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("read MAGeCK RRA gene summary", {
  testthat::expect_error(read_mageck_rra_gene_summary(filepath = NULL),
                         "Cannot read MAGeCK RRA gene summary, filepath is null.")
})

testthat::test_that("read MAGeCK RRA gene summary", {
  testthat::expect_snapshot(test_gene_summary_obj)
})

###############################################################################
#* --                                                                     -- *#
#* --                       get_mageck_sgrna_summary()                    -- *#
#* --                                                                     -- *#
###############################################################################

test_sgrna_summary_obj <- read_mageck_rra_sgrna_summary(
  filepath = system.file("testdata", "test_mageck_rra_sgrna_summary.txt", package = 'rcrispr'))
slot(test_sgrna_summary_obj, 'filepath', check = TRUE) <- 'test'

testthat::test_that("MageckSgrnaRRA filter warns if returning empty data frame", {
  testthat::expect_warning(get_mageck_sgrna_summary(test_sgrna_summary_obj,
                                                   filters = 'FDR > 10'),
                         "No rows returned from MAGeCK sgRNA summary with filter:")
})

testthat::test_that("MageckSgrnaRRA filter returns results", {
  testthat::expect_snapshot(get_mageck_sgrna_summary(test_sgrna_summary_obj,
                                                   filters = 'FDR < 0.05'))
})

###############################################################################
#* --                                                                     -- *#
#* --                   read_mageck_rra_sgrna_summary()                   -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("read MAGeCK RRA sgRNA summary", {
  testthat::expect_error(read_mageck_rra_sgrna_summary(filepath = NULL),
                         "Cannot read MAGeCK RRA sgRNA summary, filepath is null.")
})

testthat::test_that("read MAGeCK RRA sgRNA summary", {
  testthat::expect_snapshot(test_sgrna_summary_obj)
})

###############################################################################
#* --                                                                     -- *#
#* --                   get_mageck_sgrna_gene_results()                   -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot get MAGeCK RRA sgRNA summary per gene when gene is null", {
  testthat::expect_error(get_mageck_sgrna_gene_results(
                          object = test_sgrna_summary_obj, gene = NULL),
                         "Cannot get gene results from MAGeCK RRA summary, gene is null.")
})

testthat::test_that("cannot get MAGeCK RRA sgRNA summary per gene when gene doesn't exist", {
  testthat::expect_error(get_mageck_sgrna_gene_results(
    object = test_sgrna_summary_obj, gene = 'not_there'),
    "Cannot get gene results from MAGeCK RRA summary, gene is not present:")
})

testthat::test_that("get MAGeCK RRA sgRNA summary per gene", {
  testthat::expect_snapshot(get_mageck_sgrna_gene_results(
    object = test_sgrna_summary_obj, gene = 'NEG1'))
})

###############################################################################
#* --                                                                     -- *#
#* --                      gene_summary_top_n_genes()                     -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("can get top n depleted genes from MAGeCK RRA gene summary", {
  testthat::expect_snapshot(gene_summary_top_n_genes(
    gene_summary_object = test_gene_summary_obj, n = 3))
})

testthat::test_that("can get top n enriched genes from MAGeCK RRA gene summary", {
  testthat::expect_snapshot(gene_summary_top_n_genes(
    gene_summary_object = test_gene_summary_obj,
    direction = 'pos', n = 3))
})

testthat::test_that("can get top n enriched genes from MAGeCK RRA gene summary and warn if less than n", {
  testthat::expect_warning(gene_summary_top_n_genes(
    gene_summary_object = test_gene_summary_obj,
    direction = 'pos'),
    "Fewer than n top genes returned:")
})

testthat::test_that("cannot get top n genes from MAGeCK RRA gene summary when object is null", {
  testthat::expect_error(gene_summary_top_n_genes(
    gene_summary_object = NULL),
    "Cannot get top n genes, gene_summary is null.")
})

testthat::test_that("cannot get top n genes from MAGeCK RRA gene summary when fdr is not numeric", {
  testthat::expect_error(gene_summary_top_n_genes(
    gene_summary_object = test_gene_summary_obj,
    fdr = 'x'),
    "Cannot get top n genes, fdr is not numeric:")
})

testthat::test_that("cannot get top n genes from MAGeCK RRA gene summary when direction is not pos or neg", {
  testthat::expect_error(gene_summary_top_n_genes(
    gene_summary_object = test_gene_summary_obj,
    direction = 'x'),
    "Cannot get top n genes, direction is not 'pos' or 'neg':")
})

###############################################################################
#* --                                                                     -- *#
#* --                    plot_mageck_rra_gene_volcano()                   -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot plot MAGeCK RRA gene volcano", {
  testthat::expect_true(is.ggplot(plot_mageck_rra_gene_volcano(
    gene_summary_object = test_gene_summary_obj)))
})

testthat::test_that("cannot plot MAGeCK RRA gene volcano when gene_summary is null", {
  testthat::expect_error(plot_mageck_rra_gene_volcano(
    gene_summary_object = NULL),
    "Cannot plot MAGeCK gene volcano, gene_summary_object is null.")
})

testthat::test_that("cannot plot MAGeCK RRA gene volcano when fdr is not numeric", {
  testthat::expect_error(plot_mageck_rra_gene_volcano(
    gene_summary_object = test_gene_summary_obj,
    fdr = 'x'),
    "Cannot plot MAGeCK gene volcano, fdr is not numeric:")
})

testthat::test_that("cannot plot MAGeCK RRA gene volcano with bad enriched color", {
  testthat::expect_error(plot_mageck_rra_gene_volcano(
    gene_summary_object = test_gene_summary_obj,
    color_enriched = 'badColor'),
    "Cannot plot MAGeCK gene volcano, color_enriched is invalid:")
})

testthat::test_that("cannot plot MAGeCK RRA gene volcano with bad depleted color", {
  testthat::expect_error(plot_mageck_rra_gene_volcano(
    gene_summary_object = test_gene_summary_obj,
    color_depleted = 'badColor'),
    "Cannot plot MAGeCK gene volcano, color_depleted is invalid:")
})

###############################################################################
#* --                                                                     -- *#
#* --                    plot_mageck_rra_sgrna_barplot()                  -- *#
#* --                                                                     -- *#
###############################################################################

testthat::test_that("cannot plot MAGeCK RRA sgRNA barplot when sgrna_summary is null", {
  testthat::expect_error(plot_mageck_rra_sgrna_barplot(
    sgrna_summary_object = NULL),
    "Cannot plot MAGeCK RRA sgRNA LFC barplot, sgrna_summary_object is null.")
})

testthat::test_that("cannot plot MAGeCK RRA sgRNA barplot when gene is null", {
  testthat::expect_error(plot_mageck_rra_sgrna_barplot(
    sgrna_summary_object = test_sgrna_summary_obj,
    gene = NULL),
    "Cannot plot MAGeCK RRA sgRNA LFC barplot, gene is null.")
})

testthat::test_that("can plot MAGeCK RRA sgRNA barplot", {
  testthat::expect_true(is.ggplot(plot_mageck_rra_sgrna_barplot(
    sgrna_summary_object = test_sgrna_summary_obj,
    gene = 'NEG1')))
})

