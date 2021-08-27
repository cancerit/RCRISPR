#' An S4 class to represent MAGeCK RRA sgRNA summary
#'
#' @slot filepath character.
#' @slot sgrna_summary data.frame.

setClass(
  "MageckSgrnaRRA",
  representation(
    filepath = "character",
    sgrna_summary = "data.frame"
  )
)

setValidity("MageckSgrnaRRA", function(object) {
  # Check we have all data
  if (is.null(object@filepath))
    return("file must have a value")
  if (is.null(object@sgrna_summary))
    return("sgrna_summary must have a value")

  # Check sgRNA summary data frame
  if (!is.null(object@sgrna_summary))
    check_dataframe(object@sgrna_summary)

  # Check sgRNA summary has the correct column names
  # Assumes summary has been read in with `check.names = T`
  expected_column_names <- c('sgrna', 'Gene',
                             'control_count', 'treatment_count', 'control_mean', 'treat_mean',
                             'LFC', 'control_var', 'adj_var', 'score',
                             'p.low', 'p.high', 'p.twosided', 'FDR', 'high_in_treatment')
  if (length(setdiff(expected_column_names, colnames(object@sgrna_summary))) != 0)
    stop(paste("Expected columns not found:", paste(expected_column_names, sep = ",")))

  TRUE
})

#' Generic for get_mageck_sgrna_summary method
#'
#' @param object object
#' @param filters character
#'
#' @export
setGeneric("get_mageck_sgrna_summary", function(object, filters = NULL) {
  standardGeneric("get_mageck_sgrna_summary")
})

#' Get sgRNA summary from MageckSgrnaRRA object
#'
#' @param object MageckSgrnaRRA
#' @param filters filter as string (e.g. 'neg.fdr < 0.05')
#' @import dplyr
#' @importFrom rlang parse_expr
#' @return data frame of MAGeCK sgRNA summary results
#' @exportMethod get_mageck_sgrna_summary
setMethod("get_mageck_sgrna_summary",
          signature(object = "MageckSgrnaRRA"),
          function(object, filters = NULL) {
            sgrna_summary <- object@sgrna_summary
            if (!is.null(filters)) {
              # Filter using string
              # TODO: put in better error catching
              sgrna_summary <- sgrna_summary %>% filter(eval(rlang::parse_expr(filters)))
              if (nrow(sgrna_summary) == 0)
                warning(paste("No rows returned from MAGeCK sgRNA summary with filter:", filters))
            }
            return(sgrna_summary)
          })

#' Generic for get_mageck_sgrna_gene_results method
#'
#' @param object object
#' @param gene character
#'
#' @export
setGeneric("get_mageck_sgrna_gene_results", function(object, gene = NULL) {
  standardGeneric("get_mageck_sgrna_gene_results")
})

#' Get sgRNA gene results from MageckSgrnaRRA object
#'
#' @param object MageckSgrnaRRA
#' @param gene name of gene
#' @import dplyr
#' @return data frame of MAGeCK sgRNA summary results
#' @exportMethod get_mageck_sgrna_gene_results
setMethod("get_mageck_sgrna_gene_results",
          signature(object = "MageckSgrnaRRA"),
          function(object, gene = NULL) {
            # Check whether gene is null
            if (is.null(gene))
              stop("Cannot get gene results from MAGeCK RRA summary, gene is null.")
            # Get sgRNA results from object
            sgrna_summary <- object@sgrna_summary
            # Error if gene not present in results
            if (!gene %in% sgrna_summary$Gene)
              stop(paste("Cannot get gene results from MAGeCK RRA summary, gene is not present:", gene))
            # Filter to get results for gene
            sgrna_summary <- sgrna_summary %>% filter(Gene == gene)
            # Validate results
            check_dataframe(sgrna_summary)
            # Return gene sgRNA LFCs
            return(sgrna_summary)
          })
