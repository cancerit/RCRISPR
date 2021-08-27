#' An S4 class to represent MAGeCK RRA gene summary
#'
#' @slot filepath character.
#' @slot gene_summary data.frame.

setClass(
  "MageckGeneRRA",
  representation(
    filepath = "character",
    gene_summary = "data.frame"
  )
)

setValidity("MageckGeneRRA", function(object) {
  # Check we have all data
  if (is.null(object@filepath))
    return("file must have a value")
  if (is.null(object@gene_summary))
    return("gene_summary must have a value")

  # Check gene summary data frame
  if (!is.null(object@gene_summary))
    check_dataframe(object@gene_summary)

  # Check gene summary has the correct column names
  # Assumes summary has been read in with `check.names = T`
  expected_column_names <- c('id', 'num',
                             'neg.score', 'neg.p.value', 'neg.fdr',
                             'neg.rank', 'neg.goodsgrna', 'neg.lfc',
                             'pos.score', 'pos.p.value', 'pos.fdr',
                             'pos.rank', 'pos.goodsgrna', 'pos.lfc')
  if (length(setdiff(expected_column_names, colnames(object@gene_summary))) != 0)
    stop(paste("Expected columns not found:", paste(expected_column_names, sep = ",")))

  TRUE
})

#' Generic for get_mageck_gene_summary method
#'
#' @param object object
#' @param filters character
#'
#' @export
setGeneric("get_mageck_gene_summary", function(object, filters = NULL) {
  standardGeneric("get_mageck_gene_summary")
})

#' Get gene summary from MageckGeneRRA object
#'
#' @param object MageckGeneRRA
#' @param filters filter as string (e.g. 'neg.fdr < 0.05')
#' @import dplyr
#' @importFrom rlang parse_expr
#' @return data frame of MAGeCK gene summary results
#' @exportMethod get_mageck_gene_summary
setMethod("get_mageck_gene_summary",
          signature(object = "MageckGeneRRA"),
          function(object, filters = NULL) {
            gene_summary <- object@gene_summary
            if (!is.null(filters)) {
              # Filter using string
              # TODO: put in better error catching
              gene_summary <- gene_summary %>% filter(eval(rlang::parse_expr(filters)))
              if (nrow(gene_summary) == 0)
                warning(paste("No rows returned from MAGeCK gene summary with filter:", filters))
            }
            return(gene_summary)
          })
