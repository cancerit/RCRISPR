% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic.R
\name{compare_annotations}
\alias{compare_annotations}
\title{Compare annotations between two dataframes.}
\usage{
compare_annotations(
  x = NULL,
  y = NULL,
  x_id_column = 1,
  x_gene_column = 2,
  y_id_column = 1,
  y_gene_column = 2,
  ...
)
}
\arguments{
\item{x}{data frame to be compared.}

\item{y}{data frame to be compared.}

\item{x_id_column}{the index of column containing unique sgRNA identifiers.}

\item{x_gene_column}{the index of column containing gene symbols.}

\item{y_id_column}{the index of column containing unique sgRNA identifiers.}

\item{y_gene_column}{the index of column containing gene symbols.}

\item{...}{options for all.equal.character function.}
}
\value{
logical.
}
\description{
Compares guide identifier and gene symbols between two dataframes
}
