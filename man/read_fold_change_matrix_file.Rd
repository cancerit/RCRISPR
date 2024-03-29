% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fold_changes.R
\name{read_fold_change_matrix_file}
\alias{read_fold_change_matrix_file}
\title{Read in sample fold change matrix.}
\usage{
read_fold_change_matrix_file(
  filepath = NULL,
  file_separator = "\\t",
  file_header = TRUE,
  id_column = 1,
  gene_column = NULL,
  fc_column = NULL,
  is_gene = FALSE,
  processed = FALSE,
  ...
)
}
\arguments{
\item{filepath}{Character string specifying a file path.}

\item{file_separator}{fold changes file separator.}

\item{file_header}{fold changes file header.}

\item{id_column}{the index of column containing unique sgRNA identifiers.}

\item{gene_column}{the index of column containing gene symbols.}

\item{fc_column}{vector indices of columns containing sample fold changes.}

\item{is_gene}{logical of whether fold changes are at sgRNA or gene level.}

\item{processed}{logical of whether to apply predefined column names.}

\item{...}{additional read.delim parameters.}
}
\value{
a data frame containing sample fold changes
}
\description{
Read file containing fold change matrix into a data frame.
}
\details{
Reads a fold change matrix file, as defined by \code{filepath}, into a data frame.
Requires annotation column indices be defined. For sgRNA-level fold changes:
\itemize{
\item \code{id_column} - column containing guide (sgRNA) identifiers (Default = 1).
\item \code{gene_column} - column containing gene symbols/identifiers (Default = 2).
\item \code{fc_columns} - indices of columns containing sample fold changes.
}
For gene-level fold changes:
\itemize{
\item \code{id_column} - column containing guide (sgRNA) identifiers (Default = 1).
\item \code{fc_columns} - indices of columns containing sample fold changes.
}
There must also be one or more columns containing sample fold changes whose indices are
indicated using \code{fc_columns}. If \code{fc_columns} is \code{NULL} then it is assumed
that all columns except \code{id_column} and/or \code{gene_column} contain fold change data.

Assumes by default that the fold changes file has a header, as defined by \code{file_header},
and that it is tab-delimited, as defined by \code{file_separator}.
}
