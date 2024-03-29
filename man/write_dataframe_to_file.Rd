% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_helper.R
\name{write_dataframe_to_file}
\alias{write_dataframe_to_file}
\title{Write data frame to file.}
\usage{
write_dataframe_to_file(
  data = NULL,
  outfile = NULL,
  outdir = NULL,
  prefix = NULL,
  suffix = NULL,
  ignore_empty = FALSE,
  ...
)
}
\arguments{
\item{data}{a data frame.}

\item{outfile}{the name of the output file.}

\item{outdir}{the name of the output directory (defaults to current directory).}

\item{prefix}{a prefix to be added to the file name.}

\item{suffix}{a suffix to be added to the file name (before file extension).}

\item{ignore_empty}{whether to ignore empty file check.}

\item{...}{parameters for write.table}
}
\value{
The class of \code{file}.
}
\description{
Writes a data frame to file.
}
