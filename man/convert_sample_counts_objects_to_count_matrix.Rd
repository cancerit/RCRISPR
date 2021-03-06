% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_counts.R
\name{convert_sample_counts_objects_to_count_matrix}
\alias{convert_sample_counts_objects_to_count_matrix}
\title{Convert SampleCounts objects into count matrix.}
\usage{
convert_sample_counts_objects_to_count_matrix(
  sample_counts = NULL,
  sort_ids = FALSE
)
}
\arguments{
\item{sample_counts}{List of SampleCounts objects}

\item{sort_ids}{whether to order by gene and sgRNA identifier (FALSE)}
}
\value{
a data frame containing sample counts.
}
\description{
Convert a list of processed and validated sample counts into a count matrix.
}
\details{
Takes a list of SampleCount objects, extracts processed counts with pre-defined
column names and combines them into a data frame, also known as a count matrix.
}
\seealso{
\link[rcrispr]{SampleCounts-class}
}
