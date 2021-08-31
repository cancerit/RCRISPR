# Copyright (c) 2021 Genome Research Ltd
#
# Author: CASM/Cancer IT <cgphelp@sanger.ac.uk>
#
# This file is part of RCRISPR.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# 1. The usage of a range of years within a copyright statement contained within
# this distribution should be interpreted as being equivalent to a list of years
# including the first and last year specified and all consecutive years between
# them. For example, a copyright statement that reads ‘Copyright (c) 2005, 2007-
# 2009, 2011-2012’ should be interpreted as being identical to a statement that
# reads ‘Copyright (c) 2005, 2007, 2008, 2009, 2011, 2012’ and a copyright
# statement that reads ‘Copyright (c) 2005-2012’ should be interpreted as being
# identical to a statement that reads ‘Copyright (c) 2005, 2006, 2007, 2008,
# 2009, 2010, 2011, 2012’.
#
###############################################################################
#* --                                                                     -- *#
#* --                       read_sample_count_file()                      -- *#
#* --                                                                     -- *#
###############################################################################

#' Read sample count file.
#'
#' @description
#' Reads in an individual sample count file to a `SampleCounts` object.
#'
#' @details
#' Reads a file containing guide counts for a single file and creates a
#' \link[rcrispr]{SampleCounts-class} object.
#'
#' Expects at least three columns which are defined by their index:
#' \itemize{
#' \item `id_column` - column containing guide (sgRNA) identifiers (Default = 1).
#' \item `gene_column` - column containing gene symbols/identifiers (Default = 2).
#' \item `count_column` - column containing sample counts (Default = 3).
#' }
#' Assumes by default that the count file has a header, defined by `file_header`,
#' and that it is tab-delimited, defined by `file_separator`.
#'
#' @seealso \link[rcrispr]{SampleCounts-class}
#'
#' @param filepath character string specifying a file path.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param count_column the index of column containing counts.
#' @param sample_name the sample name.
#' @param file_separator count file separator.
#' @param file_header whether count file(s) contain a header.
#' @param ... additional read.delim parameters.
#'
#' @return a `SampleCounts` object.
#' @importFrom methods new
#' @importFrom methods validObject
#' @export read_sample_count_file
read_sample_count_file <-
  function(filepath = NULL,
           sample_name = NULL,
           id_column = 1,
           gene_column = 2,
           count_column = 3,
           file_separator = "\t",
           file_header = TRUE,
           ...) {
    # Try reading sample counts into dataframe
    sample_counts <- tryCatch({
      read_file_to_dataframe(
        filepath = filepath,
        file_separator = file_separator,
        file_header = file_header,
        ...
      )
    }, error = function(e) {
      # Stop if there is an error
      stop(e)
    })
    # Try to make each column an integer if it isn't already
    for (i in c('id_column', 'gene_column', 'count_column')) {
      if (!is.null(get(i)))
        assign(i, convert_variable_to_integer(get(i)))
    }
    # Try converting sample counts into a SampleCounts object
    sample_counts_object <- tryCatch({
      # Create new sample counts object
      new(
        "SampleCounts",
        filepath = filepath,
        sample_name = sample_name,
        id_column = id_column,
        gene_column = gene_column,
        count_column = count_column,
        file_separator = file_separator,
        file_header = file_header,
        counts = sample_counts
      )
    }, error = function(e) {
      # Stop if there is an error
      stop(e)
    })
    # Validate SampleCounts object
    validObject(sample_counts_object)
    # Return SampleCounts object
    return(sample_counts_object)
  }

###############################################################################
#* --                                                                     -- *#
#* --         convert_sample_counts_objects_to_count_matrix()             -- *#
#* --                                                                     -- *#
###############################################################################

#' Convert SampleCounts objects into count matrix.
#'
#' @description
#' Convert a list of processed and validated sample counts into a count matrix.
#'
#' @details
#' Takes a list of SampleCount objects, extracts processed counts with pre-defined
#' column names and combines them into a data frame, also known as a count matrix.
#'
#' @seealso \link[rcrispr]{SampleCounts-class}
#'
#' @param sample_counts List of SampleCounts objects
#'
#' @return a data frame containing sample counts.
#' @import dplyr
#' @importFrom utils head
#' @export convert_sample_counts_objects_to_count_matrix
convert_sample_counts_objects_to_count_matrix <-
  function(sample_counts = NULL) {
    tryCatch({
      # Set up empty dataframe
      df <- data.frame()
      # Loop over sample count objects
      for (i in 1:length(sample_counts)) {
        # Check that we have a SampleCounts object
        if (class(sample_counts[[i]])[1] != 'SampleCounts')
          stop(paste("Not a SampleCounts object:", head(sample_counts[[i]])))
        # Extract processed sample counts to a temporary data frame
        tmp_counts <- counts(sample_counts[[i]], processed = T)
        # No need to test if empty as we already check for this when processing the counts
        if (nrow(df) == 0) {
          # If this is the first set of counts, set these as the temporary dataframe
          df <- tmp_counts
        } else {
          # If there are already samples in the count matrix check that
          # guide IDs and gene names match between samples
          if(!setequal(df[,1],tmp_counts[,1]))
            stop(paste("Could not add sample to count matrix, sgRNA ids don't match:", sample_counts[[i]]@sample_name))
          # Add sample counts to matrix
          df <- df %>% dplyr::left_join(tmp_counts, by = c('sgRNA', 'gene'))
        }
      }
      # Reorder data frame by sgRNA and gene
      df <- df %>% arrange(gene, sgRNA)
      # Validate data frame
      check_dataframe(df)
    }, error = function(e) {
      # Stop if there is an error
      stop(e)
    })
    # Return count matrix
    return(df)
  }

###############################################################################
#* --                                                                     -- *#
#* --                      read_count_matrix_file()                       -- *#
#* --                                                                     -- *#
###############################################################################

#' Read in sample count matrix.
#'
#' @description Read file containing count matrix into a data frame.
#'
#' @details
#' Reads a count matrix file, as defined by `filepath`, into a data frame.
#' Requires two annotation column indices be defined:
#' \itemize{
#' \item `id_column` - column containing guide (sgRNA) identifiers (Default = 1).
#' \item `gene_column` - column containing gene symbols/identifiers (Default = 2).
#' \item `count_column` - indices of columns containing sample counts.
#' }
#' There must also be one or more columns containing sample counts whose indices are
#' indicated using `count_column`. If `count_column` is `NULL` then it is assumed
#' that all columns except `id_column` and `gene_column` contain count data.
#'
#' Assumes by default that the count file has a header, as defined by `file_header`,
#' and that it is tab-delimited, as defined by `file_separator`.
#'
#' @param filepath Character string specifying a file path.
#' @param file_separator count file separator.
#' @param file_header count file header
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param count_column vector indices of columns containing sample counts.
#' @param processed logical of whether to apply predefined column names.
#' @param ... additional read.delim parameters.
#'
#' @return a data frame containing sample counts.
#' @export read_count_matrix_file
read_count_matrix_file <-
  function(filepath = NULL,
           file_separator = "\t",
           file_header = TRUE,
           id_column = 1,
           gene_column = 2,
           count_column = NULL,
           count_index_increment = 0,
           processed = FALSE,
           ...) {
    # Error out if count matrix has no header as we can't determine the sample names
    if (file_header == FALSE) {
      stop("Cannot read in a count matrix file without a header.", call. = F)
    }
    # Try reading count matrix file into data frame
    df <- tryCatch({
      # Read count matrix file into a data frame
      df <- read_file_to_dataframe(
        filepath = filepath,
        file_separator = file_separator,
        file_header = file_header,
        ...
      )
    }, error = function(e) {
      # Stop if there is an error
      stop(e)
    })
    # Validate data frame
    check_dataframe(df, check_na = TRUE)
    # Try to make each column an integer if it isn't already
    for (i in c('id_column', 'gene_column')) {
        assign(i, convert_variable_to_integer(get(i)))
    }
    # Process indices of count columns
    count_column_indices <- process_column_indices(count_column)
    # Check none of the column indices overlap
    if (anyDuplicated(c(id_column, gene_column, count_column_indices)) != 0)
      stop(paste("Cannot read count matrix, duplicate column indices:",
                 c(id_column, gene_column, count_column_indices)))
    # Check indices are within dataframe
    check_dataframe(df, indices = count_column_indices)
    # Remove unwanted columns
    df <- df[,c(id_column, gene_column, count_column_indices)]
    # If processed, set the column names for id_column and gene_column
    if (processed) {
      colnames(df)[1] <- 'sgRNA'
      colnames(df)[2] <- 'gene'
    }
    # Validate data frame
    check_dataframe(df, check_na = TRUE)
    # Return count matrix
    return(df)
  }

###############################################################################
#* --                                                                     -- *#
#* --                       read_sample_count_files()                     -- *#
#* --                                                                     -- *#
###############################################################################

#' Read sample count files.
#'
#' @description
#' Reads in individual sample count files to a list of `SampleCounts` objects.
#'
#' @details
#' Using a list of file names from a \link[rcrispr]{SampleMetadata-class} object,
#' reads in individual sample counts to \link[rcrispr]{SampleCounts-class} objects.
#'
#' Requires three count columns to be defined by their index:
#' \itemize{
#' \item `id_column` - column containing guide (sgRNA) identifiers (Default = 1).
#' \item `gene_column` - column containing gene symbols/identifiers (Default = 2).
#' \item `count_column` - column containing sample counts (Default = 3).
#' }
#' Assumes by default that the count file has a header, defined by `file_header`,
#' and that it is tab-delimited, defined by `file_separator`.
#'
#' @seealso \link[rcrispr]{SampleCounts-class}
#'
#' @param count_directory path of directory containing counts.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param count_column the index of column containing counts.
#' @param file_separator count file separator.
#' @param file_header whether count file(s) contain a header.
#' @param sample_metadata_object a sample metadata object
#' @param ... additional read.delim parameters.
#'
#' @return a list of `SampleCounts` objects.
#' @export read_sample_count_files
read_sample_count_files <-
  function(count_directory = NULL,
           id_column = 1,
           gene_column = 2,
           count_column = 3,
           file_separator = "\t",
           file_header = TRUE,
           sample_metadata_object = NULL,
           ...) {
    # Try to make each column an integer if it isn't already
    for (i in c('id_column', 'gene_column', 'count_column')) {
      if (!is.null(get(i)))
        assign(i, convert_variable_to_integer(get(i)))
    }
    # Check count directory
    check_directory(count_directory)
    # Error if sample metadata is NULL
    if (is.null(sample_metadata_object))
      stop("Cannot read sample counts, sample metadata is NULL.")
    # Get processed sample metadata
    sample_metadata <- get_sample_metadata(sample_metadata_object, processed = TRUE)
    # Set up empty list for SampleCounts objects
    sample_count_objects <- list()
    # Loop over filenames in sample metadata
    for (i in 1:nrow(sample_metadata) ) {
      # Build sample count file path
      sample_count_file <- file.path(count_directory, sample_metadata$filename[i])
      # Check file
      check_file(sample_count_file)
      # Read sample count file into SampleCounts object
      sample_count_object <- read_sample_count_file(sample_count_file,
                                                    sample_name = sample_metadata$label[i],
                                                    id_column = id_column,
                                                    gene_column = gene_column,
                                                    count_column = count_column,
                                                    file_header = file_header,
                                                    file_separator = file_separator)
      # Add SampleCount object to list
      sample_count_objects <- c(sample_count_objects, sample_count_object)
    }
    return(sample_count_objects)
  }

###############################################################################
#* --                                                                     -- *#
#* --                       compare_counts_to_library()                   -- *#
#* --                                                                     -- *#
###############################################################################

#' Compare SampleCounts to LibraryAnnotation.
#'
#' @description
#' Compares guide identifier and gene symbols in `SampleCounts` and `LibraryAnnotation` objects.
#'
#' @seealso \link[rcrispr]{SampleCounts-class}
#' @seealso \link[rcrispr]{LibraryAnnotations-class}
#'
#' @param sample_counts_object SampleCounts object.
#' @param library_annotation_object LibraryAnnotations object.
#'
#' @import dplyr
#' @return logical.
#' @export compare_counts_to_library
compare_counts_to_library <-
  function (sample_counts_object = NULL,
            library_annotation_object = NULL) {
    # Check input data is not null
    if (is.null(sample_counts_object))
      stop("Cannot compare counts to library, sample counts object is null.")
    if (is.null(library_annotation_object))
      stop("Cannot compare counts to library, library annotation object is null.")
    # Get processed library annotation
    library_annotation <- get_library_annotations(library_annotation_object, processed = TRUE)
    library_annotation <- library_annotation %>% select(sgRNA, gene) %>% arrange(gene, sgRNA)
    # Get processed sample counts
    sample_counts <- counts(sample_counts_object, processed = TRUE)
    sample_counts <- sample_counts %>% select(sgRNA, gene) %>% arrange(gene, sgRNA)
    # Check number of guides is equal
    if (nrow(sample_counts) != nrow(library_annotation))
      stop(paste("Cannot compare counts to library, number of guides in library and sample count differ:", nrow(library_annotation), nrow(sample_counts)))
    # Compare sgRNA and gene columns between counts and library
    if (!isTRUE( all.equal(sample_counts[,c("sgRNA", "gene")],
                           library_annotation,
                           check.attributes = F))) {
      stop(paste("sgRNA IDs and gene names in sample counts do not match:",
                 sample_counts_object@sample_name))
    }
    return(TRUE)
  }

###############################################################################
#* --                                                                     -- *#
#* --                 compare_count_matrix_to_library()                   -- *#
#* --                                                                     -- *#
###############################################################################

#' Compare count matrix to LibraryAnnotation.
#'
#' @description
#' Compares guide identifier and gene symbols in count matrix and `LibraryAnnotation` objects.
#'
#' @param count_matrix data frame of sample counts.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param library_annotation_object LibraryAnnotations object
#'
#' @seealso \link[rcrispr]{LibraryAnnotations-class}
#'
#' @import dplyr
#' @return logical.
#' @export compare_count_matrix_to_library
compare_count_matrix_to_library <-
  function (count_matrix = NULL,
            id_column = 1,
            gene_column = 2,
            library_annotation_object = NULL) {
    # Check input data is not null
    if (is.null(count_matrix))
      stop("Cannot compare count matrix to library, sample count matrix is null.")
    if (is.null(library_annotation_object))
      stop("Cannot compare count matrix to library, library annotation object is null.")
    # Try to make each column an integer if it isn't already
    for (i in c('id_column', 'gene_column')) {
      if (!is.null(get(i)))
        assign(i, convert_variable_to_integer(get(i)))
    }
    # Check indices are within data frame
    check_dataframe(count_matrix, indices = c(id_column, gene_column))
    # Get processed library annotation
    library_annotation <- get_library_annotations(library_annotation_object, processed = TRUE)
    library_annotation <- library_annotation %>% select(sgRNA, gene) %>% arrange(gene, sgRNA)
    # Check number of guides is equal
    if (nrow(count_matrix) != nrow(library_annotation))
      stop(paste("Cannot compare counts to library, number of guides in library and sample count differ:", nrow(library_annotation), nrow(count_matrix)))
    # Get id and gene columns from sample count matrix
    sample_counts_annotations <- count_matrix[order(count_matrix[,gene_column], count_matrix[,id_column]),
                                              c(id_column, gene_column)]
    # Compare sgRNA and gene columns between counts and library
    if (!isTRUE( all.equal.character(sample_counts_annotations,
                                     library_annotation,
                                     check.attributes = F))) {
      stop("sgRNA IDs and gene names in sample count matrix and library do not match.")
    }
    return(TRUE)
  }

###############################################################################
#* --                                                                     -- *#
#* --                compare_matrix_to_sample_metadata()                  -- *#
#* --                                                                     -- *#
###############################################################################

#' Compare matrix to SampleMetadata.
#'
#' @description
#' Checks sample names in matrix are present in `SampleMetadata` object.
#'
#' @param data data frame of sample counts or log fold changes.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param gene_column the index of column containing gene symbols.
#' @param sample_columns indices of columns containing counts.
#' @param sample_metadata_object SampleMetadata object
#'
#' @seealso \link[rcrispr]{SampleMetadata-class}
#'
#' @import dplyr
#' @return logical.
#' @export compare_matrix_to_sample_metadata
compare_matrix_to_sample_metadata <-
  function (data = NULL,
            id_column = 1,
            gene_column = NULL,
            sample_columns = 3,
            sample_metadata_object = NULL) {
    # Check input data is not null
    if (is.null(data))
      stop("Cannot compare sample column names to sample metadata, data is null.")
    if (is.null(sample_metadata_object))
      stop("Cannot compare sample column names to sample metadata, sample metadata object is null.")
    # Try to make each column an integer if it isn't already
    for (i in c('id_column', 'gene_column')) {
      if (!is.null(get(i)))
        assign(i, convert_variable_to_integer(get(i)))
    }
    # Expand count indices
    sample_columns <- process_column_indices(sample_columns)
    # Check indices are within data frame
    check_dataframe(data, indices = c(id_column, gene_column, sample_columns))
    # Get processed library annotation
    sample_metadata <- get_sample_metadata(sample_metadata_object, processed = TRUE)
    # Compare samples
    sample_names <- colnames(data)[sample_columns]
    sample_labels <- sample_metadata$label
    for (sn in sample_names) {
      if (!sn %in% sample_labels)
        stop(paste("Cannot compare sample column names to sample metadata, sample name not in metadata:", sn))
    }
    return(TRUE)
  }

###############################################################################
#* --                                                                     -- *#
#* --                reorder_count_matrix_by_sample_type()                -- *#
#* --                                                                     -- *#
###############################################################################

#' Reorder count matrix by sample metadata.
#'
#' @description
#' Reorders sample columns in count matrix using sample types (plasmid, control or treatment) from a `SampleMetadata` object.
#'
#' @seealso \link[rcrispr]{SampleMetadata-class}
#'
#' @param count_matrix count matrix data frame
#' @param sample_metadata_object SampleMetadata object
#'
#' @import dplyr
#' @return count matrix data frame.
#' @export reorder_count_matrix_by_sample_type
reorder_count_matrix_by_sample_type <-
  function(count_matrix = NULL,
           sample_metadata_object = NULL) {
    # Check input data is not null
    if (is.null(count_matrix))
      stop("Cannot reorder count matrix, count matrix is null.")
    if (is.null(sample_metadata_object))
      stop("Cannot reorder count matrix, sample metadata object is null.")
    # Get processed sample metadata
    sample_metadata <- get_sample_metadata(sample_metadata_object, processed = T)
    # Check that sample names in count matrix are present in sample metadata
    count_matrix_samples <- colnames(count_matrix)[3:length(colnames(count_matrix))]
    for (sn in count_matrix_samples) {
      if (! sn %in% sample_metadata$label) {
        stop(paste("Cannot reorder count matrix, sample name not in metadata:", sn))
      }
    }
    # Get shared samples (may not be all from metadata)
    shared_samples <- sample_metadata %>% filter(label %in% colnames(count_matrix))
    # Check that we have data
    check_dataframe(shared_samples)
    # Get plasmid sample names
    plasmid_samples <- shared_samples %>% filter(plasmid == 1) %>% arrange(label) %>% pull(label)
    # Get control sample names
    control_samples <- shared_samples %>% filter(control == 1) %>% arrange(label) %>% pull(label)
    # Get treatment sample names
    treatment_samples <- shared_samples %>% filter(treatment == 1) %>% arrange(label) %>% pull(label)
    # Reorder count matrix
    ordered_count_matrix <- count_matrix[,c('sgRNA', 'gene', plasmid_samples, control_samples, treatment_samples)]
    # Check that we have data
    check_dataframe(ordered_count_matrix)
    # Check count matrix has the same number of columns
    if (ncol(count_matrix) != ncol(ordered_count_matrix))
      stop(paste("Number of columns in reordered count matrix doesn't match:",
                 ncol(count_matrix),
                 ncol(ordered_count_matrix)))
    # Return ordered count matrix
    return(ordered_count_matrix)
  }

###############################################################################
#* --                                                                     -- *#
#* --                  remove_guides_from_sample_counts()                 -- *#
#* --                                                                     -- *#
###############################################################################

#' Remove guides from sample counts.
#'
#' @description
#' Remove guides from a `SampleCounts` object.
#'
#' @seealso \link[rcrispr]{SampleCounts-class}
#'
#' @param sample_counts_object SampleCounts object.
#' @param guides_to_remove guide identifiers to remove.
#'
#' @import dplyr
#' @return SampleCounts object
#' @export remove_guides_from_sample_counts
remove_guides_from_sample_counts <-
  function(sample_counts_object = NULL,
           guides_to_remove = NULL) {
    # Check input data is not null
    if (is.null(sample_counts_object))
      stop("Cannot remove guides from sample counts, sample counts object is null.")
    if (is.null(guides_to_remove))
      stop("Cannot remove guides from sample counts, guides to remove is null.")
    # Get first column of guides to remove
    if (!is.null(dim(guides_to_remove))) {
      guides_to_remove <- unique(guides_to_remove[,1])
    } else {
      guides_to_remove <- unique(guides_to_remove)
    }
    # Get unprocessed sample counts
    sample_counts <- counts(sample_counts_object)
    # Get sample counts id column
    id_column <- sample_counts_object@id_column
    # Error if any guides to remove aren't found in counts
    number_of_guides_in_counts <- intersect(guides_to_remove, unique(sample_counts[,id_column]))
    if (length(number_of_guides_in_counts) != length(guides_to_remove)) {
      missing_guides <- setdiff(guides_to_remove, unique(sample_counts[,id_column]))
      for (g in missing_guides) {
        warning(paste("Guide not found in counts:", g))
      }
      stop(paste("Guides not found in counts:", length(missing_guides)))
    }
    # Remove guides from counts
    sample_counts <- sample_counts[!sample_counts[,id_column] %in% guides_to_remove,]
    # Check sample counts
    check_dataframe(sample_counts)
    # Replace counts into object
    slot(sample_counts_object, 'counts', check = TRUE) <- sample_counts
    # Return sample counts object
    return(sample_counts_object)
  }

###############################################################################
#* --                                                                     -- *#
#* --                 remove_guides_from_count_matrix()                   -- *#
#* --                                                                     -- *#
###############################################################################

#' Remove guides from count matrix.
#'
#' @description
#' Remove guides from a count matrix.
#'
#' @param count_matrix sample count matrix.
#' @param id_column the index of column containing unique sgRNA identifiers.
#' @param guides_to_remove guide identifiers to remove.
#'
#' @import dplyr
#' @return dataframe
#' @export remove_guides_from_count_matrix
remove_guides_from_count_matrix <-
  function(count_matrix = NULL,
           id_column = 1,
           guides_to_remove = NULL) {
    # Check input data is not null
    if (is.null(count_matrix))
      stop("Cannot remove guides from count matrix, count matrix is null.")
    if (is.null(guides_to_remove))
      stop("Cannot remove guides from count matrix, guides to remove is null.")
    # Try to make each column an integer if it isn't already
    assign('id_column', convert_variable_to_integer(id_column))
    # Check count matrix
    check_dataframe(count_matrix)
    # Get first column of guides to remove
    if (!is.null(dim(guides_to_remove))) {
      guides_to_remove <- unique(guides_to_remove[,1])
    } else {
      guides_to_remove <- unique(guides_to_remove)
    }
    # Error if any guides to remove aren't found in counts
    number_of_guides_in_counts <- intersect(guides_to_remove, unique(count_matrix[,id_column]))
    if (length(number_of_guides_in_counts) != length(guides_to_remove)) {
      missing_guides <- setdiff(guides_to_remove, unique(count_matrix[,id_column]))
      for (g in missing_guides) {
        warning(paste("Guide not found in counts:", g))
      }
      stop(paste("Guides not found in counts:", length(missing_guides)))
    }
    # Remove guides from counts
    count_matrix <- count_matrix[!count_matrix[,id_column] %in% guides_to_remove,]
    # Check sample counts
    check_dataframe(count_matrix)
    # Return count matrix
    return(count_matrix)
  }

###############################################################################
#* --                                                                     -- *#
#* --                    get_guides_failing_filter()                      -- *#
#* --                                                                     -- *#
###############################################################################
#' Identify guides failing count filter.
#'
#' @description
#' Identify guides which fail count filter.
#'
#' @param count_matrix count matrix.
#' @param id_column index of column containing unique sgRNA identifiers.
#' @param count_column indices of columns containing counts.
#' @param filter_indices indices of columns on which to apply filter.
#' @param filter_method filter method from one of: all, any, mean or median (Default: all).
#' @param min_reads minimum number of reads for filter (Default: 30).
#'
#' @import dplyr
#' @importFrom matrixStats rowMedians
#' @return dataframe
#' @export get_guides_failing_filter
get_guides_failing_filter <-
  function(count_matrix = NULL,
           id_column = 1,
           count_column = NULL,
           filter_indices = NULL,
           filter_method = 'all',
           min_reads = 30) {
    # Check input data is not null
    if (is.null(count_matrix))
      stop("Cannot get guides to filter from count matrix, count matrix is null.")
    if (is.null(count_column))
      stop("Cannot get guides to filter from count matrix, count_column is null.")
    if (is.null(min_reads))
      stop("Cannot get guides to filter from count matrix, min_reads is null.")
    if (min_reads < 0)
      stop(paste("Cannot get guides to filter from count matrix, min_reads is < 0:", min_reads))
    if (is.null(filter_indices))
      stop("Cannot get guides to filter from count matrix, filter_indices is null.")
    if (is.null(filter_method))
      stop("Cannot get guides to filter from count matrix, filter_method is null.")
    if (!filter_method %in% c('all', 'any', 'mean', 'median'))
      stop(paste("Cannot get guides to filter from count matrix, filter_method is not valid (all, any, mean or median):",
                 filter_method))
    # Try to make each column an integer if it isn't already
      for (i in c('id_column', 'min_reads')) {
        if (!is.null(get(i)))
          assign(i, convert_variable_to_integer(get(i)))
      }
    # Check count matrix
    check_dataframe(count_matrix)
    # Expand count and filter indices
    count_column <- process_column_indices(count_column)
    filter_indices <- process_column_indices(filter_indices)
    # Check that filter indices are in count columns
    if (length(setdiff(filter_indices, count_column)) != 0)
      stop("Cannot get guides to filter from count matrix, filter indices not in count columns.")
    # Prepare count matrix
    count_matrix <- count_matrix[,c(id_column, filter_indices)]
    # Set up empty vector for filtered reads
    id_column_name <- colnames(count_matrix)[1]
    if (filter_method == 'all') {
      filtered_guides <- data.frame(count_matrix[,1],
        apply(count_matrix[2:ncol(count_matrix)], 2, function(x) x < min_reads))
      filtered_guides <- filtered_guides[,1]
      filtered_guides <- count_matrix %>%
                         filter_at(vars(-!!id_column_name), all_vars(. < min_reads)) %>%
                          pull(!!id_column_name)
    } else if (filter_method == 'any') {
      filtered_guides <- count_matrix %>%
                          filter_at(vars(-!!id_column_name), any_vars(. < min_reads)) %>%
                          pull(!!id_column_name)
    } else if (filter_method == 'mean') {
      mean_cols <- colnames(count_matrix)[-1]
      filtered_guides <- count_matrix %>%
                          mutate(filter_mean = rowMeans(as.matrix(.[mean_cols]), na.rm = FALSE)) %>%
                          filter(filter_mean < min_reads ) %>%
                          pull(!!id_column_name)
    } else if (filter_method == 'median') {
      median_cols <- colnames(count_matrix)[-1]
      filtered_guides <- count_matrix %>%
                          mutate(filter_median = rowMedians(as.matrix(.[median_cols]), na.rm = FALSE)) %>%
                          filter(filter_median < min_reads) %>%
                          pull(!!id_column_name)
    } else {
      filtered_guides <- vector()
    }
    return(filtered_guides)
  }
