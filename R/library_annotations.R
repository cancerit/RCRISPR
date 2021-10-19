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
#' Read library annotation file.
#'
#' @description
#' Reads in a library annotation file to a `LibraryAnnotations` object.
#'
#' @param filepath character string specifying a file path.
#' @param id_column index of column containing unique sgRNA identifiers.
#' @param gene_column index of column containing gene symbols.
#' @param chr_column index of column containing chromosome name.
#' @param chr_start_column index of column containing chromosome start position.
#' @param chr_end_column index of column containing chromosome position.
#' @param file_separator library annotation file separator.
#' @param file_header whether library annotation file contains a header.
#' @param strip_ids whether to make syntactically valid id names.
#' @param ... additional read.delim parameters.
#'
#' @return a data frame containing library annotations.
#' @importFrom methods new
#' @importFrom methods validObject
#' @export read_library_annotation_file
read_library_annotation_file <-
  function(filepath = NULL,
           id_column = 1,
           gene_column = 2,
           chr_column = NULL,
           chr_start_column = NULL,
           chr_end_column = NULL,
           file_separator = "\t",
           file_header = TRUE,
           strip_ids = FALSE,
           ...) {
    check_file(filepath)
    library_annotations <-
      read_file_to_dataframe(
        filepath = filepath,
        file_separator = file_separator,
        file_header = file_header
      )
    # Try to make each column an integer if it isn't already
    for (i in c('id_column', 'gene_column', 'chr_column', 'chr_start_column', 'chr_end_column')) {
      if (!is.null(get(i)))
        assign(i, convert_variable_to_integer(get(i)))
    }
    #strip guide IDs if requested
    if(strip_ids){
      library_annotations[, id_column] <- gsub("[.]", "", make.names(library_annotations[, id_column]))
    }
    # Try reading library annotation into dataframe
    library_annotation_object <- tryCatch({
      # Create new library annotation object
      new(
        "LibraryAnnotations",
        filepath = filepath,
        id_column = id_column,
        gene_column = gene_column,
        chr_column = chr_column,
        chr_start_column = chr_start_column,
        chr_end_column = chr_end_column,
        file_separator = file_separator,
        file_header = file_header,
        annotations = library_annotations
      )
    }, error = function(e) {
      print(e)
      # Stop if there is an error
      stop("Cannot create library annotation object.")
    })
    # Validate LibraryAnnotation object
    validObject(library_annotation_object)
    # Return LibraryAnnotation object
    return(library_annotation_object)
  }

#' Remove guides from library annotation object.
#'
#' @description
#' Remove guides from a `LibraryAnnotations` object.
#'
#' @seealso \link[rcrispr]{LibraryAnnotations-class}
#'
#' @param library_annotations_object LibraryAnnotations object.
#' @param guides_to_remove guide identifiers to remove.
#'
#' @import dplyr
#' @return LibraryAnnotations object
#' @export remove_guides_from_library_annotations_object
remove_guides_from_library_annotations_object <-
  function(library_annotations_object = NULL,
           guides_to_remove = NULL) {
    # Check input data is not null
    if (is.null(library_annotations_object))
      stop("Cannot remove guides from library, library annotations object is null.")
    if (is.null(guides_to_remove))
      stop("Cannot remove guides from library, guides to remove is null.")
    # Get first column of guides to remove
    if (!is.null(dim(guides_to_remove))) {
      guides_to_remove <- unique(guides_to_remove[,1])
    } else {
      guides_to_remove <- unique(guides_to_remove)
    }
    # Get unprocessed library
    library_annotation <- get_library_annotations(library_annotations_object)
    # Get library annotations id column
    id_column <- library_annotations_object@id_column
    # Error if any guides to remove aren't found in library
    number_of_guides_in_library <- intersect(guides_to_remove,
                                            unique(library_annotation[,id_column]))
    if (length(number_of_guides_in_library) != length(guides_to_remove)) {
      missing_guides <- setdiff(guides_to_remove,
                                unique(library_annotation[,id_column]))
      for (g in missing_guides) {
        warning(paste("Guide not found in library:", g))
      }
      stop(paste("Guides not found in library:", length(missing_guides)))
    }
    # Remove guides from library
    library_annotation <- library_annotation[!library_annotation[,id_column] %in% guides_to_remove,]
    # Check library
    check_dataframe(library_annotation)
    # Replace library into object
    slot(library_annotations_object, 'annotations', check = TRUE) <- library_annotation
    # Return library annotations object
    return(library_annotations_object)
  }

#' Identify guides with no coordinates from library annotation object.
#'
#' @description
#' Identify guides with no coordinates from a `LibraryAnnotations` object.
#'
#' @seealso \link[rcrispr]{LibraryAnnotations-class}
#'
#' @param library_annotations_object LibraryAnnotations object.
#'
#' @import dplyr
#' @return LibraryAnnotations object
#' @export get_guides_with_no_coordinates
get_guides_with_no_coordinates <-
  function(library_annotations_object = NULL) {
    # Check whether library has coordinates
    if(!library_has_coordinates(library_annotations_object))
      stop("Cannot remove guides with no coordinates, library has no coordinates.")
    # Get processed library
    library_annotations <- get_library_annotations(library_annotations_object, processed = T)
    # Identify guides where there are no or invalid chromosome annotations
    guides_with_no_coordinates <- library_annotations %>%
      filter(is.na(chr) | is.null(chr) | is.nan(chr) | is.na(start) | is.null(start) | is.nan(start) | is.na(end) | is.null(end) | is.nan(end)) %>%
      pull(sgRNA)
    # Warn if this is all guides
    if (length(guides_with_no_coordinates) == nrow(library_annotations))
      warning("All guides have no coordinates.")
    # Return list of guide identifiers
    return(guides_with_no_coordinates)
}
