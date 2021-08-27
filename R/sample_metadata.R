#' Read a sample metadata file.
#'
#' @description Read in a sample metadata file to a `SampleMetadata` object.
#'
#' @param filepath Character string specifying a file path.
#' @param filename_column the index of column containing sample count fileanames.
#' @param label_column the index of column containing sample names/labels.
#' @param plasmid_column the index of column showing whether sample is a plasmid sample.
#' @param control_column the index of column showing whether sample is a control sample.
#' @param treatment_column the index of column showing whether sample is a treatment sample.
#' @param group_column the index of column containing sample group.
#' @param reads_column the index of column containing total sequencing reads per sample.
#' @param file_separator count file separator.
#' @param file_header count file header.
#' @param ... additional read.delim parameters.
#'
#' @seealso \link[rcrispr]{SampleMetadata-class}
#' @return a `SampleMetadata` object..
#' @importFrom methods new
#' @importFrom methods validObject
#' @export read_sample_metadata_file
read_sample_metadata_file <-
  function(filepath = NULL,
           filename_column = 1,
           label_column = 2,
           plasmid_column = 3,
           control_column = 4,
           treatment_column = 5,
           group_column = NULL,
           reads_column = NULL,
           file_separator = "\t",
           file_header = TRUE,
           ...) {
    # Try reading sample metadata file into a data frame
    sample_metadata <- tryCatch({
      read_file_to_dataframe(
        filepath = filepath,
        file_separator = file_separator,
        file_header = file_header
      )
    }, error = function(e) {
      # Stop if there is an error
      stop(paste("Cannot read sample metadata:", e))
    })
    # Convert columns to integers
    for (i in c('filename_column', 'label_column', 'plasmid_column',
                'control_column', 'treatment_column')) {
      if (!is.null(get(i)))
        assign(i, convert_variable_to_integer(get(i)))
    }
    if (!is.null(reads_column))
      assign('reads_column', convert_variable_to_integer(reads_column))
    if (!is.null(group_column))
      assign('group_column', convert_variable_to_integer(group_column))

    # Convert sample metadata into a SampleMetadata object
    sample_metadata_object <- tryCatch({
      new(
        "SampleMetadata",
        filepath = filepath,
        filename_column = filename_column,
        label_column = label_column,
        plasmid_column = plasmid_column,
        control_column = control_column,
        treatment_column = treatment_column,
        group_column = group_column,
        reads_column = reads_column,
        file_separator = file_separator,
        file_header = file_header,
        metadata = sample_metadata
      )
    }, error = function(e) {
      # Stop if there is an error
      stop(e)
    })
    # Validate sample metadata
    validObject(sample_metadata_object)
    # Return sample metadata object
    return(sample_metadata_object)
  }
