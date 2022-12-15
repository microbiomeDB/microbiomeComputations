check_abundance_data <- function(object) {
    errors <- character()
    df <- object@data
    record_id_col <- object@recordIdColumn
    ancestor_id_cols <- object@ancestorIdColumns
    
    if (length(record_id_col) != 1) {
      msg <- "Record ID column must have a single value."
      errors <- c(errors, msg) 
    }

    if (!record_id_col %in% names(df)) {
      msg <- paste("Record ID column is not present in abundance data.frame")
      errors <- c(errors, msg)
    }

    if (!!length(ancestor_id_cols)) {
      if (!all(ancestor_id_cols %in% names(df))) {
        msg <- paste("Not all ancestor ID columns are present in abundance data.frame")
        errors <- c(errors, msg)
      }
    }

    if (!all(unlist(lapply(df[, !(names(df) %in% c(record_id_col, ancestor_id_cols))], is.numeric)))) {
      msg <- paste("All columns except the ID columns must be numeric.")
      errors <- c(errors, msg)
    }

    if (uniqueN(veupathUtils::strSplit(names(df)[!names(df) %in% ancestor_id_cols], ".", ncol=2, index=1)) > 1) {
      msg <- paste("All columns must belong to the same entity.")
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

#' Abundance Data
#' 
#' A class for working with microbiome or ecological abundance data.
#' 
#' @slot data A data.frame of abundance values with species as columns and samples as rows
#' @slot recordIdColumn The name of the column containing IDs for the samples. All other columns will be treated as abundance values.
#' @slot ancestorIdColumns A character vector of column names representing parent entities of the recordIdColumn.
#' @slot imputeZero A logical indicating whether NA/ null values should be replaced with zeros.
#' 
#' @name AbundanceData-class
#' @rdname AbundanceData-class
#' @export 
AbundanceData <- setClass("AbundanceData", representation(
    data = 'data.frame',
    recordIdColumn = 'character',
    ancestorIdColumns = 'character',
    imputeZero = 'logical'
), prototype = prototype(
    recordIdColumn = NA_character_,
    imputeZero = TRUE
), validity = check_abundance_data)