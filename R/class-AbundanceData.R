check_abundance_data <- function(object) {
    errors <- character()
    df <- object@data
    record_id_col <- object@recordIdColumn
    ancestor_id_cols <- object@ancestorIdColumns

    if (any(df < 0, na.rm=TRUE)) {
      msg <- paste("Abundance data cannot contain negative values.")
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

#' Abundance Data
#' 
#' A class for working with microbiome or ecological abundance data.
#' 
#' @slot data A data.frame of abundance values with species as columns and samples as rows
#' @slot sampleMetadata A SampleMetadata object of metadata about the samples with samples as rows and metadata variables as columns
#' @slot recordIdColumn The name of the column containing IDs for the samples. All other columns will be treated as abundance values.
#' @slot ancestorIdColumns A character vector of column names representing parent entities of the recordIdColumn.
#' @slot imputeZero A logical indicating whether NA/ null values should be replaced with zeros.
#' @slot removeEmptyRecords A logical indicating whether empty (all NA/ zero) samples should be removed.
#' @name AbundanceData-class
#' @rdname AbundanceData-class
#' @importFrom veupathUtils SampleMetadata
#' @importFrom veupathUtils CollectionWithMetadata
#' @export 
AbundanceData <- setClass("AbundanceData", contains = "CollectionWithMetadata", validity = check_abundance_data)