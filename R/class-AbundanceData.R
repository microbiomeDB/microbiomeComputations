check_abundance_data <- function(object) {
    errors <- character()
    df <- object@data
    record_id_col <- object@recordIdColumn
    ancestor_id_cols <- object@ancestorIdColumns
    allIdColumns <- c(record_id_col, ancestor_id_cols)

    if (any(df[, -..allIdColumns] < 0, na.rm=TRUE)) {
      # Find negative values in df and return them for easier debugging.
      negative_indices <- which(df < 0, arr.ind = TRUE)
      row_indices <- unique(negative_indices[, 1])
      col_indices <- unique(negative_indices[, 2])
      negative_values <- unlist(lapply(seq_along(negative_indices[, 1]), function(i) dt[negative_indices[i, 1], negative_indices[i, 2], with=FALSE]))

      msg <- paste("Abundance data cannot contain negative values. Found negative values in the following samples: ", 
        paste(df[[record_id_col]][row_indices], collapse = ", "),
        " and in the following columns: ",
        paste(colnames(df)[col_indices], collapse = ", "),
        ". The values are: ",
        paste(negative_values, collapse = ", ")
      )
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