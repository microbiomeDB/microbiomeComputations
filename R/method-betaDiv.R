#' Beta diversity
#'
#' This function returns pcoa coordinates calculated from the beta diversity dissimilarity matrix.
#' 
#' @param data AbundanceData object
#' @param method string defining the the beta diversity dissimilarity method. Accepted values are 'bray','jaccard', and 'jsd'
#' @param k integer determining the number of pcoa axes to return
#' @param verbose boolean indicating if timed logging is desired
#' @return ComputedResult object
#' @importFrom Rcpp sourceCpp
#' @importFrom vegan vegdist
#' @importFrom ape pcoa
#' @importFrom stringi stri_replace_all_fixed
#' @import veupathUtils
#' @import data.table
#' @useDynLib microbiomeComputations
#' @export
setGeneric("betaDiv",
  function(data, method, k, verbose) standardGeneric("betaDiv"),
  signature = c("data")
)

#'@export 
setMethod("betaDiv", signature("AbundanceData"), function(data, method = c('bray','jaccard','jsd'), k = 2, verbose = c(TRUE, FALSE)) {
    df <- data@data
    recordIdColumn <- data@recordIdColumn
    naToZero <- data@imputeZero

    # Initialize and check inputs
    method <- veupathUtils::matchArg(method)
    verbose <- veupathUtils::matchArg(verbose)

    # Check that incoming df meets requirements - consider moving to a validateOTU function or similar
    if (!'data.table' %in% class(df)) {
      data.table::setDT(df)
    }

    computeMessage <- ''
    veupathUtils::logWithTime(paste("Received df table with", nrow(df), "samples and", (ncol(df)-1), "taxa."), verbose)

    if (naToZero) {
      # Replace NA values with 0
      veupathUtils::setNaToZero(df)
      veupathUtils::logWithTime("Replaced NAs with 0", verbose)
    }

    # Compute beta diversity using given dissimilarity method
    if (identical(method, 'bray') | identical(method, 'jaccard')) {

      dist <- try(vegan::vegdist(df[, -..recordIdColumn], method=method, binary=TRUE))

    } else if (identical(method, 'jsd')) {

      dfMat <- matrix(as.numeric(unlist(df[, -..recordIdColumn])), nrow=nrow(df))
      dist <- try({dist <- jsd(t(dfMat)); dist <- as.dist(dist)})

    } else {
      stop('Unaccepted dissimilarity method. Accepted methods are bray, jaccard, and jsd.')
    }
    
    result <- new("ComputedResult")
    result@name <- 'betaDiv'

    # Handle errors or return positive computeMessage
    if (veupathUtils::is.error(dist)) {
      
      computeMessage <- paste('Error: beta diversity', method, 'failed:', attr(dist,'condition')$message)
      
      # Return only recordIdColumn and expected attributes
      dt <- df[, ..recordIdColumn]
      result@data <- dt
      
      result@computationDetails <- computeMessage

      result@computedVariableMetadata <- veupathUtils::VariableMetadataList(S4Vectors::SimpleList(veupathUtils::VariableMetadata()))

      veupathUtils::logWithTime(paste('Beta diversity computation FAILED with parameters method=', method, ', k=', k), verbose)
      
      return(result)
      
    } else {
      veupathUtils::logWithTime("Computed dissimilarity matrix.", verbose)
      computeMessage <- paste(method, "dissimilarity matrix computation successful.")
    }

    # Ordination
    pcoa <- ape::pcoa(dist)
    dt <- data.table::as.data.table(pcoa$vectors)
    # Remove dots from names
    data.table::setnames(dt, stringi::stri_replace_all_fixed(names(dt),".",""))
    computeMessage <- paste(computeMessage, "PCoA returned results for", ncol(dt), "dimensions.")

    dt[[recordIdColumn]] <- df[[recordIdColumn]]
    data.table::setcolorder(dt, recordIdColumn)
    veupathUtils::logWithTime("Finished ordination step.", verbose)

    # Extract percent variance
    eigenvecs <- pcoa$values$Relative_eig
    percentVar <- round(100*(eigenvecs / sum(eigenvecs)), 1)

    # Keep dims 1:k
    # We should keep the same number of percentVar values as cols in the data table. However, i think we're letting the user download lots of columns? So perhaps we shouldn't have k at all? A plot can use however many it needs.
    # For now returning data and percentVar for how much is in the plot.
    percentVar <- percentVar[1:k]
    keepCols <- c(recordIdColumn,names(dt)[2:(k+1)])
    dt <- dt[, ..keepCols]
    
    entity <- veupathUtils::strSplit(recordIdColumn,".", 4, 1)
    result@computationDetails <- paste(computeMessage, ', pcoaVariance =', percentVar)
    result@parameters <- paste('method =', method)   
    
    axesNames <- names(dt[, -..recordIdColumn])
    displayNames <- paste0(axesNames, " ", sprintf(percentVar,fmt = '%#.1f'), "%")

    makeVariableMetadataObject <- function(displayName) {
      axisName <- veupathUtils::strSplit(displayName, " ")
      #bit hacky, see if you can think of something better
      plotRef <- ifelse(grepl('Axis1', displayName, fixed=T), 'xAxis', 'yAxis')

      veupathUtils::VariableMetadata(
                 variableClass = veupathUtils::VariableClass(value = "computed"),
                 variableSpec = veupathUtils::VariableSpec(variableId = axisName, entityId = entity),
                 plotReference = veupathUtils::PlotReference(value = plotRef),
                 displayName = displayName,
                 displayRangeMin = min(dt[[axisName]]),
                 displayRangeMax = max(dt[[axisName]]),
                 dataType = veupathUtils::DataType(value = "NUMBER"),
                 dataShape = veupathUtils::DataShape(value = "CONTINUOUS")
      )
    }
          
    computedVariableMetadata <- veupathUtils::VariableMetadataList(lapply(displayNames, makeVariableMetadataObject))

    result@computedVariableMetadata <- computedVariableMetadata
    
    # Add entity to column names
    data.table::setnames(dt, names(dt[, -..recordIdColumn]), paste0(entity,".",names(dt[, -..recordIdColumn])))
    result@data <- dt

    validObject(result)
    veupathUtils::logWithTime(paste('Beta diversity computation completed with parameters recordIdColumn=', recordIdColumn, ', method =', method, ', k =', k, ', verbose =', verbose), verbose)
    
    return(result)
})