#' Beta diversity
#'
#' This function returns pcoa coordinates calculated from the beta diversity dissimilarity matrix.
#' 
#' @param data AbundanceData object
#' @param method string defining the the beta diversity dissimilarity method. Accepted values are 'bray','jaccard', and 'jsd'
#' @param k integer determining the number of pcoa axes to return
#' @param verbose boolean indicating if timed logging is desired
#' @return ComputeResult object
#' @importFrom Rcpp sourceCpp
#' @importFrom vegan vegdist
#' @importFrom ape pcoa
#' @importFrom stringi stri_replace_all_fixed
#' @import veupathUtils
#' @import data.table
#' @useDynLib microbiomeComputations
#' @export
setGeneric("betaDiv",
  function(data, method = c('bray','jaccard','jsd'), k = 2, verbose = c(TRUE, FALSE)) standardGeneric("betaDiv"),
  signature = c("data")
)

#'@export 
setMethod("betaDiv", signature("AbundanceData"), function(data, method = c('bray','jaccard','jsd'), k = 2, verbose = c(TRUE, FALSE)) {
    df <- microbiomeData::getAbundances(data, verbose = verbose)
    recordIdColumn <- data@recordIdColumn
    naToZero <- data@imputeZero
    ancestorIdColumns <- data@ancestorIdColumns
    allIdColumns <- c(recordIdColumn, ancestorIdColumns)

    # Initialize and check inputs
    method <- veupathUtils::matchArg(method)
    verbose <- veupathUtils::matchArg(verbose)

    # Check that incoming df meets requirements - consider moving to a validateOTU function or similar
    if (!'data.table' %in% class(df)) {
      data.table::setDT(df)
    }

    computeMessage <- ''
    veupathUtils::logWithTime(paste("Received df table with", nrow(df), "samples and", (ncol(df)-1), "taxa."), verbose)

    # Compute beta diversity using given dissimilarity method
    if (identical(method, 'bray') | identical(method, 'jaccard')) {

      dist <- try(vegan::vegdist(df[, -..allIdColumns], method=method, binary=TRUE))

    } else if (identical(method, 'jsd')) {

      dfMat <- matrix(as.numeric(unlist(df[, -..allIdColumns])), nrow=nrow(df))
      dist <- try({dist <- jsd(t(dfMat)); dist <- as.dist(dist)})

    } else {
      stop('Unaccepted dissimilarity method. Accepted methods are bray, jaccard, and jsd.')
    }
    
    result <- new("ComputeResult")
    result@name <- 'betaDiv'
    result@recordIdColumn <- recordIdColumn
    result@ancestorIdColumns <- ancestorIdColumns

    # Handle errors or return positive computeMessage
    if (veupathUtils::is.error(dist)) {
      veupathUtils::logWithTime(paste('Beta diversity computation FAILED with parameters method=', method, ', k=', k), verbose)
      stop() 
    } else {
      veupathUtils::logWithTime("Computed dissimilarity matrix.", verbose)
      computeMessage <- paste(method, "dissimilarity matrix computation successful.")
    }

    # Ordination
    pcoa <- ape::pcoa(dist)
    dt <- data.table::as.data.table(pcoa$vectors)
    dt <- dt[, 1:k]
    # Remove dots from names
    data.table::setnames(dt, stringi::stri_replace_all_fixed(names(dt),".",""))
    computeMessage <- paste(computeMessage, "PCoA returned results for", ncol(dt), "dimensions.")

    dt <- cbind(dt, df[, ..allIdColumns])
    data.table::setcolorder(dt, allIdColumns)
    veupathUtils::logWithTime("Finished ordination step.", verbose)

    # Extract percent variance
    eigenvecs <- pcoa$values$Relative_eig
    percentVar <- round(100*(eigenvecs / sum(eigenvecs)), 1)

    # Keep dims 1:k
    # We should keep the same number of percentVar values as cols in the data table. However, i think we're letting the user download lots of columns? So perhaps we shouldn't have k at all? A plot can use however many it needs.
    # For now returning data and percentVar for how much is in the plot.
    percentVar <- percentVar[1:k]
    
    entity <- veupathUtils::strSplit(recordIdColumn,".", 4, 1)
    result@computationDetails <- paste(computeMessage, ', pcoaVariance =', percentVar)
    result@parameters <- paste('method =', method)   
    
    axesNames <- names(dt[, -..allIdColumns])
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
    names(dt) <- veupathUtils::stripEntityIdFromColumnHeader(names(dt))
    result@data <- dt

    validObject(result)
    veupathUtils::logWithTime(paste('Beta diversity computation completed with parameters recordIdColumn=', recordIdColumn, ', method =', method, ', k =', k, ', verbose =', verbose), verbose)
    
    return(result)
})