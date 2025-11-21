#' Alpha diversity
#'
#' This function returns alpha diversity values for each sample.
#' 
#' @param data AbundanceData object
#' @param method string defining the the alpha diversity method. Accepted values are 'shannon','simpson', and 'evenness'
#' @param verbose boolean indicating if timed logging is desired
#' @return ComputeResult object
#' @importFrom vegan diversity
#' @importFrom stringi stri_trans_totitle
#' @import mbioUtils
#' @import data.table
#' @importFrom methods new slot validObject
#' @importFrom stats as.dist as.formula median quantile var
#' @rdname alphaDiv-methods
#' @export
setGeneric("alphaDiv",
  function(data, method = c('shannon','simpson','evenness'), verbose = c(TRUE, FALSE)) standardGeneric("alphaDiv"),
  signature = c("data")
)

#' @rdname alphaDiv-methods
#' @aliases alphaDiv,AbundanceData-method
setMethod("alphaDiv", signature("AbundanceData"), function(data, method = c('shannon','simpson','evenness'), verbose = c(TRUE, FALSE)) {
    df <- getAbundances(data, verbose = verbose)
    recordIdColumn <- data@recordIdColumn
    ancestorIdColumns <- data@ancestorIdColumns
    allIdColumns <- c(recordIdColumn, ancestorIdColumns)
    naToZero <- data@imputeZero

    # Initialize and check inputs
    method <- mbioUtils::matchArg(method)
    verbose <- mbioUtils::matchArg(verbose)

    # Check that incoming df meets requirements
    if (!'data.table' %in% class(df)) {
      data.table::setDT(df)
    }
    
    computeMessage <- ''
    mbioUtils::logWithTime(paste("Received df table with", nrow(df), "samples and", (ncol(df)-1), "taxa."), verbose)
    
    # Compute alpha diversity
    if (identical(method, 'shannon') | identical(method, 'simpson')){

      alphaDivDT <- try(vegan::diversity(df[, -..allIdColumns], method))
      computedVarLabel <- paste(stringi::stri_trans_totitle(method), 'Diversity')

    } else if (identical(method, 'evenness')) {

      alphaDivDT <- try(vegan::diversity(df[, -..allIdColumns], 'shannon') / log(vegan::specnumber(df)))
      computedVarLabel <- "Pielou\'s Evenness"
    }

    result <- new("ComputeResult")
    result@name <- 'alphaDiv'
    result@recordIdColumn <- recordIdColumn
    result@ancestorIdColumns <- ancestorIdColumns

    # Handle errors or return positive computeMessage
    if (mbioUtils::is.error(alphaDivDT)) {
      mbioUtils::logWithTime(paste('Alpha diversity computation FAILED with parameters, method =', method), verbose)
      stop() 
    } else {
      computeMessage <- paste('Computed', method, 'alpha diversity.')
      mbioUtils::logWithTime(paste(method, 'alpha diversity computation complete.'), verbose)
    }

    # Assemble data table
    dt <- data.table::as.data.table(df[, ..allIdColumns])
    dt$alphaDiversity <- alphaDivDT

    entity <- mbioUtils::strSplit(recordIdColumn, ".", 4, 1)
    result@computationDetails <- computeMessage
    result@parameters <- paste('method =', method)
    result@recordIdColumn <- recordIdColumn
    result@ancestorIdColumns <- ancestorIdColumns

    computedVariableMetadata <- mbioUtils::VariableMetadata(
                 variableClass = mbioUtils::VariableClass(value = "computed"),
                 variableSpec = mbioUtils::VariableSpec(variableId = names(dt[, -..allIdColumns]), entityId = entity),
                 plotReference = mbioUtils::PlotReference(value = "yAxis"),
                 displayName = computedVarLabel,
                 displayRangeMin = 0,
                 displayRangeMax = max(max(dt$alphaDiversity, na.rm = TRUE),1),
                 dataType = mbioUtils::DataType(value = "NUMBER"),
                 dataShape = mbioUtils::DataShape(value = "CONTINUOUS")
      )
      
    result@computedVariableMetadata <- mbioUtils::VariableMetadataList(S4Vectors::SimpleList(computedVariableMetadata))
    names(dt) <- mbioUtils::stripEntityIdFromColumnHeader(names(dt))
    result@data <- dt 
    
    validObject(result)
    mbioUtils::logWithTime(paste('Alpha diversity computation completed with parameters method=', method), verbose)
    
    return(result)
})
