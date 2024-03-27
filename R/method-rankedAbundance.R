#' Ranked abundance
#'
#' This function returns abundances, ranked by a selected ranking function
#' 
#' @param data AbundanceData object
#' @param method string defining the ranking strategy by which to order the taxa. Accepted values are 'median','max','q3',and 'variance'. Note that taxa that return a value of 0 for a given method will not be included in the results.
#' @param cutoff integer indicating the maximium number of taxa to be kept after ranking.
#' @param verbose boolean indicating if timed logging is desired
#' @return ComputeResult object
#' @import veupathUtils
#' @import data.table
#' @importFrom S4Vectors SimpleList
#' @export
#' @rdname rankedAbundance-methods
setGeneric("rankedAbundance",
  function(data, method = c('median','max','q3','variance'), cutoff=10, verbose = c(TRUE, FALSE)) standardGeneric("rankedAbundance"),
  signature = c("data")
)

#' @rdname rankedAbundance-methods
#' @aliases rankedAbundance,AbundanceData-method
setMethod("rankedAbundance", signature("AbundanceData"), function(data, method = c('median','max','q3','variance'), cutoff=10, verbose = c(TRUE, FALSE)) {
    df <- getAbundances(data, verbose = verbose)
    recordIdColumn <- data@recordIdColumn
    naToZero <- data@imputeZero
    ancestorIdColumns <- data@ancestorIdColumns
    allIdColumns <- c(recordIdColumn, ancestorIdColumns)

    # Initialize and check inputs
    method <- veupathUtils::matchArg(method)
    verbose <- veupathUtils::matchArg(verbose)

    # Check that incoming df meets requirements
    if (!'data.table' %in% class(df)) {
      data.table::setDT(df)
    }

    computeMessage <- ''
    veupathUtils::logWithTime(paste("Received df table with", nrow(df), "samples and", (ncol(df)-1), "taxa."), verbose)

    # Reshape back to sample, taxonomicLevel, abundance
    formattedDT <- data.table::melt(df, measure.vars=colnames(df[, -..allIdColumns]), variable.factor=F, variable.name='TaxonomicLevel', value.name="Abundance")

    rankedTaxa <- rankTaxa(formattedDT, method)

    # Extract top N taxa
    topN <- rankedTaxa[Abundance > 0, TaxonomicLevel]
    isCutoff <- FALSE
    if (length(topN) > cutoff) {
      topN <- topN[1:cutoff]
      computeMessage <- paste("Only returning top", cutoff, "results.")
      isCutoff <- TRUE
    }

    keepCols <- c(allIdColumns, topN)
    dt = df[, ..keepCols]

    veupathUtils::logWithTime("Finished ranking taxa", verbose)
    
    result <- new("ComputeResult")
    result@name <- 'rankedAbundance'
    result@recordIdColumn <- recordIdColumn
    result@ancestorIdColumns <- ancestorIdColumns

    entity <- veupathUtils::strSplit(recordIdColumn,".", 4, 1)
    result@computationDetails <- computeMessage
    result@parameters <- paste0('method = ',method, ', isCutoff = ', isCutoff)

    collectionMemberVariableIds <- unlist(lapply(names(dt[, -..allIdColumns]), veupathUtils::strSplit, ".", 4, 2))

    makeVariableSpecs <- function(variableId) {
	    veupathUtils::VariableSpec(variableId = variableId, entityId = entity)
    }

    computedVariableMetadata <- veupathUtils::VariableMetadata(
                 variableClass = veupathUtils::VariableClass(value = "computed"),
                 variableSpec = veupathUtils::VariableSpec(variableId = "rankedAbundance", entityId = entity),
                 plotReference = veupathUtils::PlotReference(value = "xAxis"),
                 displayName = "To be determined by client",
                 displayRangeMin = 0,
                 displayRangeMax = 1,
                 dataType = veupathUtils::DataType(value = "NUMBER"),
                 dataShape = veupathUtils::DataShape(value = "CONTINUOUS"),
                 isCollection = TRUE,
                 members = veupathUtils::VariableSpecList(S4Vectors::SimpleList(lapply(collectionMemberVariableIds, makeVariableSpecs)))
      )
    
    result@computedVariableMetadata <- veupathUtils::VariableMetadataList(S4Vectors::SimpleList(computedVariableMetadata))
    names(dt) <- veupathUtils::stripEntityIdFromColumnHeader(names(dt))
    result@data <- dt

    validObject(result)
    veupathUtils::logWithTime(paste('Ranked abundance computation completed with parameters recordIdColumn=', recordIdColumn, ', method =', method, ', cutoff =', cutoff, ', naToZero = ', naToZero, ', verbose =', verbose), verbose)

    return(result)
  })
