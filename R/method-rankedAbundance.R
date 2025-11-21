#' Ranked abundance
#'
#' This function returns abundances, ranked by a selected ranking function
#' 
#' @param data AbundanceData object
#' @param method string defining the ranking strategy by which to order the taxa. Accepted values are 'median','max','q3',and 'variance'. Note that taxa that return a value of 0 for a given method will not be included in the results.
#' @param cutoff integer indicating the maximium number of taxa to be kept after ranking.
#' @param verbose boolean indicating if timed logging is desired
#' @return ComputeResult object
#' @import mbioUtils
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
    method <- mbioUtils::matchArg(method)
    verbose <- mbioUtils::matchArg(verbose)

    # Check that incoming df meets requirements
    if (!'data.table' %in% class(df)) {
      data.table::setDT(df)
    }

    computeMessage <- ''
    mbioUtils::logWithTime(paste("Received df table with", nrow(df), "samples and", (ncol(df)-1), "taxa."), verbose)

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

    mbioUtils::logWithTime("Finished ranking taxa", verbose)
    
    result <- new("ComputeResult")
    result@name <- 'rankedAbundance'
    result@recordIdColumn <- recordIdColumn
    result@ancestorIdColumns <- ancestorIdColumns

    entity <- mbioUtils::strSplit(recordIdColumn,".", 4, 1)
    result@computationDetails <- computeMessage
    result@parameters <- paste0('method = ',method, ', isCutoff = ', isCutoff)

    collectionMemberVariableIds <- unlist(lapply(names(dt[, -..allIdColumns]), mbioUtils::strSplit, ".", 4, 2))

    makeVariableSpecs <- function(variableId) {
	    mbioUtils::VariableSpec(variableId = variableId, entityId = entity)
    }

    computedVariableMetadata <- mbioUtils::VariableMetadata(
                 variableClass = mbioUtils::VariableClass(value = "computed"),
                 variableSpec = mbioUtils::VariableSpec(variableId = "rankedAbundance", entityId = entity),
                 plotReference = mbioUtils::PlotReference(value = "xAxis"),
                 displayName = "To be determined by client",
                 displayRangeMin = 0,
                 displayRangeMax = 1,
                 dataType = mbioUtils::DataType(value = "NUMBER"),
                 dataShape = mbioUtils::DataShape(value = "CONTINUOUS"),
                 isCollection = TRUE,
                 members = mbioUtils::VariableSpecList(S4Vectors::SimpleList(lapply(collectionMemberVariableIds, makeVariableSpecs)))
      )
    
    result@computedVariableMetadata <- mbioUtils::VariableMetadataList(S4Vectors::SimpleList(computedVariableMetadata))
    names(dt) <- mbioUtils::stripEntityIdFromColumnHeader(names(dt))
    result@data <- dt

    validObject(result)
    mbioUtils::logWithTime(paste('Ranked abundance computation completed with parameters recordIdColumn=', recordIdColumn, ', method =', method, ', cutoff =', cutoff, ', naToZero = ', naToZero, ', verbose =', verbose), verbose)

    return(result)
  })
