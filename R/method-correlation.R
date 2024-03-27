correlationGeneric <- getGeneric("correlation", package = "veupathUtils")
selfCorrelationGeneric <- getGeneric("selfCorrelation", package = "veupathUtils")

setClassUnion("missingOrNULL", c("missing", "NULL"))

#' Self Correlation
#'
#' This function returns correlation coefficients for variables in one AbundanceData object against itself. It generally serves as a 
#' convenience wrapper around veupathUtils::correlation, with the exception that it additionally supports sparcc.
#' 
#' @param data An AbundanceData object
#' @param method string defining the type of correlation to run. The currently supported values are 'spearman','pearson' and 'sparcc'
#' @param format string defining the desired format of the result. The currently supported values are 'data.table' and 'ComputeResult'.
#' @param verbose boolean indicating if timed logging is desired
#' @param proportionNonZeroThreshold numeric threshold to filter features by proportion of non-zero values across samples
#' @param varianceThreshold numeric threshold to filter features by variance across samples
#' @param stdDevThreshold numeric threshold to filter features by standard deviation across samples
#' @return ComputeResult object
#' @import veupathUtils
#' @export
#' @rdname selfCorrelation-methods
#' @aliases selfCorrelation,AbundanceData-method
setMethod(selfCorrelationGeneric, signature("AbundanceData"), 
function(data, method = c('spearman','pearson','sparcc'), format = c('ComputeResult', 'data.table'), verbose = c(TRUE, FALSE), proportionNonZeroThreshold = 0.5, varianceThreshold = 0, stdDevThreshold = 0) {
  
  format <- veupathUtils::matchArg(format)
  method <- veupathUtils::matchArg(method)
  verbose <- veupathUtils::matchArg(verbose)

  #prefilters applied
  data <- veupathUtils::pruneFeatures(data, predicateFactory('proportionNonZero', proportionNonZeroThreshold), verbose)
  data <- veupathUtils::pruneFeatures(data, predicateFactory('variance', varianceThreshold), verbose)
  data <- veupathUtils::pruneFeatures(data, predicateFactory('sd', stdDevThreshold), verbose)

  abundances <- getAbundances(data, FALSE, FALSE, verbose)
  corrResult <- veupathUtils::correlation(abundances, NULL, method = method, format = 'data.table', verbose = verbose)

  veupathUtils::logWithTime(paste("Received df table with", nrow(abundances), "samples and", (ncol(abundances)-1), "features with abundances."), verbose)

  if (format == 'data.table') {
    return(corrResult)
  } else {
    result <- veupathUtils::buildCorrelationComputeResult(corrResult, data, NULL, method, verbose)
    result@computationDetails <- 'selfCorrelation'
    return(result)
  }  
})