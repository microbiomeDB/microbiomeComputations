correlationGeneric <- getGeneric("correlation", package = "mbioUtils")
selfCorrelationGeneric <- getGeneric("selfCorrelation", package = "mbioUtils")

setClassUnion("missingOrNULL", c("missing", "NULL"))

#' Self Correlation
#'
#' This function returns correlation coefficients for variables in one AbundanceData object against itself. It generally serves as a 
#' convenience wrapper around mbioUtils::correlation, with the exception that it additionally supports sparcc.
#' 
#' @param data An AbundanceData object
#' @param method string defining the type of correlation to run. The currently supported values are 'spearman','pearson' and 'sparcc'
#' @param format string defining the desired format of the result. The currently supported values are 'data.table' and 'ComputeResult'.
#' @param verbose boolean indicating if timed logging is desired
#' @param proportionNonZeroThreshold numeric threshold to filter features by proportion of non-zero values across samples
#' @param varianceThreshold numeric threshold to filter features by variance across samples
#' @param stdDevThreshold numeric threshold to filter features by standard deviation across samples
#' @return ComputeResult object
#' @import mbioUtils
#' @export
#' @rdname selfCorrelation-methods
#' @aliases selfCorrelation,AbundanceData-method
setMethod(selfCorrelationGeneric, signature("AbundanceData"), 
function(data, method = c('spearman','pearson','sparcc'), format = c('ComputeResult', 'data.table'), verbose = c(TRUE, FALSE), proportionNonZeroThreshold = 0.5, varianceThreshold = 0, stdDevThreshold = 0) {
  
  format <- mbioUtils::matchArg(format)
  method <- mbioUtils::matchArg(method)
  verbose <- mbioUtils::matchArg(verbose)

  #prefilters applied
  data <- mbioUtils::pruneFeatures(data, predicateFactory('proportionNonZero', proportionNonZeroThreshold), verbose)
  data <- mbioUtils::pruneFeatures(data, predicateFactory('variance', varianceThreshold), verbose)
  data <- mbioUtils::pruneFeatures(data, predicateFactory('sd', stdDevThreshold), verbose)

  abundances <- getAbundances(data, FALSE, FALSE, verbose)
  corrResult <- mbioUtils::correlation(abundances, NULL, method = method, format = 'data.table', verbose = verbose)

  mbioUtils::logWithTime(paste("Received df table with", nrow(abundances), "samples and", (ncol(abundances)-1), "features with abundances."), verbose)

  if (format == 'data.table') {
    return(corrResult)
  } else {
    result <- mbioUtils::buildCorrelationComputeResult(corrResult, data, NULL, method, verbose)
    result@computationDetails <- 'selfCorrelation'
    return(result)
  }  
})