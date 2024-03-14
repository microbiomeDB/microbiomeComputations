correlationGeneric <- getGeneric("correlation", package = "veupathUtils")
selfCorrelationGeneric <- getGeneric("selfCorrelation", package = "veupathUtils")

setClassUnion("missingOrNULL", c("missing", "NULL"))

#' Correlation of abundance data and metadata
#' 
#' This function returns the correlation of all columns of an AbundanceData object with appropriate columns of a SampleMetadata object.
#' 
#' @param data1 AbundanceData object.
#' @param data2 An optional second AbundanceData object. Will correlate all columns of data1 with all columns of data2
#' @param method string defining the type of correlation to run. The currently supported values are 'spearman' and 'pearson'
#' @param format string defining the desired format of the result. The currently supported values are 'data.table' and 'ComputeResult'.
#' @param verbose boolean indicating if timed logging is desired
#' @param proportionNonZeroThreshold numeric threshold to filter features by proportion of non-zero values across samples
#' @param varianceThreshold numeric threshold to filter features by variance across samples
#' @param stdDevThreshold numeric threshold to filter features by standard deviation across samples
#' @return a ComputeResult object
#' @export
#' @rdname correlation-methods
#' @aliases correlation,AbundanceData,missing-method
#' @importFrom veupathUtils correlation
#' @importFrom microbiomeData pruneFeatures
setMethod(correlationGeneric, signature("AbundanceData", "missingOrNULL"), 
function(data1, data2, method = c('spearman','pearson'), format  = c('ComputeResult', 'data.table'), verbose = c(TRUE, FALSE), proportionNonZeroThreshold = 0.5, varianceThreshold = 0, stdDevThreshold = 0) {
  
  format <- veupathUtils::matchArg(format)
  method <- veupathUtils::matchArg(method)
  verbose <- veupathUtils::matchArg(verbose)
  
  #prefilters applied
  data1 <- microbiomeData::pruneFeatures(data1, predicateFactory('proportionNonZero', proportionNonZeroThreshold), verbose)
  data1 <- microbiomeData::pruneFeatures(data1, predicateFactory('variance', varianceThreshold), verbose)
  data1 <- microbiomeData::pruneFeatures(data1, predicateFactory('sd', stdDevThreshold), verbose)
  
  abundances <- microbiomeData::getAbundances(data1, FALSE, FALSE, verbose)
  corrResult <- veupathUtils::correlation(abundances, microbiomeData::getSampleMetadata(data1, TRUE, FALSE), method = method, format = 'data.table', verbose = verbose)

  veupathUtils::logWithTime(paste("Received df table with", nrow(abundances), "samples and", (ncol(abundances)-1), "features with abundances."), verbose)

  if (format == 'data.table') {
    return(corrResult)
  } else {
    result <- veupathUtils::buildCorrelationComputeResult(corrResult, data1, data1@sampleMetadata, method, verbose)
    return(result)
  }
})

#' Self Correlation
#'
#' This function returns correlation coefficients for variables in one AbundanceData object against itself. It generally serves as a 
#' convenience wrapper around veupathUtils::correlation.
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
  data <- microbiomeData::pruneFeatures(data, predicateFactory('proportionNonZero', proportionNonZeroThreshold), verbose)
  data <- microbiomeData::pruneFeatures(data, predicateFactory('variance', varianceThreshold), verbose)
  data <- microbiomeData::pruneFeatures(data, predicateFactory('sd', stdDevThreshold), verbose)

  abundances <- microbiomeData::getAbundances(data, FALSE, FALSE, verbose)
  corrResult <- veupathUtils::correlation(abundances, NULL, method = method, format = 'data.table', verbose = verbose)

  veupathUtils::logWithTime(paste("Received df table with", nrow(abundances), "samples and", (ncol(abundances)-1), "features with abundances."), verbose)

  if (format == 'data.table') {
    return(corrResult)
  } else {
    result <- veupathUtils::buildCorrelationComputeResult(corrResult, data, NULL, method, verbose)
    return(result)
  }  
})

#' @rdname selfCorrelation-methods
#' @aliases selfCorrelation,SampleMetadata-method
setMethod(selfCorrelationGeneric, signature("SampleMetadata"), 
function(data, method = c('spearman','pearson','sparcc'), format = c('ComputeResult', 'data.table'), verbose = c(TRUE, FALSE)) {

  format <- veupathUtils::matchArg(format)
  method <- veupathUtils::matchArg(method)
  verbose <- veupathUtils::matchArg(verbose)
  
  corrResult <- veupathUtils::correlation(microbiomeData::getSampleMetadata(data, TRUE, FALSE), NULL, method = method, format = 'data.table', verbose = verbose)

  veupathUtils::logWithTime(paste("Received df table with", nrow(data), "samples and", (ncol(data)-1), "variables."), verbose)

  if (format == 'data.table') {
    return(corrResult)
  } else {
    result <- veupathUtils::buildCorrelationComputeResult(corrResult, data, NULL, method, verbose)
    return(result)
  }
})

#' @rdname correlation-methods
#' @aliases correlation,AbundanceData,AbundanceData-method
setMethod(correlationGeneric, signature("AbundanceData", "AbundanceData"), 
function(data1, data2, method = c('spearman','pearson'), format = c('ComputeResult', 'data.table'), verbose = c(TRUE, FALSE), proportionNonZeroThreshold = 0.5, varianceThreshold = 0, stdDevThreshold = 0) {
  
  format <- veupathUtils::matchArg(format)
  method <- veupathUtils::matchArg(method)
  verbose <- veupathUtils::matchArg(verbose)
  
  #prefilters applied
  data1 <- microbiomeData::pruneFeatures(data1, predicateFactory('proportionNonZero', proportionNonZeroThreshold), verbose)
  data1 <- microbiomeData::pruneFeatures(data1, predicateFactory('variance', varianceThreshold), verbose)
  data1 <- microbiomeData::pruneFeatures(data1, predicateFactory('sd', stdDevThreshold), verbose)
  data2 <- microbiomeData::pruneFeatures(data2, predicateFactory('proportionNonZero', proportionNonZeroThreshold), verbose)
  data2 <- microbiomeData::pruneFeatures(data2, predicateFactory('variance', varianceThreshold), verbose)
  data2 <- microbiomeData::pruneFeatures(data2, predicateFactory('sd', stdDevThreshold), verbose)
  
  abundances1 <- microbiomeData::getAbundances(data1, FALSE, TRUE, verbose)
  abundances2 <- microbiomeData::getAbundances(data2, FALSE, TRUE, verbose)

  # empty samples removed from data by microbiomeData::getAbundances, means we need to keep samples common to both datasets and remove ids
  # get id col names
  recordIdColumn <- data1@recordIdColumn
  allIdColumns <- c(recordIdColumn, data1@ancestorIdColumns)
  # should we verify that ids are the same in both datasets?

  # remove samples that are not common
  commonSamples <- intersect(abundances1[[recordIdColumn]], abundances2[[recordIdColumn]])
  abundances1 <- abundances1[abundances1[[recordIdColumn]] %in% commonSamples, ]
  abundances2 <- abundances2[abundances2[[recordIdColumn]] %in% commonSamples, ]

  # remove ids
  abundances1 <- abundances1[, -..allIdColumns]
  abundances2 <- abundances2[, -..allIdColumns]  

  corrResult <- veupathUtils::correlation(abundances1, abundances2, method = method, format = 'data.table', verbose = verbose)

  veupathUtils::logWithTime(paste("Received first df table with", nrow(abundances1), "samples and", (ncol(abundances1)-1), "features with abundances."), verbose)
  veupathUtils::logWithTime(paste("Received second df table with", nrow(abundances2), "samples and", (ncol(abundances2)-1), "features with abundances."), verbose)

  if (format == 'data.table') {
    return(corrResult)
  } else {
    result <- veupathUtils::buildCorrelationComputeResult(corrResult, data1, data2, method, verbose)
    return(result)
  } 
})
