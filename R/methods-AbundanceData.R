#' Get data.table of abundances from AbundanceData
#'
#' Returns a data.table of abundances, respecting the
#' `imputeZero` slot.
#' 
#' @param object AbundanceData
#' @param ignoreImputeZero boolean indicating whether we should respect the imputeZero slot
#' @param includeIds boolean indicating whether we should include recordIdColumn and ancestorIdColumns
#' @param verbose boolean indicating if timed logging is desired
#' @return data.table of abundances
#' @rdname getAbundances
#' @export
setGeneric("getAbundances",
  function(object, ignoreImputeZero = c(FALSE, TRUE), includeIds = c(TRUE, FALSE), verbose = c(TRUE, FALSE)) standardGeneric("getAbundances"),
  signature = c("object")
)

#' @rdname getAbundances
#' @aliases getAbundances,AbundanceData-method
setMethod("getAbundances", signature("AbundanceData"), function(object, ignoreImputeZero = c(FALSE, TRUE), includeIds = c(TRUE, FALSE), verbose = c(TRUE, FALSE)) {
  ignoreImputeZero <- veupathUtils::matchArg(ignoreImputeZero)
  includeIds <- veupathUtils::matchArg(includeIds)
  verbose <- veupathUtils::matchArg(verbose)

  dt <- veupathUtils::getCollectionData(object, ignoreImputeZero = ignoreImputeZero, includeIds = includeIds, verbose = verbose)

  return(dt)
})

#' Drop samples with incomplete SampleMetadata
#'
#' Modifies the data and sampleMetadata slots of an 
#' AbundanceData object, to exclude samples with 
#' missing SampleMetadata for a specified column.
#' 
#' @param object AbundanceData
#' @param colName String providing the column name in SampleMetadata to check for completeness
#' @param verbose boolean indicating if timed logging is desired
#' @return AbundanceData with modified data and sampleMetadata slots
#' @rdname removeIncompleteSamples
#' @export
setGeneric("removeIncompleteSamples",
  function(object, colName = character(), verbose = c(TRUE, FALSE)) standardGeneric("removeIncompleteSamples"),
  signature = c("object")
)

#' @rdname removeIncompleteSamples
#' @aliases removeIncompleteSamples,AbundanceData-method
setMethod("removeIncompleteSamples", signature("AbundanceData"), function(object, colName = character(), verbose = c(TRUE, FALSE)) {
  verbose <- veupathUtils::matchArg(verbose)

  object <- veupathUtils::removeIncompleteRecords(object, colName = colName, verbose = verbose)

  return(object)
})