
setGeneric("toJSON",
  function(object, ...) standardGeneric("toJSON"),
  signature = "object"
)

setMethod("toJSON", signature("DifferentialAbundanceResult"), function(object, ...) {
  tmp <- character()

  tmp <- paste0(tmp, '"effectSizeLabel": ', jsonlite::toJSON(jsonlite::unbox(object@effectSizeLabel)), ',')
  tmp <- paste0(tmp, '"pValueFloor": ', jsonlite::toJSON(jsonlite::unbox(as.character(object@pValueFloor))), ',')
  tmp <- paste0(tmp, '"adjustedPValueFloor": ', jsonlite::toJSON(jsonlite::unbox(as.character(object@adjustedPValueFloor))), ',')

  outObject <- data.frame(lapply(object@statistics, as.character))
  tmp <- paste0(tmp, paste0('"statistics": ', jsonlite::toJSON(outObject)))

  tmp <- paste0("{", tmp, "}")
  return(tmp)
})

setMethod("toJSON", signature("CorrelationResult"), function(object, ...) {
  tmp <- character()

  # The outputted CorrelationResult json object should have properties data1Metadata, data2Metadata, and statistics.
  # All values in statistics should be strings.
  tmp <- paste0(tmp, '"data1Metadata": ', jsonlite::toJSON(jsonlite::unbox(object@data1Metadata)), ',')
  tmp <- paste0(tmp, '"data2Metadata": ', jsonlite::toJSON(jsonlite::unbox(object@data2Metadata)), ',')
  outObject <- data.frame(lapply(object@statistics, as.character))
  tmp <- paste0(tmp, paste0('"statistics": ', jsonlite::toJSON(outObject)))

  tmp <- paste0("{", tmp, "}")
  return(tmp)
})

# these let jsonlite::toJSON work by using the custom toJSON method for our custom result class
asJSONGeneric <- getGeneric("asJSON", package = "jsonlite")
setMethod(asJSONGeneric, "DifferentialAbundanceResult", function(x, ...) toJSON(x))
setMethod(asJSONGeneric, "CorrelationResult", function(x, ...) toJSON(x))
