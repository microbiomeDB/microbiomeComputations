#' Get labels from a group in a Comparator
#'
#' For any Comparator object, returns the bin labels of either groupA or groupB
#' 
#' @param object Comparator 
#' @param group String, either "groupA" or "groupB"
#' @return charactor vector of labels from groupA or groupB
#' @export
#' @rdname Comparator-methods
setGeneric("getGroupLabels",
  function(object, group = c("groupA", "groupB")) standardGeneric("getGroupLabels"),
  signature = c("object")
)

#' @rdname Comparator-methods
#' @aliases getGroupLabels,Comparator-method 
setMethod("getGroupLabels", signature("Comparator"), function(object, group = c("groupA", "groupB")) {
  group <- veupathUtils::matchArg(group)

  groupBinList <- slot(object, group)

  groupLabels <- unlist(lapply(groupBinList, function(bin) {return(bin@binLabel)}))

  return(groupLabels)
})