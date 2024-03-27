# a helper, to reuse and separate some logic
cleanComparatorVariable <- function(data, comparator, verbose = c(TRUE, FALSE)) {
  if (!inherits(data, 'AbundanceData')) stop("data must be of the AbundanceData class.")
  if (!inherits(comparator, 'Comparator')) stop("comparator must be of the Comparator class.")

  comparatorColName <- veupathUtils::getColName(comparator@variable@variableSpec)
  data <- removeIncompleteSamples(data, comparatorColName, verbose)
  abundances <- getAbundances(data, verbose = verbose)
  sampleMetadata <- veupathUtils::getSampleMetadata(data)
  recordIdColumn <- data@recordIdColumn

  veupathUtils::logWithTime(paste("Received abundance table with", nrow(abundances), "samples and", (ncol(abundances)-1), "taxa."), verbose)

  # Subset to only include samples with metadata defined in groupA and groupB
    if (identical(comparator@variable@dataShape@value, "CONTINUOUS")) {
      
      # Ensure bin starts and ends are numeric
      comparator@groupA <- as.numeric(comparator@groupA)
      comparator@groupB <- as.numeric(comparator@groupB)


      # We need to turn the numeric comparison variable into a categorical one with those values
      # that fall within group A or group B bins marked with some string we know.

      # Collect all instances where the comparatorColName has values in the bins from each group.
      # So inGroupA is a vector with 0 if the value in comparatorColName is not within any of the group A bins and >0 otherwise.
      inGroupA <- veupathUtils::whichValuesInBinList(sampleMetadata[[comparatorColName]], comparator@groupA)
      inGroupB <- veupathUtils::whichValuesInBinList(sampleMetadata[[comparatorColName]], comparator@groupB)

      # Eventually move this check to Comparator validation. See #47
      if ((any(inGroupA * inGroupB) > 0)) {
        stop("Group A and Group B cannot have overlapping bins.")
      }

      # Make the comparatorColName a character vector and replace the in-group values with a bin.
      sampleMetadata[, (comparatorColName) := as.character(get(comparatorColName))]
      
      # Now we can reassign groupA and groupB and can replace values in sampleMetadata our new group values
      # We don't care about the values in the comparisonVariable column anymore. They were only
      # useful to help us assign groups.
      sampleMetadata[inGroupA, c(comparatorColName)] <- "groupA"
      sampleMetadata[inGroupB, c(comparatorColName)] <- "groupB"

      # Finally, subset the sampleMetadata to only include those samples in groupA or B
      sampleMetadata <- sampleMetadata[get(comparatorColName) %in% c("groupA", "groupB"), ]

    } else {
      # The comparator must be ordinal, binary, or categorical
      groupAValues <- getGroupLabels(comparator, "groupA")
      groupBValues <- getGroupLabels(comparator, "groupB")

      # Filter sampleMetadata to keep only those samples that are labeled as groupA or groupB. Filter
      # data *before* reassigning values to 'groupA' and 'groupB' to avoid issues with the original variable
      # value being 'groupA' or 'groupB'
      sampleMetadata <- sampleMetadata[get(comparatorColName) %in% c(groupAValues, groupBValues), ]

      # Turn comparatorColName into a binary variable
      sampleMetadata[get(comparatorColName) %in% groupAValues, c(comparatorColName)] <- 'groupA'
      sampleMetadata[get(comparatorColName) %in% groupBValues, c(comparatorColName)] <- 'groupB'
    } 

    # sampleMetadata has already been filtered so it now only contains the samples we care about
    keepSamples <- sampleMetadata[[recordIdColumn]]
    if (!length(keepSamples)) {
      stop("No samples remain after subsetting based on the comparator variable.")
    }
    # need to make sure we actually have two groups
    if (length(unique(sampleMetadata[[comparatorColName]])) < 2) {
      stop("The comparator variable must have at least two values/ groups within the subset.")
    }
    veupathUtils::logWithTime(paste0("Found ",length(keepSamples)," samples with a value for ", comparatorColName, " in either groupA or groupB. The calculation will continue with only these samples."), verbose)

    # Subset the abundance data based on the kept samples
    abundances <- abundances[get(recordIdColumn) %in% keepSamples, ]

    data@data <- abundances
    data@sampleMetadata <- veupathUtils::SampleMetadata(
      data = sampleMetadata,
      recordIdColumn = data@sampleMetadata@recordIdColumn
    )
    validObject(data)

    return(data)
}

#' Differential Abundance Result
#' 
#' A DifferentialAbundanceResult object contains the results of differential
#'  abundance analysis.
#' 
#' @slot effectSizeLabel label for the effect size metric used. Default is 'log2(Fold Change)'.
#' @slot statistics data frame containing the statistics
#' @slot pValueFloor the p-value floor. Any p-values less than this will be set to this value.
#' @slot adjustedPValueFloor the adjusted p-value floor. 
#' @export
DifferentialAbundanceResult <- setClass("DifferentialAbundanceResult", representation(
    effectSizeLabel = 'character',
    statistics = 'data.frame',
    pValueFloor = 'numeric',
    adjustedPValueFloor = 'numeric'
), prototype = prototype(
    effectSizeLabel = 'log2(Fold Change)',
    statistics = data.frame(effectSize = numeric(0),
                            pValue = numeric(0),
                            adjustedPValue = numeric(0),
                            pointID = character(0))
))


setGeneric("deseq",
  function(data, comparator, verbose = c(TRUE, FALSE)) standardGeneric("deseq"),
  signature = c("data", "comparator")
)

setMethod("deseq", signature("AbsoluteAbundanceData", "Comparator"), function(data, comparator, verbose = c(TRUE, FALSE)) {
  recordIdColumn <- data@recordIdColumn
  ancestorIdColumns <- data@ancestorIdColumns
  allIdColumns <- c(recordIdColumn, ancestorIdColumns)
  sampleMetadata <- veupathUtils::getSampleMetadata(data)
  comparatorColName <- veupathUtils::getColName(comparator@variable@variableSpec)

  # First, remove id columns and any columns that are all 0s.
  cleanedData <- purrr::discard(data@data[, -..allIdColumns], function(col) {identical(union(unique(col), c(0, NA)), c(0, NA))})
  # Next, transpose abundance data to get a counts matrix with taxa as rows and samples as columns
  counts <- data.table::transpose(cleanedData)
  rownames(counts) <- names(cleanedData)
  colnames(counts) <- data@data[[recordIdColumn]]

  # Then, format metadata. Recall samples are rows and variables are columns
  rownames(sampleMetadata) <- sampleMetadata[[recordIdColumn]]
  
  # Finally, check to ensure samples are in the same order in counts and metadata. Both DESeq
  # and ANCOMBC expect the order to match, and will not perform this check.
  if (!identical(rownames(sampleMetadata), colnames(counts))){
    # Reorder sampleMetadata to match counts
    veupathUtils::logWithTime("Sample order differs between data and metadata. Reordering data based on the metadata sample order.", verbose)
    data.table::setcolorder(counts, rownames(sampleMetadata))
  }

  deseq_output <- try({

    # Create DESeqDataSet (dds)
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = counts,
                                          colData = sampleMetadata,
                                          design = as.formula(paste0("~",comparatorColName)),
                                          tidy = FALSE)

    # Estimate size factors before running deseq to avoid errors about 0 counts
    geoMeans = apply(DESeq2::counts(dds), 1, function(x){exp(sum(log(x[x > 0]), na.rm=T) / length(x))})
    dds <- DESeq2::estimateSizeFactors(dds, geoMeans = geoMeans)

    # Run DESeq
    deseq_output <- DESeq2::DESeq(dds)
  })

  if (veupathUtils::is.error(deseq_output)) {
    veupathUtils::logWithTime(paste0('Differential abundance FAILED with parameters recordIdColumn=', recordIdColumn, ', method = DESeq', ', verbose =', verbose), verbose)
    stop()
  }

  # Extract deseq results
  deseq_results <- DESeq2::results(deseq_output)

  # Format results for easier access
  statistics <- data.frame(effectSize = deseq_results$log2FoldChange,
                           pValue = deseq_results$pvalue,
                           adjustedPValue = deseq_results$padj,
                           pointID = rownames(counts))

  result <- DifferentialAbundanceResult('effectSizeLabel' = 'log2(Fold Change)', 'statistics' = statistics)

  return(result)
})

setMethod("deseq", signature("AbundanceData", "Comparator"), function(data, comparator, verbose = c(TRUE, FALSE)) {
  stop("Please use the AbsoluteAbundanceData class with DESeq2.")
})

setGeneric("maaslin",
  function(data, comparator, verbose = c(TRUE, FALSE)) standardGeneric("maaslin"),
  signature = c("data", "comparator")
)

# this leaves room for us to grow into dedicated params (normalization and analysis method etc) for counts if desired
setMethod("maaslin", signature("AbundanceData", "Comparator"), function(data, comparator, verbose = c(TRUE, FALSE)) {
  recordIdColumn <- data@recordIdColumn
  ancestorIdColumns <- data@ancestorIdColumns
  allIdColumns <- c(recordIdColumn, ancestorIdColumns)
  sampleMetadata <- veupathUtils::getSampleMetadata(data)
  comparatorColName <- veupathUtils::getColName(comparator@variable@variableSpec)
  abundances <- data@data

  # First, remove id columns and any columns that are all 0s.
  cleanedData <- purrr::discard(abundances[, -..allIdColumns], function(col) {identical(union(unique(col), c(0, NA)), c(0, NA))})
  rownames(cleanedData) <- abundances[[recordIdColumn]]
  rownames(sampleMetadata) <- sampleMetadata[[recordIdColumn]]

  maaslinOutput <- Maaslin2::Maaslin2(
        input_data = cleanedData, 
        input_metadata = sampleMetadata,
        output = tempfile("maaslin"),
        #min_prevalence = 0,
        fixed_effects = c(comparatorColName),
        analysis_method = "LM", # default LM
        normalization = "TSS", # default TSS
        transform = "LOG", # default LOG
        plot_heatmap = F,
        plot_scatter = F)

      # NOTE!!!! Coefficient in place of Log2FC only makes sense for LM
      # see https://forum.biobakery.org/t/trying-to-understand-coef-column-and-how-to-convert-it-to-fold-change/3136/8

      statistics <- data.frame(effectSize = maaslinOutput$results$coef,
                          pValue = maaslinOutput$results$pval,
                          adjustedPValue = maaslinOutput$results$qval,
                          pointID = maaslinOutput$results$feature)

      result <- DifferentialAbundanceResult('effectSizeLabel' = 'Model Coefficient (Effect Size)', 'statistics' = statistics)

  return(result)
})

#' Differential abundance
#'
#' This function returns the fold change and associated p value for a differential abundance analysis comparing samples in two groups.
#' 
#' @param data AbsoluteAbundanceData object
#' @param comparator Comparator object specifying the variable and values or bins to be used in dividing samples into groups.
#' @param method string defining the the differential abundance method. Accepted values are 'DESeq2' and 'Maaslin2'.
#' @param pValueFloor numeric value that indicates the smallest p value that should be returned. 
#' The corresponding adjusted p value floor will also be updated based on this value, and will be set to the maximum adjusted p value of all floored p values.
#' The default value uses the P_VALUE_FLOOR=1e-200 constant defined in this package.
#' @param verbose boolean indicating if timed logging is desired
#' @return ComputeResult object
#' @import veupathUtils
#' @import data.table
#' @import DESeq2
#' @importFrom Maaslin2 Maaslin2
#' @importFrom purrr none
#' @importFrom purrr discard
#' @useDynLib microbiomeComputations
#' @export
#' @rdname differentialAbundance-methods
setGeneric("differentialAbundance",
  function(data, comparator, method = c('DESeq', 'Maaslin'), pValueFloor = P_VALUE_FLOOR, verbose = c(TRUE, FALSE)) standardGeneric("differentialAbundance"),
  signature = c("data", "comparator")
)

# this is consistent regardless of rel vs abs abund. the statistical methods will differ depending on that. 
#' @rdname differentialAbundance-methods
#' @aliases differentialAbundance,AbundanceData,Comparator-method
setMethod("differentialAbundance", signature("AbundanceData", "Comparator"), function(data, comparator, method = c('DESeq', 'Maaslin'), pValueFloor = P_VALUE_FLOOR, verbose = c(TRUE, FALSE)) {
    data <- cleanComparatorVariable(data, comparator, verbose)
    recordIdColumn <- data@recordIdColumn
    ancestorIdColumns <- data@ancestorIdColumns
    allIdColumns <- c(recordIdColumn, ancestorIdColumns)
    comparatorColName <- veupathUtils::getColName(comparator@variable@variableSpec)

    ## Initialize and check inputs
    method <- veupathUtils::matchArg(method)
    verbose <- veupathUtils::matchArg(verbose)

    
    ## Compute differential abundance
    if (identical(method, 'DESeq')) {
      statistics <- deseq(data, comparator, verbose)
#    } else if (identical(method, 'ANCOMBC')) {
#
#      se <- TreeSummarizedExperiment::TreeSummarizedExperiment(list(counts = counts), colData = sampleMetadata)
#
#      # Currently getting this error: Error in is.infinite(o1) : default method not implemented for type 'list'
#      # Ignoring for now.
#      output_abs = ANCOMBC::ancombc2(data = se, assay_name = "counts", tax_level = NULL,
#                  fix_formula = comparatorColName, rand_formula = NULL,
#                  p_adj_method = "holm", prv_cut=0,
#                  group = comparatorColName)
#
    } else if (identical(method, 'Maaslin')) {
      statistics <- maaslin(data, comparator, verbose)
    } else {
      stop('Unaccepted differential abundance method. Accepted methods are "DESeq" and "Maaslin".')
    }
    veupathUtils::logWithTime(paste0('Completed method=',method,'. Formatting results.'), verbose)

    # Sometimes p-values can be very small, even smaller than the smallest representable number (gives p-value=0). The smallest
    # representable number changes based on env, so to avoid inconsistency set a p-value floor so that any
    # returned p-value less than the floor becomes the floor. 
    # The default floor is a constant defined in the microbiomeComputations package.

    # First find indices of the small p-values and update these values to be the pValueFloor
    smallPValueIndices <- which(statistics@statistics[["pValue"]] < pValueFloor)
    if (length(smallPValueIndices) > 0) {
      statistics@statistics[smallPValueIndices, "pValue"] <- pValueFloor

      # Second, find the adjusted p value floor by taking the largest adjusted p-value of those p-values that were floored
      smallAdjPValues <- statistics@statistics[smallPValueIndices, "adjustedPValue"]
      adjustedPValueFloor <- max(smallAdjPValues)

      # Finally, update the adjusted p-values with the floor
      statistics@statistics[smallPValueIndices, "adjustedPValue"] <- adjustedPValueFloor
    } else {
      adjustedPValueFloor <- NA_real_
    }
    statistics@pValueFloor <- pValueFloor
    statistics@adjustedPValueFloor <- adjustedPValueFloor

    
    # this is droppedTaxa, or pathways etc ?? can we rename it?
    droppedColumns <- setdiff(names(data@data[, -..allIdColumns, with=FALSE]), statistics@statistics$pointID)

    ## Construct the ComputeResult
    result <- new("ComputeResult")
    result@name <- 'differentialAbundance'
    result@recordIdColumn <- recordIdColumn
    result@ancestorIdColumns <- ancestorIdColumns
    result@statistics <- statistics
    result@parameters <- paste0('recordIdColumn = ', recordIdColumn,", comparatorColName = ", comparatorColName, ', method = ', method, ', groupA =', getGroupLabels(comparator, "groupA"), ', groupB = ', getGroupLabels(comparator, "groupB"))
    result@droppedColumns <- droppedColumns


    # The resulting data should contain only the samples actually used.
    result@data <- data@data[, ..allIdColumns]
    names(result@data) <- veupathUtils::stripEntityIdFromColumnHeader(names(result@data))


    validObject(result)
    veupathUtils::logWithTime(paste('Differential abundance computation completed with parameters recordIdColumn = ', recordIdColumn,", comparatorColName = ", comparatorColName, ', method = ', method, ', groupA =', getGroupLabels(comparator, "groupA"), ', groupB = ', getGroupLabels(comparator, "groupB")), verbose)
    
    return(result)
})
