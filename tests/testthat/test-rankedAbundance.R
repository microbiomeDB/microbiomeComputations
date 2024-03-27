# Tests for ranked abundance functions
test_that('rankedAbundance returns a correctly formatted data.table', {
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  data <- AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID')

  dt <- rankedAbundance(data, method='max', verbose=F)@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','Gilliamella','Tyzzerella','Pseudomonas','Klebsiella','unclassified Chloroplast','Lactobacillus','unclassified Enterobacterales','Serratia','Frischella','Bombella'))
  expect_equal(unname(unlist(lapply(dt, class))), c('character', rep('numeric',10)))
  
  dt <- rankedAbundance(data, method='median', verbose=F)@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','Lactobacillus','Snodgrassella','Gilliamella','Bifidobacterium','Frischella','Commensalibacter','unclassified Mitochondria','unclassified Rhizobiaceae','unclassified Chloroplast','Bombella'))
  expect_equal(unname(unlist(lapply(dt, class))), c('character', rep('numeric',10)))
  
  dt <- rankedAbundance(data, method='variance', verbose=F)@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','Lactobacillus','Gilliamella','Bombella','Snodgrassella','Klebsiella','unclassified Rhizobiaceae','unclassified Enterobacterales','Fructobacillus','Pseudomonas','unclassified Chloroplast'))
  expect_equal(unname(unlist(lapply(dt, class))), c('character', rep('numeric',10)))
  
  dt <- rankedAbundance(data, method='q3', verbose=F)@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','Lactobacillus','Snodgrassella','Gilliamella','Frischella','Commensalibacter','unclassified Rhizobiaceae','Bifidobacterium','unclassified Mitochondria','unclassified Chloroplast','Bombella'))
  expect_equal(unname(unlist(lapply(dt, class))), c('character', rep('numeric',10)))

  # With NAs
  nNAs <- 20
  df[sample(1:nrow(df), size=nNAs, replace = F), 2] <- NA
  data <- AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID',
              imputeZero = FALSE,
              removeEmptyRecords = FALSE)

  expect_error(rankedAbundance(data, method='q3', verbose=F))

  data <- AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID')

  dt <- rankedAbundance(data, method='q3', verbose=F)@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','Lactobacillus','Snodgrassella','Gilliamella','Frischella','Commensalibacter','unclassified Rhizobiaceae','Bifidobacterium','unclassified Mitochondria','unclassified Chloroplast','Bombella'))
  expect_equal(unname(unlist(lapply(dt, class))), c('character', rep('numeric',10)))
  expect_equal(sum(dt > 0) > 2500, TRUE)   # ensure output is not all 0s.

  
})


test_that("rankedAbundance returns a data.table with the correct attributes", {
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU
  
  data <- AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID')
  
  result <- rankedAbundance(data, method='median', verbose=F)
  dt <- result@data
  expect_equal(result@parameters, 'method = median, isCutoff = TRUE')
  expect_equal(inherits(result@computedVariableMetadata, "VariableMetadataList"), TRUE)
  expect_equal(unlist(lapply(as.list(result@computedVariableMetadata[[1]]@members), function(x) {x@variableId})), c('Lactobacillus','Snodgrassella','Gilliamella','Bifidobacterium','Frischella','Commensalibacter','unclassified Mitochondria','unclassified Rhizobiaceae','unclassified Chloroplast','Bombella'))
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@entityId, 'entity')
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMin, 0)
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMax, 1)
  
  result <- rankedAbundance(data, method='max', verbose=F)
  dt <- result@data
  expect_equal(result@parameters, 'method = max, isCutoff = TRUE')
  expect_equal(inherits(result@computedVariableMetadata, "VariableMetadataList"), TRUE)
  expect_equal(unlist(lapply(as.list(result@computedVariableMetadata[[1]]@members), function(x) {x@variableId})), c('Gilliamella','Tyzzerella','Pseudomonas','Klebsiella','unclassified Chloroplast','Lactobacillus','unclassified Enterobacterales','Serratia','Frischella','Bombella'))
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@entityId, 'entity')
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMin, 0)
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMax, 1)
  
  result <- rankedAbundance(data, method='q3', verbose=F)
  dt <- result@data
  expect_equal(result@parameters, 'method = q3, isCutoff = TRUE')
  expect_equal(inherits(result@computedVariableMetadata, "VariableMetadataList"), TRUE)
  expect_equal(unlist(lapply(as.list(result@computedVariableMetadata[[1]]@members), function(x) {x@variableId})), c('Lactobacillus','Snodgrassella','Gilliamella','Frischella','Commensalibacter','unclassified Rhizobiaceae','Bifidobacterium','unclassified Mitochondria','unclassified Chloroplast','Bombella'))
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@entityId, 'entity')
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMin, 0)
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMax, 1)
  
  result <- rankedAbundance(data, method='variance', verbose=F)
  dt <- result@data
  expect_equal(result@parameters, 'method = variance, isCutoff = TRUE')
  expect_equal(inherits(result@computedVariableMetadata, "VariableMetadataList"), TRUE)
  expect_equal(unlist(lapply(as.list(result@computedVariableMetadata[[1]]@members), function(x) {x@variableId})), c('Lactobacillus','Gilliamella','Bombella','Snodgrassella','Klebsiella','unclassified Rhizobiaceae','unclassified Enterobacterales','Fructobacillus','Pseudomonas','unclassified Chloroplast'))
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@entityId, 'entity')
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMin, 0)
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMax, 1)
  
})