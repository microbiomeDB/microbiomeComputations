# Tests for alpha diversity functions
test_that('alphaDiv returns a correctly formatted data.table', {


  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID')
  
  result <- alphaDiv(data, method='shannon', verbose=F)
  dt <- result@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','alphaDiversity'))
  expect_equal(c(class(dt$SampleID), class(dt$alphaDiversity)), c('character','numeric'))
  
  result <- alphaDiv(data, method='simpson', verbose=F)
  dt <- result@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','alphaDiversity'))
  expect_equal(c(class(dt$SampleID), class(dt$alphaDiversity)), c('character','numeric'))
  
  result <- alphaDiv(data, method='evenness', verbose=F)
  dt <- result@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','alphaDiversity'))
  expect_equal(c(class(dt$SampleID), class(dt$alphaDiversity)), c('character','numeric'))

  # With NAs
  nNAs <- 20
  df[sample(1:nrow(df), size=nNAs, replace = F), 2] <- NA

  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID',
              imputeZero = FALSE,
              removeEmptyRecords = FALSE
  )

  result <- alphaDiv(data, method='shannon', verbose=F)  # vegan diversity sets output to NA if input has NA. See issue #187
  dt <- result@data
  expect_equal(sum(is.na(dt)), nNAs)

  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID')

  result <- alphaDiv(data, method='shannon', verbose=F)
  dt <- result@data
  expect_equal(sum(is.na(dt)), 0)
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','alphaDiversity'))
  expect_equal(c(class(dt$SampleID), class(dt$alphaDiversity)), c('character','numeric'))
  expect_equal(all(dt>0), TRUE)  # ensure output is not all 0s.
})

test_that("alphaDiv returns a ComputeResult with good ComputedVariableMetadata", {
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID')
  
  result <- alphaDiv(data, method='shannon', verbose=F)

  expect_equal(result@parameters, 'method = shannon')
  expect_equal(inherits(result@computedVariableMetadata, "VariableMetadataList"), TRUE)
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@variableId, 'alphaDiversity')
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@entityId, 'entity')
  expect_equal(result@computedVariableMetadata[[1]]@displayName, 'Shannon Diversity')
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMin, 0)
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMax, 3.9660925)
  
  result <- alphaDiv(data, method='simpson', verbose=F)

  expect_equal(result@parameters, 'method = simpson')
  expect_equal(inherits(result@computedVariableMetadata, "VariableMetadataList"), TRUE)
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@variableId, 'alphaDiversity')
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@entityId, 'entity')
  expect_equal(result@computedVariableMetadata[[1]]@displayName, 'Simpson Diversity')
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMin, 0)
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMax, 1)
  
  result <- alphaDiv(data, method='evenness', verbose=F)
 
  expect_equal(result@parameters, 'method = evenness')
  expect_equal(inherits(result@computedVariableMetadata, "VariableMetadataList"), TRUE)
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@variableId, 'alphaDiversity')
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@entityId, 'entity')
  expect_equal(result@computedVariableMetadata[[1]]@displayName, 'Pielou\'s Evenness')
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMin, 0)
  expect_equal(result@computedVariableMetadata[[1]]@displayRangeMax, 1)
  
})

test_that("alphaDiv fails gracefully", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU
  
  df$entity.Abiotrophia <- NA

  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID',
              imputeZero = FALSE,
              removeEmptyRecords = FALSE)

  result <- alphaDiv(data, method='simpson', verbose=T)
  dt <- result@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID', 'alphaDiversity'))
  expect_true(all(is.na(dt[['alphaDiversity']])))
  expect_equal(typeof(dt$SampleID), c('character'))
  attr <- attributes(dt)
  # expect_equal(attr$computationDetails, "Error: alpha diversity evenness failed: input data must be non-negative")
  expect_equal(typeof(result@parameters), 'character')
  expect_equal(typeof(result@computedVariableMetadata[[1]]@variableSpec@variableId), 'character')
  expect_equal(typeof(result@computedVariableMetadata[[1]]@variableSpec@entityId), 'character')
  expect_equal(typeof(result@computedVariableMetadata[[1]]@dataType@value), 'character')
  expect_equal(typeof(result@computedVariableMetadata[[1]]@dataShape@value), 'character')
  expect_equal(typeof(result@computedVariableMetadata[[1]]@displayName), 'character')
  expect_equal(typeof(result@computedVariableMetadata[[1]]@displayRangeMin), 'double')
  expect_equal(typeof(result@computedVariableMetadata[[1]]@displayRangeMax), 'double')
  
  
})
