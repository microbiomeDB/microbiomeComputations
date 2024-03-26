# Tests for beta diversity functions
test_that('betaDiv returns a correctly formatted data.table', {
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID')

  dt <- betaDiv(data, method='bray', verbose=F)@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','Axis1','Axis2'))
  expect_equal(unname(unlist(lapply(dt, class))), c('character','numeric','numeric'))
  
  dt <- betaDiv(data, method='jaccard', verbose=F)@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','Axis1','Axis2'))
  expect_equal(unname(unlist(lapply(dt, class))), c('character','numeric','numeric'))
  
  dt <- betaDiv(data, method='jsd', verbose=F)@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','Axis1','Axis2'))
  expect_equal(unname(unlist(lapply(dt, class))), c('character','numeric','numeric'))
  
  # With NAs
  nNAs <- 20
  df[sample(1:nrow(df), size=nNAs, replace = F), 2] <- NA
  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID',
              imputeZero = FALSE,
              removeEmptyRecords = FALSE)

  expect_error(betaDiv(data, method='jsd', verbose=F))  # all three methods err

  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID')

  dt <- betaDiv(data, method='bray', verbose=F)@data
  expect_equal(nrow(dt), nrow(df))
  expect_s3_class(dt, 'data.table')
  expect_equal(names(dt), c('SampleID','Axis1','Axis2'))
  expect_equal(unname(unlist(lapply(dt, class))), c('character','numeric','numeric'))
  expect_true(sum(dt > 0) > 100)   # ensure output is not all 0
  
})

test_that("betaDiv returns a data.table with the correct attributes" , {
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU
  
  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              recordIdColumn = 'entity.SampleID')
  
  result <- betaDiv(data, method='bray', verbose=F)
  expect_equal(result@parameters, 'method = bray')
  expect_equal(inherits(result@computedVariableMetadata, "VariableMetadataList"), TRUE)
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@variableId, 'Axis1')
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@entityId, 'entity')
  expect_equal(result@computedVariableMetadata[[1]]@displayName, 'Axis1 15.3%')
  
  result <- betaDiv(data, method='jaccard', verbose=F)
  expect_equal(result@parameters, 'method = jaccard')
  expect_equal(inherits(result@computedVariableMetadata, "VariableMetadataList"), TRUE)
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@variableId, 'Axis1')
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@entityId, 'entity')
  expect_equal(result@computedVariableMetadata[[1]]@displayName, 'Axis1 10.0%')
  
  result <- betaDiv(data, method='jsd', verbose=F)
  expect_equal(result@parameters, 'method = jsd')
  expect_equal(inherits(result@computedVariableMetadata, "VariableMetadataList"), TRUE)
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@variableId, 'Axis1')
  expect_equal(result@computedVariableMetadata[[1]]@variableSpec@entityId, 'entity')
  expect_equal(result@computedVariableMetadata[[1]]@displayName, 'Axis1 25.2%')

})