test_that('AbsoluteAbundanceData validation works', {

  # Most tests are handled by the parent class.

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU
  counts <- round(testOTU[, -c("entity.SampleID"), with=F]*1000) # make into "counts"
  counts[ ,entity.SampleID:= testOTU$entity.SampleID]

  # Expect error when input data is not integers (not rounded)
  expect_error(microbiomeData::AbsoluteAbundanceData(
              name = 'test',
              data = df,
              recordIdColumn = c('entity.SampleID')))

  
  testing <- microbiomeData::AbsoluteAbundanceData(
              name = 'test',
              data = counts,
              recordIdColumn = c('entity.SampleID'))

  expect_true(inherits(testing, 'AbundanceData'))
  expect_true(inherits(testing, 'AbsoluteAbundanceData'))
  expect_equal(slotNames(testing), c('sampleMetadata', 'name', 'data', 'recordIdColumn', 'ancestorIdColumns', 'imputeZero', 'removeEmptyRecords'))
  expect_equal(nrow(testing@data), 288)
  expect_equal(ncol(testing@data), 909)
  expect_equal(testing@recordIdColumn, 'entity.SampleID')
  expect_equal(testing@ancestorIdColumns, character(0))
  expect_true(testing@imputeZero)
})