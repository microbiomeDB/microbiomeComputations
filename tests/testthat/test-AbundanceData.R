test_that('AbundanceData validation works', {
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU
  df[,1][10] <- -10
  expect_error(microbiomeData::AbundanceData(
              name = 'test',
              data = df,
              recordIdColumn = c('entity.SampleID')))
})

test_that("getAbundances works", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  testing <- microbiomeData::AbundanceData(
    name = 'test',
    data = df,
    recordIdColumn = 'entity.SampleID'
  )
  
  abundances <- getAbundances(testing)
  expect_equal(nrow(abundances), nrow(df))
  expect_equal(ncol(abundances), ncol(df))

  ## remove ids
  abundances <- getAbundances(testing, includeIds = FALSE)
  expect_equal(nrow(abundances), nrow(df))
  expect_equal(ncol(abundances), ncol(df) - 1)

  ## remove empty samples
  df <- rbind(df,df[nrow(df)+1,])
  df$entity.SampleID[nrow(df)] <- 'im.a.sample'
  testing <- microbiomeData::AbundanceData(
    name = 'test',
    data = df,
    recordIdColumn = 'entity.SampleID',
    removeEmptyRecords = TRUE
  )

  abundances <- getAbundances(testing)
  expect_equal(nrow(abundances), nrow(df) -1)
  expect_equal(ncol(abundances), ncol(df))
})