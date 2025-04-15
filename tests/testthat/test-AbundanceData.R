test_that('AbundanceData validation works', {
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  # Handle negatives more gracefully
  df_neg <- df
  df_neg[3, 5] <- -1
  df_neg[7, 9] <- -2
  expect_error(microbiomeComputations::AbundanceData(
              data = df_neg,
              recordIdColumn = c('entity.SampleID')))

  # Ensure funky ids do not throw off our negative value check (see issue #30)
  df[2, 1] <- "__id__"
  
  abundance_collection <- AbundanceData(
    name = 'test',
    data = df,
    recordIdColumn = 'entity.SampleID'
  )
  
  abundances <- getAbundances(abundance_collection)
  expect_equal(nrow(abundances), nrow(df))
  expect_equal(ncol(abundances), ncol(df))

})

test_that("getAbundances works", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  testing <- AbundanceData(
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
  testing <- AbundanceData(
    name = 'test',
    data = df,
    recordIdColumn = 'entity.SampleID',
    removeEmptyRecords = TRUE
  )

  abundances <- getAbundances(testing)
  expect_equal(nrow(abundances), nrow(df) -1)
  expect_equal(ncol(abundances), ncol(df))
})