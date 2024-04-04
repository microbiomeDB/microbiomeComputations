# Tests for mbio specific correlation methods

test_that("correlation returns an appropriately structured result for assay against self", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  #manually prefilter, so we can test on smaller data set w known column names
  predicate <- predicateFactory('proportionNonZero', 0.5)
  keepCols <- df[, lapply(.SD, predicate), .SDcols = colnames(df)[!(colnames(df) %in% "entity.SampleID")]]
  keepCols <- names(keepCols)[keepCols == TRUE]
  df <- df[, c("entity.SampleID", keepCols), with = FALSE]

  nSamples <- dim(df)[1]
  sampleMetadata <- veupathUtils::SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
      # "entity.dateA" = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), nSamples)
      )),
    recordIdColumn = "entity.SampleID"
  )

  data <- AbundanceData(
              name = 'testing',
              data = df,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  result <- selfCorrelation(data, method='pearson', verbose = FALSE)
  expect_equal(result@statistics@data1Metadata, 'assay')
  expect_equal(result@statistics@data2Metadata, 'assay')

  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 66) # Should be number of taxa * number of taxa
  expect_equal(as.character(unique(statsData$data1)), names(df)[2:(length(names(df))-1)])
  expect_equal(as.character(unique(statsData$data2)), names(df)[3:length(names(df))])
  expect_true(all(!is.na(statsData)))

  # method = spearman
  result <- selfCorrelation(data, method='spearman', verbose = FALSE)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 66) # Should be number of taxa * number of taxa, less pruned taxa
  expect_equal(as.character(unique(statsData$data1)), names(df)[2:(length(names(df)) - 1)])
  expect_equal(as.character(unique(statsData$data2)), names(df)[3:length(names(df))])
  expect_true(all(!is.na(statsData)))

  # method = sparcc
  result <- selfCorrelation(data, method='sparcc', verbose = FALSE)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 66) # Should be number of taxa * number of taxa
  expect_equal(as.character(unique(statsData$data1)), names(df)[2:(length(names(df)) - 1)])
  expect_equal(as.character(unique(statsData$data2)), names(df)[3:length(names(df))])
  expect_true(all(!is.na(statsData$correlationCoef))) # sparcc returns NA for pvalues sometimes
})