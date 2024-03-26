# Tests for mbio specific correlation methods

test_that('correlation returns an appropriately structured result for abundance data vs metadata', {
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

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


  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')
  
  ## All numeric sample variables
  result <- correlation(data, method='pearson', verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 36) # Should be number of taxa * number of metadata vars, less pruned taxa
  #expect_equal(as.character(unique(statsData$data1)), names(testOTU)[2:length(names(testOTU))])
  #expect_equal(as.character(unique(statsData$data2)), c('entity.contA', 'entity.contB', 'entity.contC'))
  expect_true(all(!is.na(statsData)))


  ## With method = spearman
  result <- correlation(data, method='spearman', verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 36) # Should be number of taxa * number of metadata vars, less pruned taxa
  #expect_equal(as.character(unique(statsData$data1)), names(testOTU)[2:length(names(testOTU))])
  #expect_equal(as.character(unique(statsData$data2)), c('entity.contA', 'entity.contB', 'entity.contC'))
  expect_true(all(!is.na(statsData)))


  # with a single metadata var
  sampleMetadata <- veupathUtils::SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.contA" = rnorm(nSamples)
      )),
    recordIdColumn = "entity.SampleID"
  )

  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  result <- correlation(data, method='spearman', verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 12) # Should be number of taxa * number of metadata vars, less pruned taxa
  #expect_equal(as.character(unique(statsData$data1)), names(testOTU)[2:length(names(testOTU))])
  #expect_equal(as.character(unique(statsData$data2)), c('entity.contA'))
  expect_true(all(!is.na(statsData)))

  # ## With a date <3
  #   variables <- new("VariableMetadataList", SimpleList(
  #   new("VariableMetadata",
  #     variableSpec = new("VariableSpec", variableId = 'contA', entityId = 'entity'),
  #     dataType = new("DataType", value = 'NUMBER'),
  #     dataShape = new("DataShape", value = 'CONTINUOUS')),
  #   new("VariableMetadata",
  #     variableClass = new("VariableClass", value = 'native'),
  #     variableSpec = new("VariableSpec", variableId = 'dateA', entityId = 'entity'),
  #     dataType = new("DataType", value = 'DATE'),
  #     dataShape = new("DataShape", value = 'CONTINUOUS'))
  # ))


})


test_that("correlation returns an appropriately structured result for metadata vs metadata", {
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

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

  result <- selfCorrelation(sampleMetadata, method='pearson', verbose = FALSE)
  expect_equal(result@statistics@data1Metadata, 'sampleMetadata')
  expect_equal(result@statistics@data2Metadata, 'sampleMetadata')

  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  nNumericCols <- length(veupathUtils::findNumericCols(sampleMetadata@data))
  expect_equal(nrow(statsData), ((nNumericCols * nNumericCols) - 3)/2) # Should be number of number of numeric vars * number of numeric vars
  expect_equal(as.character(unique(statsData$data1)), c('entity.contA', 'entity.contB'))
  expect_equal(as.character(unique(statsData$data2)), c('entity.contB', 'entity.contC'))
  expect_true(all(!is.na(statsData)))

  ## With method = spearman
  result <- selfCorrelation(sampleMetadata, method='spearman', verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), ((nNumericCols * nNumericCols) - 3)/2) # Should be number of taxa * number of metadata vars
  expect_equal(as.character(unique(statsData$data1)), c('entity.contA', 'entity.contB'))
  expect_equal(as.character(unique(statsData$data2)), c('entity.contB', 'entity.contC'))
  expect_true(all(!is.na(statsData)))

})

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

  data <- microbiomeData::AbundanceData(
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

test_that("correlation returns an appropriately structured result for assay vs assay", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df1 <- testOTU[, 1:500]
  df2 <- testOTU[,c(1,501:909)]
  nSamples <- dim(df1)[1]
  sampleMetadata <- veupathUtils::SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df1[["entity.SampleID"]],
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
      # "entity.dateA" = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), nSamples)
      )),
    recordIdColumn = "entity.SampleID"
  )

  data1 <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df1,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  data2 <- microbiomeData::AbundanceData(
              name = 'testing2',
              data = df2,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')
  
  ## All numeric sample variables
  result <- correlation(data1, data2, method='pearson', verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 32) # Should be number of taxa in df1 * number of taxa in df2, less pruned taxa
  #expect_equal(as.character(unique(statsData$data1)), names(df1)[2:length(names(df1))])
  #expect_equal(as.character(unique(statsData$data2)), names(df2)[2:length(names(df2))])
  expect_true(all(!is.na(statsData)))


  ## With method = spearman
  result <- correlation(data1, data2, method='spearman', verbose = FALSE)
  # Check stats (all correlation outputs)
  statsData <- result@statistics@statistics
  expect_s3_class(statsData, 'data.frame')
  expect_equal(names(statsData), c('data1','data2','correlationCoef','pValue'))
  expect_equal(nrow(statsData), 32) # Should be number of taxa in df1 * number of taxa in df2, less pruned taxa
  #expect_equal(as.character(unique(statsData$data1)), names(df1)[2:length(names(df1))])
  #expect_equal(as.character(unique(statsData$data2)), names(df2)[2:length(names(df2))])
  #expect_true(all(!is.na(statsData)))
})

test_that("correlation returns a ComputeResult with the correct slots" , {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

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


  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  ## Pearson
  result <- correlation(data, method='pearson', verbose = FALSE)
  expect_equal(result@parameters, 'method = pearson')
  expect_equal(result@recordIdColumn, 'entity.SampleID')
  expect_equal(result@statistics@data1Metadata, 'assay')
  expect_equal(result@statistics@data2Metadata, 'sampleMetadata')

  ## With spearman
  result <- correlation(data, method='spearman', verbose = FALSE)
  expect_equal(result@parameters, 'method = spearman')
  expect_equal(result@recordIdColumn, 'entity.SampleID')
  expect_equal(result@statistics@data1Metadata, 'assay')
  expect_equal(result@statistics@data2Metadata, 'sampleMetadata')

})

test_that("correlation fails with improper inputs", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU
  
  counts <- round(df[, -c("entity.SampleID")]*1000) # make into "counts"
  counts[ ,entity.SampleID:= df$entity.SampleID]
  nSamples <- dim(df)[1]
  sampleMetadata <- veupathUtils::SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.binA" = sample(c("binA_a", "binA_b"), nSamples, replace=T),
      "entity.cat2" = sample(c("cat2_a", "cat2_b"), nSamples, replace=T),
      "entity.cat3" = sample(paste0("cat3_", letters[1:3]), nSamples, replace=T),
      "entity.cat4" = sample(paste0("cat4_", letters[1:4]), nSamples, replace=T)
      )),
    recordIdColumn = "entity.SampleID"
  )


  data <- microbiomeData::AbsoluteAbundanceData(
              name = 'testing',
              data = counts,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  data@sampleMetadata <- sampleMetadata

  # Fail when we send in only categorical metadata
  expect_error(correlation(data, verbose=F))

  # Fail when sample metadata is missing a sample
  sampleMetadata@data <- sampleMetadata@data[-1, ]
  expect_error(corrleation(data, verbose=F))
})

test_that("toJSON works as expected for the CorrelationResult class", {
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU
  
  nSamples <- dim(df)[1]
  sampleMetadata <- veupathUtils::SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.contA" = rnorm(nSamples),
      "entity.contB" = rnorm(nSamples),
      "entity.contC" = rnorm(nSamples)
      )),
    recordIdColumn = "entity.SampleID"
  )


  data <- microbiomeData::AbundanceData(
              name = 'testing',
              data = df,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  result <- correlation(data, method='pearson', verbose = FALSE)
  jsonList <- jsonlite::fromJSON(toJSON(result@statistics))
  expect_equal(names(jsonList), c('data1Metadata', 'data2Metadata', 'statistics'))
  expect_equal(class(jsonList$data1Metadata), "character")
  expect_equal(class(jsonList$data2Metadata), "character")
  expect_equal(names(jsonList$statistics), c('data1', 'data2', 'correlationCoef', 'pValue'))
  expect_equal(unname(unlist(lapply(jsonList$statistics, class))), c('character', 'character', 'character', 'character'))
})
