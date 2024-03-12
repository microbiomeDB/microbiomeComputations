# Tests for differential abundance methods

test_that('differentialAbundance returns a correctly formatted data.table', {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU
  
  counts <- round(df[, -c("entity.SampleID")]*1000) # make into "counts"
  counts[ ,entity.SampleID:= df$entity.SampleID]
  nSamples <- dim(df)[1]
  testSampleMetadata <- data.frame(list(
    "entity.SampleID" = df[["entity.SampleID"]],
    "entity.binA" = rep(c("binA_a", "binA_b"), nSamples/2, replace=T),
    "entity.cat3" = rep(paste0("cat3_", letters[1:3]), nSamples/3, replace=T),
    "entity.cat4" = rep(paste0("cat4_", letters[1:4]), nSamples/4, replace=T),
    "entity.contA" = rnorm(nSamples, sd=5),
    "entity.dateA" = sample(seq(as.Date('1988/01/01'), as.Date('2000/01/01'), by="day"), nSamples)
    ))


  testData <- microbiomeData::AbsoluteAbundanceData(
              data = counts,
              sampleMetadata = SampleMetadata(
                data = testSampleMetadata,
                recordIdColumn = "entity.SampleID"
              ),
              recordIdColumn = 'entity.SampleID')



  # A Binary comparator variable
  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'binA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="BINARY")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_b"
                              ))
                            )
                          )
  )

  result <- differentialAbundance(testData, comparator=comparatorVariable, method='DESeq', verbose=F)
  expect_equal(length(result@droppedColumns), 182)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))
  expect_true(all(!is.na(stats[, c('effectSize', 'pValue', 'pointID')])))


  # When defined groups end up subsetting the incoming data
    comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'cat4',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CATEGORICAL")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="cat4_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="cat4_b"
                              ))
                            )
                          )
  )
  result <- differentialAbundance(testData, comparator=comparatorVariable, method='DESeq', verbose=F)
  expect_equal(length(result@droppedColumns), 407)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  expect_equal(sum(testSampleMetadata$entity.cat4 %in% c('cat4_a','cat4_b')), nrow(dt))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))
  expect_true(all(!is.na(stats[, c('effectSize', 'pValue', 'pointID')])))


  # With a continuous variable
  bin1 <- veupathUtils::Bin(binStart='2', binEnd='3', binLabel="[2, 3)")
  bin2 <- veupathUtils::Bin(binStart='3', binEnd='4', binLabel="[3, 4)")
  bin3 <- veupathUtils::Bin(binStart='4', binEnd='5', binLabel="[4, 5)")
  bin4 <- veupathUtils::Bin(binStart='5', binEnd='6', binLabel="[5, 6)")

  groupABins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  groupBBins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin3, bin4)))

  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'contA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CONTINUOUS")
                          ),
                          groupA = groupABins,
                          groupB = groupBBins
  )

  result <- differentialAbundance(testData, comparator=comparatorVariable, method='DESeq', verbose=F)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  expect_equal(nrow(dt), sum((testSampleMetadata[['entity.contA']] >= 2) * (testSampleMetadata[['entity.contA']] < 6)))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))

  ## With dates
  bin1 <- Bin(binStart=as.Date('1989-01-01'), binEnd=as.Date('1990-01-01'), binLabel='1989')
  bin2 <- Bin(binStart=as.Date('1990-01-01'), binEnd=as.Date('1991-01-01'), binLabel='1990')
  bin3 <- Bin(binStart=as.Date('1991-01-01'), binEnd=as.Date('1992-01-01'), binLabel='1991')
  bin4 <- Bin(binStart=as.Date('1992-01-01'), binEnd=as.Date('1993-01-01'), binLabel='1992')
  groupABins <- BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  groupBBins <- BinList(S4Vectors::SimpleList(c(bin3, bin4)))

  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'dateA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CONTINUOUS")
                          ),
                          groupA = groupABins,
                          groupB = groupBBins
  )

  result <- differentialAbundance(testData, comparator=comparatorVariable, method='DESeq', verbose=F)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  expect_equal(nrow(dt), sum((testSampleMetadata[['entity.dateA']] >= as.Date('1989-01-01')) * (testSampleMetadata[['entity.dateA']] < as.Date('1993-01-01'))))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))

})

test_that("differentialAbundance can handle messy inputs", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  counts <- round(df[, -c("entity.SampleID")]*1000) # make into "counts"
  counts[ ,entity.SampleID:= df$entity.SampleID]
  nSamples <- dim(df)[1]
  testSampleMetadataMessy <- data.frame(list(
    "entity.SampleID" = df[["entity.SampleID"]],
    "entity.binA" = rep(c("binA_a", "binA_b"), nSamples/2, replace=T),
    "entity.cat3" = rep(paste0("cat3_", letters[1:3]), nSamples/3, replace=T),
    "entity.cat4" = rep(paste0("cat4_", letters[1:4]), nSamples/4, replace=T),
    "entity.contA" = rnorm(nSamples, sd=5),
    "entity.dateA" = sample(seq(as.Date('1988/01/01'), as.Date('2000/01/01'), by="day"), nSamples)
    ))
  testSampleMetadataMessy$entity.contA[sample(1:nSamples, 50)] <- NA
  testSampleMetadataMessy$entity.cat4[sample(1:nSamples, 50)] <- NA


  testDataMessy <- microbiomeData::AbsoluteAbundanceData(
              data = counts,
              sampleMetadata = SampleMetadata(
                data = testSampleMetadataMessy,
                recordIdColumn = "entity.SampleID"
              ),
              recordIdColumn = 'entity.SampleID')


  # With only some comparisonVariable values found in the metadata
  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'cat4',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CATEGORICAL")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="cat4_a"
                              ), veupathUtils::Bin(
                                binLabel="cat4_c"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="cat4_b"
                              ), veupathUtils::Bin(
                                binLabel="test"
                              ))
                            )
                          )
  )

  result <- differentialAbundance(testDataMessy, comparator=comparatorVariable, method='DESeq', verbose=F)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  expect_equal(sum(testSampleMetadataMessy$entity.cat4 %in% c('cat4_a','cat4_b','cat4_c')), nrow(dt))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))
  expect_true(all(!is.na(stats[, c('effectSize', 'pValue', 'pointID')])))


  # With a continuous variable that has NAs
  bin1 <- veupathUtils::Bin(binStart='2', binEnd='3', binLabel="[2, 3)")
  bin2 <- veupathUtils::Bin(binStart='3', binEnd='4', binLabel="[3, 4)")
  bin3 <- veupathUtils::Bin(binStart='4', binEnd='5', binLabel="[4, 5)")
  bin4 <- veupathUtils::Bin(binStart='5', binEnd='6', binLabel="[5, 6)")

  groupABins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  groupBBins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin3, bin4)))

  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'contA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CONTINUOUS")
                          ),
                          groupA = groupABins,
                          groupB = groupBBins
  )

  result <- differentialAbundance(testDataMessy, comparator=comparatorVariable, method='DESeq', verbose=F)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  expect_equal(nrow(dt), sum((testSampleMetadataMessy[['entity.contA']] >= 2) * (testSampleMetadataMessy[['entity.contA']] < 6), na.rm=T))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))


  # With a categorical variable that has NAs
  bin1 <- veupathUtils::Bin(binLabel="cat4_a")
  bin2 <- veupathUtils::Bin(binLabel="cat4_b")
  bin3 <- veupathUtils::Bin(binLabel="cat4_c")

  groupABins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  groupBBins <- veupathUtils::BinList(S4Vectors::SimpleList(c(bin3)))

  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'cat4',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CATEGORICAL")
                          ),
                          groupA = groupABins,
                          groupB = groupBBins
  )
  result <- differentialAbundance(testDataMessy, comparator=comparatorVariable, method='DESeq', verbose=T)
  dt <- result@data
  expect_equal(names(dt), c('SampleID'))
  expect_s3_class(dt, 'data.table')
  expect_equal(nrow(dt), sum(testSampleMetadataMessy$entity.cat4 %in% c('cat4_a','cat4_b','cat4_c')))
  stats <- result@statistics@statistics
  expect_s3_class(stats, 'data.frame')
  expect_equal(result@statistics@effectSizeLabel, 'log2(Fold Change)')
  expect_equal(names(stats), c('effectSize','pValue','adjustedPValue','pointID'))
  expect_equal(unname(unlist(lapply(stats, class))), c('numeric','numeric','numeric','character'))


})


test_that("differentialAbundance returns a ComputeResult with the correct slots" , {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  counts <- round(df[, -c("entity.SampleID")]*1000) # make into "counts"
  counts[ ,entity.SampleID:= df$entity.SampleID]
  nSamples <- dim(df)[1]
  sampleMetadata <- SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.binA" = sample(c("binA_a", "binA_b"), nSamples, replace=T),
      "entity.cat2" = sample(c("cat2_a", "cat2_b"), nSamples, replace=T),
      "entity.cat3" = sample(paste0("cat3_", letters[1:3]), nSamples, replace=T),
      "entity.cat4" = sample(paste0("cat4_", letters[1:4]), nSamples, replace=T)
      )),
    recordIdColumn = "entity.SampleID"
  )


  testData <- microbiomeData::AbsoluteAbundanceData(
              data = counts,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'binA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="BINARY")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_b"
                              ))
                            )
                          )
  )

  result <- differentialAbundance(testData, comparator=comparatorVariable, method='DESeq', verbose=F)
  expect_equal(result@parameters, 'recordIdColumn = entity.SampleID, comparatorColName = entity.binA, method = DESeq, groupA =binA_a, groupB = binA_b')
  expect_equal(result@recordIdColumn, 'entity.SampleID')
  expect_equal(class(result@droppedColumns), 'character')
})

test_that("differentialAbundance fails with improper inputs", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  counts <- round(df[, -c("entity.SampleID")]*1000) # make into "counts"
  counts[ ,entity.SampleID:= df$entity.SampleID]
  nSamples <- dim(df)[1]
  sampleMetadata <- SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.binA" = sample(c("binA_a", "binA_b"), nSamples, replace=T),
      "entity.cat2" = sample(c("cat2_a", "cat2_b"), nSamples, replace=T),
      "entity.cat3" = sample(paste0("cat3_", letters[1:3]), nSamples, replace=T),
      "entity.cat4" = sample(paste0("cat4_", letters[1:4]), nSamples, replace=T),
      "entity.contA" = rnorm(nSamples, sd=5)
      )),
    recordIdColumn = "entity.SampleID"
  )


  testData <- microbiomeData::AbsoluteAbundanceData(
              data = counts,
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')


  
  # Fail when bins in Group A and Group B overlap
  bin1 <- veupathUtils::Bin(binStart=2, binEnd=3, binLabel="[2, 3)")
  bin2 <- veupathUtils::Bin(binStart=3, binEnd=4, binLabel="[3, 4)")
  bin3 <- veupathUtils::Bin(binStart=3, binEnd=5, binLabel="[3, 5)")
  bin4 <- veupathUtils::Bin(binStart=5, binEnd=6, binLabel="[5, 6)")
  groupABins <- BinList(S4Vectors::SimpleList(c(bin1, bin2)))
  groupBBins <- BinList(S4Vectors::SimpleList(c(bin3, bin4)))
  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'contA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CONTINUOUS")
                          ),
                          groupA = groupABins,
                          groupB = groupBBins
  )

  expect_error(differentialAbundance(testData, comparator=comparisonVariable, method='DESeq', verbose=F))

})

test_that("differentialAbundance catches deseq errors", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  counts <- round(df[, -c("entity.SampleID")]*1000) # make into "counts"
  counts[ ,entity.SampleID:= df$entity.SampleID]
  nSamples <- dim(df)[1]
  sampleMetadata <- SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.binA" = rep(c("binA_a", "binA_b"), nSamples/2, replace=T)
      )),
    recordIdColumn ="entity.SampleID"
  )

  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'binA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="BINARY")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_b"
                              ))
                            )
                          )
  )

  # Use only a few taxa
  testData <- microbiomeData::AbsoluteAbundanceData(
              data = counts[, c("entity.SampleID","entity.1174-901-12","entity.A2")],
              sampleMetadata = sampleMetadata,
              recordIdColumn = 'entity.SampleID')

  expect_error(differentialAbundance(testData, comparator=comparisonVariable, method='DESeq', verbose=T))


})

test_that("differentialAbundance method Maaslin does stuff",{
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  counts <- round(df[, -c("entity.SampleID")]*1000)
  counts[ ,entity.SampleID:= df$entity.SampleID]
  nSamples <- dim(df)[1]
  testSampleMetadata <- SampleMetadata(
    data = data.frame(list(
    "entity.SampleID" = df[["entity.SampleID"]],
    "entity.binA" = rep(c("binA_a", "binA_b"), nSamples/2, replace=T),
    "entity.cat3" = rep(paste0("cat3_", letters[1:3]), nSamples/3, replace=T),
    "entity.cat4" = rep(paste0("cat4_", letters[1:4]), nSamples/4, replace=T),
    "entity.contA" = rnorm(nSamples, sd=5)
    )),
    recordIdColumn ="entity.SampleID"
  )


  testCountsData <- microbiomeData::AbsoluteAbundanceData(
              data = counts,
              sampleMetadata = testSampleMetadata,
              recordIdColumn = 'entity.SampleID')

  testData <- microbiomeData::AbundanceData(
    data = df,
    sampleMetadata = testSampleMetadata,
    recordIdColumn = 'entity.SampleID'
  )

  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'cat4',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CATEGORICAL")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="cat4_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="cat4_b"
                              ))
                            )
                          )
  )

  result <- differentialAbundance(testData,
              comparator = comparatorVariable,
              method='Maaslin',
              verbose=F)
  dt <- result@data
  stats <- result@statistics@statistics


  resultCounts <- differentialAbundance(testCountsData,
              comparator = comparatorVariable,
              method='Maaslin',
              verbose=F)
  dtCounts <- result@data
  statsCounts <- result@statistics@statistics

  expect_equal(dt, dtCounts)
  expect_equal(result@statistics@effectSizeLabel, 'Model Coefficient (Effect Size)')
  expect_true(length(stats$pointID) > 0)
  expect_true(length(statsCounts$pointID) > 0)
  expect_equal(stats, statsCounts)
})

test_that("toJSON for DifferentialAbundanceResult works",{
  
  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  nSamples <- dim(df)[1]
  df$entity.wowtaxa <- rep(c(0.01, 0.99), nSamples/2, replace=T) # will 'wow' us with its significance
  nSamples <- dim(df)[1]
  testSampleMetadata <- data.frame(list(
    "entity.SampleID" = df[["entity.SampleID"]],
    "entity.binA" = rep(c("binA_a", "binA_b"), nSamples/2, replace=T)
    ))

  testData <- microbiomeData::AbundanceData(
    data = df,
    sampleMetadata = SampleMetadata(
                      data = testSampleMetadata,
                      recordIdColumn = "entity.SampleID"
    ),
    recordIdColumn = 'entity.SampleID'
  )

  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'binA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="CATEGORICAL")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_b"
                              ))
                            )
                          )
  )

  result <- differentialAbundance(testData,
              comparator = comparatorVariable,
              method='Maaslin',
              verbose=F)
  stats <- result@statistics
  jsonList <- jsonlite::fromJSON(toJSON(result@statistics))

  expect_true(all(c('effectSizeLabel', 'statistics', 'pValueFloor', 'adjustedPValueFloor') %in% names(jsonList)))
  expect_true(all(c('effectSize', 'pValue', 'adjustedPValue', 'pointID') %in% names(jsonList$statistics)))
  expect_true(is.character(jsonList$statistics$effectSize))
  expect_true(is.character(jsonList$statistics$pValue))
  expect_true(is.character(jsonList$statistics$adjustedPValue))
  expect_true(is.character(jsonList$pValueFloor))
  expect_true(is.character(jsonList$adjustedPValueFloor))
})

test_that("The smallest pvalue we can get is our p value floor", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU

  counts <- round(df[, -c("entity.SampleID")]*1000) # make into "counts"
  counts[ ,entity.SampleID:= df$entity.SampleID]
  nSamples <- dim(df)[1]
  counts$entity.wowtaxa <- rep(c(1, 100), nSamples/2, replace=T) # will 'wow' us with its significance
  nSamples <- dim(df)[1]
  testSampleMetadata <- data.frame(list(
    "entity.SampleID" = df[["entity.SampleID"]],
    "entity.binA" = rep(c("binA_a", "binA_b"), nSamples/2, replace=T)
    ))

  testData <- microbiomeData::AbsoluteAbundanceData(
    data = counts,
    sampleMetadata = SampleMetadata(
      data = testSampleMetadata,
      recordIdColumn = "entity.SampleID"
    ),
    recordIdColumn = 'entity.SampleID'
  )

  # A Binary comparator variable
  comparatorVariable <- microbiomeComputations::Comparator(
                          variable = veupathUtils::VariableMetadata(
                            variableSpec = VariableSpec(
                              variableId = 'binA',
                              entityId = 'entity'
                            ),
                            dataShape = veupathUtils::DataShape(value="BINARY")
                          ),
                          groupA = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_a"
                              ))
                            )
                          ),
                          groupB = veupathUtils::BinList(
                            S4Vectors::SimpleList(
                              c(veupathUtils::Bin(
                                binLabel="binA_b"
                              ))
                            )
                          )
  )

  # Try with different p value floors
  result <- differentialAbundance(testData, comparator=comparatorVariable, method='DESeq', pValueFloor = 0, verbose=F)
  expect_equal(min(result@statistics@statistics$pValue), 0)
  expect_equal(min(result@statistics@statistics$adjustedPValue, na.rm=T), 0) # Confirmed NAs are for pvalue=1

  result <- differentialAbundance(testData, comparator=comparatorVariable, method='DESeq', pValueFloor = P_VALUE_FLOOR, verbose=F)
  expect_equal(min(result@statistics@statistics$pValue), P_VALUE_FLOOR)
  expect_equal(min(result@statistics@statistics$adjustedPValue, na.rm=T), result@statistics@adjustedPValueFloor) # Confirmed NAs are for pvalue=1



  # Repeat with Maaslin
  result <- differentialAbundance(testData, comparator=comparatorVariable, method='Maaslin', pValueFloor = 0, verbose=F)
  expect_equal(min(result@statistics@statistics$pValue), 0)
  expect_equal(min(result@statistics@statistics$adjustedPValue), 0)

  result <- differentialAbundance(testData, comparator=comparatorVariable, method='Maaslin', pValueFloor = P_VALUE_FLOOR, verbose=F)
  expect_equal(min(result@statistics@statistics$pValue), P_VALUE_FLOOR)
  expect_equal(min(result@statistics@statistics$adjustedPValue), result@statistics@adjustedPValueFloor)


})

test_that("differentialAbundance fails if comparator has one value", {

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU
  
  sampleMetadata <- SampleMetadata(
    data = data.frame(list(
      "entity.SampleID" = df[["entity.SampleID"]],
      "entity.binA" = rep(c("binA"), nrow(df))
    )),
    recordIdColumn ="entity.SampleID"
  )

  testData <- microbiomeData::AbundanceData(
    data = df,
    sampleMetadata = sampleMetadata,
    recordIdColumn = 'entity.SampleID'
  )

  comparatorVariable <- microbiomeComputations::Comparator(
    variable = veupathUtils::VariableMetadata(
      variableSpec = VariableSpec(
        variableId = 'binA',
        entityId = 'entity'
      ),
      dataShape = veupathUtils::DataShape(value="BINARY")
    ),
    groupA = veupathUtils::BinList(S4Vectors::SimpleList(c(veupathUtils::Bin(binLabel="binA")))),
    groupB = veupathUtils::BinList(S4Vectors::SimpleList(c(veupathUtils::Bin(binLabel="binB"))))
  )

  expect_error(differentialAbundance(testData, comparator=comparatorVariable, method='DESeq', verbose=F))
  expect_error(differentialAbundance(testData, comparator=comparatorVariable, method='Maaslin', verbose=F))
})