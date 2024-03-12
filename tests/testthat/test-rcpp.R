# Test Rcpp functions

test_that("microbiomeComputations jsd output matches phyloseq's", {
  
  # Adaptation of running phyloseq's pairwise jsd distance
  phyloseq_jsd <- function(df) {
    results <- matrix(0, ncol(df), ncol(df))
    for (i in 1:ncol(df)) {
      for (j in 1:i) {
        x <- df[, i]
        y <- df[, j]
        # Copied from phyloseq
        
        # Function to compute Shannon-Jensen Divergence
        # x and y are the frequencies for the same p categories
        # Assumes relative abundance transformation already happened (for efficiency)
        
        # Define the mean point
        m <- (x+y)/2
        # Define each samples component
        P1 <- x*log(x/m)
        P2 <- y*log(y/m)
        # In the case of zeroes entries log is undefined, JSD is defined as zero
        P1[!is.finite(P1)] <- 0
        P2[!is.finite(P2)] <- 0
        d <- (P1+P2)/2
        results[i, j] <- sum(d, na.rm = TRUE)
        results[j, i] <- results[i, j]
      }
    }

    return(results)
  }

  testOTU_path <- testthat::test_path('testdata','testOTU.rda')
  load(testOTU_path)

  df <- testOTU
  
  sampleIdColumn <- "entity.SampleID"

  # Compute distance matrices and test for equality
  dfMat <- matrix(as.numeric(unlist(df[, -..sampleIdColumn])), nrow=NROW(df))
  phyloseq_jsd_distance <- phyloseq_jsd(t(dfMat))
  mbc_jsd_distance <- jsd(t(dfMat))

  expect_true(all.equal(phyloseq_jsd_distance, mbc_jsd_distance))
  
  
})