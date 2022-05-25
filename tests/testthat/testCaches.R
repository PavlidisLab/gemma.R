test_that(' caches work',{
  forgetGemmaMemoised()

  timeNonMemo =
  microbenchmark::microbenchmark(
    searchDatasets("bipolar", limit = 100, taxon = "human",
                   memoise = FALSE),
    times = 1,unit = 'ms') %>% summary

  result = searchDatasets("bipolar", limit = 100, taxon = "human", memoise = TRUE)

  timeMemo =
    microbenchmark::microbenchmark(
      searchDatasets("bipolar", limit = 100, taxon = "human",
                     memoise = TRUE),
      times = 1,unit = 'ms') %>% summary

  forgetGemmaMemoised()

  timeForgot =
    microbenchmark::microbenchmark(
      searchDatasets("bipolar", limit = 100, taxon = "human",
                     memoise = TRUE),
      times = 1,unit = 'ms') %>% summary

  testthat::expect_lt(timeMemo$mean,timeForgot$mean)
  testthat::expect_lt(timeMemo$mean,timeNonMemo$mean)

  # testing caches for high level functions

  timeNonMemo = microbenchmark::microbenchmark(
    getDataset("GSE46416", memoised = FALSE),
    times = 1, unit = 'ms') %>% summary

  result = getDataset("GSE46416", memoised = TRUE)

  timeMemo = microbenchmark::microbenchmark(
    getDataset("GSE46416", memoised = TRUE),
    times = 1, unit = 'ms') %>% summary

  forgetGemmaMemoised()

  timeForgot = microbenchmark::microbenchmark(
    getDataset("GSE46416", memoised = TRUE),
    times = 1, unit = 'ms') %>% summary

  testthat::expect_lt(timeMemo$mean,timeForgot$mean)
  testthat::expect_lt(timeMemo$mean,timeNonMemo$mean)

})
