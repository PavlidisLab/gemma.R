test_that('persistent caches work',{
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
  testthat::expect_lt(timeMemo$mean,timeForgot$mean)

})
