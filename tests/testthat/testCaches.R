test_that('caches work',{
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
  skip_on_ci()
  skip_on_bioc()

  test_gse = 'GSE2018'

  timeNonMemo = microbenchmark::microbenchmark(
    getDataset(test_gse, memoised = FALSE),
    times = 1, unit = 'ms') %>% summary

  result = getDataset(test_gse, memoised = TRUE)

  timeMemo = microbenchmark::microbenchmark(
    getDataset(test_gse, memoised = TRUE),
    times = 1, unit = 'ms') %>% summary

  options(gemma.memoised = TRUE)

  optionMemo = microbenchmark::microbenchmark(
    getDataset(test_gse),
    times = 1, unit = 'ms') %>% summary

  forgetGemmaMemoised()

  timeForgot = microbenchmark::microbenchmark(
    getDataset(test_gse, memoised = TRUE),
    times = 1, unit = 'ms') %>% summary

  testthat::expect_lt(timeMemo$mean,timeForgot$mean)
  testthat::expect_lt(timeMemo$mean,timeNonMemo$mean)
  testthat::expect_lt(optionMemo$mean,timeNonMemo$mean)

  # test custom caches
  non_default = tempfile()
  options(gemma.cache = non_default)
  forgetGemmaMemoised()

  testthat::expect_true(length(list.files(non_default)) == 0)
  result = getDataset(test_gse, memoised = TRUE)

  testthat::expect_true(length(list.files(non_default)) >= 0)
  nonDefaultPath = microbenchmark::microbenchmark(
    getDataset(test_gse,memoised = TRUE),
    times = 1, unit = 'ms') %>% summary

  testthat::expect_lt(nonDefaultPath$mean,timeNonMemo$mean)

  forgetGemmaMemoised()
  testthat::expect_true(length(list.files(non_default)) == 0)

  expect_warning(getPlatformsInfo(1,file = file,memoised = TRUE),'Saving to files is not supported')

  # don't forget to unmemoise at the end
  options(gemma.memoised = FALSE)


})
