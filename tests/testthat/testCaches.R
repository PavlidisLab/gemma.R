
test_that('caches work',{
    forget_gemma_memoised()

  timeNonMemo =
  microbenchmark::microbenchmark(
    search_datasets("bipolar", limit = 100, taxon = "human",
                   memoise = FALSE),
    times = 1,unit = 'ms') %>% summary

  result = search_datasets("bipolar", limit = 100, taxon = "human", memoise = TRUE)

  timeMemo =
    microbenchmark::microbenchmark(
        search_datasets("bipolar", limit = 100, taxon = "human",
                     memoise = TRUE),
      times = 1,unit = 'ms') %>% summary

  forget_gemma_memoised()

  timeForgot =
    microbenchmark::microbenchmark(
        search_datasets("bipolar", limit = 100, taxon = "human",
                     memoise = TRUE),
      times = 1,unit = 'ms') %>% summary

  testthat::expect_lt(timeMemo$mean,timeForgot$mean)
  testthat::expect_lt(timeMemo$mean,timeNonMemo$mean)

  # testing caches for high level functions
  skip_on_ci()
  skip_on_bioc()

  test_gse = 'GSE2018'

  timeNonMemo = microbenchmark::microbenchmark(
      get_dataset_object(test_gse, memoised = FALSE),
    times = 1, unit = 'ms') %>% summary

  result = get_dataset_object(test_gse, memoised = TRUE)

  timeMemo = microbenchmark::microbenchmark(
      get_dataset_object(test_gse, memoised = TRUE),
    times = 1, unit = 'ms') %>% summary

  options(gemma.memoised = TRUE)

  optionMemo = microbenchmark::microbenchmark(
      get_dataset_object(test_gse),
    times = 1, unit = 'ms') %>% summary

  forget_gemma_memoised()

  timeForgot = microbenchmark::microbenchmark(
      get_dataset_object(test_gse, memoised = TRUE),
    times = 1, unit = 'ms') %>% summary

  testthat::expect_lt(timeMemo$mean,timeForgot$mean)
  testthat::expect_lt(timeMemo$mean,timeNonMemo$mean)
  testthat::expect_lt(optionMemo$mean,timeNonMemo$mean)

  # test custom caches
  non_default = tempfile()
  options(gemma.cache = non_default)
  forget_gemma_memoised()

  testthat::expect_true(length(list.files(non_default)) == 0)
  result = get_dataset_object(test_gse, memoised = TRUE)

  testthat::expect_true(length(list.files(non_default)) >= 0)
  nonDefaultPath = microbenchmark::microbenchmark(
      get_dataset_object(test_gse,memoised = TRUE),
    times = 1, unit = 'ms') %>% summary

  testthat::expect_lt(nonDefaultPath$mean,timeNonMemo$mean)
  forget_gemma_memoised()
  testthat::expect_true(length(list.files(non_default)) == 0)

  expect_warning(get_platforms_by_ids(1,file = tempfile(),memoised = TRUE),'Saving to files is not supported')

})
# don't forget to unmemoise at the end
forget_gemma_memoised()
options(gemma.memoised = FALSE)
options(gemma.cache = NULL)


test_that('in memory caches work',{
    options(gemma.cache = 'cache_in_memory')
    forget_gemma_memoised()
    
    timeNonMemo =
        microbenchmark::microbenchmark(
            search_datasets("bipolar", limit = 100, taxon = "human",
                            memoise = FALSE),
            times = 1,unit = 'ms') %>% summary
    
    result = search_datasets("bipolar", limit = 100, taxon = "human", memoise = TRUE)
    
    timeMemo =
        microbenchmark::microbenchmark(
            search_datasets("bipolar", limit = 100, taxon = "human",
                            memoise = TRUE),
            times = 1,unit = 'ms') %>% summary
    
    forget_gemma_memoised()
    
    timeForgot =
        microbenchmark::microbenchmark(
            search_datasets("bipolar", limit = 100, taxon = "human",
                            memoise = TRUE),
            times = 1,unit = 'ms') %>% summary
    
    testthat::expect_lt(timeMemo$mean,timeForgot$mean)
    testthat::expect_lt(timeMemo$mean,timeNonMemo$mean)
})
forget_gemma_memoised()
options(gemma.cache = NULL)
