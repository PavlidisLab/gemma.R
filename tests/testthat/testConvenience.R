test_that('getExpressionSet information is preserved', {
  expr <- getDatasetData(1, filter = TRUE)
  eset <- getExpressionSet(1, filter = TRUE)
  expect_equal(expr %>% nrow, eset %>% nrow %>% unname)
})
