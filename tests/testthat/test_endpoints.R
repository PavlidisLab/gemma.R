test_that('getDatasets queries work', {
  expect_type(getDatasets('GSE2018'), 'list')
  expect_type(getDatasets('GSE2018', raw = TRUE), 'list')
  expect_equal(getDatasets(c("GSE2018", "GSE2872")) %>% nrow, 2)
  expect_equal(getDatasets(limit = 10) %>% nrow, 10)
  # Offset not working as expected
  # expect_equal(getDatasets(ofsset = 3) %>% nrow, 7)
  expect_equal(getDatasets(filter = 'curationDetails.troubled = true')$ee.troubled %>% all, TRUE)

})

test_that('getDatasets queries work', {
  expect_type(getDatasets('GSE2018'), 'list')
  expect_type(getDatasets('GSE2018', raw = TRUE), 'list')
  expect_equal(getDatasets(c("GSE2018", "GSE2872")) %>% nrow, 2)
  expect_equal(getDatasets(limit = 10) %>% nrow, 10)
  # Offset not working as expected
  # expect_equal(getDatasets(ofsset = 3) %>% nrow, 7)
  expect_equal(getDatasets(filter = 'curationDetails.troubled = true')$ee.troubled %>% all, TRUE)
  expect_equal()
})


