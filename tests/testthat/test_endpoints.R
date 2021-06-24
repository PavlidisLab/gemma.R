library(testthat)

test_that('getDatasets queries work', {
  expect_type(getDatasets('GSE2018'), 'list')
  expect_type(getDatasets('GSE2018', raw = TRUE), 'list')
  # TODO: check implementation to test filters, limit, offset, sort
})


