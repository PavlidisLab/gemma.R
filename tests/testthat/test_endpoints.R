test_that('getDatasets queries work', {
  expect_type(getDatasets('GSE2018'), 'list')
  expect_type(getDatasets('GSE2018', raw = TRUE), 'list')
  expect_equal(getDatasets(c("GSE2018", "GSE2872")) %>% nrow, 2)
  expect_equal(getDatasets(limit = 10) %>% nrow, 10)
  expect_false(getDatasets(offset = 3)[1,1] == getDatasets(offset = 0)[1,1])
  expect_equal(getDatasets(filter = 'curationDetails.troubled = true')$ee.troubled %>% all, TRUE)
  expect_false(getDatasets(sort = '-id')[1, 1] == getDatasets(sort = '+id')[1, 1])
})

test_that('datasetSearch queries work', {
  # TODO: Taxon should not be required
  expect_type(searchDatasets(taxon = 'human', query = 'bipolar'), 'list')
  expect_type(searchDatasets(taxon = 'human', query = 'bipolar', raw = TRUE), 'list')

  # Checks that search term is present in every queried dataset's name or description
  expect_true(lapply(searchDatasets(taxon = 'human', query = 'bipolar'), function(x){stringr::str_detect(x, 'bipolar')}) %>%
                as.data.frame() %>% mutate(term_found = (ee.Name || ee.Description)) %>%
                .$term_found %>% all())

  expect_equal(searchDatasets(taxon = 'human', query = 'bipolar', limit = 10) %>% nrow, 10)
})

test_that('datasetPlatforms queries work', {
  expect_type(getDatasetPlatforms('GSE2018'), 'list')
  expect_type(getDatasetPlatforms('GSE2018', raw = TRUE), 'list')
})

test_that('datasetSamples queries work', {
  expect_type(getDatasetSamples('GSE2018'), 'list')
  expect_type(getDatasetSamples('GSE2018', raw = TRUE), 'list')
})
