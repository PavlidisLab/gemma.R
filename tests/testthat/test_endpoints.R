test_that('getDatasets queries work', {
  expect_type(getDatasets('GSE2018'), 'list')
  expect_type(getDatasets('GSE2018', raw = TRUE), 'list')
  expect_equal(getDatasets(c("GSE2018", "GSE2872")) %>% nrow, 2)
  expect_equal(getDatasets(c(1, 2)) %>% nrow, 2)
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
  expect_true(lapply(searchDatasets(taxon = 'human', query = 'bipolar'), function(x){grepl('bipolar', x)}) %>%
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

test_that('datasetDEA queries work', {
  expect_type(getDatasetDEA('GSE2018'), 'list')
  expect_type(getDatasetDEA('GSE2018', raw = TRUE), 'list')
  # Offset and limit values do not seem to work in the API
  # expect_equal(getDatasetDEA(limit = 10) %>% nrow, 10)
  # expect_false(getDatasetDEA('GSE2018', offset = 3)[1,1] == getDatasetDEA('GSE2018', offset = 0)[1,1])
})

test_that('datasetSVD queries work', {
  expect_type(getDatasetSVD('GSE2018'), 'list')
  expect_type(getDatasetSVD('GSE2018', raw = TRUE), 'list')
})

test_that('datasetSVD queries work', {
  expect_type(getDatasetSVD('GSE2018'), 'list')
  expect_type(getDatasetSVD('GSE2018', raw = TRUE), 'list')
})

test_that('datasetAnnotations queries work', {
  expect_type(getDatasetSVD('GSE2018'), 'list')
  expect_type(getDatasetSVD('GSE2018', raw = TRUE), 'list')
})

test_that('getDatasetData queries work', {
  expect_type(getDatasetData('GSE2018'), 'list')
  expect_type(getDatasetData('GSE2018', raw = TRUE), 'list')
  expect_gt(getDatasetData('GSE2018') %>% nrow(), getDatasetData('GSE2018', filter = TRUE) %>% nrow())
})

test_that('datasetDesign queries work', {
  expect_type(getDatasetDesign('GSE2018'), 'list')
  expect_type(getDatasetDesign('GSE2018', raw = TRUE), 'list')
})

test_that('datasetDE queries work', {

})

