test_that('getDatasets queries work', {
  dat <- getDatasets(1)
  raw <- getDatasets(1, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(ee.ShortName, ee.ID, ee.Description)] %>% paste0(collapse = ''),
               raw[, c('shortName', 'id', 'description')] %>% paste0(collapse = ''))

  expect_equal(getDatasets(c('GSE2018', 'GSE2872')) %>% nrow, 2)
  expect_equal(getDatasets(c(1, 2)) %>% nrow, 2)
  expect_equal(getDatasets(limit = 10) %>% nrow, 10)
  expect_false(getDatasets(offset = 3)[1,1] == getDatasets(offset = 0)[1,1])
  expect_true(getDatasets(filter = 'curationDetails.troubled = true')$ee.troubled %>% all)
  expect_false(getDatasets(sort = '-id')[1, 1] == getDatasets(sort = '+id')[1, 1])
})

test_that('datasetSearch queries work', {
  dat <- searchDatasets(query = 'bipolar', limit = 10)
  raw <- searchDatasets(query = 'bipolar', limit = 10, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(ee.ShortName, ee.ID, ee.Description)],
               c(raw$shortName, raw$id, raw$description))

  expect_equal(searchDatasets(taxon = 'human', query = 'bipolar', limit = 5) %>% nrow, 5)
  expect_lt(searchDatasets(query = 'bipolar', taxon = 'human') %>% nrow(),
            searchDatasets(query = 'bipolar') %>% nrow())

  # Checks that search term is present in every queried dataset's name or description
  ### Search not not working as expected (issue in API not wrapper)
  # expect_true(lapply(searchDatasets(taxon = 'human', query = 'bipolar'), function(x){grepl('bipolar', x)}) %>%
  #               as.data.frame() %>% dplyr::mutate(term_found = ee.Name | ee.Description) %>%
  #               .$term_found %>% all)
})

test_that('datasetPlatforms queries work', {
  dat <- getDatasetPlatforms(1)
  raw <- getDatasetPlatforms(1, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(platform.ID, platform.ShortName, platform.Description)],
               c(raw$id, raw$shortName, raw$description))
})

test_that('datasetSamples queries work', {
  dat <- getDatasetSamples(1)
  raw <- getDatasetSamples(1, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(sample.Name, sample.ID, sample.Description)],
               c(raw$name, raw$id, raw$description))
})

test_that('datasetDEA queries work', {
  dat <- getDatasetDEA(1)
  raw <- getDatasetDEA(1, raw = TRUE)
  rs <- raw$resultSets[[1]]
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(analysis.ID, stats.DE, stats.Up, stats.Down)],
               c(raw$id, rs$numberOfDiffExpressedProbes, rs$upregulatedCount, rs$downregulatedCount))
  # Offset and limit values do not seem to work in the API
  # expect_equal(getDatasetDEA(limit = 10) %>% nrow, 10)
  # expect_false(getDatasetDEA('GSE2018', offset = 3)[1,1] == getDatasetDEA('GSE2018', offset = 0)[1,1])
})

test_that('datasetSVD queries work', {
  dat <- getDatasetSVD(1)
  raw <- getDatasetSVD(1, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(c(dat$variance, dat$VMatrix),
               c(raw$variances, raw$vMatrix$rawMatrix))
})

test_that('datasetAnnotations queries work', {
  dat <- getDatasetAnnotations(1)
  raw <- getDatasetAnnotations(1, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(class.Type, class.Name, term.Name, term.URL)],
               c(raw$objectClass, raw$className, raw$termName, raw$termUri))
})

test_that('getDatasetData queries work', {
  expect_type(getDatasetData(1), 'list')
  expect_type(getDatasetData(1, raw = TRUE), 'list')
  expect_gt(getDatasetData(1) %>% nrow, getDatasetData(1, filter = TRUE) %>% nrow)
})

test_that('datasetDesign queries work', {
  expect_type(getDatasetDesign('GSE2018'), 'list')
  expect_type(getDatasetDesign('GSE2018', raw = TRUE), 'list')
})

test_that('datasetPCA queries work', {
  dat <- getDatasetPCA(1)
  raw <- getDatasetPCA(1, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat$ee.ID, raw$datasetId)

  # Check the expression vector for one gene
  datExpV <- dat$expr[[1]][1] %>% select(-c(gene.ID, gene.Symbol, probe))
  rawExpV <- raw$geneExpressionLevels[[1]]$vectors[1][[1]]$bioAssayExpressionLevels %>% data.table
  expect_equal(datExpV, rawExpV)

  expect_gt(getDatasetPCA(1, limit = 100)$expr[[1]] %>% nrow,
            getDatasetPCA(1, limit = 50)$expr[[1]] %>% nrow)
})

test_that('datasetDE queries work', {
  dat <- getDatasetDE(1, diffExSet = 468329)
  raw <- getDatasetDE(1, diffExSet = 468329, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat$ee.ID, raw$datasetId)

  # Check the expression vector for one gene
  datExpV <- dat$expr[[1]][1] %>% select(-c(gene.ID, gene.Symbol, probe))
  rawExpV <- raw$geneExpressionLevels[[1]]$vectors[1][[1]]$bioAssayExpressionLevels %>% data.table
  expect_equal(datExpV, rawExpV)

  expect_gt(getDatasetDE(1, diffExSet = 468329, limit = 100)$expr[[1]] %>% nrow,
            getDatasetDE(1, diffExSet = 468329, limit = 50)$expr[[1]] %>% nrow)

  # Threshold not working as expected in API
  # expect_gt(getDatasetDE(1, diffExSet = 468329, threshold = 10)$expr[[1]] %>% nrow,
  #           getDatasetDE(1, diffExSet = 468329, threshold = 200)$expr[[1]] %>% nrow)
})
