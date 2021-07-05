test_that('getTaxa queries work', {
  dat <- getTaxa('human')
  raw <- getTaxa('human', raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(getTaxa(c(1,2)) %>% nrow, 2)
  expect_equal(dat[, c(taxon.Name, taxon.Scientific, taxon.ID, taxon.NCBI, taxon.Database.Name)],
               c(raw$commonName, raw$scientificName, raw$id, raw$ncbiId, raw$externalDatabase$name))
})

test_that('getTaxonDatasets queries work', {
  dat <- getTaxonDatasets('human', limit = 15)
  raw <- getTaxonDatasets('human', limit = 15, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat %>% nrow, 15)
  expect_true(getTaxonDatasets('human', filter = 'curationDetails.troubled = true')$ee.troubled %>% all)
  expect_false(getTaxonDatasets('human', offset = 3)[1,1] == getTaxonDatasets('human', offset = 0)[1,1])
  expect_false(getTaxonDatasets('human', sort = '-id')[1, 1] == getTaxonDatasets('human', sort = '+id')[1, 1])
})

# How useful is this endpoint?
# test_that('getTaxonPhenotypes queries work', {
#   dat <- getTaxonPhenotypes('human')
#   raw <- getTaxonPhenotypes('human', raw = TRUE)
#
# })
