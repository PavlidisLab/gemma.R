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

# TODO: How useful is this endpoint? Fetches a lot of data and is very hard to filter
# test_that('getTaxonPhenotypes queries work', {
#   dat <- getTaxonPhenotypes('human')
#   raw <- getTaxonPhenotypes('human', raw = TRUE)
#
# })

test_that('getTaxonPhenotypeCandidates queries work', {
  dat <- getTaxonPhenotypeCandidates('human', phenotypes = 'http://purl.obolibrary.org/obo/DOID_11934')
  raw <- getTaxonPhenotypeCandidates('human', phenotypes = 'http://purl.obolibrary.org/obo/DOID_11934', raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(gene.Name, gene.NCBI, taxon.Name)],
               c(raw$officialName, raw$ncbiId, raw$taxonCommonName))
})

test_that('getGeneOnTaxon queries work', {
  dat <- getGeneOnTaxon('human', 1859)
  raw <- getGeneOnTaxon('human', 1859, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(gene.NCBI, gene.Name, taxon.ID, taxon.Scientific)],
               c(raw$ncbiId, raw$officialName, raw$taxonId, raw$taxonScientificName))
})

test_that('getEvidenceOnTaxon queries work', {
  dat <- getEvidenceOnTaxon('human', 1859)
  datEv <- dat$evidence[[1]]
  raw <- getEvidenceOnTaxon('human', 1859, raw = TRUE)
  rawEv <- raw$evidence[[1]]
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(gene.NCBI, gene.Name, taxon.ID, taxon.Scientific)],
               c(raw$ncbiId, raw$officialName, raw$taxonId, raw$taxonScientificName))

})

test_that('getGeneLocationOnTaxon queries work', {
  dat <- getGeneLocationOnTaxon('human', 1859)
  raw <- getGeneLocationOnTaxon('human', 1859, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(chromosome, strand, nucleotide, taxon.Name, taxon.Database.Name)],
               c(raw$chromosome, raw$strand, raw$nucleotide, raw$taxon$commonName, raw$taxon$externalDatabase$name))
})

test_that('getGenesAtLocation queries work', {
  dat <- getGenesAtLocation('human', chromosome = 21, strand = '+', start = 37365790, 3000)
  raw <- getGenesAtLocation('human', chromosome = 21, strand = '+', start = 37365790, 3000, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(gene.Symbol, gene.NCBI, gene.Name, taxon.ID, taxon.Scientific)],
               c(raw$officialSymbol, raw$ncbiId, raw$officialName, raw$taxonId, raw$taxonScientificName))
})
