test_that('getGenes queries work', {
  # TODO: Ensure you can enter numerical id's as numerical (not char)
  dat <- getGenes(c('1859', '5728'))
  raw <- getGenes(c('1859', '5728'), raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(gene.Symbol, gene.Ensembl, gene.Name, taxon.Name)],
               c(raw$officialSymbol, raw$ensemblId, raw$officialName, raw$taxonCommonName))
})


# Throws warnings
# test_that('getGeneEvidence queries work', {
#   # TODO: throws warnings
#   dat <- getGeneEvidence(1859)
#   raw <- getGeneEvidence(1859, raw = TRUE)
#   expect_type(dat, 'list')
#   expect_type(raw, 'list')
#   expect_equal(dat[, c(gene.Symbol, gene.Ensembl, gene.Name, taxon.Name)],
#                c(raw$officialSymbol, raw$ensemblId, raw$officialName, raw$taxonCommonName))
#
# datEv <- dat$evidence[[1]]
#   rawEv <- raw$evidence[[1]]$phenotypeAssPubVO[[1]]$citationValueObject
#   expect_equal(datEv[1, c(pubmed.URL, citation, pubmed.Accession)],
#                c(rawEv$pubmedURL, rawEv$citation, rawEv$pubmedAccession))
# })

test_that('getGeneLocation queries work', {
  dat <- getGeneLocation(1859)
  raw <- getGeneLocation(1859, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')
  expect_equal(dat[, c(chromosome, strand, nucleotide, length)],
              c(raw$chromosome, raw$strand, raw$nucleotide, raw$nucleotideLength))
})

test_that('getGeneProbes queries work', {
  dat <- getGeneProbes(1859)
  raw <- getGeneProbes(1859, raw = TRUE)
  expect_type(dat, 'list')
  expect_type(raw, 'list')

})
