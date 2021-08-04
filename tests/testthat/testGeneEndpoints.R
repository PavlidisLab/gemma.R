test_that("getGenes queries work", {
    dat <- getGenes(1859)
    raw <- getGenes(1859, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(gene.Symbol, gene.Ensembl, gene.Name, taxon.Name)],
        c(raw$officialSymbol, raw$ensemblId, raw$officialName, raw$taxonCommonName)
    )
})

test_that("getGeneEvidence queries work", {
    dat <- getGeneEvidence(1859)
    raw <- getGeneEvidence(1859, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(gene.Symbol, gene.Ensembl, gene.Name, taxon.Name)],
        c(raw$officialSymbol, raw$ensemblId, raw$officialName, raw$taxonCommonName)
    )

    datEv <- dat$evidence[[1]]
    rawEv <- raw$evidence[[1]]$phenotypeAssPubVO[[1]]$citationValueObject
    expect_equal(
        datEv[1, c(pubmed.URL, citation, pubmed.Accession)],
        c(rawEv$pubmedURL, rawEv$citation, rawEv$pubmedAccession)
    )
})

test_that("getGeneLocation queries work", {
    dat <- getGeneLocation(1859)
    raw <- getGeneLocation(1859, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(chromosome, strand, nucleotide, length)],
        c(raw$chromosome, raw$strand, raw$nucleotide, raw$nucleotideLength)
    )
})

test_that("getGeneProbes queries work", {
    dat <- getGeneProbes(1859)
    raw <- getGeneProbes(1859, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(mapping.Description, array.ShortName, array.Type)],
        c(raw$description, raw$arrayDesign$shortName, raw$arrayDesign$technologyType)
    )

    expect_equal(getGeneProbes(1859, limit = 10) %>% nrow(), 10)
})

test_that("getGeneGO queries work", {
    dat <- getGeneGO(1859)
    raw <- getGeneGO(1859, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(term.Name, term.ID, term.URL)],
        c(raw$term, raw$goId, raw$uri)
    )
})

# Takes very long to respond, but passes
# test_that('getGeneCoexpression queries work', {
#   dat <- getGeneCoexpression(8291, 2218, limit = 50)
#   # TODO: API fetches 2 extra records
#   # expect_equal(dat %>% nrow, 50)
#   raw <- getGeneCoexpression(8291, 2218, limit = 50, raw = TRUE)
#   expect_type(dat, 'list')
#   expect_type(raw, 'list')
#   expect_equal(dat[, c(query.Degree, tested, support.N, query.gene.Name, found.gene.Name)],
#                c(raw$queryGeneNodeDegree, raw$numTestedIn, raw$support, raw$queryGene$officialName, raw$foundGene$officialName))
# })
