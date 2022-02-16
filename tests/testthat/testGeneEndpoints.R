test_that("getGenesInfo queries work", {
    dat <- getGenesInfo(1859)
    raw <- getGenesInfo(1859, raw = TRUE)
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
