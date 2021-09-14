test_that("getPlatforms queries work", {
    dat <- getPlatforms(1)
    raw <- getPlatforms(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(platform.ID, platform.ShortName, platform.Name, platform.Description, platform.ExperimentCount)] %>% paste0(collapse = ""),
        raw[, c("id", "shortName", "name", "description", "expressionExperimentCount")] %>% paste0(collapse = "")
    )

    expect_equal(getPlatforms(c(1, 2)) %>% nrow(), 2)
    expect_equal(getPlatforms(limit = 10) %>% nrow(), 10)
    expect_true(getPlatforms(offset = 3)[4, 1] == getDatasets(offset = 0)[1, 1])
    expect_true(getPlatforms(filter = "curationDetails.troubled = true")$ee.troubled %>% all())
    expect_false(getPlatforms(sort = "-id")[1, 1] == getDatasets(sort = "+id")[1, 1])
})

test_that("getPlatformDatasets queries work", {
    dat <- getPlatformDatasets(1)
    raw <- getPlatformDatasets(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(ee.ShortName, ee.Name, ee.Description, ee.Samples, geeq.qScore)],
        c(raw$shortName, raw$name, raw$description, raw$bioAssayCount, raw$geeq$publicQualityScore)
    )
    expect_equal(getPlatformDatasets(1, limit = 10) %>% nrow(), 10)
    # TODO: Offset not working as expected in API
    # expect_true(getPlatformDatasets(1, offset = 3)[1,1] == getPlatformDatasets(1, offset = 0)[5,1])
})

test_that("getPlatformElements queries work", {
    dat <- getPlatformElements(1)
    raw <- getPlatformElements(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    # TODO: Throws warning when using short name ids (ex. 'GPL1355') or with large limits
    expect_equal(
        dat[, c(mapping.Name, array.ShortName, array.Name, array.Type)],
        c(raw$name, raw$arrayDesign$shortName, raw$arrayDesign$name, raw$arrayDesign$technologyType)
    )

    dat <- getPlatformElements("GPL1355", element = "AFFX_Rat_beta-actin_M_at")
    raw <- getPlatformElements("GPL1355", element = "AFFX_Rat_beta-actin_M_at", raw = TRUE)
    expect_equal(
        dat[, c(mapping.Name, array.ShortName, array.Name, array.Type)],
        c(raw$name, raw$arrayDesign$shortName, raw$arrayDesign$name, raw$arrayDesign$technologyType)
    )

    expect_equal(getPlatformElements(1, limit = 10) %>% nrow(), 10)
    expect_equal(getPlatformElements(1, offset = 2)[1, 1], getPlatformElements(1, offset = 0)[3, 1])
})

test_that("getPlatformElementsGenes queries work", {
    dat <- getPlatformElementGenes("GPL1355", element = "AFFX_Rat_beta-actin_M_at")
    raw <- getPlatformElementGenes("GPL1355", element = "AFFX_Rat_beta-actin_M_at", raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(gene.Symbol, gene.Ensembl, taxon.ID)],
        c(raw$officialSymbol, raw$ensemblId, raw$taxonId)
    )
})
