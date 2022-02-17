test_that("getPlatformsInfo queries work", {
    dat <- getPlatformsInfo(1)
    raw <- getPlatformsInfo(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(platform.ID, platform.ShortName, platform.Name, platform.Description, platform.ExperimentCount)] %>% paste0(collapse = ""),
        raw[, c("id", "shortName", "name", "description", "expressionExperimentCount")] %>% paste0(collapse = "")
    )
    expect_equal(getPlatformsInfo(c(1, 2)) %>% nrow(), 2)
    expect_equal(getPlatformsInfo(limit = 10) %>% nrow(), 10)
    expect_true(getPlatformsInfo(offset = 3)[1, 1] == getPlatformsInfo(offset = 0)[4, 1])
    expect_true(getPlatformsInfo(filter = "curationDetails.troubled = true")$ee.troubled %>% all())
    expect_false(getPlatformsInfo(sort = "-id")[1, 1] == getPlatformsInfo(sort = "+id")[1, 1])
})

test_that("getPlatformDatasets queries work", {
    dat <- getPlatformDatasets(1)
    raw <- getPlatformDatasets(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(ee.ShortName, ee.Name, ee.Samples, geeq.qScore)],
        c(raw$shortName, raw$name, raw$bioAssayCount, raw$geeq$publicQualityScore)
    )
    expect_equal(getPlatformDatasets(1, limit = 10) %>% nrow(), 10)
    expect_equal(getPlatformDatasets(1, offset = 0)[2, ], getPlatformDatasets(1, offset = 1)[1, ])
})

test_that("getPlatformElements queries work", {
    dat <- getPlatformElements(1)
    raw <- getPlatformElements(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(mapping.Name, mapping.Description)],
        c(raw$name, raw$description)
    )
    dat <- getPlatformElements("GPL1355", element = "AFFX_Rat_beta-actin_M_at")
    raw <- getPlatformElements("GPL1355", element = "AFFX_Rat_beta-actin_M_at", raw = TRUE)
    expect_equal(
        dat[, c(mapping.Name, mapping.Description)],
        c(raw$name, raw$description)
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
