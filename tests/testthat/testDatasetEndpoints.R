test_that("getDatasets queries work", {
    dat <- getDatasets(1)
    raw <- getDatasets(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(ee.ShortName, ee.ID, ee.Description)] %>% paste0(collapse = ""),
        raw[, c("shortName", "id", "description")] %>% paste0(collapse = "")
    )

    expect_equal(getDatasets(c("GSE2018", "GSE2872")) %>% nrow(), 2)
    expect_equal(getDatasets(c(1, 2)) %>% nrow(), 2)
    expect_equal(getDatasets(limit = 10) %>% nrow(), 10)
    expect_equal(getDatasets(offset = 2)[1, 1], getDatasets(offset = 0)[3, 1])
    expect_true(getDatasets(filter = "curationDetails.troubled = true")$ee.troubled %>% all())
    expect_false(getDatasets(sort = "-id")[1, 1] == getDatasets(sort = "+id")[1, 1])
})

test_that("datasetSearch queries work", {
    dat <- searchDatasets("bipolar", limit = 50)
    raw <- searchDatasets("bipolar", limit = 50, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(ee.ShortName, ee.ID, ee.Description)],
        c(raw$shortName, raw$id, raw$description)
    )
    expect_equal(dat %>% nrow(), 50)
    expect_lt(
        searchDatasets("bipolar", taxon = "human") %>% nrow(),
        searchDatasets("bipolar") %>% nrow()
    )
})

test_that("datasetPlatforms queries work", {
    dat <- getDatasetPlatforms(1)
    raw <- getDatasetPlatforms(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(platform.ID, platform.ShortName, platform.Description)],
        c(raw$id, raw$shortName, raw$description)
    )
})

test_that("datasetSamples queries work", {
    dat <- getDatasetSamples(1)
    raw <- getDatasetSamples(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(sample.Name, sample.ID, sample.Description)],
        c(raw$name, raw$id, raw$description)
    )
})

test_that("datasetDEA queries work", {
    dat <- getDatasetDEA(1)
    raw <- getDatasetDEA(1, raw = TRUE)
    rs <- raw$resultSets[[1]]
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(analysis.ID, stats.DE, stats.Up, stats.Down)],
        c(raw$id, rs$numberOfDiffExpressedProbes, rs$upregulatedCount, rs$downregulatedCount)
    )
})

test_that("datasetSVD queries work", {
    dat <- getDatasetSVD(1)
    raw <- getDatasetSVD(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        c(dat$variance, dat[,2:6] %>% unname %>% as.matrix),
        c(raw$variances, raw$vMatrix$rawMatrix)
    )
})

test_that("datasetAnnotations queries work", {
    dat <- getDatasetAnnotations(1)
    raw <- getDatasetAnnotations(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(class.Type, class.Name, term.Name, term.URL)],
        c(raw$objectClass, raw$className, raw$termName, raw$termUri)
    )
})

test_that("getDatasetData queries work", {
    expect_type(getDatasetData(1), "list")
    expect_type(getDatasetData(1, raw = TRUE), "list")
    expect_gt(getDatasetData(1) %>% nrow(), getDatasetData(1, filter = TRUE) %>% nrow())
})

test_that("datasetDesign queries work", {
    expect_type(getDatasetDesign("GSE2018"), "list")
    expect_type(getDatasetDesign("GSE2018", raw = TRUE), "list")
})

test_that("datasetPCA queries work", {
    dat <- getDatasetPCA(1)
    raw <- getDatasetPCA(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(dat$ee.ID, raw$datasetId)

    # Check the expression vector for one gene
    datExpV <- dat$expr[[1]][1] %>% dplyr::select(-c(gene.ID, gene.Symbol, probe))
    rawExpV <- raw$geneExpressionLevels[[1]]$vectors[1][[1]]$bioAssayExpressionLevels %>% data.table()
    expect_equal(datExpV, rawExpV)
    expect_false(dat$expr[[1]][1, 1] ==  getDatasetPCA(1, 2)$expr[[1]][1, 1])
    expect_gt(
        getDatasetPCA(1, limit = 100)$expr[[1]] %>% nrow(),
        getDatasetPCA(1, limit = 50)$expr[[1]] %>% nrow()
    )
})

test_that("datasetDE queries work", {
    dat <- getDatasetDE(1, diffExSet = 500184)
    raw <- getDatasetDE(1, diffExSet = 500184, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(dat$ee.ID, raw$datasetId)

    # Check the expression vector for one gene
    datExpV <- dat$expr[[1]][1] %>% dplyr::select(-c(gene.ID, gene.Symbol, probe))
    rawExpV <- raw$geneExpressionLevels[[1]]$vectors[1][[1]]$bioAssayExpressionLevels %>% data.table()
    expect_equal(datExpV, rawExpV)

    expect_gt(
        getDatasetDE(1, diffExSet = 500184, limit = 100)$expr[[1]] %>% nrow(),
        getDatasetDE(1, diffExSet = 500184, limit = 50)$expr[[1]] %>% nrow()
    )

    # Threshold not working as expected in API
    # expect_false(all.equal(getDatasetDE(1, diffExSet = 468329, threshold = 10)[['expr']],
    # getDatasetDE(1, diffExSet = 468329, threshold = 200)[['expr']]))
})
