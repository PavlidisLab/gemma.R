test_that("getDatasetsInfo queries work", {
    dat <- getDatasetsInfo(1)
    raw <- getDatasetsInfo(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(ee.ShortName, ee.ID)] %>% paste0(collapse = ""),
        raw[, c("shortName", "id")] %>% paste0(collapse = "")
    )
    expect_equal(getDatasetsInfo(c("GSE2018", "GSE2872")) %>% nrow(), 2)
    expect_equal(getDatasetsInfo(c(1, 2)) %>% nrow(), 2)
    expect_equal(getDatasetsInfo(limit = 10) %>% nrow(), 10)
    expect_equal(getDatasetsInfo(offset = 2,attributes = FALSE)[1, 1], getDatasetsInfo(offset = 0,attributes = FALSE)[3, 1])
    expect_false(getDatasetsInfo(sort = "-id")[1, 1] == getDatasetsInfo(sort = "+id")[1, 1])
})

test_that("searchDatasets queries work", {
    dat <- searchDatasets("bipolar", limit = 20)
    raw <- searchDatasets("bipolar", limit = 20, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(ee.ShortName, ee.ID)],
        c(raw$shortName, raw$id)
    )
    expect_equal(nrow(dat), 20)
    expect_equal(
        searchDatasets("bipolar", taxon = "rat")[[1,"taxon.Name"]],
        "rat"
    )
})

test_that("datasetPlatforms queries work", {
    dat <- getDatasetPlatforms(1)
    raw <- getDatasetPlatforms(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(platform.ID, platform.ShortName, platform.Description)],
        c(raw$id, raw$shortName, raw$description)
    )
})

test_that("datasetSamples queries work", {
    dat <- getDatasetSamples(1)
    raw <- getDatasetSamples(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(sample.Name, sample.ID, sample.Description)],
        c(raw$name, raw$id, raw$description)
    )
})

test_that("datasetDEA queries work", {
    dat <- getDatasetDEA(1)
    raw <- getDatasetDEA(1, raw = TRUE) %>% jsonlite:::simplify()
    rs <- raw$resultSets[[1]]
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(analysis.ID, stats.DE, stats.Up, stats.Down)],
        c(raw$id, rs$numberOfDiffExpressedProbes, rs$upregulatedCount, rs$downregulatedCount)
    )
})

test_that("datasetAnnotations queries work", {
    dat <- getDatasetAnnotations(1)
    raw <- getDatasetAnnotations(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(class.Type, class.Name, term.Name, term.URL)],
        c(raw$objectClass, raw$className, raw$termName, raw$termUri)
    )
})

test_that("getDatasetExpression queries work", {
    # Skip during checks since long runtime causes RMD/biocCheck fails during CI
    # These tests pass when run locally
    skip_on_ci()
    skip_on_bioc()
    dat <- expect_type(getDatasetExpression(1), "list")
    expect_gt(getDatasetExpression(1) %>% nrow(), getDatasetExpression(1, filter = TRUE) %>% nrow())
})

test_that("datasetDesign queries work", {
    expect_type(getDatasetDesign("GSE2018"), "list")
    expect_type(getDatasetDesign("GSE2018", raw = TRUE), "raw") %>% jsonlite:::simplify()
})
