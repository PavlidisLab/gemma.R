test_that("getPlatformAnnotation queries work", {
    dat <- getPlatformAnnotation(1)
    expect_equal(colnames(dat), c("ProbeName", "GeneSymbols", "GeneNames", "GOTerms", "GemmaIDs", "NCBIids"))
    expect_false(nrow(dat) == 0)
})

test_that("getDataset works properly", {
    # Skip during checks since long runtime causes RMD/biocCheck fails during CI
    # These tests pass when run locally
    skip_on_ci()
    skip_on_bioc()
    expr <- getDatasetExpression(1, filter = TRUE)
    eset <- getDataset("GSE2018", filter = TRUE, type = "eset")
    sumexp <- getDataset("GSE2018", filter = TRUE, type = "se")
    design <- getDatasetDesign(1)

    expect_equal(nrow(expr), eset %>% nrow() %>% unname())
    expect_equal(Biobase::featureNames(eset), expr$Probe)
    expect_equal(Biobase::varLabels(eset), colnames(design))

    expect_equal(nrow(expr), nrow(sumexp))
    expect_equal(rownames(sumexp), expr$Probe)
    expect_equal(
        colnames(SummarizedExperiment::colData(sumexp)),
        colnames(design)
    )
})

test_that("getDatasetTidy works properly", {
    # Skip during checks since long runtime causes RMD/biocCheck fails during CI
    # These tests pass when run locally
    skip_on_ci()
    skip_on_bioc()
    dat <- getDatasetExpression(1)
    tidy <- getDatasetTidy(1)
    design <- getDatasetDesign(1)
    # Check number of rows = samples * probes (4 columns are gene info, not samples)
    expect_equal((ncol(dat) - 4) * nrow(dat), nrow(tidy))
    # Check design matrix
    expect_equal(colnames(design), colnames(tidy[, 4:ncol(tidy)]))
})

test_that("getDatasetDE works properly",{
    dat <- getDatasetDE(1)
    expect_gt(nrow(dat), 10)
    expect_error(getDatasetDE(2))
    expect_error(getDatasetDE(2, 500184))
    expect_error(getDataserDE(1))
    dat <- getDatasetDE(resultSet = 500184)
    expect_gt(nrow(dat), 10)
    dat <- getDatasetDE(2, all = TRUE)
    expect_equal(length(dat), 3)
})
