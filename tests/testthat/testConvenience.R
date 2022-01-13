test_that("getPlatformAnnotation queries work", {
    dat <- getPlatformAnnotation(1)
    expect_equal(colnames(dat), c("ProbeName", "GeneSymbols", "GeneNames", "GOTerms", "GemmaIDs", "NCBIids"))
    expect_false(nrow(dat) == 0)
})

test_that("getBioc data is preserved", {
    expr <- getDatasetData(1, filter = TRUE)
    eset <- getBioc("ExpressionSet", "GSE2018", filter = TRUE)
    sumexp <- getBioc("SummarizedExperiment", "GSE2018", filter = TRUE)
    design <- getDatasetDesign(1)

    expect_equal(nrow(expr), eset %>% nrow() %>% unname())
    expect_equal(Biobase::featureNames(eset), expr$Probe)
    expect_equal(Biobase::varLabels(eset), colnames(design))

    expect_equal(nrow(expr), nrow(sumexp))
    expect_equal(rownames(sumexp), expr$Probe)
    expect_equal(colnames(SummarizedExperiment::colData(sumexp)),
                 colnames(design))
})

test_that("getTidyDataset works properly", {
    dat <- getDatasetData(1, filter = TRUE)
    tidy <- getTidyDataset(1)
    design <- getDatasetDesign(1)
    # Check number of rows = samples * probes (4 columns are gene info, not samples)
    expect_equal((ncol(dat) - 4) * nrow(dat), nrow(tidy))
    # Check design matrix
    expect_equal(colnames(design), colnames(tidy[, 4:ncol(tidy)]))
})

