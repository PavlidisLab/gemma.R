test_that("getPlatformAnnotation queries work", {
    dat <- get_platform_annotations(1)
    expect_equal(colnames(dat), c("ProbeName", "GeneSymbols", "GeneNames", "GOTerms", "GemmaIDs", "NCBIids"))
    expect_false(nrow(dat) == 0)
    
    dat <- expect_warning(get_platform_annotations(1313),"Unable to access annotation file")
    expect_null(dat)
})

test_that("getDataset works properly", {
    # Skip during checks since long runtime causes RMD/biocCheck fails during CI
    # These tests pass when run locally
    skip_on_ci()
    skip_on_bioc()
    expr <- get_dataset_expression(1, filter = TRUE)
    eset <- get_dataset_object("GSE2018", filter = TRUE, type = "eset")
    sumexp <- get_dataset_object("GSE2018", filter = TRUE, type = "se")
    design <- get_dataset_design(1)

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
    dat <- get_dataset_expression(1)
    tidy <- get_dataset_object(1,type = 'tidy')
    design <- get_dataset_design(1)
    # Check number of rows = samples * probes (4 columns are gene info, not samples)
    expect_equal((ncol(dat) - 4) * nrow(dat), nrow(tidy))
    # Check design matrix
    expect_equal(colnames(design), colnames(tidy[, 4:ncol(tidy)]))
})

test_that("getDatasetDE works properly",{
    dat <- get_differential_expression_values(1)
    expect_gt(nrow(dat[[1]]), 10)
    dat <- get_differential_expression_values(resultSet = names(dat))[[1]]
    expect_gt(nrow(dat), 10)
    dat <- get_differential_expression_values(2)
    expect_equal(length(dat), 3)
})


test_that('getGenomeVersions works properly',{
    out <- get_taxa()
    expect_true(!is.null(out$taxon.Database.Name))
})

test_that('gemmaCall works properly',{
    out<- gemma_call('datasets/{dataset}/svd',dataset = 1)
    expect_is(out,'list')
})
