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
    eset <- get_dataset_object("GSE2018", keepNonSpecific = TRUE, filter = TRUE, type = "eset")
    sumexp <- get_dataset_object("GSE2018",keepNonSpecific = TRUE, filter = TRUE, type = "se")
    design <- get_dataset_samples(1)

    expect_equal(nrow(expr), eset[[1]] %>% nrow() %>% unname())
    expect_equal(Biobase::featureNames(eset[[1]]), expr$Probe)
    expect_equal(eset[[1]] %>% colnames(), design$sample.Name)

    expect_equal(nrow(expr), nrow(sumexp[[1]]))
    expect_equal(rownames(sumexp[[1]]), expr$Probe)
    expect_equal(
        colnames(sumexp[[1]]),
        design$sample.Name
    )
})

test_that("getDatasetTidy works properly", {
    # Skip during checks since long runtime causes RMD/biocCheck fails during CI
    # These tests pass when run locally
    skip_on_ci()
    skip_on_bioc()
    dat <- get_dataset_expression(1)
    tidy <- get_dataset_object(1,type = 'tidy',keepNonSpecific = TRUE)
    design <- get_dataset_samples(1)
    # Check number of rows = samples * probes (4 columns are gene info, not samples)
    expect_equal((ncol(dat) - 4) * nrow(dat), nrow(tidy))
})

test_that("get_dataset_object with multiple datasets and genes",{
    genes <- c(8913,7840)
    datasets <- c(549, 873, 1869)
    tidy <- get_dataset_object(datasets,genes,type = 'tidy')
    expect_equal(tidy$experiment.ID %>% unique, datasets)
    expect_true(tidy$NCBIid %>% unique %in% genes %>% all)
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
