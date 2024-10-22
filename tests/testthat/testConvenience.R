test_that("getPlatformAnnotation queries work", {
    dat <- get_platform_annotations(1)
    # ProbeName will turn to ElementName in a later version
    expect_equal(colnames(dat), c("ElementName", "GeneSymbols", "GeneNames", "GOTerms", "GemmaIDs", "NCBIids"))
    expect_false(nrow(dat) == 0)
    
    dat <- expect_warning(get_platform_annotations(1313),"Unable to access annotation file")
    expect_null(dat)
})

test_that("getDataset works properly", {
    # Skip during checks since long runtime causes RMD/biocCheck fails during CI
    # These tests pass when run locally
    skip_on_ci()
    skip_on_bioc()
    expr <- get_dataset_processed_expression(1)
    eset <- get_dataset_object("GSE2018", keepNonSpecific = TRUE, type = "eset")
    sumexp <- get_dataset_object("GSE2018",keepNonSpecific = TRUE, type = "se")
    design <- get_dataset_samples(1)
    
    expect_equal(nrow(expr), eset[[1]] %>% nrow() %>% unname())
    expect_equal(Biobase::featureNames(eset[[1]]), expr$Probe)
    expect_equal(eset[[1]] %>% colnames(), design$sample.name)
    
    expect_equal(nrow(expr), nrow(sumexp[[1]]))
    expect_equal(rownames(sumexp[[1]]), expr$Probe)
    expect_equal(
        colnames(sumexp[[1]]),
        design$sample.name
    )
})

test_that("getDatasetTidy works properly", {
    # Skip during checks since long runtime causes RMD/biocCheck fails during CI
    # These tests pass when run locally
    skip_on_ci()
    skip_on_bioc()
    dat <- get_dataset_processed_expression(1)
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

test_that('get_dataset_object with aggeration methods',{
    av <- get_dataset_object(549,consolidate = 'average')
    mx <- get_dataset_object(549,consolidate = 'pickmax')
    vr <- get_dataset_object(549,consolidate = 'pickvar')
    
    
    expect_identical(av$`549` %>% dim,mx$`549` %>% dim)
    expect_identical(av$`549` %>% dim,vr$`549` %>% dim)
    
    expect_true(all(av$`549`@elementMetadata$GeneSymbol %in%  mx$`549`@elementMetadata$GeneSymbol))
    expect_true(all(av$`549`@elementMetadata$GeneSymbol %in%  vr$`549`@elementMetadata$GeneSymbol))
})


test_that('get_dataset_object with resultSets',{
    dea <- get_dataset_differential_expression_analyses(442)
    
    obj <- get_dataset_object(rep(442,times=nrow(dea)),
                              resultSets = dea$result.ID,
                              contrasts = dea$contrast.ID,type = 'list')
    
    expect_true(length(obj)==nrow(dea))
    
    for(i in seq_along(obj)){
        ids <- obj[[i]]$design$factorValues %>% purrr::map('ID') 
        subset_id <- dea$subsetFactor[[i]]$ID
        
        experimental_ids <- dea$experimental.factors[[i]]$ID
        control_ids <- dea$baseline.factors[[i]]$ID
        
        expect_true(ids %>% purrr::map_lgl(function(x){
            subset_id %in% x
        }) %>% all)
        
        expect_true(ids %>% purrr::map_lgl(function(x){
            any(x %in% c(control_ids,experimental_ids))
        }) %>% all)
        
    }
})

test_that("getDatasetDE works properly",{
    dat <- get_differential_expression_values(2)
    expect_equal(length(dat), 3)
    expect_gt(nrow(dat[[1]]), 10)
    dat <- get_differential_expression_values(resultSets = names(dat)[1])[[1]]
    expect_gt(nrow(dat), 10)
    
    # check compatibility with get_result_sets and get_dataset_differential_expression_analyses
    dat <- get_differential_expression_values('GSE18728')
    dataset_de <- get_dataset_differential_expression_analyses('GSE18728')
    result_set <- get_result_sets('GSE18728')
    
    testthat::expect_true(all(dataset_de$contrast.ID %in% result_set$contrast.ID))
    
    for (ds in names(dat)){
        d = dat[[ds]]
        
        expected_cols <- dataset_de %>% dplyr::filter(result.ID == ds) %>% {
            glue::glue("contrast_{.$contrast.ID}_log2fc")
        }
        
        testthat::expect_true(all(expected_cols %in% colnames(d)))
        
    }

})


test_that('getGenomeVersions works properly',{
    out <- get_taxa()
    expect_true(!is.null(out$taxon.database.name))
})

test_that('gemmaCall works properly',{
    dataset = c(1,2,3) # tests for collusion
    out<- gemma_call('datasets/{dataset}/svd',dataset = 1)
    expect_is(out,'list')
})

test_that('get_all_pages works properly',{
    out <- get_datasets_by_ids(datasets = c(1,2,3),
                        limit = 1) %>%
        get_all_pages(step_size = 1)
    expect_true(nrow(out)==3)
    
})
