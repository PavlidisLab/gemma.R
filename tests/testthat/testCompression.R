test_that("Compressed inputs work",{
    
    
    # get_result_sets
    options('gemma.always.compress' = FALSE)
    uncompressed <- get_result_sets(filter = "baselineGroup.characteristics.value = disease")
    
    options('gemma.always.compress' = TRUE)
    compressed <-  get_result_sets(filter = "baselineGroup.characteristics.value = disease")
    expect_true(attributes(uncompressed)$call != attributes(compressed)$call)
    expect_true(all(uncompressed$result.ID %in% compressed$result.ID))
    
    
    # search_annotations
    options('gemma.always.compress' = FALSE)
    uncompressed <- search_annotations(query = c('brain','blood'))
    
    options('gemma.always.compress' = TRUE)
    compressed <-  search_annotations(query = c('brain','blood'))
    expect_true(attributes(uncompressed)$call != attributes(compressed)$call)
    expect_true(all(uncompressed$value.URI %in% compressed$value.URI))
    
    
    
    # get_datasets
    options('gemma.always.compress' = FALSE)
    uncompressed <- get_datasets(taxa = c("mouse", "human"), uris = "http://purl.obolibrary.org/obo/UBERON_0002048",query = "brain")

    options('gemma.always.compress' = TRUE)
    compressed <- get_datasets(taxa = c("mouse", "human"), uris = "http://purl.obolibrary.org/obo/UBERON_0002048",query = "brain")
    
    expect_true(attributes(uncompressed)$call != attributes(compressed)$call)
    expect_true(all(uncompressed$experiment.shortName %in% compressed$experiment.shortName))
    
    
    # get_datasets_by_ids
    options('gemma.always.compress' = FALSE)
    datasets = c("GSE2018", "GSE2178", "GSE476", "GSE478", "GSE484", "GSE485")
    uncompressed <- get_datasets_by_ids(datasets,taxa = c("mouse", "human"), uris = "http://purl.obolibrary.org/obo/UBERON_0002048")
    
    options('gemma.always.compress' = TRUE)
    compressed <- get_datasets_by_ids(datasets,taxa = c("mouse", "human"), uris = "http://purl.obolibrary.org/obo/UBERON_0002048")
    
    expect_true(attributes(uncompressed)$call != attributes(compressed)$call)
    expect_true(all(uncompressed$experiment.shortName %in% compressed$experiment.shortName))
    
    # get_platforms_by_ids
    platforms = c(1, 3, 4, 5, 7, 8)
    options('gemma.always.compress' = FALSE)
    uncompressed <- get_platforms_by_ids(platforms, taxa = c("human","mouse"))
    
    
    options('gemma.always.compress' = TRUE)
    compressed <-  get_platforms_by_ids(platforms, taxa = c("human","mouse"))
    
    
    
    expect_true(attributes(uncompressed)$call != attributes(compressed)$call)
    expect_true(all(uncompressed$platform.ID %in% compressed$platform.ID))
    
    
    options('gemma.always.compress' = FALSE)
    
})
