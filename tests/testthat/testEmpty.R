test_that("Empty output on simple processors",{
    
    non_empty = get_datasets_by_ids(1)
    
    empty = get_datasets_by_ids('potato')
    
    expect_true(all(colnames(non_empty) == colnames(empty)))
    expect_true(nrow(empty) == 0)
})


test_that("Empty output on DEAs",{
    
    non_empty = get_dataset_differential_expression_analyses(1)
    
    empty = get_dataset_differential_expression_analyses(31312) # a dataset without metadata and differential expression
    
    expect_true(all(colnames(non_empty) == colnames(empty)))
    expect_true(nrow(empty) == 0)
})


test_that("Empty output on resultsets",{
    
    non_empty = get_result_sets(1)
    
    empty = get_result_sets(31312) # a dataset without metadata and differential expression
    
    expect_true(all(colnames(non_empty) == colnames(empty)))
    expect_true(nrow(empty) == 0)
})

