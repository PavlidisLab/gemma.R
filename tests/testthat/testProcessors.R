test_that('processor file writing',{
    file = tempfile()
    get_platforms(1,file = file)

    expect_warning(get_platforms(1,file = file),'exists. Not overwriting')

    file = tempfile()
    gemma.R::get_dataset_expression(dataset = 'GSE2018', file = file)
    expect_true(file.exists(paste0(file,'.csv')))

    file = tempfile()
    gemma.R::get_dataset_expression(dataset = 'GSE2018', file = file, raw = TRUE)
    expect_true(file.exists(paste0(file,'.gz')))


    file = tempfile()
    gemma.R::get_dataset_expression(dataset = 'GSE2018', file = file, raw = FALSE)
    expect_true(file.exists(paste0(file,'.csv')))


})
