test_that('processor file writing',{
    file = tempfile()
    getPlatformsInfo(1,file = file)

    expect_warning(getPlatformsInfo(1,file = file),'exists. Not overwriting')

    file = tempfile()
    gemma.R::getDatasetExpression(dataset = 'GSE2018', file = file)
    expect_true(file.exists(paste0(file,'.csv')))

    file = tempfile()
    gemma.R::getDatasetExpression(dataset = 'GSE2018', file = file, raw = TRUE)
    expect_true(file.exists(paste0(file,'.gz')))


    file = tempfile()
    gemma.R::getDatasetExpression(dataset = 'GSE2018', file = file, raw = FALSE)
    expect_true(file.exists(paste0(file,'.csv')))


})
