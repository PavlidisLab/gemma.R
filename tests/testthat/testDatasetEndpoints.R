test_that("getDatasetsInfo queries work", {
    dat <- get_datasets_by_ids(1)
    raw <- get_datasets_by_ids(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(experiment.shortName, experiment.ID)] %>% paste0(collapse = ""),
        raw[, c("shortName", "id")] %>% paste0(collapse = "")
    )
    expect_equal(get_datasets_by_ids(c("GSE2018", "GSE2872")) %>% nrow(), 2)
    expect_equal(get_datasets_by_ids(c(1, 2)) %>% nrow(), 2)
    expect_equal(get_datasets_by_ids(limit = 10) %>% nrow(), 10)

    a = get_datasets_by_ids(offset = 2)[1, 1]
    b = get_datasets_by_ids(offset = 0)[3, 1]
    attributes(a) = NULL
    attributes(b) = NULL

    expect_equal(a, b)
    expect_false(get_datasets_by_ids(sort = "-id")[1, 1] == get_datasets_by_ids(sort = "+id")[1, 1])
})

test_that("datasetPlatforms queries work", {
    dat <- get_dataset_platforms(1)
    raw <- get_dataset_platforms(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(platform.ID, platform.shortName, platform.description)],
        c(raw$id, raw$shortName, raw$description)
    )
})

test_that("datasetSamples queries work", {
    dat <- get_dataset_samples(1)
    raw <- get_dataset_samples(1, raw = TRUE) %>% jsonlite:::simplify()
    raw <- raw[match(dat$sample.ID,raw$id),]
    
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(sample.name, sample.ID, sample.description)],
        c(raw$name, raw$id, raw$description)
    )
})

test_that("datasetDEA queries work", {
    dat <- get_dataset_differential_expression_analyses(2)
    raw <- get_dataset_differential_expression_analyses(2, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(dat$result.ID %>% unique,
                 raw %>% purrr::map('resultSets') %>%
                     purrr::map(function(x){x %>%
                             gemma.R:::accessField('id')}) %>%
                     unlist)

})

test_that("datasetAnnotations queries work", {
    dat <- get_dataset_annotations(1)
    raw <- get_dataset_annotations(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    # Gemma 2.0 serves category/categoryUri/value/valueUri where Gemma 1.x
    # served className/classUri/termName/termUri. Read whichever spelling this
    # server used so the test passes against both.
    rawField <- function(field, legacy){
        if(!is.null(raw[[field]])) raw[[field]] else raw[[legacy]]
    }
    expect_true(all(dat$class.name %in% rawField("category", "className")))
    expect_true(all(dat$class.URI %in% rawField("categoryUri", "classUri")))
    expect_true(all(dat$term.name %in% rawField("value", "termName")))
    expect_true(all(dat$term.URI %in% rawField("valueUri", "termUri")))

    # the columns must actually be populated: reading a field the server does
    # not serve yields an all-NA column rather than an error
    expect_gt(nrow(dat), 0)
    expect_false(all(is.na(dat$class.name)))
    expect_false(all(is.na(dat$term.name)))

})

test_that("get_dataset_expression_for_genes work", {
    dat <- get_dataset_expression_for_genes(c(1,4),c(7265,7072,1809))
    raw <- get_dataset_expression_for_genes(c(1,4),c(7265,7072,1809),raw = TRUE)
    expect_length(dat,2)
    expect_type(dat,"list")
    expect_type(raw, 'list')
    expect_s3_class(dat[[1]],'data.table')
    expect_s3_class(dat[[2]],'data.table')
})


test_that("getDatasetExpression queries work", {
    # Skip during checks since long runtime causes RMD/biocCheck fails during CI
    # These tests pass when run locally
    skip_on_ci()
    skip_on_bioc()
    dat <- expect_type(get_dataset_processed_expression(1), "list")
})


