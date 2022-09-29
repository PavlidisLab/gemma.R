test_that("getDatasetsInfo queries work", {
    dat <- get_datasets_by_ids(1)
    raw <- get_datasets_by_ids(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(experiment.ShortName, experiment.ID)] %>% paste0(collapse = ""),
        raw[, c("shortName", "id")] %>% paste0(collapse = "")
    )
    expect_equal(get_datasets_by_ids(c("GSE2018", "GSE2872")) %>% nrow(), 2)
    expect_equal(get_datasets_by_ids(c(1, 2)) %>% nrow(), 2)
    expect_equal(get_datasets_by_ids(limit = 10) %>% nrow(), 10)
    expect_equal(get_datasets_by_ids(offset = 2,attributes = FALSE)[1, 1], get_datasets_by_ids(offset = 0,attributes = FALSE)[3, 1])
    expect_false(get_datasets_by_ids(sort = "-id")[1, 1] == get_datasets_by_ids(sort = "+id")[1, 1])
})

test_that("searchDatasets queries work", {
    dat <- search_datasets("bipolar", limit = 20)
    raw <- search_datasets("bipolar", limit = 20, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(experiment.ShortName, experiment.ID)],
        c(raw$shortName, raw$id)
    )
    expect_equal(nrow(dat), 20)
    expect_equal(
        search_datasets("bipolar", taxon = "rat")[[1,"taxon.Name"]],
        "rat"
    )
})

test_that("datasetPlatforms queries work", {
    dat <- get_dataset_platforms(1)
    raw <- get_dataset_platforms(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(platform.ID, platform.ShortName, platform.Description)],
        c(raw$id, raw$shortName, raw$description)
    )
})

test_that("datasetSamples queries work", {
    dat <- get_dataset_samples(1)
    raw <- get_dataset_samples(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(sample.Name, sample.ID, sample.Description)],
        c(raw$name, raw$id, raw$description)
    )
})

test_that("datasetDEA queries work", {
    dat <- get_dataset_differential_expression_analyses(1)
    raw <- get_dataset_differential_expression_analyses(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
})

test_that("datasetAnnotations queries work", {
    dat <- get_dataset_annotations(1)
    raw <- get_dataset_annotations(1, raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(class.Type, class.Name, term.Name, term.URI)],
        c(raw$objectClass, raw$className, raw$termName, raw$termUri)
    )
})

test_that("getDatasetExpression queries work", {
    # Skip during checks since long runtime causes RMD/biocCheck fails during CI
    # These tests pass when run locally
    skip_on_ci()
    skip_on_bioc()
    dat <- expect_type(get_dataset_expression(1), "list")
    expect_gt(get_dataset_expression(1) %>% nrow(), get_dataset_expression(1, filter = TRUE) %>% nrow())
})

test_that("datasetDesign queries work", {
    expect_type(get_dataset_design("GSE2018"), "list")
    expect_type(get_dataset_design("GSE2018", raw = TRUE), "raw") %>% jsonlite:::simplify()
})
