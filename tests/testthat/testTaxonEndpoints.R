test_that("getTaxonInfo queries work", {
    dat <- getTaxonInfo(c("mouse", "human"))
    raw <- getTaxonInfo(c("mouse", "human"),raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat$taxon.Scientific,
        raw$scientificName
    )
    expect_true(nrow(dat)==2)
})

test_that("getTaxonDatasets queries work", {
    dat <- getTaxonDatasets("mouse")
    raw <- getTaxonDatasets("mouse",raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat$ee.Name,
        raw$name
    )
    expect_true(nrow(dat)==20)
})