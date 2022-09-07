test_that("getTaxonInfo queries work", {
    dat <- gemma.R:::get_taxa_by_ids(c("mouse", "human"))
    raw <- gemma.R:::get_taxa_by_ids(c("mouse", "human"),raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat$taxon.Scientific,
        raw %>% purrr::map_chr('scientificName')
    )
    expect_true(nrow(dat)==2)
})

test_that("getTaxonDatasets queries work", {
    dat <- get_taxon_datasets("mouse")
    raw <- get_taxon_datasets("mouse",raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat$ee.Name,
        raw %>% purrr::map_chr('name')
    )
    expect_true(nrow(dat)==20)
})
