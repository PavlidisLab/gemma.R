test_that("getPlatformsInfo queries work", {
    dat <- get_platforms_by_ids(1)
    raw <- get_platforms_by_ids(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(platform.ID, platform.shortName, platform.name, platform.description, platform.experimentCount)] %>% paste0(collapse = ""),
        c(raw[[1]]$"id", raw[[1]]$"shortName", raw[[1]]$"name", raw[[1]]$"description", raw[[1]]$"expressionExperimentCount") %>% paste0(collapse = "")
    )
    expect_equal(get_platforms_by_ids(c(1, 2)) %>% nrow(), 2)
    expect_equal(get_platforms_by_ids(limit = 10) %>% nrow(), 10)
    expect_true(get_platforms_by_ids(offset = 3)[1, 1] == get_platforms_by_ids(offset = 0)[4, 1])
    expect_false(get_platforms_by_ids(sort = "-id")[1, 1] == get_platforms_by_ids(sort = "+id")[1, 1])
})

test_that("getPlatformDatasets queries work", {
    dat <- get_platform_datasets(1)
    raw <- get_platform_datasets(1, raw = TRUE)
    expect_type(dat, "list")
    expect_type(raw, "list")
    expect_equal(
        dat[, c(experiment.shortName, experiment.name, experiment.sampleCount)],
        c(raw %>% purrr::map_chr('shortName'),
          raw %>% purrr::map_chr('name'),
          raw %>% purrr::map('bioAssayCount') %>% purrr::map_chr(as.character)
    ))
    

    expect_equal(get_platform_datasets(1, limit = 10) %>% nrow(), 10)
    a = get_platform_datasets(1, offset = 0)[2, ]
    b = get_platform_datasets(1, offset = 1)[1, ]
    attributes(a) = NULL
    attributes(b) = NULL
    expect_equal(a, b)
})

# function tentatively removed
# test_that("getPlatformElements queries work", {
#     dat <- getPlatformElements(1)
#     raw <- getPlatformElements(1, raw = TRUE) %>% jsonlite:::simplify()
#     expect_type(dat, "list")
#     expect_type(raw, "list")
#     expect_equal(
#         dat[, c(mapping.Name, mapping.Description)],
#         c(raw$name, raw$description)
#     )
#     dat <- getPlatformElements("GPL1355", elements = "AFFX_Rat_beta-actin_M_at")
#     raw <- getPlatformElements("GPL1355", elements = "AFFX_Rat_beta-actin_M_at", raw = TRUE) %>% jsonlite:::simplify()
#     expect_equal(
#         dat[, c(mapping.Name, mapping.Description)],
#         c(raw$name, raw$description)
#     )
# 
#     expect_equal(getPlatformElements(1, limit = 10) %>% nrow(), 10)
#     expect_equal(getPlatformElements(1, offset = 2,attributes = FALSE)[1, 1], getPlatformElements(1, offset = 0,attributes = FALSE)[3, 1])
# })

test_that("getPlatformElementsGenes queries work", {
    dat <- get_platform_element_genes("GPL1355", probe = "AFFX_Rat_beta-actin_M_at")
    raw <- get_platform_element_genes("GPL1355", probe = "AFFX_Rat_beta-actin_M_at", raw = TRUE) %>% jsonlite:::simplify()
    expect_type(dat, "list")
    expect_type(raw, "list")

    expect_equal(
        dat[, c(gene.symbol, gene.ensembl, taxon.ID)],
        c(raw$officialSymbol, raw$ensemblId, raw$taxon$id)
    )
})
