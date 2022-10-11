test_that("search_gemma work", {
    dat <- search_gemma('bipolar')
    raw <- search_gemma('bipolar',raw = TRUE)
    testthat::expect_true(nrow(dat) == length(raw))
    testthat::expect_true('data.table' %in% class(dat))
    testthat::expect_true(class(raw) == 'list')
    })