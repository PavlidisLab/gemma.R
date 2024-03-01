test_that("search_gemma work", {
    dat <- search_gemma('bipolar')
    raw <- search_gemma('bipolar',raw = TRUE)
    testthat::expect_true(nrow(dat) == length(raw))
    testthat::expect_true('data.table' %in% class(dat))
    testthat::expect_true(class(raw) == 'list')
    })

test_that("test query formation",{
    # tests for removal of empty elements inlcusion of filter parameters
    res_dataset <- get_result_sets(1)
    
    query_string <- attributes(res_dataset)$call %>% 
        strsplit('\\?') %>% 
        {.[[1]]} %>% 
        {.[[2]]} %>%
        shiny::parseQueryString()
    testthat::expect_true(query_string$datasets == 1)

    res_result_set <- get_result_sets(resultSets = res_dataset$result.ID)
    query_string <- attributes(res_result_set)$call %>% strsplit('\\?') %>%
        {.[[1]]} %>%
        {.[[2]]} %>% 
        shiny::parseQueryString()
    testthat::expect_true(!'dataset' %in% names(query_string))
    testthat::expect_true(query_string$filter == glue::glue('id in ({res_dataset$result.ID})'))
    
    
})