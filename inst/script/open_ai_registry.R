library(snakecase)
library(jsonlite)
library(here)
source('inst/script/open_api_helpers.R')

download.file('https://dev.gemma.msl.ubc.ca/rest/v2/openapi.json',
              destfile = here('inst/script/openapi.json'))
open_api = jsonlite::fromJSON(here('inst/script/openapi.json'),simplifyVector = FALSE)


open_api$paths %>% names %>%
    lapply(function(path){
        endpoint = open_api$paths[[path]]

        name = paste0('api_',endpoint$get$operationId %>% snakecase::to_snake_case())
        description = endpoint$get$summary
        params = endpoint$get$parameters

        where = params %>% purrr::map_chr('in')
        which(where == 'query')
    })

