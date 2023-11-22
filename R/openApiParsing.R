


get_response_schema <- function(open_api_name){
    api_file_fun_names = api_spec$paths %>% purrr::map('get') %>% purrr::map_chr('operationId') %>% snakecase::to_snake_case()
    
    resp = api_spec$paths[[which(api_file_fun_names %in% open_api_name)]]$get$responses
    
    if('default' %in% names(resp)){
        return(resp$default$content$`application/json`$schema)
    } else{
        return(resp$`200`$content$`application/json`$schema)
    }
}


annotate_response <- function(response,schema){
    
}