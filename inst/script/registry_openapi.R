
register_openapi = function(name){
    api_instance = DefaultApi$new()
    endpoint = api_file$paths[[which(api_file_fun_names %in% name)]]
    args = formals(api_instance[[name]])
    fargs = args[!names(args) %in% c('data_file','...')]
    
    
    f <- function() {}

    fargs$raw <- quote(getOption("gemma.raw", FALSE))
    fargs$memoised <- quote(getOption("gemma.memoised", FALSE))
    fargs$file <- quote(getOption("gemma.file", NA_character_))
    fargs$overwrite <- quote(getOption("gemma.overwrite", FALSE))
    fargs$attributes <- quote(getOption("gemma.attributes", TRUE))
    
    formals(f) <- fargs
    
    body(f) = quote({
        .openapi_body()
    })
}