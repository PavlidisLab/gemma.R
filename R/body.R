#' Get gemma path
#' @return Link to Gemma API
#' @keywords internal
gemmaPath <- function(){
    getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/")
}


#' Set gemma path
#' @param path "dev", "prod" or a link to use to access gemma API
#' @return Link to Gemma API 
#' @keywords internal
setGemmaPath <- function(path){
    if(path == 'dev'){
        path <- "https://dev.gemma.msl.ubc.ca/rest/v2/"
    }else if(path == 'prod'){
        path <- "https://gemma.msl.ubc.ca/rest/v2/"
    } else if(path == 'staging'){
        path <- "https://staging-gemma.msl.ubc.ca/rest/v2/"
    }
    options(gemma.API = path)
    gemmaPath()
}

#' Prototype function body
#'
#' This should not be called directly, but is called from the API functions.
#'
#' @param memoised Whether or not to memoise results
#' @param fname The function name
#' @param validators Validators for parameters
#' @param endpoint Formatted endpoint URL
#' @param envWhere Environment to evaluate in
#' @param isFile Whether or not the endpoint is expect to return a file
#' @param header Specific HTTP header for the request
#' @param raw Whether to return JSON (`TRUE`) or data.table (`FALSE`)
#' @param overwrite Whether or not to overwrite the file if @param file is specified
#' @param file A filename to save results to
#' @param open_api_name name of the call in the openapi file.
#' @param .call The original function call
#'
#' @noRd
.body <- function(fname, validators, endpoint, envWhere, isFile, header, raw, overwrite, file, attributes = TRUE,open_api_name, .call) {
    # Set header
    if (header == "text/tab-separated-values") {
        names(header) <- "Accept"
    }
    envWhere$header <- header
    original_env <- rlang::env_clone(envWhere)
    
    # Validate arguments
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v, envir = envWhere, inherits = FALSE), name = v), envir = envWhere)
        }
    }
    # Generate request
    call <- quote(paste0(gemmaPath(), gsub("/((NA)?/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA", "", glue::glue(endpoint)))))) %>% eval(envir = envWhere)
    
    # remove empty parameters
    call<- call %>% stringr::str_split('&') %>% 
        {.[[1]]} %>% {.[!grepl("\\=$",.)]} %>%
        paste0(collapse = '&')

    if (!is.null(getOption('gemma.username')) && !is.null(getOption('gemma.password'))){
        requestExpr <- quote(httr::GET(
            call,
            c(httr::authenticate(getOption('gemma.username'),
                                 getOption("gemma.password")),
              httr::add_headers(header),
              httr::user_agent(paste0('gemma.R/',packageVersion('gemma.R'))))))
    } else{
        requestExpr <- quote(httr::GET(
            call,
            c(httr::add_headers(header),
              httr::user_agent(paste0('gemma.R/',packageVersion('gemma.R'))))))
    }

    envWhere$call <- call
    response <- eval(requestExpr, envir = envWhere)
    ## Uncomment for debugging
    # print(response$url)

    # if 429. wait a bit and re-try.
    i <- 0
    while(i<3 && (is.null(response$status_code) || response$status_code == 429)){
        i <- i + 1
        Sys.sleep(5)
        response <- eval(requestExpr, envir = envWhere)
    }


    if (response$status_code == 200) {
        mData <- tryCatch(
            {
                if (isFile) {
                    out <- response$content
                    attributes(out) <- list(call=call)
                    out
                } else {
                    data <- jsonlite::fromJSON(rawToChar(response$content),simplifyVector = FALSE)
                    out <- data$data
                    if (attributes){
                        attributes(out) <-
                            c(attributes(out),data[!names(data) %in% 'data'],call=call)
                    }
                    out
                }
            },
            error = function(e) {
                message("Failed to parse ", response$type, " from ", response$url)
                warning(e$message)
            }
        )
        envWhere$mData <- mData
        if (raw || length(mData) == 0) {
            mOut <- mData
        } else {
            mOut <- eval(quote(eval(preprocessor)(mData)), envir = envWhere)
            if (attributes){
                attributes(mOut) <-
                    c(attributes(mOut), attributes(mData)[!names(attributes(mData)) %in% c('names','class','row.names')])
            }
        }


        if(attributes){
            original_env$response <- mData
            attributes(mOut) <- c(attributes(mOut),
                                 env = original_env)
        }
        
        
        if (!is.null(file) && !is.na(file)) {
            if (file.exists(file) && !overwrite && !file.info(file)$isdir) {
                warning(file, " exists. Not overwriting.")
            } else{
                dir.create(dirname(file),showWarnings = FALSE,recursive = TRUE)
                if(raw){
                    writeBin(response$content,file)
                } else{
                    saveRDS(mOut, file)
                }
            }
            

        }
        
        
        mOut
    } else if (response$status_code == 403) {
        stop(call,'\n',response$status_code, ": Forbidden. You do not have permission to access this data.")
    } else if (response$status_code == 404) {
        stop(call,'\n',response$status_code, ": Not found. Ensure your parameters are spelled correctly and that you're querying an existing ID.")
    } else if (response$status_code == 500) {
        stop(call,'\n',response$status_code, ": Internal server error.")
    } else if (response$status_code == 503) {
        stop(call,'\n',response$status_code, ": Service Unavailable. Gemma might be under maintenance.")
    } else {
        stop(call, '\n', "HTTP code ", response$status_code)
    }
}

#' URL encode a string safely
#'
#' @param url The string to URL encode. Vectors are delimited by a comma.
#'
#' @return A URL encoding of url
#'
#' @keywords internal
encode <- function(url) {
    if (is.na(url) || !is.character(url)) {
        url
    } else {
        if (length(url) > 1) {
            url <- paste0(url, collapse = ",")
        }
        utils::URLencode(url, TRUE)
    }
}
