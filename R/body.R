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
    } else if(path == "local"){
        path <- "http://localhost:8080/rest/v2/"
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
.body <- function(fname, validators, endpoint, envWhere, isFile, header, raw, overwrite, file, attributes = TRUE, in_data, open_api_name, .call) {
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
    form_call <- function(){
        call <- quote(paste0(gemmaPath(), gsub("/((NA)?/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA", "", glue::glue(endpoint)))))) %>% eval(envir = envWhere)
        # remove empty parameters
        call<- call %>% stringr::str_split('&|\\?') %>% 
            {.[[1]]} %>% {.[!grepl("\\=$",.)]} %>%
            {if(length(.)>1){c(paste(.[1],.[2],sep = "?"),.[c(-1,-2)])}else{.}} %>%
            paste0(collapse = '&')
        return(call)
    }
    
    call <- form_call()
    # decide if we want to compress some parameters.
    if(nchar(call)>getOption('gemma.URL.limit',5000) || getOption('gemma.always.compress', FALSE)){
        envWhere$compressibles %>% 
            lapply(function(x){
                compressed <- compress_arg(envWhere[[x]])
                envWhere[[x]] <- ifelse(nchar(compressed) < nchar(envWhere[[x]]) || getOption('gemma.always.compress', FALSE),
                                        compressed,
                                        envWhere[[x]])
            })
        call <- form_call()
    }
    envWhere$call <- call
    
    
    username = ifelse(!is.null(getOption('gemma.username')),
                      getOption('gemma.username'),
                      ifelse(Sys.getenv('GEMMA_USERNAME')!= "",
                             Sys.getenv('GEMMA_USERNAME'),
                             NA))
    
    password = ifelse(!is.null(getOption('gemma.password')),
                      getOption('gemma.password'),
                      ifelse(Sys.getenv('GEMMA_PASSWORD')!= "",
                             Sys.getenv('GEMMA_PASSWORD'),
                             NA))

    header = envWhere$header

    if (!is.na(username) && !is.na(password)){
        requestExpr <- quote(httr::GET(
            call,
            c(httr::authenticate(username,
                                 password),
              httr::add_headers(header),
              httr::user_agent(paste0('gemma.R/',packageVersion('gemma.R')))),
            handle = httr::handle("")))
    } else{
        requestExpr <- quote(httr::GET(
            call,
            c(httr::add_headers(header),
              httr::user_agent(paste0('gemma.R/',packageVersion('gemma.R')))),
            handle = httr::handle("")))
    }

    response <- eval(requestExpr)
    ## Uncomment for debugging
    # print(response$url)

    # if 429, 500 or 503. wait a bit and re-try. a 503 that tells us when to come
    # back is server-directed, so it doesn't count towards the retry limit
    i <- 0
    while(is.null(response$status_code) || response$status_code %in% c(429,500,503)){
        directed <- if(isTRUE(response$status_code == 503)) retryAfter(response) else NULL
        if(is.null(directed)){
            if(i >= 3) break
            i <- i + 1
        }
        wait <- if(is.null(directed)) 5 else directed
        if(!is.null(directed)){
            message('High traffic in Gemma. Retrying in ', wait, ' seconds.')
        } else{
            message('No valid response from Gemma',
             if(is.null(response$status_code)) '' else paste0(' (',response$status_code ,')'),
             '. Retrying in ',
              wait, ' seconds.')
        }
        Sys.sleep(wait)
        response <- eval(requestExpr)
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
                    # in_data argument will not be needed after fix in API
                    if ('data' %in% names(data)){
                        in_data = TRUE
                    }
                    if(in_data){
                        out <- data$data 
                        if (attributes){
                            attributes(out) <-
                                c(attributes(out),data[!names(data) %in% 'data'],call=call)
                        }
                    } else{
                        out <- data
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
        if (raw) {
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
        if(isFile){
            message <- response$content %>% rawToChar() %>% strsplit('\n') %>% {.[[1]]} %>% {.[grepl('Message',.)]} %>% stringr::str_extract('(?<=Message: ).*')
        } else{
            message <- response$content %>% rawToChar() %>% jsonlite::fromJSON() %>% {.$error$message}
        }
        stop(call, '\n', "HTTP code ", response$status_code,": ",message)
    }
}

#' Seconds the server asked us to wait before re-trying
#'
#' Reads the \code{Retry-After} response header, accepting both the delay in
#' seconds and the HTTP-date forms. The delay is rounded to whole seconds and
#' floored at one so a server-directed re-try can't spin.
#'
#' @param response An httr response object
#' @return Number of seconds to wait, or NULL if the header is absent or unusable
#' @keywords internal
retryAfter <- function(response){
    header <- tryCatch(httr::headers(response)[['retry-after']], error = function(e) NULL)
    if(is.null(header)){
        return(NULL)
    }
    seconds <- suppressWarnings(as.numeric(header))
    if(is.na(seconds)){
        seconds <- as.numeric(difftime(httr::parse_http_date(header), Sys.time(), units = 'secs'))
    }
    if(is.na(seconds)){
        return(NULL)
    }
    max(round(seconds), 1)
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
