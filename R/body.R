#' Get gemma path
#' @return Link to Gemma API
#' @keywords internal
gemmaPath <- function(){
    getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/")
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
#' @param .call The original function call
#'
#' @noRd
.body <- function(fname, validators, endpoint, envWhere, isFile, header, raw, overwrite, file, attributes, .call) {

    # Set header
    if (header == "text/tab-separated-values") {
        names(header) <- "Accept"
    }
    envWhere$header <- header

    # Validate arguments
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v, envir = envWhere, inherits = FALSE), name = v), envir = envWhere)
        }
    }
    # Generate request
    call <- quote(paste0(gemmaPath(), gsub("/((NA)?/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA", "", glue::glue(endpoint)))))) %>% eval(envir = envWhere)

    if (!is.null(getOption('gemma.username')) && !is.null(getOption('gemma.password'))){
        requestExpr <- quote(httr::GET(
            call,
            c(httr::authenticate(getOption('gemma.username'),
                                 getOption("gemma.password")),
              httr::add_headers(header))))
    } else{
        requestExpr <- quote(httr::GET(
            call,
            httr::add_headers(header)))
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
                    response$content
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

        if (!is.null(file) && !is.na(file)) {
            extension <- ifelse(raw, ".json", ifelse(any(vapply(mOut, typeof, character(1)) == "list"), ".rds", ".csv"))
            if (isFile && raw){
                extension <- '.gz'
            }

            file <- paste0(tools::file_path_sans_ext(file), extension)

            if (file.exists(file) && !overwrite && !file.info(file)$isdir) {
                warning(file, " exists. Not overwriting.")
            } else {
                if (extension == ".json") {
                    write(jsonlite::toJSON(mOut, pretty = 2), file)
                } else if (extension == ".rds") {
                    saveRDS(mOut, file)
                } else if (extension == '.gz'){
                    writeBin(mOut,file)
                } else {
                    utils::write.csv2(mOut, file, row.names = FALSE)
                }
            }
        }
        mOut
    } else if (response$status_code == 403) {
        stop(response$status_code, ": Forbidden. You do not have permission to access this data.")
    } else if (response$status_code == 404) {
        stop(response$status_code, ": Not found. Ensure your parameters are spelled correctly and that you're querying an existing ID.")
    } else if (response$status_code == 500) {
        stop(response$status_code, ": Internal server error.")
    } else if (response$status_code == 503) {
        stop(response$status_code, ": Service Unavailable. Gemma might be under maintenance.")
    } else {
        stop("HTTP code ", response$status_code)
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
