
# adapted from
# https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html
#' Roxygen table maker
#'
#' Use to create tables to fit inside documentation
#'
#' @param df data.frame
#' @param col.names logical. If colnames should be included
#' @param ... variables for format function
#'
roxygenTabular <- function(df,col.names= TRUE,  ...) {
    stopifnot(is.data.frame(df))

    align <- function(x) if (is.numeric(x)) "r" else "l"
    col_align <- vapply(df, align, character(1))

    if(col.names){
        df = rbind(paste0('\\strong{',colnames(df),'}'),df)
    }

    cols <- lapply(df, format, ...)
    contents <- do.call("paste",
                        c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))

    paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
          contents, "\n}\n", sep = "")
}

#' Parse open api parameters
#' 
#' @param prm A parameter taken from an openAPI endpoint description
parse_open_api_params <- function(prm){
    if (!is.null(prm$schema$description)){
        out = prm$schema$description
    } else if(!is.null(prm$schema$items$oneOf)){
        out = prm$schema$items$oneOf %>% 
            purrr::map_chr('description') %>% 
            gsub('.','',.,fixed=  TRUE) %>% paste(collapse = ' or ') %>% snakecase::to_sentence_case()
    } else if(!is.null(prm$schema$oneOf)){
        out = prm$schema$oneOf %>% purrr::map_chr('description') %>%  
            gsub('.','',.,fixed=  TRUE) %>% paste(collapse = ' or ') %>% 
            snakecase::to_sentence_case()
    } else if(!is.null(prm$schema$type)){
        out = prm$schema$type
    } else{
        browser()
        stop('help me!')
    }
    
    return(out)
}

#' Register an API endpoint (internal use)
#'
#' @param endpoint The API endpoint URL with parameters in glue formatting
#' @param fname The name of the function to create
#' @param preprocessor The preprocessing function to run on the output
#' @param defaults Default values for the endpoint
#' @param validators Validators for the inputs
#' @param logname The activating phrase in the category endpoint
#' @param keyword The category keyboard for use in documentation
#' @param internal Whether the endpoint will be exposed to users
#' @param where The environment to add the new function to
#' @param document A file to print information for pasting generating the package
#' @param isFile Whether the endpoint is expected to return a gzipped file or not
#' @param header Specific HTTP header for the request
registerEndpoint <- function(endpoint,
                             fname,
                             open_api_name = fname,
                             preprocessor,
                             compressibles = NULL,
                             defaults = NULL,
                             validators = NULL,
                             keyword = NULL,
                             internal = FALSE,
                             where = parent.env(environment()),
                             document = getOption("gemmaAPI.document", "R/allEndpoints.R"),
                             isFile = FALSE,
                             header = "") {
    if (missing(endpoint) || missing(fname) || missing(preprocessor)) {
        stop("Please specify an endpoint, function name and preprocessor.")
    }
    if (exists(fname, envir = where, inherits = FALSE)) {
        warning(glue::glue("{fname} already exists. Skipping."))
        return(NULL)
    }

    logEndpoint(fname, open_api_name)

    # Make sure arguments are URL encoded
    endpoint <- gsub("\\{([^\\}]+)\\}", "\\{encode\\(\\1\\)\\}", endpoint)

    f <- function() {}

    fargs <- alist()
    for (d in names(defaults)) {
        fargs[[d]] <- defaults[[d]]
    }

    fargs$raw <- quote(getOption("gemma.raw", FALSE))
    fargs$memoised <- quote(getOption("gemma.memoised", FALSE))
    fargs$file <- quote(getOption("gemma.file", NA_character_))
    fargs$overwrite <- quote(getOption("gemma.overwrite", FALSE))
    # fargs$attributes <- quote(getOption("gemma.attributes", TRUE))

    formals(f) <- fargs
    body(f) <- quote({
        .body(fname = fname, 
              validators = validators, 
              endpoint = endpoint, 
              envWhere = environment(), 
              isFile = isFile, header = header, 
              raw = raw, 
              overwrite = overwrite, 
              file = file, 
              attributes = TRUE,
              open_api_name = open_api_name,
              .call = match.call())
    })

    # here we create a call for the memoised version of the function that simply
    # passes the variables forward.
    memoise_args = names(fargs) %>% purrr::map_chr(function(x){
        if(x != 'memoised'){
            glue::glue('{x} = {x}')
        } else{
            glue::glue('memoised = FALSE')
        }
    }) %>% paste(collapse= ',\n')

    memoise_call = str2expression(glue::glue('if(memoised){
        if (!is.na(file)){
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() == "cache_in_memory"){
            return(mem_in_memory_cache("[fname]",[memoise_args]))
        } else{
            out <- mem[fname]([memoise_args])
            return(out)
        }
        
    }',.open = '[',.close = ']'))

    body(f) = body(f) %>%
        as.list() %>%
        append(memoise_call,1) %>%
        as.call()

    # Add our variables

    for (i in c("endpoint", "validators", "preprocessor", "fname", "isFile", "header", "keyword", "internal","open_api_name","compressibles")) {
        v <- paste0(glue::glue('{capture.output(dput(get(i)))}'),collapse = '\n')


        body(f) <- body(f) %>%
            as.list() %>%
            append(str2expression(glue::glue("{i} <- {v}")), 1) %>%
            as.call()
    }

    # And our memoised function
    environment(f) <- where

    # Make this function available in the parent environment...
    assign(fname, f, env = where)
    memF <- glue::glue("mem", fname)

    memo_fun = function(){}

    formals(memo_fun) = formals(f)
    body(memo_fun) <- body(memo_fun) %>%
        as.list() %>%
        append(str2expression(glue::glue(
            'mem_call<-memoise::memoise({fname}, cache = gemmaCache())
            mem_call({memoise_args})'
        ))) %>% as.call()


    assign(memF, memoise::memoise(f), where)

    if (!exists("forget_gemma_memoised", envir = where, inherits = FALSE)) {
        assign("forget_gemma_memoised", function() {}, envir = where)
    }


    if (!is.null(document)) {
        # cat(glue::glue("#' {fname}\n"), file = document, append = T)
        comment(fname = fname,
                open_api_name = open_api_name,
                parameters = names(fargs), 
                document = document)
        if (internal == TRUE) {
            cat(glue::glue("#' @keywords internal\n#' \n#' @examples\n\n"), file = document, append = TRUE)
        } else {
            cat(glue::glue("#' @export\n#'\n#' @keywords {keyword}\n#' \n#' @examples\n\n"), file = document, append = TRUE)
        }

        overrides[[fname]]$tags %>% lapply(class) %>% sapply(function(x){
            any(x %in% 'roxy_tag_examples')
        }) -> example_override
        if(any(example_override)){
            assertthat::assert_that(sum(example_override) == 1)
            val = overrides[[fname]]$tags[[which(example_override)]]$raw %>% strsplit('\n') %>% {.[[1]]}
            val = val[val!=""]
            cat(paste0("#' ", val, "\n") %>% paste0(collapse = ""), file = document, append = TRUE)
        }
        cat(glue::glue("{fname} <- "), file = document, append = TRUE)
        cat(deparse(f) %>% paste0(collapse = "\n"), file = document, append = TRUE)
        cat("\n\n", file = document, append = TRUE)
        cat(glue::glue("#' Memoise {fname}\n#'\n#' @noRd\n\n"), file = document, append = TRUE)
        cat(glue::glue("mem{fname} <-"), file = document, append = TRUE)
        cat(deparse(memo_fun) %>% paste0(collapse = "\n"), file = document, append = TRUE)
        cat("\n\n", file = document, append = TRUE)
    }
}



#' Log an endpoint for the currently active category endpoint
#'
#' @param fname The function name to call
#' @param logname The activating phrase
logEndpoint <- function(fname, logname) {
    options(gemmaAPI.logged = c(getOption("gemmaAPI.logged"), setNames(fname, logname)))
}


clearLog <- function(){
    options(gemmaAPI.logged = c())
}

getLog <- function(){
    getOption('gemmaAPI.logged')
}

#' Comment a function
#'
#' @param fname The name of the function to comment
#' @param open_api_name of the endpoint in openAPI
#' @param parameters The parameters that the function accepts
#' @param document A file to print information for pasting generating the package
comment <- function(fname, open_api_name = fname, parameters, document = getOption("gemmaAPI.document", "R/allEndpoints.R")) {
    pandoc <- function(text) {
        tmp <- tempfile()
        write(text, tmp)
        ret <- system2(paste0(Sys.getenv("RSTUDIO_PANDOC"), "/pandoc"), c("-f html", "-t markdown", tmp), stdout = TRUE)
        unlink(tmp)
        gsub("\n#' \n#' ", "\n#' ", gsub("\n", "\n#' ", paste0(ret, collapse = "\n"), fixed = TRUE), fixed = TRUE) %>%
            {
                # Fix badly formatted URLs (from unescaping []), remove unsupported glyphicons and unescape
                gsub("\\[\\[([^\\]]+)\\]\\]", "\\[\\1\\]", gsub("\\[\\]\\{\\.glyphicon[^\\}]+\\} ", "", gsub("\\", "", ., fixed = TRUE)), perl = TRUE)
            } %>%
            {
                # Fix multiline URLs
                gsub("\\[(.*)\n#' ([^\\]]+)\\]\\(([^\\)]+)\\)", "[\\1 \\2](\\3)", ., perl = TRUE)
            }
    }


    mName <- paste0(fname)
    mDesc <- ''
    mResp <- "Varies"
    return = glue::glue("#'\n#' @return {pandoc(mResp)}\n\n")
    


    if(open_api_name %in% api_file_fun_names){
        endpoint <- api_file$paths[[which(api_file_fun_names %in% open_api_name)]]
        # mDesc <- endpoint$get$summary
        mName <- endpoint$get$summary
        # mName <- endpoint$get$operationId %>% snakecase::to_sentence_case()
    }
    
    overrides[[fname]]$tags %>% lapply(class) %>% sapply(function(x){
        any(x %in% 'roxy_tag_description')
    }) -> description_override
    
    if(any(description_override)){
        assertthat::assert_that(sum(description_override)==1)
        mDesc <- overrides[[fname]]$tags[[which(description_override)]]$val %>% stringr::str_replace_all('\n',"\n#' ")
    }
    
    
    # documentation overrides
    # uses examples file as an override if provided

    overrides[[fname]]$tags %>% lapply(class) %>% sapply(function(x){
        any(x %in% 'roxy_tag_return')
    }) -> return_override

    if (any(return_override)){
        assertthat::assert_that(sum(return_override)==1)
        val = overrides[[fname]]$tags[[which(return_override)]]$val %>% stringr::str_replace_all('\n',"\n#' ")
        return = glue::glue("#'\n#' @return {val}\n\n")

    }
    
    # @inherit tag only works for return for now
    overrides[[fname]]$tags %>% lapply(class) %>% sapply(function(x){
        any(x %in% 'roxy_tag_inherit')
    }) -> inherit_override
    
    if(any(inherit_override) && !any(return_override)){
        assertthat::assert_that(sum(inherit_override)==1)
        val = overrides[[fname]]$tags[[which(inherit_override)]]$val
        return = glue::glue("#'\n#' @inherit {val$source} {val$fields}\n\n")
    }

    cat(glue::glue("#' {mName}\n#'"), file = document, append = TRUE)
    cat(glue::glue("\n\n#' {mDesc}\n#'\n\n"), file = document, append = TRUE)
    
    overrides[[fname]]$tags %>% lapply(class) %>% sapply(function(x){
        any(x %in% 'roxy_tag_details')
    }) -> details_override
    
    if(any(details_override)){
        assertthat::assert_that(sum(details_override)==1)
        val = overrides[[fname]]$tags[[which(details_override)]]$val
        val = val %>% strsplit('\n') %>% {.[[1]]} %>% paste(collapse = "\n#' ")
        cat(glue::glue("#' @details {val}\n#'\n\n"), file = document, append = TRUE)
    }


    overrides[[fname]]$tags %>% lapply(class) %>% sapply(function(x){
        any(x %in% 'roxy_tag_param')
    }) -> param_override
    overridden_params = overrides[[fname]]$tags[param_override] %>% sapply(function(x){
        x$val$name
    })

    overrides[['generic_params']]$tags %>% lapply(class) %>% sapply(function(x){
        any(x %in% 'roxy_tag_param')
    }) -> generic_param_override
    generic_overriden_params = overrides[['generic_params']]$tags[generic_param_override] %>% sapply(function(x){
        x$val$name
    })

    for (arg in parameters) {
        if (arg %in% overridden_params){
            mAdd <- overrides[[fname]]$tags[param_override][[which(overridden_params%in%arg)]]$val$description %>% stringr::str_replace_all('\n',"\n#' ")
        } else if (arg %in% generic_overriden_params){
            mAdd <- overrides$generic_params$tags[generic_param_override][[which(generic_overriden_params%in%arg)]]$val$description %>% stringr::str_replace_all('\n',"\n#' ")
        } else if((open_api_name %in% api_file_fun_names) && (arg %in% purrr::map_chr(endpoint$get$parameters ,'name'))){
            prm = endpoint$get$parameters[[which(purrr::map_chr(endpoint$get$parameters ,'name') %in% arg)]]
            mAdd = parse_open_api_params(prm)
        } else {
            mAdd <- ''
        }
        param = glue::glue("#' @param {arg} {mAdd}\n\n")


        cat(param, file = document, append = TRUE)
    }

    cat(return, file = document, append = TRUE)
}

