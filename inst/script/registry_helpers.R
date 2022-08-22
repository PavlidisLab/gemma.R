
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


#' Register an API endpoint (internal use)
#'
#' @param endpoint The API endpoint URL with parameters in glue formatting
#' @param fname The name of the function to create
#' @param preprocessor The preprocessing function to run on the output
#' @param defaults Default values for the endpoint
#' @param validators Validators for the inputs
#' @param logname The activating phrase in the category endpoint
#' @param roxygen The name to pull roxygen information from
#' @param keyword The category keyboard for use in documentation
#' @param internal Whether the endpoint will be exposed to users
#' @param where The environment to add the new function to
#' @param document A file to print information for pasting generating the package
#' @param isFile Whether the endpoint is expected to return a gzipped file or not
#' @param header Specific HTTP header for the request
registerEndpoint <- function(endpoint,
                             fname,
                             preprocessor,
                             defaults = NULL,
                             validators = NULL,
                             logname = fname,
                             roxygen = NULL,
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

    logEndpoint(fname, logname)

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

    formals(f) <- fargs
    body(f) <- quote({
        .body(fname, validators, endpoint, environment(), isFile, header, raw, overwrite, file, match.call())
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
        out <- mem[fname]([memoise_args])
        return(out)
    }',.open = '[',.close = ']'))

    body(f) = body(f) %>%
        as.list() %>%
        append(memoise_call,1) %>%
        as.call()

    # Add our variables
    for (i in c("endpoint", "validators", "preprocessor", "fname", "isFile", "header", "keyword", "internal")) {
        if (is.character(get(i))) {
            v <- glue::glue('"{get(i)}"')
        } else if (is.list(get(i))) {
            v <- get(i) %>%
                {
                    paste0("list(", paste0(names(.), " = ", ., collapse = ", "), ")")
                }
        } else {
            v <- get(i)
        }

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

    if (!exists("forgetGemmaMemoised", envir = where, inherits = FALSE)) {
        assign("forgetGemmaMemoised", function() {}, envir = where)
    }


    if (!is.null(document)) {
        # cat(glue::glue("#' {fname}\n"), file = document, append = T)
        comment(fname, roxygen, names(fargs), document)
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
            val = overrides[[fname]]$tags[[which(example_override)]]$val %>% strsplit('\n') %>% {.[[1]]}
            cat(paste0("#' ", val, "\n") %>% paste0(collapse = ""), file = document, append = TRUE)
        } else{
            cat(paste0("#' ", mExamples$example[[fname]], "\n") %>% paste0(collapse = ""), file = document, append = TRUE)
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


#' Comment a function
#'
#' @param fname The name of the function to comment
#' @param src The name of the entry to use (in endpoints)
#' @param parameters The parameters that the function accepts
#' @param document A file to print information for pasting generating the package
comment <- function(fname, src, parameters, document = getOption("gemmaAPI.document", "R/allEndpoints.R")) {
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

    if (is.null(src)) {
        cat(glue::glue("#' {fname}\n"), file = document, append = TRUE)
        cat("\n", file = document, append = TRUE)
        return(NULL)
    }


    node <- Filter(function(elem) {
        xml2::xml_attr(elem, ":name") == paste0("'", src, "'")
    }, endpoints)
    if (length(node) == 0) {
        mName <- paste0("'", fname, "'")
        mDesc <- paste0("'", src, "'")
        mResp <- "Varies"
        return = glue::glue("#'\n#' @return {pandoc(mResp)}\n\n")
    } else {
        mName <- xml2::xml_attr(node, ":name")
        mDesc <- xml2::xml_attr(node, ":description")
        # this reads the descriptions environment defined in the global
        # scope. used to use global environment but wanted to clean that up
        # when debuggin -ogan
        mResp <- get(xml2::xml_attr(node, ":response-description"),descriptions)

        return = glue::glue("#'\n#' @return {pandoc(mResp)}\n\n")

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

    } else if(!is.null(mExamples$value[[fname]])){ # to be deprecated old example file
        # remove quotes from examples file if needed.
        val = gsub('^"|"$','',paste0(mExamples$value[[fname]],collapse = "\n#' "))
        return = glue::glue("#'\n#' @return {val}\n\n")
    }

    cat(glue::glue("#' {pandoc(mName %>% { substring(., 2, nchar(.) - 1) })}\n#'"), file = document, append = TRUE)
    cat(glue::glue("\n\n#' {pandoc(mDesc %>% { substring(., 2, nchar(.) - 1) })}\n#'\n\n"), file = document, append = TRUE)


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
            mAdd <- overrides[[fname]]$tags[generic_param_override][[which(generic_overriden_params%in%arg)]]$val$description %>% stringr::str_replace_all('\n',"\n#' ")
        }else if (arg == "raw") {
            mAdd <- "`TRUE` to receive results as-is from Gemma, or `FALSE` to enable parsing. Raw results usually contain additional fields and flags that are omitted in the parsed results."
        } else if (arg == "memoised") {
            mAdd <- "Whether or not to save to cache for future calls with the same inputs and use the result saved in cache if a result is already saved. Doing `options(gemma.memoised = TRUE)` will ensure that the cache is always used. Use \\code{\\link{forgetGemmaMemoised}} to clear the cache."
        } else if (arg == "file") {
            mAdd <- "The name of a file to save the results to, or `NULL` to not write results to a file. If `raw == TRUE`, the output will be a JSON file. Otherwise, it will be a RDS file."
        } else if (arg == "overwrite") {
            mAdd <- "Whether or not to overwrite if a file exists at the specified filename."
        } else if (arg == "request") {
            mAdd <- "Which specific endpoint to request."
        } else if (arg == "taxon") {
            mAdd <- "Not required, part of the URL path. can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers: scientific name, common name."
        } else if (arg == "...") {
            mAdd <- "Parameters to forward to the endpoint selected in `request`."
        } else if (arg == "filter") {
            mAdd <- "The filtered version (`filter = TRUE`) corresponds to what is used in most Gemma analyses, removing some probes/elements. Unfiltered includes all elements."
        } else if (arg == "excludeResults") {
            mAdd <- "Only keep factor values and exclude numerical results from resultSets."
        } else if (arg == "limit") {
            mAdd <- "Optional, defaults to 20. Limits the result to specified amount of objects."
        } else if (arg == "resultSet") {
            mAdd <- "Optional, defaults to empty. A single resultSet identifier (ex. 423176)"
        } else if (arg == 'element') {
            mAdd = "Required, part of the URL path. Can either be the probe name or ID."
        } else if (arg == 'elements'){
            mAdd = "Optional, defaults to empty. Limits the result to entities with given identifiers. A vector of identifiers (e.g: AFFX_Rat_beta-actin_M_at, AFFX_Rat_Hexokinase_M_at)"
        } else {
            mArg <- arg
            if (arg == "threshold") {
                mArg <- "diffExThreshold"
            } else if (arg == "with") {
                mArg <- "geneWith"
            } else if (arg == "start") {
                mArg <- "nuclStart"
            } else if (arg == "size") {
                mArg <- "nuclSize"
            } else if (arg == "query") mArg <- "search"

            # this reads the descriptions environment defined in the global
            # scope. used to use global environment but wanted to clean that up
            # when debuggin -ogan
            mAdd <- pandoc(get(paste0(mArg, "Description"),descriptions))
        }
        param = glue::glue("#' @param {arg} {mAdd}\n\n")


        cat(param, file = document, append = TRUE)
    }

    cat(return, file = document, append = TRUE)
}

