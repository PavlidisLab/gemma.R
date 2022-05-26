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
    fargs$memoised <- quote(getOption("gemma.memoise", FALSE))
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
    assign(memF, memoise::memoise(f), where)

    if (!exists("forgetGemmaMemoised", envir = where, inherits = FALSE)) {
        assign("forgetGemmaMemoised", function() {}, envir = where)
    }

    forgetMe <- get("forgetGemmaMemoised", envir = where, inherits = FALSE)

    body(forgetMe) <- body(forgetMe) %>%
        as.list() %>%
        append(str2expression(glue::glue("memoise::forget({memF})"))) %>%
        as.call()

    assign("forgetGemmaMemoised", forgetMe, envir = where, inherits = FALSE)

    if (!is.null(document)) {
        # cat(glue::glue("#' {fname}\n"), file = document, append = T)
        comment(fname, roxygen, names(fargs), document)
        if (internal == TRUE) {
            cat(glue::glue("#' @keywords internal\n#' \n#' @examples\n\n"), file = document, append = TRUE)
        } else {
            cat(glue::glue("#' @export\n#'\n#' @keywords {keyword}\n#' \n#' @examples\n\n"), file = document, append = TRUE)
        }
        cat(paste0("#' ", mExamples$example[[fname]], "\n") %>% paste0(collapse = ""), file = document, append = TRUE)
        cat(glue::glue("{fname} <- "), file = document, append = TRUE)
        cat(deparse(f) %>% paste0(collapse = "\n"), file = document, append = TRUE)
        cat("\n\n", file = document, append = TRUE)
        cat(glue::glue("#' Memoise {fname}\n#'\n#' @noRd\n\n"), file = document, append = TRUE)
        cat(glue::glue("mem{fname} <- memoise::memoise({fname}, cache = gemmaCache())"), file = document, append = TRUE)
        cat("\n\n", file = document, append = TRUE)
    }
}



#' Register a simple endpoint (ie. one that accepts no parameters) (internal use)
#'
#' @param root The type of endpoint (ie. dataset, platform, gene)
#' @param query The last part of the endpoint URL
#' @param fname The name of the function to create
#' @param preprocessor The preprocessing function to run on the output
#' @param validator The validator to run on the input. Defaults to validateSingleID
#' @param logname The activating phrase in the category endpoint
#' @param roxygen The name to pull roxygen information from
#' @param keyword The category keyboard for use in documentation
#' @param internal Whether the endpoint will be exposed to users
#' @param where The environment to add the new function to
#' @param plural If equal to FALSE (the default), assumes that this endpoint is pluralized by adding an "s". Otherwise, you can override this behavior by specifying the desired plural form.
#' @param document A file to print information for pasting generating the package
#' @param isFile Whether the endpoint is expected to return a gzipped file or not
#' @param header Specific HTTP header for the request
registerSimpleEndpoint <- function(root, query, fname, preprocessor, validator = NULL,
                                   logname = fname, roxygen = NULL, keyword = root,
                                   internal = FALSE, where = parent.env(environment()),
                                   plural = FALSE, document = getOption("gemmaAPI.document", "R/allEndpoints.R"),
                                   isFile = FALSE, header = "") {
    registerEndpoint(
        ifelse(plural == FALSE, glue::glue('{ifelse(endsWith(root, "s"), root, paste0(root, "s"))}/{{{root}}}/{query}'),
               glue::glue("{root}/{{{plural}}}")
        ),
        fname,
        defaults = setNames(NA_character_, root),
        validators = switch(is.null(validator) + 1,
                            validator,
                            alist(validateSingleID) %>% `names<-`(root)
        ),
        logname = logname,
        roxygen = roxygen,
        keyword = keyword,
        internal = internal,
        preprocessor = preprocessor,
        where = where,
        document = document,
        isFile = isFile,
        header = header
    )
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
    } else {
        mName <- xml2::xml_attr(node, ":name")
        mDesc <- xml2::xml_attr(node, ":description")
        mResp <- get(xml2::xml_attr(node, ":response-description"))
    }

    cat(glue::glue("#' {pandoc(mName %>% { substring(., 2, nchar(.) - 1) })}\n#'"), file = document, append = TRUE)
    cat(glue::glue("\n\n#' {pandoc(mDesc %>% { substring(., 2, nchar(.) - 1) })}\n#'\n\n"), file = document, append = TRUE)

    for (arg in parameters) {
        if (arg == "raw") {
            mAdd <- "<p><code>TRUE</code> to receive results as-is from Gemma, or <code>FALSE</code> to enable parsing. Raw results usually contain additional fields and flags that are omitted in the parsed results.</p>"
        } else if (arg == "memoised") {
            mAdd <- "<p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>"
        } else if (arg == "file") {
            mAdd <- "<p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>"
        } else if (arg == "overwrite") {
            mAdd <- "<p>Whether or not to overwrite if a file exists at the specified filename.</p>"
        } else if (arg == "request") {
            mAdd <- "<p>Which specific endpoint to request.</p>"
        } else if (arg == "taxon") {
            mAdd <- "<p>Not required, part of the URL path. can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers: scientific name, common name.<p>"
        } else if (arg == "...") {
            mAdd <- "<p>Parameters to forward to the endpoint selected in <code>request</code>.</p>"
        } else if (arg == "filter") {
            mAdd <- "<p>The filtered version (<code>filter = TRUE</code>) corresponds to what is used in most Gemma analyses, removing some probes/elements. Unfiltered includes all elements.<p>"
        } else if (arg == "excludeResults") {
            mAdd <- "<p>Only keep factor values and exclude numerical results from resultSets.<p>"
        } else if (arg == "limit") {
            mAdd <- "<p>Optional, defaults to 20. Limits the result to specified amount of objects.<p>"
        } else if (arg == "resultSet") {
            mAdd <- "<p>Optional, defaults to empty. A single resultSet identifier (ex. 423176)<p>"
        } else {
            mArg <- arg
            if (arg == "threshold") {
                mArg <- "diffExThreshold"
            } else if (arg == "element") {
                mArg <- "probes"
            } else if (arg == "with") {
                mArg <- "geneWith"
            } else if (arg == "start") {
                mArg <- "nuclStart"
            } else if (arg == "size") {
                mArg <- "nuclSize"
            } else if (arg == "query") mArg <- "search"

            mAdd <- get(paste0(mArg, "Description"))
        }

        cat(glue::glue("#' @param {arg} {pandoc(mAdd)}\n\n"), file = document, append = TRUE)
    }

    cat(glue::glue("#'\n#' @return {pandoc(mResp)}\n\n"), file = document, append = TRUE)
}

