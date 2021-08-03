# -------------------------------
# You should define all endpoints in this file. This ensures everything is uniform
# and prevents you from rewriting boilerplate.
# To package the wrapper, just source this file after you're done making changes. Functions will be written to allEndpoints.R
# -------------------------------

library(magrittr)
library(async)

if (!file.exists("R/aaa-async.R")) {
  synchronise(http_get("https://raw.githubusercontent.com/r-lib/pkgcache/master/R/aaa-async.R", file = "R/aaa-async.R"))
}

if (file.exists(getOption("gemmaAPI.document", "R/allEndpoints.R"))) {
  file.remove(getOption("gemmaAPI.document", "R/allEndpoints.R"))
}

file.create(getOption("gemmaAPI.document", "R/allEndpoints.R"))

#' Register an API endpoint (internal use)
#'
#' @param endpoint The API endpoint URL with parameters in glue formatting
#' @param fname The name of the function to create
#' @param preprocessor The preprocessing function to run on the output
#' @param defaults Default values for the endpoint
#' @param validators Validators for the inputs
#' @param logname The activating phrase in the category endpoint
#' @param roxygen The name to pull roxygen information from
#' @param where The environment to add the new function to
#' @param document A file to print information for pasting generating the package
#' @param isFile Whether the endpoint is expected to return a gzipped file or not
registerEndpoint <- function(endpoint,
                             fname,
                             preprocessor,
                             defaults = NULL,
                             validators = NULL,
                             logname = fname,
                             roxygen = NULL,
                             where = parent.env(environment()),
                             document = getOption("gemmaAPI.document", "R/allEndpoints.R"),
                             isFile = FALSE) {
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
  fargs$async <- quote(getOption("gemma.async", FALSE))
  fargs$memoised <- quote(getOption("gemma.memoise", FALSE))
  fargs$file <- quote(getOption("gemma.file", NA_character_))
  fargs$overwrite <- quote(getOption("gemma.overwrite", FALSE))

  formals(f) <- fargs
  body(f) <- quote({
    .body(memoised, fname, validators, endpoint, environment(), isFile, raw, overwrite, file, async, match.call())
  })

  # Add our variables
  for (i in c("endpoint", "validators", "preprocessor", "fname", "isFile")) {
    if (is.character(get(i))) {
      v <- glue::glue('"{get(i)}"')
    } else if (is.list(get(i))) {
      v <- get(i) %>% {
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
    cat(glue::glue("#' @export\n#'\n#' @keywords {getOption('gemmaAPI.loggingCharacter', 'misc')}\n#' \n#' @examples\n\n"), file = document, append = TRUE)
    cat(paste0("#' ", mExamples$example[[fname]], '\n') %>% paste0(collapse = ''), file = document, append = TRUE)
    cat(glue::glue("{fname} <- "), file = document, append = TRUE)
    cat(deparse(f) %>% paste0(collapse = "\n"), file = document, append = TRUE)
    cat("\n\n", file = document, append = TRUE)
    cat(glue::glue("#' Memoise {fname}\n#'\n#' @keywords internal\n\n"), file = document, append = TRUE)
    cat(glue::glue("mem{fname} <- memoise::memoise({fname})"), file = document, append = TRUE)
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
#' @param where The environment to add the new function to
#' @param plural If equal to FALSE (the default), assumes that this endpoint is pluralized by adding an "s". Otherwise, you can override this behavior by specifying the desired plural form.
#' @param document A file to print information for pasting generating the package
#' @param isFile Whether the endpoint is expected to return a gzipped file or not
registerSimpleEndpoint <- function(root, query, fname, preprocessor, validator = NULL,
                                   logname = fname, roxygen = NULL, where = parent.env(environment()),
                                   plural = FALSE, document = getOption("gemmaAPI.document", "R/allEndpoints.R"),
                                   isFile = FALSE) {
  registerEndpoint(ifelse(plural == FALSE, glue::glue('{ifelse(endsWith(root, "s"), root, paste0(root, "s"))}/{{{root}}}/{query}'),
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
  preprocessor = preprocessor,
  where = where,
  document = document,
  isFile = isFile
  )
}

#' A compound endpoint is one that makes multiple API calls and composes the response (internal use)
#'
#' @param endpoints A list of endpoints (function names from @seealso registerSimpleEndpoint or @seealso registerEndpoint)
#' @param depends A list of indices (corresponding to entries in @param endpoints) which this endpoint depends on
#' @param passthrough A list of variable names to take and passthrough (names = new name, value = fetched value)
#' @param fname The name of the function to create
#' @param preprocessors The preprocessing functions to run on the output (same order as endpoints)
#' @param defaults Default values for the endpoint
#' @param validators Validators for the inputs
#' @param logname The activating phrase in the category endpoint
#' @param roxygen The name to pull roxygen information from
#' @param where The environment to add the new function to
#' @param document A file to print information for pasting generating the package
#' @param isFile Whether the endpoint is expected to return a gzipped file or not
registerCompoundEndpoint <- function(endpoints, depends, passthrough,
                                     fname, logname = fname,
                                     roxygen = NULL,
                                     where = parent.env(environment()), document = getOption("gemmaAPI.document", "R/allEndpoints.R")) {
  if (missing(endpoints) || missing(fname) || missing(depends) || missing(passthrough)) {
    stop("Please specify endpoints, depends, passthrough and a function name.")
  }
  if (exists(fname, envir = where, inherits = FALSE)) {
    warning(glue::glue("{fname} already exists. Skipping."))
    return(NULL)
  }

  logEndpoint(fname, logname)

  f <- function() {}

  fargs <- alist()
  for (d in endpoints) {
    fargs <- append(fargs, formals(get(d, envir = where, inherits = FALSE))) %>% .[!duplicated(names(.))]
  }

  # Remove any arguments that are listed as passthroughs
  fargs[lapply(passthrough, names) %>%
    unlist() %>%
    unname() %>%
    unique()] <- NULL

  # Make sure these are the last arguments
  fargs[c("raw", "async", "memoised", "file", "overwrite")] <- NULL
  fargs$raw <- quote(getOption("gemma.raw", FALSE))
  fargs$async <- quote(getOption("gemma.async", FALSE))
  fargs$memoised <- quote(getOption("gemma.memoise", FALSE))
  fargs$file <- quote(getOption("gemma.file", NA_character_))
  fargs$overwrite <- quote(getOption("gemma.overwrite", FALSE))

  formals(f) <- fargs
  body(f) <- quote({
    env <- environment()

    makeCall <- async(function(i) {
      .fargs <- formals(get(endpoints[i]))
      mCallable <- call(endpoints[i])

      for (f in names(.fargs)) {
        mCallable[[f]] <- .fargs[[f]]
      }
      for (f in intersect(names(.fargs), ls(envir = env))) {
        mCallable[[f]] <- get(f, envir = env, inherits = FALSE)
      }
      mCallable[c("raw", "async")] <- c(FALSE, TRUE)
      mCallable[setdiff(names(mCallable), names(.fargs))] <- NULL

      if (isTRUE(any(depends == i))) {
        eval(mCallable, env)$then(function(response) {
          # Assign passthroughs
          for (p in names(passthrough[[i]])) {
            assign(p, response[[passthrough[[i]][p]]], envir = env)
          }

          async_map(which(depends == i), makeCall)
        })
      } else {
        eval(mCallable, env)
      }
    })

    # Start by sending requests that don't depend on other endpoints
    request <- async(function() {
      async_map(which(is.na(depends)), function(i) {
        makeCall(i)
      })
    })

    if (!async) {
      synchronise(when_all(request()))
    } else {
      request()
    }
  })

  # Add our variables
  for (i in c("endpoints", "depends", "passthrough", "fname")) {
    if (is.character(get(i))) {
      if (length(get(i)) > 1) {
        v <- get(i) %>% {
          paste0("c(", paste0(paste0('"', ., '"'), collapse = ", "), ")")
        }
      } else {
        v <- glue::glue('"{get(i)}"')
      }
    } else if (is.list(get(i))) {
      if (!is.null(names(get(i)))) {
        if (any(lapply(get(i), names) %>% lapply(is.null) %>% unlist())) {
          listContents <- paste0(names(get(i)), " = ", paste0("c(", lapply(get(i), names) %>% {
            mapply(function(x, y) {
              if (is.null(y)) {
                y <- "NULL"
              } else if (is.na(y)) {
                y <- "NA"
              } else if (is.character(y)) {
                y <- paste0('"', y, '"')
              }

              if (!is.null(x)) {
                paste0(x, " = ", y, collapse = ", ")
              } else {
                paste0(y, collapse = ", ")
              }
            }, ., get(i))
          }, ")"), collapse = ", ")
        } else {
          listContents <- paste0(names(.), " = ", ., collapse = ", ")
        }
      }
      v <- get(i) %>% {
        paste0("list(", listContents, ")")
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

  if (!is.null(document)) {
    # cat(glue::glue("#' {fname}\n"), file = document, append = T)
    comment(fname, roxygen, names(fargs), document)
    cat(glue::glue("#' @export\n#'\n#' @keywords {getOption('gemmaAPI.loggingCharacter', 'misc')}\n#' \n#' @examples\n\n"), file = document, append = TRUE)
    cat(paste0("#' ", mExamples$example[[fname]], '\n') %>% paste0(collapse = ''), file = document, append = TRUE)
    cat(glue::glue("{fname} <- "), file = document, append = TRUE)
    cat(deparse(f) %>% paste0(collapse = "\n"), file = document, append = TRUE)
    cat("\n\n", file = document, append = TRUE)
  }
}

#' Log an endpoint for the currently active category endpoint (@seealso registerCategoryEndpoint)
#'
#' @param fname The function name to call
#' @param logname The activating phrase
logEndpoint <- function(fname, logname) {
  options(gemmaAPI.logged = c(getOption("gemmaAPI.logged"), setNames(fname, logname)))
}

#' Register a category of endpoints that will expose multiple endpoint calls through a single function (internal use)
#'
#' @param fname The name of the category endpoint, or `NULL` to finish logging
#' @param characteristic The characteristic required parameter for this category
#' @param where The environment to add the new function to
#' @param document A file to print information for pasting generating the package
#' @param roxygen A description to use for the function
registerCategoryEndpoint <- function(fname = NULL, characteristic = NULL,
                                     where = parent.env(environment()),
                                     document = getOption("gemmaAPI.document", "R/allEndpoints.R"),
                                     roxygen = NULL) {
  if (is.null(fname)) {
    if (is.null(getOption("gemmaAPI.logging", NULL))) {
      stop("No categories were being logged")
    }

    fname <- getOption("gemmaAPI.logging")
    characteristic <- getOption("gemmaAPI.loggingCharacter")
    options(gemmaAPI.logging = NULL, gemmaAPI.loggingCharacter = NULL)

    f <- function() {}

    # Default behavior is to call the first registration if no request is passed
    fargs <- alist()
    fargs[[characteristic]] <- NA_character_
    fargs$request <- NA_character_
    fargs$`...` <- alist(... = )$...

    fargs$raw <- quote(getOption("gemma.raw", FALSE))
    fargs$async <- quote(getOption("gemma.async", FALSE))
    fargs$memoised <- quote(getOption("gemma.memoise", FALSE))
    fargs$file <- quote(getOption("gemma.file", NA_character_))
    fargs$overwrite <- quote(getOption("gemma.overwrite", FALSE))

    formals(f) <- fargs
    body(f) <- quote({
      if (!is.na(request) && !(request %in% names(argMap))) {
        stop(paste0("Invalid request parameter. Options include: ", paste0(names(argMap), collapse = ", ")))
      }

      if (is.na(request)) request <- 1

      mCallable <- call(argMap[[request]], raw = raw, async = async, memoised = memoised, file = file, overwrite = overwrite)
      mCallable[[characteristicValue]] <- if (exists(characteristicValue, inherits = FALSE)) { # TODO maybe this inserts the name incorrectly
        get(characteristicValue)
      } else if (exists(paste0(characteristicValue, "s"), inherits = FALSE)) {
        get(paste0(characteristicValue, "s"))
      } else if (characteristicValue == "taxon" && exists("taxa", inherits = FALSE)) {
        get("taxa", inherits = FALSE)
      }
      for (i in names(list(...))) {
        mCallable[[i]] <- list(...)[[i]]
      }

      eval(mCallable, envir = parent.env(environment()))
    })

    argMap <- getOption("gemmaAPI.logged") %>% {
      paste0("c(", paste0(names(.), ' = "', ., '"', collapse = ", "), ")")
    }

    # Add the argument map
    body(f) <- body(f) %>%
      as.list() %>%
      append(str2expression(glue::glue("argMap <- {argMap}")), 1) %>%
      append(str2expression(glue::glue('characteristicValue <- "{characteristic}"')), 1) %>%
      as.call()

    environment(f) <- where

    if (!is.null(document)) {
      comment(fname, roxygen, names(fargs), document)
      cat(glue::glue("#' @export\n#'\n#' @keywords {characteristic}\n#'\n#' @examples\n\n"), file = document, append = TRUE)
      cat(paste0("#' ", mExamples$example[[fname]], '\n') %>% paste0(collapse = ''), file = document, append = TRUE)
      cat(glue::glue("{fname} <- "), file = document, append = TRUE)
      cat(deparse(f) %>% paste0(collapse = "\n"), file = document, append = TRUE)
      cat("\n\n", file = document, append = TRUE)
    }
  } else {
    options(gemmaAPI.logging = fname, gemmaAPI.loggingCharacter = characteristic, gemmaAPI.logged = NULL)
  }
}

# Documentation ----
`+` <- function(A, B) {
  paste0(A, B, collapse = "")
}

# Load in descriptions from the JS
eval(synchronise(http_get("https://gemma.msl.ubc.ca/resources/restapidocs/js/vue/descriptions.js"))$content %>%
  rawToChar() %>%
  {
    gsub("\\/\\*[\\s\\S]*?\\*\\/", "", ., perl = TRUE)
  } %>%
  {
    gsub("(\n(var )?)", "", .)
  } %>%
  {
    parse(text = .)
  })

rm(`+`)

# And endpoints from the HTML
endpoints <- synchronise(http_get("https://gemma.msl.ubc.ca/resources/restapidocs/"))$content %>%
  rawToChar() %>%
  xml2::read_html() %>%
  xml2::xml_find_all(".//endpoint")

examples <- readLines('examples.R') %>% { gsub('# ', '', .) }
mExamples <- list(example = list(), value = list())
n <- 1
mTitle <- NULL
for(i in examples) {
  if(i == '')
    mTitle <- NULL
  else if(i %in% c('example', 'value'))
    n <- i
  else {
    if(is.null(mTitle))
      mTitle <- i
    else {
      mExamples[[n]][[mTitle]] <- c(mExamples[[n]][[mTitle]], i)
    }
  }
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
      mAdd <- "<p><code>TRUE</code> to receive results as-is from Gemma, or <code>FALSE</code> to enable parsing.</p>"
    } else if (arg == "async") {
      mAdd <- "<p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>"
    } else if (arg == "memoised") {
      mAdd <- "<p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>"
    } else if (arg == "file") {
      mAdd <- "<p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>"
    } else if (arg == "overwrite") {
      mAdd <- "<p>Whether or not to overwrite if a file exists at the specified filename.</p>"
    } else if (arg == "request") {
      mAdd <- "<p>Which specific endpoint to request.</p>"
    } else if (arg == "...") {
      mAdd <- "<p>Parameters to forward to the endpoint selected in <code>request</code>.</p>"
    } else if (arg == "resultSet") {
      mAdd <- "Optional, defaults to empty. A single resultSet identifier (ex. 423176). Only datasets that user has access to will be available."
    } else {
      mArg <- arg
      if (arg == "component") {
        mArg <- "pcaComponent"
      } else if (arg == "threshold") {
        mArg <- "diffExThreshold"
      } else if (arg == "keepNonSpecific") {
        mArg <- "datasetsExpressSpec"
      } else if (arg == "element") {
        mArg <- "probes"
      } else if (arg == "with") {
        mArg <- "geneWith"
      } else if (arg == "editableOnly") {
        mArg <- "editable"
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

# Dataset endpoints ----
registerCategoryEndpoint("datasetInfo", "dataset")

registerEndpoint("datasets/{datasets}?filter={filter}&offset={offset}&limit={limit}&sort={sort}",
  "getDatasets",
  logname = "datasets", roxygen = "Datasets",
  defaults = list(
    datasets = NA_character_,
    filter = NA_character_,
    offset = 0L,
    limit = 20L,
    sort = "+id"
  ),
  validators = alist(
    datasets = validateOptionalID,
    filter = validateFilter,
    offset = validatePositiveInteger,
    limit = validatePositiveInteger,
    sort = validateSort
  ),
  preprocessor = quote(processDatasets)
)

registerEndpoint("datasets/{datasets}/expressions/pca?component={component}&limit={limit}&keepNonSpecific={keepNonSpecific}&consolidate={consolidate}",
  "getDatasetPCA",
  logname = "PCA", roxygen = "Datasets pca component expression levels",
  defaults = list(
    datasets = NA_character_,
    component = 1L,
    limit = 100L,
    keepNonSpecific = FALSE,
    consolidate = NA_character_
  ),
  validators = alist(
    datasets = validateID,
    component = validatePositiveInteger,
    limit = validatePositiveInteger,
    keepNonSpecific = validateBoolean,
    consolidate = validateConsolidate
  ),
  preprocessor = quote(processExpression)
)

registerEndpoint("resultSets/{resultSets}?filter={filter}&offset={offset}&limit={limit}&sort={sort}",
  "getResultSets",
  logname = "resultSets", roxygen = "Lists resultSets filtered and organized by given parameters",
  defaults = list(
    resultSet = NA_character_,
    filter = NA_character_,
    offset = 0L,
    limit = 20L,
    sort = "+id"
  ),
  validators = alist(
    resultSet = validateOptionalID,
    filter = validateFilter,
    offset = validatePositiveInteger,
    limit = validatePositiveInteger,
    sort = validateSort
  ),
  preprocessor = quote(processResultSets)
)

registerEndpoint("datasets/{datasets}/expressions/differential?keepNonSpecific={keepNonSpecific}&diffExSet={diffExSet}&threshold={threshold}&limit={limit}&consolidate={consolidate}",
  "getDatasetDE",
  logname = "diffEx", roxygen = "Datasets differential expression levels",
  defaults = list(
    datasets = NA_character_,
    keepNonSpecific = FALSE,
    diffExSet = NA_integer_,
    threshold = 100.0,
    limit = 100L,
    consolidate = NA_character_
  ),
  validators = alist(
    datasets = validateID,
    keepNonSpecific = validateBoolean,
    diffExSet = validatePositiveInteger,
    threshold = validatePositiveReal,
    limit = validatePositiveInteger,
    consolidate = validateConsolidate
  ),
  preprocessor = quote(processExpression)
)

registerEndpoint("datasets/{dataset}/data?filter={filter}",
  "getDatasetData",
  logname = "data", roxygen = "Dataset data",
  isFile = TRUE,
  defaults = list(
    dataset = NA_character_,
    filter = FALSE
  ),
  validators = alist(
    dataset = validateID,
    filter = validateBoolean
  ),
  preprocessor = quote(processFile)
)

registerSimpleEndpoint("dataset", "samples",
  logname = "samples", roxygen = "Dataset samples",
  "getDatasetSamples",
  preprocessor = quote(processSamples)
)

registerSimpleEndpoint("dataset", "analyses/differential",
  logname = "differential", roxygen = "Dataset differential analysis",
  "getDatasetDEA",
  preprocessor = quote(processDEA)
)

registerSimpleEndpoint("dataset", "svd",
  logname = "SVD", roxygen = "Dataset SVD information",
  "getDatasetSVD",
  preprocessor = quote(processSVD)
)

registerSimpleEndpoint("dataset", "platforms",
  logname = "platforms", roxygen = "Dataset platforms",
  "getDatasetPlatforms",
  preprocessor = quote(processPlatforms)
)

registerSimpleEndpoint("dataset", "annotations",
  logname = "annotations", roxygen = "Dataset annotations",
  "getDatasetAnnotations",
  preprocessor = quote(processAnnotations)
)

registerSimpleEndpoint("dataset", "design",
  logname = "design", roxygen = "Dataset design",
  "getDatasetDesign", isFile = TRUE,
  preprocessor = quote(processFile)
)

# TODO Consider whether this belongs in the API or not...

# registerCompoundEndpoint(endpoints = c('getDatasetDEA', 'getDiffExData'),
#                         depends = list(getDatasetDEA = NA, getDiffExData = 1),
#                         passthrough = list(getDatasetDEA = c(diffExSet = 'analysis.ID'), getDiffExData = NULL),
#                         'getDiffExpr', logname = 'diffExData', roxygen = 'Calls @seealso getDatasetDEA and @seealso getDiffExprData to get the differential expression results for a given dataset')

registerCategoryEndpoint(roxygen = "A common entrypoint to the various dataset endpoints.")
# Platform endpoints ----
registerCategoryEndpoint("platformInfo", "platform")

registerEndpoint("platforms/{platforms}?filter={filter}&offset={offset}&limit={limit}&sort={sort}",
  "getPlatforms",
  logname = "platforms", roxygen = "Platforms",
  defaults = list(
    platforms = NA_character_,
    filter = NA_character_,
    offset = 0L,
    limit = 20L,
    sort = "+id"
  ),
  validators = alist(
    platforms = validateOptionalID,
    filter = validateFilter,
    offset = validatePositiveInteger,
    limit = validatePositiveInteger,
    sort = validateSort
  ),
  preprocessor = quote(processPlatforms)
)

registerEndpoint("platforms/{platform}/datasets?offset={offset}&limit={limit}",
  "getPlatformDatasets",
  logname = "datasets", roxygen = "Platform datasets",
  defaults = list(
    platform = NA_character_,
    offset = 0L,
    limit = 20L
  ),
  validators = alist(
    platform = validateSingleID,
    offset = validatePositiveInteger,
    limit = validatePositiveInteger
  ),
  preprocessor = quote(processDatasets)
)

registerEndpoint("platforms/{platform}/elements/{element}?offset={offset}&limit={limit}",
  "getPlatformElements",
  logname = "elements", roxygen = "Platform elements",
  defaults = list(
    platform = NA_character_,
    element = NA_character_,
    offset = 0L,
    limit = 20L
  ),
  validators = alist(
    platform = validateSingleID,
    element = validateOptionalID,
    offset = validatePositiveInteger,
    limit = validatePositiveInteger
  ),
  preprocessor = quote(processElements)
)

registerEndpoint("platforms/{platform}/elements/{element}/genes?offset={offset}&limit={limit}",
  "getPlatformElementGenes",
  logname = "genes", roxygen = "Platform element genes",
  defaults = list(
    platform = NA_character_,
    element = NA_character_,
    offset = 0L,
    limit = 20L
  ),
  validators = alist(
    platform = validateSingleID,
    element = validateSingleID,
    offset = validatePositiveInteger,
    limit = validatePositiveInteger
  ),
  preprocessor = quote(processGenes)
)

registerCategoryEndpoint(roxygen = "A common entrypoint to the various platform endpoints.")
# Gene endpoints ----
registerCategoryEndpoint("geneInfo", "gene")

registerSimpleEndpoint("genes", "",
  logname = "genes", roxygen = "Genes",
  "getGenes",
  validator = alist(genes = validateID),
  preprocessor = quote(processGenes)
)

registerSimpleEndpoint("gene", "evidence",
  logname = "evidence", roxygen = "Gene evidence",
  "getGeneEvidence",
  preprocessor = quote(processGeneEvidence)
)

registerSimpleEndpoint("gene", "locations",
  logname = "locations", roxygen = "Gene locations",
  "getGeneLocation",
  preprocessor = quote(processGeneLocation)
)

registerEndpoint("genes/{gene}/probes?offset={offset}&limit={limit}",
  "getGeneProbes",
  logname = "probes", roxygen = "Gene probes",
  defaults = list(
    gene = NA_character_,
    offset = 0L,
    limit = 20L
  ),
  validators = alist(
    gene = validateSingleID,
    offset = validatePositiveInteger,
    limit = validatePositiveInteger
  ),
  preprocessor = quote(processElements)
)

registerSimpleEndpoint("gene", "goTerms",
  logname = "goTerms", roxygen = "Gene goTerms",
  "getGeneGO",
  preprocessor = quote(processGO)
)

registerEndpoint("genes/{gene}/coexpression?with={with}&limit={limit}&stringency={stringency}",
  "getGeneCoexpression",
  logname = "coexpression", roxygen = "Gene coexpression",
  defaults = list(
    gene = NA_character_,
    with = NA_character_,
    limit = 20L,
    stringency = 1L
  ),
  validators = alist(
    gene = validateSingleID,
    with = validateSingleID,
    limit = validatePositiveInteger,
    stringency = validatePositiveInteger
  ),
  preprocessor = quote(processCoexpression)
)

registerCategoryEndpoint(roxygen = "A common entrypoint to the various gene endpoints.")
# Taxon endpoints ----
registerCategoryEndpoint("taxonInfo", "taxon")

registerSimpleEndpoint("taxa", "",
  logname = "taxa", roxygen = "Taxa",
  "getTaxa", plural = "taxa",
  validator = alist(taxa = validateOptionalTaxon),
  preprocessor = quote(processTaxon)
)

registerEndpoint("taxa/{taxon}/datasets?filter={filter}&offset={offset}&limit={limit}&sort={sort}",
  "getTaxonDatasets",
  logname = "datasets", roxygen = "Taxon datasets",
  defaults = list(
    taxon = NA_character_,
    filter = NA_character_,
    offset = 0L,
    limit = 20L,
    sort = "+id"
  ),
  validators = alist(
    taxon = validateSingleTaxon,
    filter = validateFilter,
    offset = validatePositiveInteger,
    limit = validatePositiveInteger,
    sort = validateSort
  ),
  preprocessor = quote(processDatasets)
)

registerEndpoint("taxa/{taxon}/phenotypes?editableOnly={editableOnly}&tree={tree}",
  "getTaxonPhenotypes",
  logname = "phenotypes", roxygen = "Taxon phenotypes",
  defaults = list(
    taxon = NA_character_,
    editableOnly = FALSE,
    tree = FALSE
  ),
  validators = alist(
    taxon = validateSingleTaxon,
    editableOnly = validateBoolean,
    tree = validateBoolean
  ),
  preprocessor = quote(processPhenotypes)
)

registerEndpoint("taxa/{taxon}/phenotypes/candidates?editableOnly={editableOnly}&phenotypes={phenotypes}",
  "getTaxonPhenotypeCandidates",
  logname = "phenoCandidateGenes", roxygen = "Taxon phenotypes candidate genes",
  defaults = list(
    taxon = NA_character_,
    editableOnly = FALSE,
    phenotypes = NA_character_
  ),
  validators = alist(
    taxon = validateSingleTaxon,
    editableOnly = validateBoolean,
    phenotypes = validateID
  ),
  preprocessor = quote(processGeneEvidence)
)

registerEndpoint("taxa/{taxon}/genes/{gene}",
  "getGeneOnTaxon",
  logname = "gene", roxygen = "Gene on specific taxon",
  defaults = list(
    taxon = NA_character_,
    gene = NA_character_
  ),
  validators = alist(
    taxon = validateSingleTaxon,
    gene = validateSingleID
  ),
  preprocessor = quote(processGenes)
)

registerEndpoint("taxa/{taxon}/genes/{gene}/evidence",
  "getEvidenceOnTaxon",
  logname = "geneEvidence", roxygen = "Gene evidence on specific taxon",
  defaults = list(
    taxon = NA_character_,
    gene = NA_character_
  ),
  validators = alist(
    taxon = validateSingleTaxon,
    gene = validateSingleID
  ),
  preprocessor = quote(processGeneEvidence)
)

registerEndpoint("taxa/{taxon}/genes/{gene}/locations",
  "getGeneLocationOnTaxon",
  logname = "geneLocation", roxygen = "Gene location on specific taxon",
  defaults = list(
    taxon = NA_character_,
    gene = NA_character_
  ),
  validators = alist(
    taxon = validateSingleTaxon,
    gene = validateSingleID
  ),
  preprocessor = quote(processGeneLocation)
)

registerEndpoint("taxa/{taxon}/chromosomes/{chromosome}/genes?strand={strand}&start={start}&size={size}",
  "getGenesAtLocation",
  logname = "genesAtLocation", roxygen = "Genes at location",
  defaults = list(
    taxon = NA_character_,
    chromosome = NA_character_,
    strand = "+",
    start = NA_integer_,
    size = NA_integer_
  ),
  validators = alist(
    taxon = validateSingleTaxon,
    chromosome = validateSingleID,
    strand = validateStrand,
    start = validatePositiveInteger,
    size = validatePositiveInteger
  ),
  preprocessor = quote(processGenes)
)

registerEndpoint("annotations/{taxon}/search/{query}/datasets?filter={filter}&offset={offset}&limit={limit}&sort={sort}",
  "searchDatasets",
  logname = "datasets", roxygen = "Dataset search",
  defaults = list(
    query = NA_character_,
    taxon = NA_character_,
    filter = NA_character_,
    offset = 0L,
    limit = 0L,
    sort = "+id"
  ),
  validators = alist(
    query = validateQuery,
    taxon = validateOptionalTaxon,
    filter = validateFilter,
    offset = validatePositiveInteger,
    limit = validatePositiveInteger,
    sort = validateSort
  ),
  preprocessor = quote(processDatasets)
)

registerCategoryEndpoint(roxygen = "A common entrypoint to the various taxon endpoints.")

registerEndpoint("annotations/search/{query}",
  "searchAnnotations",
  roxygen = "Annotation search",
  defaults = list(query = NA_character_),
  validators = alist(query = validateQuery),
  preprocessor = quote(processAnnotations)
)

# Clean up
doFinalize <- function(document = getOption("gemmaAPI.document", "R/allEndpoints.R")) {
  cat("\n", file = document, append = TRUE)
  cat(glue::glue("#' Clear Gemma API cache\n\n"), file = document, append = TRUE)
  cat("#'\n", file = document, append = TRUE)
  cat("#' Forget past results from memoised calls to the Gemma API (ie. using functions with memoised = `TRUE`)\n#'\n", file = document, append = TRUE)
  cat("#' @export\n#'\n#' @keywords misc\n", file = document, append = TRUE)
  cat("forgetGemmaMemoised <- ", file = document, append = TRUE)
  cat(deparse(get("forgetGemmaMemoised", envir = globalenv(), inherits = FALSE)) %>% paste0(collapse = "\n"), file = document, append = TRUE)

  rm(list = ls(envir = globalenv(), all.names = TRUE), envir = globalenv())

  styler::style_file("./R/allEndpoints.R", transformers = biocthis::bioc_style())
  devtools::document()
  devtools::build(vignettes = FALSE)
  devtools::install()
}

doFinalize()
