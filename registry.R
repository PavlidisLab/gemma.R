# -------------------------------
# You should define all endpoints in this file. This ensures everything is uniform
# and prevents you from rewriting boilerplate.
# To package the wrapper, just source this file after you're done making changes. Functions will be written to allEndpoints.R
# -------------------------------

library(data.table)
library(glue)
library(async)
library(dplyr)
library(jsonlite)
library(memoise)
library(xml2)

if(file.exists(getOption('gemmaAPI.document', 'R/allEndpoints.R')))
  file.remove(getOption('gemmaAPI.document', 'R/allEndpoints.R'))

file.create(getOption('gemmaAPI.document', 'R/allEndpoints.R'))

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
registerEndpoint <- function(endpoint,
                             fname,
                             preprocessor,
                             defaults = NULL,
                             validators = NULL,
                             logname = fname,
                             roxygen = NULL,
                             where = parent.env(environment()),
                             document = getOption('gemmaAPI.document', 'R/allEndpoints.R')) {
  if(missing(endpoint) || missing(fname) || missing(preprocessor))
    stop('Please specify an endpoint, function name and preprocessor.')
  if(exists(fname, envir = where, inherits = F)) {
    warning(glue::glue('{fname} already exists. Skipping.'))
    return(NULL)
  }

  logEndpoint(fname, logname)

  # Make sure arguments are URL encoded
  endpoint <- gsub('\\{([^\\}]+)\\}', '\\{encode\\(\\1\\)\\}', endpoint)

  f <- function() {}

  fargs <- alist()
  for(d in names(defaults)) {
    fargs[[d]] <- defaults[[d]]
  }
  # TODO consider enquoting these so that they would show up for users as getOption still rather than the evaluated version
  # Like this, we can change the defaults easily per-package but not easy for users to change their own defaults
  fargs$raw <- getOption('gemma.raw', F)
  fargs$async <- getOption('gemma.async', F)
  fargs$memoised <- getOption('gemma.memoise', F)
  fargs$file <- getOption('gemma.file', NA_character_)
  fargs$overwrite <- getOption('gemma.overwrite', F)

  formals(f) <- fargs
  body(f) <- quote({
    # Call a memoised version if applicable
    if(memoised) {
      newArgs <- as.list(match.call())[-1]
      newArgs$memoised <- F
      return(do.call(glue::glue('mem{fname}'), newArgs))
    }

    # Validate arguments
    if(!is.null(validators)) {
      for(v in names(validators)) {
        assign(v, eval(validators[[v]])(get(v), name = v))
      }
    }

    # Generate request
    endpoint <- paste0(getOption('gemma.API', 'https://gemma.msl.ubc.ca/rest/v2/'), gsub('/(NA|/)', '/', gsub('\\?[^=]+=NA', '\\?', gsub('&[^=]+=NA', '', glue::glue(endpoint)))))
    envWhere <- environment()

    request <- quote(http_get(endpoint, options = switch(is.null(getOption('gemma.password', NULL)) + 1,
                                                         list(userpwd = paste0(getOption('gemma.username'), ':', getOption('gemma.password'))),
                                                         list()))$then(function(response) {
      if(response$status_code == 200) {
        mData <- tryCatch({
          fromJSON(rawToChar(response$content))$data
        }, error = function(e) {
          message(paste0('Failed to parse ', response$type, ' from ', response$url))
          warning(e$message)
          NULL
        })

        if(raw || length(mData) == 0)
          mOut <- mData
        else
          mOut <- eval(preprocessor, envir = envWhere)(mData)

        if(!is.null(file) && !is.na(file) && file.exists(file)) {
          if(!overwrite)
            warning(paste0(file, ' exists. Not overwriting.'))
          else {
            if(raw)
              write(mOut, paste0(tools::file_path_sans_ext(file), '.json'))
            else
              saveRDS(mOut, paste0(tools::file_path_sans_ext(file), '.rds'))
          }
        }

        mOut
      } else
        response
    }))

    if(!async)
      synchronise(eval(request, envir = envWhere))
    else
      eval(request)
  })

  # Add our variables
  for(i in c('endpoint', 'validators', 'preprocessor', 'fname')) {
    if(is.character(get(i)))
      v <- glue::glue('"{get(i)}"')
    else if(is.list(get(i)))
      v <- get(i) %>% { paste0('list(', paste0(names(.), ' = ', ., collapse = ', '), ')') }
    else
      v <- get(i)

    body(f) <- body(f) %>% as.list %>%
      append(str2expression(glue::glue('{i} <- {v}')), 1) %>%
      as.call
  }

  # And our memoised function
  environment(f) <- where

  # Make this function available in the parent environment...
  assign(fname, f, env = where)
  memF <- glue::glue('mem', fname)
  assign(memF, memoise(f), where)

  if(!exists('forgetGemmaMemoised', envir = where, inherits = F))
    assign('forgetGemmaMemoised', function() {}, envir = where)

  forgetMe <- get('forgetGemmaMemoised', envir = where, inherits = F)

  body(forgetMe) <- body(forgetMe) %>% as.list %>%
    append(str2expression(glue::glue('forget({memF})'))) %>% as.call

  assign('forgetGemmaMemoised', forgetMe, envir = where, inherits = F)

  if(!is.null(document)) {
    cat(glue::glue("#' {fname}\n"), file = document, append = T)
    comment(roxygen, names(fargs), document)
    cat("#' @export\n", file = document, append = T)
    cat(glue::glue('{fname} <- '), file = document, append = T)
    cat(deparse(f) %>% paste0(collapse = '\n'), file = document, append = T)
    cat('\n\n', file = document, append = T)
    cat(glue::glue("#' Memoise {fname}\n#'\n\n"), file = document, append = T)
    cat(glue::glue('mem{fname} <- memoise::memoise({fname})'), file = document, append = T)
    cat('\n\n', file = document, append = T)
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
#' @param plural If equal to `root` (the default), assumes that this endpoint is pluralized by adding an "s". Otherwise, you can override this behavior by specifying the desired plural form
#' @param document A file to print information for pasting generating the package
registerSimpleEndpoint <- function(root, query, fname, preprocessor, validator = NULL,
                                   logname = fname, roxygen = NULL, where = parent.env(environment()),
                                   plural = root, document = getOption('gemmaAPI.document', 'R/allEndpoints.R')) {
  registerEndpoint(ifelse(plural == root, glue::glue('{paste0(root, "s")}/{{{root}}}/{query}'), glue::glue('{root}/{{{plural}}}')),
                   fname,
                   defaults = setNames(NA_character_, plural),
                   validators = switch(is.null(validator) + 1, validator, alist(validateSingleID) %>% `names<-`(plural)),
                   logname = logname,
                   roxygen = roxygen,
                   preprocessor = preprocessor,
                   where = where,
                   document = document)
}

#' A compound endpoint is one that makes multiple API calls and composes the response (internal use)
#'
#' @param endpoints A list of endpoints, indices of other endpoints they activate and which variables they send forwards (of the form list(list(endpoint = "", activates = 1, variables = c(bound_name = 'access name'))))
#' @param fname The name of the function to create
#' @param preprocessors The preprocessing functions to run on the output (same order as endpoints)
#' @param defaults Default values for the endpoint
#' @param validators Validators for the inputs
#' @param logname The activating phrase in the category endpoint
#' @param roxygen The name to pull roxygen information from
#' @param where The environment to add the new function to
#' @param document A file to print information for pasting generating the package
registerCompoundEndpoint <- function(endpoints, fname, preprocessors, defaults = list(NULL),
                                     validators = alist(NULL), logname = fname,
                                     roxygen = NULL,
                                     where = parent.env(environment()), document = getOption('gemmaAPI.document', 'R/allEndpoints.R')) {
  if(missing(endpoints) || missing(fname) || missing(preprocessors))
    stop('Please specify endpoints, function name and preprocessors.')
  if(exists(fname, envir = where, inherits = F)) {
    warning(glue::glue('{fname} already exists. Skipping.'))
    return(NULL)
  }

  logEndpoint(fname, logname)

  # Make sure arguments are URL encoded
  endpoints <- lapply(endpoints, function(endpoint) {
    endpoint$endpoint <- gsub('\\{([^\\}]+)\\}', '\\{encode\\(\\1\\)\\}', endpoint$endpoint)
    endpoint
  })

  f <- function() {}

  fargs <- alist()
  for(d in names(defaults)) {
    fargs[[d]] <- defaults[[d]]
  }
  # TODO consider enquoting these so that they would show up for users as getOption still rather than the evaluated version
  # Like this, we can change the defaults easily per-package but not easy for users to change their own defaults
  fargs$raw <- getOption('gemma.raw', F)
  fargs$async <- getOption('gemma.async', F)
  fargs$memoised <- getOption('gemma.memoise', F)

  formals(f) <- fargs
  body(f) <- quote({
    # Call a memoised version if applicable
    if(memoised) {
      newArgs <- as.list(match.call())[-1]
      newArgs$memoised <- F
      return(do.call(glue::glue('mem{fname}'), newArgs))
    }

    # Validate arguments
    if(!is.null(validators)) {
      for(v in names(validators)) {
        assign(v, eval(validators[[v]])(get(v), name = v))
      }
    }

    # Generate request
    endpointURLs <- lapply(endpoints, '[[', 'endpoint')
    endpointMap <- lapply(endpoints, '[[', 'activates')
    endpointExtract <- lapply(endpoints, '[[', 'variables')

    makeRequest <- async(function(index, URL) {
      # Make a request
      http_get(URL)$then(function(response) {
        if(response$status_code == 200) {
          mData <- tryCatch({
            fromJSON(rawToChar(response$content))$data
          }, error = function(e) {
            message(paste0('Failed to parse ', response$type, ' from ', response$url))
            warning(e$message)
            NULL
          })
          if(raw || length(mData) == 0)
            pData <- mData
          else
            pData <- eval(preprocessors[[index]])(mData)

          # If we activate another endpoint...
          if(!is.null(endpointMap[[index]])) {
            # Extract any passthrough variables and pass them on
            if(!is.null(endpointExtract[[index]])) {
              e <- as.environment(do.call(data.frame, mData))
              for(var in names(endpointExtract[[index]])) {
                assign(var, get(endpointExtract[[index]][var], envir = e))
              }
            }

            # Make the new request
            newIndex <- endpointMap[[index]]
            newURL <- paste0(getOption('gemma.API', 'https://gemma.msl.ubc.ca/rest/v2/'),
                             gsub('/(NA|/)', '/',
                                  gsub('\\?[^=]+=NA', '\\?',
                                       gsub('&[^=]+=NA', '', glue::glue(endpointURLs[[newIndex]])))))

            synchronise(makeRequest(newIndex, newURL)$then(function(x) {
              list(pData, x)
            }))
          } else
            pData
        } else
          response
      })
    })

    # Start by sending requests that aren't activated by other endpoints
    request <- async(function() {
      lapply(setdiff(1:length(endpoints), unique(unlist(endpointMap))), function(x) {
        synchronise(makeRequest(x, paste0(getOption('gemma.API', 'https://gemma.msl.ubc.ca/rest/v2/'),
                                          gsub('/(NA|/)', '/',
                                               gsub('\\?[^=]+=NA', '\\?',
                                                    gsub('&[^=]+=NA', '', glue::glue(endpointURLs[[x]])))))))
      }) %>% unlist(F)
    })

    if(!async)
      synchronise(when_all(request()))
    else
      request()
  })

  # Add our variables
  for(i in c('endpoints', 'validators', 'preprocessors', 'fname')) {
    if(is.character(get(i)))
      v <- glue::glue('"{get(i)}"')
    else if(is.list(get(i)))
      v <- get(i) %>% { paste0('list(', paste0(names(.), ' = ', ., collapse = ', '), ')') }
    else
      v <- get(i)

    body(f) <- body(f) %>% as.list %>%
      append(str2expression(glue::glue('{i} <- {v}')), 1) %>%
      as.call
  }

  # And our memoised function
  environment(f) <- where

  # Make this function available in the parent environment...
  assign(fname, f, env = where)
  memF <- glue::glue('mem', fname)
  assign(memF, memoise(f), where)

  if(!exists('forgetGemmaMemoised', envir = where, inherits = F))
    assign('forgetGemmaMemoised', function() {}, envir = where, inherits = F)

  forgetMe <- get('forgetGemmaMemoised', envir = where, inherits = F)

  body(forgetMe) <- body(forgetMe) %>% as.list %>%
    append(str2expression(glue::glue('forget({memF})'))) %>% as.call

  assign('forgetGemmaMemoised', forgetMe, envir = where, inherits = F)

  if(!is.null(document)) {
    cat(glue::glue("#' {fname}\n"), file = document, append = T)
    comment(roxygen, names(fargs), document)
    cat("#' @export\n", file = document, append = T)
    cat(glue::glue('{fname} <- '), file = document, append = T)
    cat(deparse(f) %>% paste0(collapse = '\n'), file = document, append = T)
    cat('\n\n', file = document, append = T)
    cat(glue::glue("#' Memoise {fname}\n#'\n\n"), file = document, append = T)
    cat(glue::glue('mem{fname} <- memoise::memoise({fname})'), file = document, append = T)
    cat('\n\n', file = document, append = T)
  }
}

#' Log an endpoint for the currently active category endpoint (@seealso registerCategoryEndpoint)
#'
#' @param fname The function name to call
#' @param logname The activating phrase
logEndpoint <- function(fname, logname) {
  options(gemmaAPI.logged = c(getOption('gemmaAPI.logged'), setNames(fname, logname)))
}

#' Register a category of endpoints that will expose multiple endpoint calls through a single function (internal use)
#'
#' @param fname The name of the category endpoint, or `NULL` to finish logging
#' @param characteristic The characteristic required parameter for this category
#' @param where The environment to add the new function to
#' @param document A file to print information for pasting generating the package
registerCategoryEndpoint <- function(fname = NULL, characteristic = NULL,
                                     where = parent.env(environment()),
                                     document = getOption('gemmaAPI.document', 'R/allEndpoints.R')) {
  if(is.null(fname)) {
    if(is.null(getOption('gemmaAPI.logging', NULL)))
      stop('No categories were being logged')

    fname <- getOption('gemmaAPI.logging')
    options(gemmaAPI.logging = NULL)

    f <- function() {}

    # Default behavior is to call the first registration if no request is passed
    fargs <- alist()
    fargs[[characteristic]] <- NA_character_
    fargs$request <- NA_character_
    fargs$`...` <- alist(... = )$...

    # TODO consider enquoting these so that they would show up for users as getOption still rather than the evaluated version
    # Like this, we can change the defaults easily per-package but not easy for users to change their own defaults
    fargs$raw <- getOption('gemma.raw', F)
    fargs$async <- getOption('gemma.async', F)
    fargs$memoised <- getOption('gemma.memoise', F)
    fargs$file <- getOption('gemma.file', NA_character_)
    fargs$overwrite <- getOption('gemma.overwrite', F)

    formals(f) <- fargs
    body(f) <- quote({
      if(!is.na(request) && !(request %in% names(argMap)))
        stop(paste0('Invalid request parameter. Options include: ', paste0(names(argMap), collapse = ', ')))

      if(is.na(request)) request <- 1

      mCallable <- call(argMap[[request]], raw = raw, async = async, memoised = memoised, file = file, overwrite = overwrite)
      mCallable[[characteristicValue]] <- get(characteristicValue)
      for(i in names(list(...))) {
        mCallable[[i]] <- list(...)[[i]]
      }

      eval(mCallable, envir = parent.env(environment()))
    })

    argMap <- getOption('gemmaAPI.logged') %>% { paste0('c(', paste0(names(.), ' = "', ., '"', collapse = ', '), ')') }

    # Add the argument map
    body(f) <- body(f) %>% as.list %>%
      append(str2expression(glue::glue('argMap <- {argMap}')), 1) %>%
      append(str2expression(glue::glue('characteristicValue <- "{characteristic}"')), 1) %>%
      as.call

    environment(f) <- where

    if(!is.null(document)) {
      # TODO comment these
      cat(glue::glue("#' {fname}\n#' @export\n\n"), file = document, append = T)
      cat(glue::glue('{fname} <- '), file = document, append = T)
      cat(deparse(f) %>% paste0(collapse = '\n'), file = document, append = T)
      cat('\n\n', file = document, append = T)
    }
  } else
    options(gemmaAPI.logging = fname, gemmaAPI.logged = NULL)
}

# Documentation ----
`+` <- function(A, B) {
  paste0(A, B, collapse = '')
}

# Load in descriptions from the JS
eval(synchronise(http_get('https://gemma.msl.ubc.ca/resources/restapidocs/js/vue/descriptions.js'))$content %>%
       rawToChar %>% {
         gsub('\\/\\*[\\s\\S]*?\\*\\/', '', ., perl = T)
       } %>% {
         gsub('(\n(var )?)', '', .)
       } %>% {
         parse(text = .)
       })

rm(`+`)

# And endpoints from the HTML
endpoints <- synchronise(http_get('https://gemma.msl.ubc.ca/resources/restapidocs/'))$content %>%
  rawToChar %>%
  read_html() %>%
  xml_find_all('.//endpoint')

#' Comment a function
#'
#' @param src The name of the entry to use (in endpoints)
#' @param parameters The parameters that the function accepts
#' @param document A file to print information for pasting generating the package
comment <- function(src, parameters, document = getOption('gemmaAPI.document', 'R/allEndpoints.R')) {
  if(is.null(src)) {
    cat('\n', file = document, append = T)
    return(NULL)
  }

  node <- Filter(function(elem) {
    xml_attr(elem, ':name') == paste0("'", src, "'")
  }, endpoints)

  cat(glue::glue("\n\n#' {xml_attr(node, ':description') %>% { substring(., 2, nchar(.) - 1) }}\n#'\n\n"), file = document, append = T)

  for(arg in parameters) {
    if(arg == 'raw') {
      mAdd <- '<p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>'
    } else if(arg == 'async') {
      mAdd <- '<p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>'
    } else if(arg == 'memoised') {
      mAdd <- '<p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>'
    } else if(arg == 'file') {
      mAdd <- '<p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>'
    } else if(arg == 'overwrite') {
      mAdd <- '<p>Whether or not to overwrite if a file exists at the specified filename.</p>'
    } else {
      mArg <- arg
      if(arg == 'component') mArg <- 'pcaComponent'
      else if(arg == 'threshold') mArg <- 'diffExThreshold'
      else if(arg == 'keepNonSpecific') mArg <- 'datasetsExpressSpec'
      else if(arg == 'element') mArg <- 'probes'
      else if(arg == 'with') mArg <- 'geneWith'
      else if(arg == 'editableOnly') mArg <- 'editable'
      else if(arg == 'start') mArg <- 'nuclStart'
      else if(arg == 'size') mArg <- 'nuclSize'
      else if(arg == 'query') mArg <- 'search'

      mAdd <- get(paste0(mArg, 'Description'))
    }

    cat(glue::glue("#' @param {arg} {mAdd}\n\n"), file = document, append = T)
  }

  cat(glue::glue("#'\n#' @return {get(xml_attr(node, ':response-description'))}\n\n"), file = document, append = T)
}

# Dataset endpoints ----
registerCategoryEndpoint('datasetInfo')

registerEndpoint('datasets/{dataset}?filter={filter}&offset={offset}&limit={limit}&sort={sort}',
                 'getDatasets', logname = 'datasets', roxygen = 'Datasets',
                 defaults = list(dataset = NA_character_,
                                 filter = NA_character_,
                                 offset = 0L,
                                 limit = 20L,
                                 sort = '+id'),
                 validators = alist(dataset = validateID,
                                    filter = validateFilter,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    sort = validateSort),
                 preprocessor = quote(processDatasets))

registerEndpoint('datasets/{dataset}/analyses/differential?offset={offset}&limit={limit}',
                 'getDatasetDEA', logname = 'differential', roxygen = 'Dataset differential analysis',
                 defaults = list(dataset = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(dataset = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processDEA))

registerEndpoint('datasets/{dataset}/expressions/pca?component={component}&limit={limit}&keepNonSpecific={keepNonSpecific}&consolidate={consolidate}',
                 'getDatasetPCA', logname = 'PCA', roxygen = 'Datasets pca component expression levels',
                 defaults = list(dataset = NA_character_,
                                 component = 1L,
                                 limit = 100L,
                                 keepNonSpecific = F,
                                 consolidate = NA_character_),
                 validators = alist(dataset = validateID,
                                    component = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    keepNonSpecific = validateBoolean,
                                    consolidate = validateConsolidate),
                 preprocessor = quote(processExpression))

registerEndpoint('datasets/{dataset}/expressions/differential?keepNonSpecific={keepNonSpecific}&diffExSet={diffExSet}&threshold={threshold}&limit={limit}&consolidate={consolidate}',
                 'getDatasetDE', logname = 'diffEx', roxygen = 'Datasets differential expression levels',
                 defaults = list(dataset = NA_character_,
                                 keepNonSpecific = F,
                                 diffExSet = NA_integer_,
                                 threshold = 100.0,
                                 limit = 100L,
                                 consolidate = NA_character_),
                 validators = alist(dataset = validateID,
                                    keepNonSpecific = validateBoolean,
                                    diffExSet = validatePositiveInteger,
                                    threshold = validatePositiveReal,
                                    limit = validatePositiveInteger,
                                    consolidate = validateConsolidate),
                 preprocessor = quote(processExpression))

registerSimpleEndpoint('dataset', 'samples', logname = 'samples', roxygen = 'Dataset samples',
                       'getDatasetSamples',
                       preprocessor = quote(processSamples))

registerSimpleEndpoint('dataset', 'svd', logname = 'SVD', roxygen = 'Dataset SVD information',
                       'getDatasetSVD',
                       preprocessor = quote(processSVD))

registerSimpleEndpoint('dataset', 'platforms', logname = 'platforms', roxygen = 'Dataset platforms',
                       'getDatasetPlatforms',
                       preprocessor = quote(processPlatforms))

registerSimpleEndpoint('dataset', 'annotations', logname = 'annotations', roxygen = 'Dataset annotations',
                       'getDatasetAnnotations',
                       preprocessor = quote(processAnnotations))

registerCompoundEndpoint(endpoints = list(
  A = list(endpoint = 'datasets/{dataset}/analyses/differential?offset={offset}&limit={limit}',
           activates = 2,
           variables = c(diffExSet = 'resultSets.resultSetId')),
  B = list(endpoint = 'datasets/{dataset}/expressions/differential?keepNonSpecific={keepNonSpecific}&diffExSet={diffExSet}&threshold={threshold}&limit={limit}&consolidate={consolidate}')),
  'getDiffExpr', logname = 'diffExData',
  defaults = list(dataset = NA_character_,
                  offset = 0L,
                  keepNonSpecific = F,
                  threshold = 100.0,
                  limit = 100L,
                  consolidate = NA_character_),
  validators = alist(dataset = validateSingleID,
                     offset = validatePositiveInteger,
                     keepNonSpecific = validateBoolean,
                     threshold = validatePositiveReal,
                     limit = validatePositiveInteger,
                     consolidate = validateConsolidate),
  preprocessors = alist(A = processDEA, B = processExpression))

registerCategoryEndpoint(characteristic = 'dataset')
# Platform endpoints ----
registerCategoryEndpoint('platformInfo')

registerEndpoint('platforms/{platform}?filter={filter}&offset={offset}&limit={limit}&sort={sort}',
                 'getPlatforms', logname = 'platforms', roxygen = 'Platforms',
                 defaults = list(platform = NA_character_,
                                 filter = NA_character_,
                                 offset = 0L,
                                 limit = 20L,
                                 sort = '+id'),
                 validators = alist(filter = validateFilter,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    sort = validateSort),
                 preprocessor = quote(processPlatforms))

registerEndpoint('platforms/{platform}/datasets?offset={offset}&limit={limit}',
                 'getPlatformDatasets', logname = 'datasets', roxygen = 'Platform datasets',
                 defaults = list(platform = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(platform = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processDatasets))

registerEndpoint('platforms/{platform}/elements?offset={offset}&limit={limit}',
                 'getPlatformElements', logname = 'elements', roxygen = 'Platform elements',
                 defaults = list(platform = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(platform = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processElements))

registerEndpoint('platforms/{platform}/elements/{element}/genes?offset={offset}&limit={limit}',
                 'getPlatformElementGenes', logname = 'genes', roxygen = 'Platform element genes',
                 defaults = list(platform = NA_character_,
                                 element = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(platform = validateSingleID,
                                    element = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processGenes))

registerCategoryEndpoint(characteristic = 'platform')
# Gene endpoints ----
registerCategoryEndpoint('geneInfo')

registerSimpleEndpoint('gene', '', logname = 'genes', roxygen = 'Genes',
                       'getGenes',
                       preprocessor = quote(processGenes))

registerSimpleEndpoint('gene', 'evidence', logname = 'evidence', roxygen = 'Gene evidence',
                       'getGeneEvidence',
                       preprocessor = quote(processGeneEvidence))

registerSimpleEndpoint('gene', 'locations', logname = 'locations', roxygen = 'Gene locations',
                       'getGeneLocation',
                       preprocessor = quote(processGeneLocation))

registerEndpoint('gene/{gene}/probes?offset={offset}&limit={limit}',
                 'getGeneProbes', logname = 'probes', roxygen = 'Gene probes',
                 defaults = list(gene = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(gene = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processElements))

registerSimpleEndpoint('gene', 'goTerms', logname = 'goTerms', roxygen = 'Gene goTerms',
                       'getGeneGO',
                       preprocessor = quote(processGO))

registerEndpoint('genes/{gene}/coexpression?with={with}&limit={limit}&stringency={stringency}',
                 'getGeneCoexpression', logname = 'coexpression', roxygen = 'Gene coexpression',
                 defaults = list(gene = NA_character_,
                                 with = NA_character_,
                                 limit = 20L,
                                 stringency = 1L),
                 validators = alist(gene = validateSingleID,
                                    with = validateSingleID,
                                    limit = validatePositiveInteger,
                                    stringency = validatePositiveInteger),
                 preprocessor = quote(processCoexpression))

registerCategoryEndpoint(characteristic = 'gene')
# Taxon endpoints ----
registerCategoryEndpoint('taxonInfo')

registerSimpleEndpoint('taxa', '', logname = 'taxa', roxygen = 'Taxa',
                       'getTaxa',
                       plural = 'taxon', # Misleading plural but oh well
                       validator = alist(taxon = validateOptionalTaxon),
                       preprocessor = quote(processTaxon))

registerEndpoint('taxa/{taxon}/datasets?filter={filter}&offset={offset}&limit={limit}&sort={sort}',
                 'getTaxonDatasets', logname = 'datasets', roxygen = 'Taxon datasets',
                 defaults = list(taxon = NA_character_,
                                 filter = NA_character_,
                                 offset = 0L,
                                 limit = 20L,
                                 sort = '+id'),
                 validators = alist(taxon = validateSingleID,
                                    filter = validateFilter,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    sort = validateSort),
                 preprocessor = quote(processDatasets))

registerEndpoint('taxa/{taxon}/phenotypes?editableOnly={editableOnly}&tree={tree}',
                 'getTaxonPhenotypes', logname = 'phenotypes', roxygen = 'Taxon phenotypes',
                 defaults = list(taxon = NA_character_,
                                 editableOnly = F,
                                 tree = F),
                 validators = alist(taxon = validateSingleID,
                                    editableOnly = validateBoolean,
                                    tree = validateBoolean),
                 preprocessor = quote(processPhenotypes))

registerEndpoint('taxa/{taxon}/phenotypes/candidates?editableOnly={editableOnly}&phenotypes={phenotypes}',
                 'getTaxonPhenotypeCandidates', logname = 'phenoCandidateGenes', roxygen = 'Taxon phenotypes candidate genes',
                 defaults = list(taxon = NA_character_,
                                 editableOnly = F,
                                 phenotypes = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    editableOnly = validateBoolean,
                                    phenotypes = validateSingleID),
                 preprocessor = quote(processGeneEvidence))

registerEndpoint('taxa/{taxon}/genes/{gene}',
                 'getGeneOnTaxon', logname = 'gene', roxygen = 'Gene on specific taxon',
                 defaults = list(taxon = NA_character_,
                                 gene = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    gene = validateSingleID),
                 preprocessor = quote(processGenes))

registerEndpoint('taxa/{taxon}/genes/{gene}/evidence',
                 'getEvidenceOnTaxon', logname = 'geneEvidence', roxygen = 'Gene evidence on specific taxon',
                 defaults = list(taxon = NA_character_,
                                 gene = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    gene = validateSingleID),
                 preprocessor = quote(processGeneEvidence))

registerEndpoint('taxa/{taxon}/genes/{gene}/locations',
                 'getGeneLocationOnTaxon', logname = 'geneLocation', roxygen = 'Gene location on specific taxon',
                 defaults = list(taxon = NA_character_,
                                 gene = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    gene = validateSingleID),
                 preprocessor = quote(processGeneLocation))

registerEndpoint('taxa/{taxon}/chromosomes/{chromosome}/genes?strand={strand}&start={start}&size={size}',
                 'getGenesAtLocation', logname = 'genesAtLocation', roxygen = 'Genes at location',
                 defaults = list(taxon = NA_character_,
                                 chromosome = NA_character_,
                                 strand = '+',
                                 start = NA_integer_,
                                 size = NA_integer_),
                 validators = alist(taxon = validateSingleID,
                                    chromosome = validateSingleID,
                                    strand = validateStrand,
                                    start = validatePositiveInteger,
                                    size = validatePositiveInteger),
                 preprocessor = quote(processGenes))

registerEndpoint('annotations/{taxon}/search/{query}/datasets?filter={filter}&offset={offset}&limit={limit}&sort={sort}',
                 'searchDatasets', logname = 'datasets', roxygen = 'Dataset search',
                 defaults = list(taxon = '',
                                 query = NA_character_,
                                 filter = NA_character_,
                                 offset = 0L,
                                 limit = 0L,
                                 sort = '+id'),
                 validators = alist(taxon = validateTaxon,
                                    query = validateQuery,
                                    filter = validateFilter,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    sort = validateSort),
                 preprocessor = quote(processDatasets))

registerCategoryEndpoint(characteristic = 'taxon')

registerEndpoint('annotations/search/{query}',
                 'searchAnnotations', roxygen = 'Annotation search',
                 defaults = list(query = NA_character_),
                 validators = alist(query = validateQuery),
                 preprocessor = quote(processAnnotations))

# Clean up
doFinalize <- function(document = getOption('gemmaAPI.document', 'R/allEndpoints.R')) {
  cat('\n', file = document, append = T)
  cat(glue::glue("#' forgetGemmaMemoised\n\n"), file = document, append = T)
  cat("#'\n", file = document, append = T)
  cat("#' Forget past results from memoised calls to the Gemma API (ie. using functions with memoised = `TRUE`)\n#'\n", file = document, append = T)
  cat("#' @export\n", file = document, append = T)
  cat('forgetGemmaMemoised <- ', file = document, append = T)
  cat(deparse(get('forgetGemmaMemoised', envir = globalenv(), inherits = F)) %>% paste0(collapse = '\n'), file = document, append = T)

  rm(list = ls(envir = globalenv()), envir = globalenv())

  devtools::document()
  devtools::build()
  devtools::install()
}

doFinalize()
