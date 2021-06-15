#' getDatasets
#' @export
getDatasets <- function (dataset = NA_character_, filter = NA_character_, offset = 0L,
    limit = 20L, sort = "+id", raw = FALSE, async = FALSE, memoised = FALSE,
    file = NA_character_, overwrite = FALSE)
{
    fname <- "getDatasets"
    preprocessor <- processDatasets
    validators <- list(dataset = validateID, filter = validateFilter,
        offset = validatePositiveInteger, limit = validatePositiveInteger,
        sort = validateSort)
    endpoint <- "datasets/{encode(dataset)}?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getDatasets
#'
memgetDatasets <- memoise::memoise(getDatasets)

#' getDatasetDEA
#' @export
getDatasetDEA <- function (dataset = NA_character_, offset = 0L, limit = 20L,
    raw = FALSE, async = FALSE, memoised = FALSE, file = NA_character_,
    overwrite = FALSE)
{
    fname <- "getDatasetDEA"
    preprocessor <- processDEA
    validators <- list(dataset = validateSingleID, offset = validatePositiveInteger,
        limit = validatePositiveInteger)
    endpoint <- "datasets/{encode(dataset)}/analyses/differential?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getDatasetDEA
#'
memgetDatasetDEA <- memoise::memoise(getDatasetDEA)

#' getDatasetPCA
#' @export
getDatasetPCA <- function (dataset = NA_character_, component = 1L, limit = 100L,
    keepNonSpecific = FALSE, consolidate = NA_character_, raw = FALSE,
    async = FALSE, memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getDatasetPCA"
    preprocessor <- processExpression
    validators <- list(dataset = validateID, component = validatePositiveInteger,
        limit = validatePositiveInteger, keepNonSpecific = validateBoolean,
        consolidate = validateConsolidate)
    endpoint <- "datasets/{encode(dataset)}/expressions/pca?component={encode(component)}&limit={encode(limit)}&keepNonSpecific={encode(keepNonSpecific)}&consolidate={encode(consolidate)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getDatasetPCA
#'
memgetDatasetPCA <- memoise::memoise(getDatasetPCA)

#' getDatasetDE
#' @export
getDatasetDE <- function (dataset = NA_character_, keepNonSpecific = FALSE, diffExSet = NA_integer_,
    threshold = 100, limit = 100L, consolidate = NA_character_,
    raw = FALSE, async = FALSE, memoised = FALSE, file = NA_character_,
    overwrite = FALSE)
{
    fname <- "getDatasetDE"
    preprocessor <- processExpression
    validators <- list(dataset = validateID, keepNonSpecific = validateBoolean,
        diffExSet = validatePositiveInteger, threshold = validatePositiveReal,
        limit = validatePositiveInteger, consolidate = validateConsolidate)
    endpoint <- "datasets/{encode(dataset)}/expressions/differential?keepNonSpecific={encode(keepNonSpecific)}&diffExSet={encode(diffExSet)}&threshold={encode(threshold)}&limit={encode(limit)}&consolidate={encode(consolidate)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getDatasetDE
#'
memgetDatasetDE <- memoise::memoise(getDatasetDE)

#' getDatasetSamples
#' @export
getDatasetSamples <- function (dataset = NA_character_, raw = FALSE, async = FALSE,
    memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getDatasetSamples"
    preprocessor <- processSamples
    validators <- list(dataset = validator)
    endpoint <- "NA"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getDatasetSamples
#'
memgetDatasetSamples <- memoise::memoise(getDatasetSamples)

#' getDatasetSVD
#' @export
getDatasetSVD <- function (dataset = NA_character_, raw = FALSE, async = FALSE,
    memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getDatasetSVD"
    preprocessor <- processSVD
    validators <- list(dataset = validator)
    endpoint <- "NA"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getDatasetSVD
#'
memgetDatasetSVD <- memoise::memoise(getDatasetSVD)

#' getDatasetPlatforms
#' @export
getDatasetPlatforms <- function (dataset = NA_character_, raw = FALSE, async = FALSE,
    memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getDatasetPlatforms"
    preprocessor <- processPlatforms
    validators <- list(dataset = validator)
    endpoint <- "NA"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getDatasetPlatforms
#'
memgetDatasetPlatforms <- memoise::memoise(getDatasetPlatforms)

#' getDatasetAnnotations
#' @export
getDatasetAnnotations <- function (dataset = NA_character_, raw = FALSE, async = FALSE,
    memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getDatasetAnnotations"
    preprocessor <- processAnnotations
    validators <- list(dataset = validator)
    endpoint <- "NA"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getDatasetAnnotations
#'
memgetDatasetAnnotations <- memoise::memoise(getDatasetAnnotations)

#' getDiffExpr
#' @export
getDiffExpr <- function (dataset = NA_character_, offset = 0L, keepNonSpecific = FALSE,
    threshold = 100, limit = 100L, consolidate = NA_character_,
    raw = FALSE, async = FALSE, memoised = FALSE)
{
    fname <- "getDiffExpr"
    preprocessors <- list(A = processDEA, B = processExpression)
    validators <- list(dataset = validateSingleID, offset = validatePositiveInteger,
        keepNonSpecific = validateBoolean, threshold = validatePositiveReal,
        limit = validatePositiveInteger, consolidate = validateConsolidate)
    endpoints <- list(A = list(endpoint = "datasets/{encode(dataset)}/analyses/differential?offset={encode(offset)}&limit={encode(limit)}",
        activates = 2, variables = c(diffExSet = "resultSets.resultSetId")),
        B = list(endpoint = "datasets/{encode(dataset)}/expressions/differential?keepNonSpecific={encode(keepNonSpecific)}&diffExSet={encode(diffExSet)}&threshold={encode(threshold)}&limit={encode(limit)}&consolidate={encode(consolidate)}"))
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpointURLs <- lapply(endpoints, "[[", "endpoint")
    endpointMap <- lapply(endpoints, "[[", "activates")
    endpointExtract <- lapply(endpoints, "[[", "variables")
    makeRequest <- async(function(index, URL) {
        http_get(URL)$then(function(response) {
            if (response$status_code == 200) {
                mData <- tryCatch({
                  fromJSON(rawToChar(response$content))$data
                }, error = function(e) {
                  message(paste0("Failed to parse ", response$type,
                    " from ", response$url))
                  warning(e$message)
                  NULL
                })
                if (raw || length(mData) == 0)
                  pData <- mData
                else pData <- eval(preprocessors[[index]])(mData)
                if (!is.null(endpointMap[[index]])) {
                  if (!is.null(endpointExtract[[index]])) {
                    e <- as.environment(do.call(data.frame, mData))
                    for (var in names(endpointExtract[[index]])) {
                      assign(var, get(endpointExtract[[index]][var],
                        envir = e))
                    }
                  }
                  newIndex <- endpointMap[[index]]
                  newURL <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
                    gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA",
                      "\\?", gsub("&[^=]+=NA", "", glue(endpointURLs[[newIndex]])))))
                  synchronise(makeRequest(newIndex, newURL)$then(function(x) {
                    list(pData, x)
                  }))
                }
                else pData
            }
            else response
        })
    })
    request <- async(function() {
        lapply(setdiff(1:length(endpoints), unique(unlist(endpointMap))),
            function(x) {
                synchronise(makeRequest(x, paste0(getOption("gemma.API",
                  "https://gemma.msl.ubc.ca/rest/v2/"), gsub("/(NA|/)",
                  "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
                    "", glue(endpointURLs[[x]])))))))
            }) %>% unlist(F)
    })
    if (!async)
        synchronise(when_all(request()))
    else request()
}

#' Memoise getDiffExpr
#'
memgetDiffExpr <- memoise::memoise(getDiffExpr)

#' datasetInfo
#' @export
datasetInfo <- function (dataset = NA_character_, request = NA_character_, ...,
    raw = FALSE, async = FALSE, memoised = FALSE, file = NA_character_,
    overwrite = FALSE)
{
    characteristicValue <- "dataset"
    argMap <- c(datasets = "getDatasets", differential = "getDatasetDEA",
        PCA = "getDatasetPCA", diffEx = "getDatasetDE", samples = "getDatasetSamples",
        SVD = "getDatasetSVD", platforms = "getDatasetPlatforms",
        annotations = "getDatasetAnnotations", diffExData = "getDiffExpr")
    if (!is.na(request) && !(request %in% names(argMap)))
        stop(paste0("Invalid request parameter. Options include: ",
            paste0(names(argMap), collapse = ", ")))
    if (is.na(request))
        request <- 1
    mCallable <- call(argMap[[request]], raw = raw, async = async,
        memoised = memoised, file = file, overwrite = overwrite)
    mCallable[[characteristicValue]] <- get(characteristicValue)
    for (i in names(list(...))) {
        mCallable[[i]] <- list(...)[[i]]
    }
    eval(mCallable, envir = parent.env(environment()))
}

#' getPlatforms
#' @export
getPlatforms <- function (platform = NA_character_, filter = NA_character_, offset = 0L,
    limit = 20L, sort = "+id", raw = FALSE, async = FALSE, memoised = FALSE,
    file = NA_character_, overwrite = FALSE)
{
    fname <- "getPlatforms"
    preprocessor <- processPlatforms
    validators <- list(filter = validateFilter, offset = validatePositiveInteger,
        limit = validatePositiveInteger, sort = validateSort)
    endpoint <- "platforms/{encode(platform)}?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getPlatforms
#'
memgetPlatforms <- memoise::memoise(getPlatforms)

#' getPlatformDatasets
#' @export
getPlatformDatasets <- function (platform = NA_character_, offset = 0L, limit = 20L,
    raw = FALSE, async = FALSE, memoised = FALSE, file = NA_character_,
    overwrite = FALSE)
{
    fname <- "getPlatformDatasets"
    preprocessor <- processDatasets
    validators <- list(platform = validateSingleID, offset = validatePositiveInteger,
        limit = validatePositiveInteger)
    endpoint <- "platforms/{encode(platform)}/datasets?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getPlatformDatasets
#'
memgetPlatformDatasets <- memoise::memoise(getPlatformDatasets)

#' getPlatformElements
#' @export
getPlatformElements <- function (platform = NA_character_, offset = 0L, limit = 20L,
    raw = FALSE, async = FALSE, memoised = FALSE, file = NA_character_,
    overwrite = FALSE)
{
    fname <- "getPlatformElements"
    preprocessor <- processElements
    validators <- list(platform = validateSingleID, offset = validatePositiveInteger,
        limit = validatePositiveInteger)
    endpoint <- "platforms/{encode(platform)}/elements?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getPlatformElements
#'
memgetPlatformElements <- memoise::memoise(getPlatformElements)

#' getPlatformElementGenes
#' @export
getPlatformElementGenes <- function (platform = NA_character_, element = NA_character_,
    offset = 0L, limit = 20L, raw = FALSE, async = FALSE, memoised = FALSE,
    file = NA_character_, overwrite = FALSE)
{
    fname <- "getPlatformElementGenes"
    preprocessor <- processGenes
    validators <- list(platform = validateSingleID, element = validateSingleID,
        offset = validatePositiveInteger, limit = validatePositiveInteger)
    endpoint <- "platforms/{encode(platform)}/elements/{encode(element)}/genes?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getPlatformElementGenes
#'
memgetPlatformElementGenes <- memoise::memoise(getPlatformElementGenes)

#' platformInfo
#' @export
platformInfo <- function (platform = NA_character_, request = NA_character_,
    ..., raw = FALSE, async = FALSE, memoised = FALSE, file = NA_character_,
    overwrite = FALSE)
{
    characteristicValue <- "platform"
    argMap <- c(platforms = "getPlatforms", datasets = "getPlatformDatasets",
        elements = "getPlatformElements", genes = "getPlatformElementGenes")
    if (!is.na(request) && !(request %in% names(argMap)))
        stop(paste0("Invalid request parameter. Options include: ",
            paste0(names(argMap), collapse = ", ")))
    if (is.na(request))
        request <- 1
    mCallable <- call(argMap[[request]], raw = raw, async = async,
        memoised = memoised, file = file, overwrite = overwrite)
    mCallable[[characteristicValue]] <- get(characteristicValue)
    for (i in names(list(...))) {
        mCallable[[i]] <- list(...)[[i]]
    }
    eval(mCallable, envir = parent.env(environment()))
}

#' getGenes
#' @export
getGenes <- function (gene = NA_character_, raw = FALSE, async = FALSE, memoised = FALSE,
    file = NA_character_, overwrite = FALSE)
{
    fname <- "getGenes"
    preprocessor <- processGenes
    validators <- list(gene = validator)
    endpoint <- "NA"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getGenes
#'
memgetGenes <- memoise::memoise(getGenes)

#' getGeneEvidence
#' @export
getGeneEvidence <- function (gene = NA_character_, raw = FALSE, async = FALSE, memoised = FALSE,
    file = NA_character_, overwrite = FALSE)
{
    fname <- "getGeneEvidence"
    preprocessor <- processGeneEvidence
    validators <- list(gene = validator)
    endpoint <- "NA"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getGeneEvidence
#'
memgetGeneEvidence <- memoise::memoise(getGeneEvidence)

#' getGeneLocation
#' @export
getGeneLocation <- function (gene = NA_character_, raw = FALSE, async = FALSE, memoised = FALSE,
    file = NA_character_, overwrite = FALSE)
{
    fname <- "getGeneLocation"
    preprocessor <- processGeneLocation
    validators <- list(gene = validator)
    endpoint <- "NA"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getGeneLocation
#'
memgetGeneLocation <- memoise::memoise(getGeneLocation)

#' getGeneProbes
#' @export
getGeneProbes <- function (gene = NA_character_, offset = 0L, limit = 20L, raw = FALSE,
    async = FALSE, memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getGeneProbes"
    preprocessor <- processElements
    validators <- list(gene = validateSingleID, offset = validatePositiveInteger,
        limit = validatePositiveInteger)
    endpoint <- "gene/{encode(gene)}/probes?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getGeneProbes
#'
memgetGeneProbes <- memoise::memoise(getGeneProbes)

#' getGeneGO
#' @export
getGeneGO <- function (gene = NA_character_, raw = FALSE, async = FALSE, memoised = FALSE,
    file = NA_character_, overwrite = FALSE)
{
    fname <- "getGeneGO"
    preprocessor <- processGO
    validators <- list(gene = validator)
    endpoint <- "NA"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getGeneGO
#'
memgetGeneGO <- memoise::memoise(getGeneGO)

#' getGeneCoexpression
#' @export
getGeneCoexpression <- function (gene = NA_character_, with = NA_character_, limit = 20L,
    stringency = 1L, raw = FALSE, async = FALSE, memoised = FALSE,
    file = NA_character_, overwrite = FALSE)
{
    fname <- "getGeneCoexpression"
    preprocessor <- processCoexpression
    validators <- list(gene = validateSingleID, with = validateSingleID,
        limit = validatePositiveInteger, stringency = validatePositiveInteger)
    endpoint <- "genes/{encode(gene)}/coexpression?with={encode(with)}&limit={encode(limit)}&stringency={encode(stringency)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getGeneCoexpression
#'
memgetGeneCoexpression <- memoise::memoise(getGeneCoexpression)

#' geneInfo
#' @export
geneInfo <- function (gene = NA_character_, request = NA_character_, ...,
    raw = FALSE, async = FALSE, memoised = FALSE, file = NA_character_,
    overwrite = FALSE)
{
    characteristicValue <- "gene"
    argMap <- c(genes = "getGenes", evidence = "getGeneEvidence",
        locations = "getGeneLocation", probes = "getGeneProbes",
        goTerms = "getGeneGO", coexpression = "getGeneCoexpression")
    if (!is.na(request) && !(request %in% names(argMap)))
        stop(paste0("Invalid request parameter. Options include: ",
            paste0(names(argMap), collapse = ", ")))
    if (is.na(request))
        request <- 1
    mCallable <- call(argMap[[request]], raw = raw, async = async,
        memoised = memoised, file = file, overwrite = overwrite)
    mCallable[[characteristicValue]] <- get(characteristicValue)
    for (i in names(list(...))) {
        mCallable[[i]] <- list(...)[[i]]
    }
    eval(mCallable, envir = parent.env(environment()))
}

#' getTaxa
#' @export
getTaxa <- function (taxon = NA_character_, raw = FALSE, async = FALSE,
    memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getTaxa"
    preprocessor <- processTaxon
    validators <- list(taxon = validateOptionalTaxon)
    endpoint <- "NA"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getTaxa
#'
memgetTaxa <- memoise::memoise(getTaxa)

#' getTaxonDatasets
#' @export
getTaxonDatasets <- function (taxon = NA_character_, filter = NA_character_, offset = 0L,
    limit = 20L, sort = "+id", raw = FALSE, async = FALSE, memoised = FALSE,
    file = NA_character_, overwrite = FALSE)
{
    fname <- "getTaxonDatasets"
    preprocessor <- processDatasets
    validators <- list(taxon = validateSingleID, filter = validateFilter,
        offset = validatePositiveInteger, limit = validatePositiveInteger,
        sort = validateSort)
    endpoint <- "taxa/{encode(taxon)}/datasets?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getTaxonDatasets
#'
memgetTaxonDatasets <- memoise::memoise(getTaxonDatasets)

#' getTaxonPhenotypes
#' @export
getTaxonPhenotypes <- function (taxon = NA_character_, editableOnly = FALSE, tree = FALSE,
    raw = FALSE, async = FALSE, memoised = FALSE, file = NA_character_,
    overwrite = FALSE)
{
    fname <- "getTaxonPhenotypes"
    preprocessor <- processPhenotypes
    validators <- list(taxon = validateSingleID, editableOnly = validateBoolean,
        tree = validateBoolean)
    endpoint <- "taxa/{encode(taxon)}/phenotypes?editableOnly={encode(editableOnly)}&tree={encode(tree)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getTaxonPhenotypes
#'
memgetTaxonPhenotypes <- memoise::memoise(getTaxonPhenotypes)

#' getTaxonPhenotypeCandidates
#' @export
getTaxonPhenotypeCandidates <- function (taxon = NA_character_, editableOnly = FALSE, phenotypes = NA_character_,
    raw = FALSE, async = FALSE, memoised = FALSE, file = NA_character_,
    overwrite = FALSE)
{
    fname <- "getTaxonPhenotypeCandidates"
    preprocessor <- processGeneEvidence
    validators <- list(taxon = validateSingleID, editableOnly = validateBoolean,
        phenotypes = validateSingleID)
    endpoint <- "taxa/{encode(taxon)}/phenotypes/candidates?editableOnly={encode(editableOnly)}&phenotypes={encode(phenotypes)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getTaxonPhenotypeCandidates
#'
memgetTaxonPhenotypeCandidates <- memoise::memoise(getTaxonPhenotypeCandidates)

#' getGeneOnTaxon
#' @export
getGeneOnTaxon <- function (taxon = NA_character_, gene = NA_character_, raw = FALSE,
    async = FALSE, memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getGeneOnTaxon"
    preprocessor <- processGenes
    validators <- list(taxon = validateSingleID, gene = validateSingleID)
    endpoint <- "taxa/{encode(taxon)}/genes/{encode(gene)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getGeneOnTaxon
#'
memgetGeneOnTaxon <- memoise::memoise(getGeneOnTaxon)

#' getEvidenceOnTaxon
#' @export
getEvidenceOnTaxon <- function (taxon = NA_character_, gene = NA_character_, raw = FALSE,
    async = FALSE, memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getEvidenceOnTaxon"
    preprocessor <- processGeneEvidence
    validators <- list(taxon = validateSingleID, gene = validateSingleID)
    endpoint <- "taxa/{encode(taxon)}/genes/{encode(gene)}/evidence"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getEvidenceOnTaxon
#'
memgetEvidenceOnTaxon <- memoise::memoise(getEvidenceOnTaxon)

#' getGeneLocationOnTaxon
#' @export
getGeneLocationOnTaxon <- function (taxon = NA_character_, gene = NA_character_, raw = FALSE,
    async = FALSE, memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getGeneLocationOnTaxon"
    preprocessor <- processGeneLocation
    validators <- list(taxon = validateSingleID, gene = validateSingleID)
    endpoint <- "taxa/{encode(taxon)}/genes/{encode(gene)}/locations"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getGeneLocationOnTaxon
#'
memgetGeneLocationOnTaxon <- memoise::memoise(getGeneLocationOnTaxon)

#' getGenesAtLocation
#' @export
getGenesAtLocation <- function (taxon = NA_character_, chromosome = NA_character_,
    strand = "+", start = NA_integer_, size = NA_integer_, raw = FALSE,
    async = FALSE, memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "getGenesAtLocation"
    preprocessor <- processGenes
    validators <- list(taxon = validateSingleID, chromosome = validateSingleID,
        strand = validateStrand, start = validatePositiveInteger,
        size = validatePositiveInteger)
    endpoint <- "taxa/{encode(taxon)}/chromosomes/{encode(chromosome)}/genes?strand={encode(strand)}&start={encode(start)}&size={encode(size)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise getGenesAtLocation
#'
memgetGenesAtLocation <- memoise::memoise(getGenesAtLocation)

#' searchDatasets
#' @export
searchDatasets <- function (taxon = "", query = NA_character_, filter = NA_character_,
    offset = 0L, limit = 0L, sort = "+id", raw = FALSE, async = FALSE,
    memoised = FALSE, file = NA_character_, overwrite = FALSE)
{
    fname <- "searchDatasets"
    preprocessor <- processDatasets
    validators <- list(taxon = validateTaxon, query = validateQuery,
        filter = validateFilter, offset = validatePositiveInteger,
        limit = validatePositiveInteger, sort = validateSort)
    endpoint <- "annotations/{encode(taxon)}/search/{encode(query)}/datasets?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        newArgs <- as.list(match.call())[-1]
        newArgs$memoised <- F
        return(do.call(glue("mem{fname}"), newArgs))
    }
    if (!is.null(validators)) {
        for (v in names(validators)) {
            assign(v, eval(validators[[v]])(get(v), name = v))
        }
    }
    endpoint <- paste0(getOption("gemma.API", "https://gemma.msl.ubc.ca/rest/v2/"),
        gsub("/(NA|/)", "/", gsub("\\?[^=]+=NA", "\\?", gsub("&[^=]+=NA",
            "", glue(endpoint)))))
    envWhere <- environment()
    request <- quote(http_get(endpoint, options = switch(is.null(getOption("gemma.password",
        NULL)) + 1, list(userpwd = paste0(getOption("gemma.username"),
        ":", getOption("gemma.password"))), list()))$then(function(response) {
        if (response$status_code == 200) {
            mData <- tryCatch({
                fromJSON(rawToChar(response$content))$data
            }, error = function(e) {
                message(paste0("Failed to parse ", response$type,
                  " from ", response$url))
                warning(e$message)
                NULL
            })
            if (raw || length(mData) == 0) mOut <- mData else mOut <- eval(preprocessor,
                envir = envWhere)(mData)
            if (!is.null(file) && !is.na(file) && file.exists(file)) {
                if (!overwrite) warning(paste0(file, " exists. Not overwriting.")) else {
                  if (raw) write(mOut, paste0(tools::file_path_sans_ext(file),
                    ".json")) else saveRDS(mOut, paste0(tools::file_path_sans_ext(file),
                    ".rds"))
                }
            }
            mOut
        } else response
    }))
    if (!async)
        synchronise(eval(request, envir = envWhere))
    else eval(request)
}

#' Memoise searchDatasets
#'
memsearchDatasets <- memoise::memoise(searchDatasets)

#' taxonInfo
#' @export
taxonInfo <- function (taxon = NA_character_, request = NA_character_, ...,
    raw = FALSE, async = FALSE, memoised = FALSE, file = NA_character_,
    overwrite = FALSE)
{
    characteristicValue <- "taxon"
    argMap <- c(taxa = "getTaxa", datasets = "getTaxonDatasets",
        phenotypes = "getTaxonPhenotypes", phenoCandidateGenes = "getTaxonPhenotypeCandidates",
        gene = "getGeneOnTaxon", geneEvidence = "getEvidenceOnTaxon",
        geneLocation = "getGeneLocationOnTaxon", genesAtLocation = "getGenesAtLocation",
        datasets = "searchDatasets")
    if (!is.na(request) && !(request %in% names(argMap)))
        stop(paste0("Invalid request parameter. Options include: ",
            paste0(names(argMap), collapse = ", ")))
    if (is.na(request))
        request <- 1
    mCallable <- call(argMap[[request]], raw = raw, async = async,
        memoised = memoised, file = file, overwrite = overwrite)
    mCallable[[characteristicValue]] <- get(characteristicValue)
    for (i in names(list(...))) {
        mCallable[[i]] <- list(...)[[i]]
    }
    eval(mCallable, envir = parent.env(environment()))
}

forgetGemmaMemoised <- function() {
    forget(memgetDatasets)
    forget(memgetDatasetDEA)
    forget(memgetDatasetPCA)
    forget(memgetDatasetDE)
    forget(memgetDatasetSamples)
    forget(memgetDatasetSVD)
    forget(memgetDatasetPlatforms)
    forget(memgetDatasetAnnotations)
    forget(memgetDiffExpr)
    forget(memgetPlatforms)
    forget(memgetPlatformDatasets)
    forget(memgetPlatformElements)
    forget(memgetPlatformElementGenes)
    forget(memgetGenes)
    forget(memgetGeneEvidence)
    forget(memgetGeneLocation)
    forget(memgetGeneProbes)
    forget(memgetGeneGO)
    forget(memgetGeneCoexpression)
    forget(memgetTaxa)
    forget(memgetTaxonDatasets)
    forget(memgetTaxonPhenotypes)
    forget(memgetTaxonPhenotypeCandidates)
    forget(memgetGeneOnTaxon)
    forget(memgetEvidenceOnTaxon)
    forget(memgetGeneLocationOnTaxon)
    forget(memgetGenesAtLocation)
    forget(memsearchDatasets)
}
