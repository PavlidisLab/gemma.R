#' getDatasets
#' @export
getDatasets <- function (datasets = NA_character_, filter = NA_character_, offset = 0L,
    limit = 20L, sort = "+id", raw = TRUE, async = FALSE, memoised = FALSE)
{
    fname <- "getDatasets"
    preprocessor <- processDatasets
    validators <- list(datasets = validateID, filter = validateFilter,
        offset = validatePositiveInteger, limit = validatePositiveInteger,
        sort = validateSort)
    endpoint <- "datasets/{encode(datasets)}?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getDatasets
#'
memgetDatasets <- memoise::memoise(getDatasets)

#' searchDatasets
#' @export
searchDatasets <- function (taxon = "", query = NA_character_, filter = NA_character_,
    offset = 0L, limit = 0L, sort = "+id", raw = TRUE, async = FALSE,
    memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise searchDatasets
#'
memsearchDatasets <- memoise::memoise(searchDatasets)

#' getDatasetDEA
#' @export
getDatasetDEA <- function (dataset = NA_character_, offset = 0L, limit = 20L,
    raw = TRUE, async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getDatasetDEA
#'
memgetDatasetDEA <- memoise::memoise(getDatasetDEA)

#' getDatasetPCA
#' @export
getDatasetPCA <- function (datasets = NA_character_, component = 1L, limit = 100L,
    keepNonSpecific = FALSE, consolidate = NA_character_, raw = TRUE,
    async = FALSE, memoised = FALSE)
{
    fname <- "getDatasetPCA"
    preprocessor <- processExpression
    validators <- list(datasets = validateID, component = validatePositiveInteger,
        limit = validatePositiveInteger, keepNonSpecific = validateBoolean,
        consolidate = validateConsolidate)
    endpoint <- "datasets/{encode(datasets)}/expressions/pca?component={encode(component)}&limit={encode(limit)}&keepNonSpecific={encode(keepNonSpecific)}&consolidate={encode(consolidate)}"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getDatasetPCA
#'
memgetDatasetPCA <- memoise::memoise(getDatasetPCA)

#' getDatasetDE
#' @export
getDatasetDE <- function (datasets = NA_character_, keepNonSpecific = FALSE,
    diffExSet = NA_integer_, threshold = 100, limit = 100L, consolidate = NA_character_,
    raw = TRUE, async = FALSE, memoised = FALSE)
{
    fname <- "getDatasetDE"
    preprocessor <- processExpression
    validators <- list(datasets = validateID, keepNonSpecific = validateBoolean,
        diffExSet = validatePositiveInteger, threshold = validatePositiveReal,
        limit = validatePositiveInteger, consolidate = validateConsolidate)
    endpoint <- "datasets/{encode(datasets)}/expressions/differential?keepNonSpecific={encode(keepNonSpecific)}&diffExSet={encode(diffExSet)}&threshold={encode(threshold)}&limit={encode(limit)}&consolidate={encode(consolidate)}"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getDatasetDE
#'
memgetDatasetDE <- memoise::memoise(getDatasetDE)

#' getDatasetSamples
#' @export
getDatasetSamples <- function (dataset = NA_character_, raw = TRUE, async = FALSE,
    memoised = FALSE)
{
    fname <- "getDatasetSamples"
    preprocessor <- preprocessor
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/samples"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getDatasetSamples
#'
memgetDatasetSamples <- memoise::memoise(getDatasetSamples)

#' getDatasetSVD
#' @export
getDatasetSVD <- function (dataset = NA_character_, raw = TRUE, async = FALSE,
    memoised = FALSE)
{
    fname <- "getDatasetSVD"
    preprocessor <- preprocessor
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/svd"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getDatasetSVD
#'
memgetDatasetSVD <- memoise::memoise(getDatasetSVD)

#' getDatasetPlatforms
#' @export
getDatasetPlatforms <- function (dataset = NA_character_, raw = TRUE, async = FALSE,
    memoised = FALSE)
{
    fname <- "getDatasetPlatforms"
    preprocessor <- preprocessor
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/platforms"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getDatasetPlatforms
#'
memgetDatasetPlatforms <- memoise::memoise(getDatasetPlatforms)

#' getDatasetAnnotations
#' @export
getDatasetAnnotations <- function (dataset = NA_character_, raw = TRUE, async = FALSE,
    memoised = FALSE)
{
    fname <- "getDatasetAnnotations"
    preprocessor <- preprocessor
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/annotations"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getDatasetAnnotations
#'
memgetDatasetAnnotations <- memoise::memoise(getDatasetAnnotations)

#' getPlatforms
#' @export
getPlatforms <- function (platforms = NA_character_, filter = NA_character_,
    offset = 0L, limit = 20L, sort = "+id", raw = TRUE, async = FALSE,
    memoised = FALSE)
{
    fname <- "getPlatforms"
    preprocessor <- processPlatforms
    validators <- list(filter = validateFilter, offset = validatePositiveInteger,
        limit = validatePositiveInteger, sort = validateSort)
    endpoint <- "platforms/{encode(platforms)}?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getPlatforms
#'
memgetPlatforms <- memoise::memoise(getPlatforms)

#' getPlatformDatasets
#' @export
getPlatformDatasets <- function (platform = NA_character_, offset = 0L, limit = 20L,
    raw = TRUE, async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getPlatformDatasets
#'
memgetPlatformDatasets <- memoise::memoise(getPlatformDatasets)

#' getPlatformElements
#' @export
getPlatformElements <- function (platform = NA_character_, offset = 0L, limit = 20L,
    raw = TRUE, async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getPlatformElements
#'
memgetPlatformElements <- memoise::memoise(getPlatformElements)

#' getPlatformElementGenes
#' @export
getPlatformElementGenes <- function (platform = NA_character_, element = NA_character_,
    offset = 0L, limit = 20L, raw = TRUE, async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getPlatformElementGenes
#'
memgetPlatformElementGenes <- memoise::memoise(getPlatformElementGenes)

#' getGenes
#' @export
getGenes <- function (gene = NA_character_, raw = TRUE, async = FALSE, memoised = FALSE)
{
    fname <- "getGenes"
    preprocessor <- preprocessor
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getGenes
#'
memgetGenes <- memoise::memoise(getGenes)

#' getGeneEvidence
#' @export
getGeneEvidence <- function (gene = NA_character_, raw = TRUE, async = FALSE, memoised = FALSE)
{
    fname <- "getGeneEvidence"
    preprocessor <- preprocessor
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/evidence"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getGeneEvidence
#'
memgetGeneEvidence <- memoise::memoise(getGeneEvidence)

#' getGeneLocation
#' @export
getGeneLocation <- function (gene = NA_character_, raw = TRUE, async = FALSE, memoised = FALSE)
{
    fname <- "getGeneLocation"
    preprocessor <- preprocessor
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/locations"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getGeneLocation
#'
memgetGeneLocation <- memoise::memoise(getGeneLocation)

#' getGeneProbes
#' @export
getGeneProbes <- function (gene = NA_character_, offset = 0L, limit = 20L, raw = TRUE,
    async = FALSE, memoised = FALSE)
{
    fname <- "getGeneProbes"
    preprocessor <- processElements
    validators <- list(gene = validateSingleID, offset = validatePositiveInteger,
        limit = validatePositiveInteger)
    endpoint <- "genes/{encode(gene)}/probes?offset={encode(offset)}&limit={encode(limit)}"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getGeneProbes
#'
memgetGeneProbes <- memoise::memoise(getGeneProbes)

#' getGeneGO
#' @export
getGeneGO <- function (gene = NA_character_, raw = TRUE, async = FALSE, memoised = FALSE)
{
    fname <- "getGeneGO"
    preprocessor <- preprocessor
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/goTerms"
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getGeneGO
#'
memgetGeneGO <- memoise::memoise(getGeneGO)

#' getGeneCoexpression
#' @export
getGeneCoexpression <- function (gene = NA_character_, with = NA_character_, limit = 20L,
    stringency = 1L, raw = TRUE, async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getGeneCoexpression
#'
memgetGeneCoexpression <- memoise::memoise(getGeneCoexpression)

#' getTaxonDatasets
#' @export
getTaxonDatasets <- function (taxon = NA_character_, filter = NA_character_, offset = 0L,
    limit = 20L, sort = "+id", raw = TRUE, async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getTaxonDatasets
#'
memgetTaxonDatasets <- memoise::memoise(getTaxonDatasets)

#' getTaxonPhenotypes
#' @export
getTaxonPhenotypes <- function (taxon = NA_character_, editableOnly = FALSE, tree = FALSE,
    raw = TRUE, async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getTaxonPhenotypes
#'
memgetTaxonPhenotypes <- memoise::memoise(getTaxonPhenotypes)

#' getTaxonPhenotypeCandidates
#' @export
getTaxonPhenotypeCandidates <- function (taxon = NA_character_, editableOnly = FALSE, phenotypes = NA_character_,
    raw = TRUE, async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getTaxonPhenotypeCandidates
#'
memgetTaxonPhenotypeCandidates <- memoise::memoise(getTaxonPhenotypeCandidates)

#' getGeneOnTaxon
#' @export
getGeneOnTaxon <- function (taxon = NA_character_, gene = NA_character_, raw = TRUE,
    async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getGeneOnTaxon
#'
memgetGeneOnTaxon <- memoise::memoise(getGeneOnTaxon)

#' getEvidenceOnTaxon
#' @export
getEvidenceOnTaxon <- function (taxon = NA_character_, gene = NA_character_, raw = TRUE,
    async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getEvidenceOnTaxon
#'
memgetEvidenceOnTaxon <- memoise::memoise(getEvidenceOnTaxon)

#' getGeneLocationOnTaxon
#' @export
getGeneLocationOnTaxon <- function (taxon = NA_character_, gene = NA_character_, raw = TRUE,
    async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getGeneLocationOnTaxon
#'
memgetGeneLocationOnTaxon <- memoise::memoise(getGeneLocationOnTaxon)

#' getGenesAtLocation
#' @export
getGenesAtLocation <- function (taxon = NA_character_, chromosome = NA_character_,
    strand = "+", start = NA_integer_, size = NA_integer_, raw = TRUE,
    async = FALSE, memoised = FALSE)
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
    request <- quote(http_get(endpoint)$then(function(response) {
        if (response$status_code == 200) {
            mData <- fromJSON(rawToChar(response$content))$data
            if (raw) mData else eval(preprocessor)(mData)
        } else response
    }))
    if (!async)
        synchronise(eval(request))
    else eval(request)
}

#' Memoise getGenesAtLocation
#'
memgetGenesAtLocation <- memoise::memoise(getGenesAtLocation)

forgetGemmaMemoised <- {
    memoise::forget(memgetDatasets)
    memoise::forget(memsearchDatasets)
    memoise::forget(memgetDatasetDEA)
    memoise::forget(memgetDatasetPCA)
    memoise::forget(memgetDatasetDE)
    memoise::forget(memgetDatasetSamples)
    memoise::forget(memgetDatasetSVD)
    memoise::forget(memgetDatasetPlatforms)
    memoise::forget(memgetDatasetAnnotations)
    memoise::forget(memgetPlatforms)
    memoise::forget(memgetPlatformDatasets)
    memoise::forget(memgetPlatformElements)
    memoise::forget(memgetPlatformElementGenes)
    memoise::forget(memgetGenes)
    memoise::forget(memgetGeneEvidence)
    memoise::forget(memgetGeneLocation)
    memoise::forget(memgetGeneProbes)
    memoise::forget(memgetGeneGO)
    memoise::forget(memgetGeneCoexpression)
    memoise::forget(memgetTaxonDatasets)
    memoise::forget(memgetTaxonPhenotypes)
    memoise::forget(memgetTaxonPhenotypeCandidates)
    memoise::forget(memgetGeneOnTaxon)
    memoise::forget(memgetEvidenceOnTaxon)
    memoise::forget(memgetGeneLocationOnTaxon)
    memoise::forget(memgetGenesAtLocation)
}
