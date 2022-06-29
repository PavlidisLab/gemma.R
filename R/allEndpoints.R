#' Datasets
#'
#' Lists datasets filtered and organized by given parameters
#'
#' @param datasets Optional, defaults to `empty`.
#' Limits the result to entities with given identifiers.
#' A list of identifiers, separated by commas (e.g:
#' `GSE2871,GSE2869,GSE2868`). Identifiers can either be the Dataset ID or
#' its short name. Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available.
#' Do not combine different identifiers in one query.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to 20. Limits the result to specified amount of
#' objects.
#' @param sort Optional, defaults to `+id`.
#' Sets the ordering property and direction.
#' Format is `[+,-][property name]`. E.g. `-accession` will translate to
#' descending ordering by the 'Accession' property.
#' Note that this does [not guarantee the order of the returned
#' entities!]{.description-imp} This is merely a signal to how the data
#' should be pre-sorted before the limit and offset are applied.
#' Nested properties are also supported (recursively).
#' E.g: `+curationDetails.lastTroubledEvent.date`
#' When
#' using in scripts, remember to URL-encode the '+' plus character (see
#' the compiled URL below).
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of value objects representing the objects that matched the
#' query.
#' Empty array if no objects matched.
#' A successful response may contain a sub-element with 'Geeq'
#' information, which aims to provide a unified metric to measure
#' experiments by the quality of their data, and their suitability for use
#' in Gemma. You can [read more about the geeq properties here](https://pavlidislab.github.io/Gemma/geeq.html).
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' getDatasetsInfo("GSE2018")
#' getDatasetsInfo(c("GSE2018", "GSE2872"))
getDatasetsInfo <- function(datasets = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "getDatasetsInfo"
    preprocessor <- processDatasets
    validators <- list(
        datasets = validateOptionalID, offset = validatePositiveInteger,
        limit = validateLimit, sort = validateSort
    )
    endpoint <- "datasets/{encode(datasets)}?&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        out <- memgetDatasetsInfo(
            datasets = datasets, offset = offset,
            limit = limit, sort = sort, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getDatasetsInfo
#'
#' @noRd
memgetDatasetsInfo <- function(datasets = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    mem_call <- memoise::memoise(getDatasetsInfo, cache = gemmaCache())
    mem_call(
        datasets = datasets, offset = offset, limit = limit,
        sort = sort, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' .getResultSets
#'
#' Lists resultSets filtered and organized by given parameters.
#'
#' @param resultSet Optional, defaults to empty. A single resultSet identifier (ex. 423176)
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return Varies
#' @keywords internal
#'
#' @examples
.getResultSets <- function(resultSet = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- TRUE
    header <- "text/tab-separated-values"
    isFile <- TRUE
    fname <- ".getResultSets"
    preprocessor <- processFile
    validators <- list(resultSet = validateOptionalID)
    endpoint <- "resultSets/{encode(resultSet)}"
    if (memoised) {
        out <- mem.getResultSets(
            resultSet = resultSet, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise .getResultSets
#'
#' @noRd
mem.getResultSets <- function(resultSet = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(.getResultSets, cache = gemmaCache())
    mem_call(
        resultSet = resultSet, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' .getResultSetFactors
#'
#' Returns the factor values for the queried resultSet.
#'
#' @param resultSet Optional, defaults to empty. A single resultSet identifier (ex. 423176)
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to 20. Limits the result to specified amount of
#' objects.
#' @param sort Optional, defaults to `+id`.
#' Sets the ordering property and direction.
#' Format is `[+,-][property name]`. E.g. `-accession` will translate to
#' descending ordering by the 'Accession' property.
#' Note that this does [not guarantee the order of the returned
#' entities!]{.description-imp} This is merely a signal to how the data
#' should be pre-sorted before the limit and offset are applied.
#' Nested properties are also supported (recursively).
#' E.g: `+curationDetails.lastTroubledEvent.date`
#' When
#' using in scripts, remember to URL-encode the '+' plus character (see
#' the compiled URL below).
#' @param excludeResults Only keep factor values and exclude numerical results from resultSets.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return Varies
#' @keywords internal
#'
#' @examples
.getResultSetFactors <- function(resultSet = NA_character_, dataset = NA_character_,
    offset = 0L, limit = 20L, sort = "+id", excludeResults = TRUE,
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    internal <- TRUE
    header <- ""
    isFile <- FALSE
    fname <- ".getResultSetFactors"
    preprocessor <- processResultSetFactors
    validators <- list(
        resultSet = validateOptionalID, dataset = validateOptionalID,
        offset = validatePositiveInteger, limit = validateLimit,
        sort = validateSort
    )
    endpoint <- "resultSets/{encode(resultSet)}?&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}&excludeResults={encode(excludeResults)}"
    if (memoised) {
        out <- mem.getResultSetFactors(
            resultSet = resultSet,
            dataset = dataset, offset = offset, limit = limit,
            sort = sort, excludeResults = excludeResults, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise .getResultSetFactors
#'
#' @noRd
mem.getResultSetFactors <- function(resultSet = NA_character_, dataset = NA_character_,
    offset = 0L, limit = 20L, sort = "+id", excludeResults = TRUE,
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    mem_call <- memoise::memoise(.getResultSetFactors, cache = gemmaCache())
    mem_call(
        resultSet = resultSet, dataset = dataset, offset = offset,
        limit = limit, sort = sort, excludeResults = excludeResults,
        raw = raw, memoised = FALSE, file = file, overwrite = overwrite
    )
}

#' getDatasetResultSets
#'
#' Lists the available resultSets for the queried dataset.
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return Varies
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' getDatasetResultSets("GSE2018")
getDatasetResultSets <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "getDatasetResultSets"
    preprocessor <- processDatasetResultSets
    validators <- list(dataset = validateID)
    endpoint <- "resultSets?datasets={encode(dataset)}"
    if (memoised) {
        out <- memgetDatasetResultSets(
            dataset = dataset, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getDatasetResultSets
#'
#' @noRd
memgetDatasetResultSets <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getDatasetResultSets, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' getDatasetExpression
#'
#' Dataset expression
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param filter The filtered version (`filter = TRUE`) corresponds to what is used in
#' most Gemma analyses, removing some probes/elements. Unfiltered includes
#' all elements.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return Varies
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' \donttest{
#' dat <- getDatasetExpression("GSE2018")
#' }
getDatasetExpression <- function(dataset, filter = FALSE, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- TRUE
    fname <- "getDatasetExpression"
    preprocessor <- processFile
    validators <- list(dataset = validateID, filter = validateBoolean)
    endpoint <- "datasets/{encode(dataset)}/data?filter={encode(filter)}"
    if (memoised) {
        out <- memgetDatasetExpression(
            dataset = dataset, filter = filter,
            raw = raw, memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getDatasetExpression
#'
#' @noRd
memgetDatasetExpression <- function(dataset, filter = FALSE, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getDatasetExpression, cache = gemmaCache())
    mem_call(
        dataset = dataset, filter = filter, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Dataset samples
#'
#' Retrieves samples for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of samples (bio assay value objects) in the given dataset.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' dat <- getDatasetSamples("GSE2018")
#' head(dat)
getDatasetSamples <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "getDatasetSamples"
    preprocessor <- processSamples
    validators <- list(dataset = function(name, ...) {
        ID <- unlist(list(...))
        if (length(ID) > 1) {
            stop(glue::glue("Please specify one valid identifier for {name}."),
                call. = FALSE
            )
        }
        validateID(name, ...)
    })
    endpoint <- "datasets/{encode(dataset)}/samples"
    if (memoised) {
        out <- memgetDatasetSamples(
            dataset = dataset, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getDatasetSamples
#'
#' @noRd
memgetDatasetSamples <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getDatasetSamples, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Dataset platforms
#'
#' Retrieves platforms for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of platforms (array design value objects) containing the given
#' dataset.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' getDatasetPlatforms("GSE2018")
getDatasetPlatforms <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "getDatasetPlatforms"
    preprocessor <- processPlatforms
    validators <- list(dataset = function(name, ...) {
        ID <- unlist(list(...))
        if (length(ID) > 1) {
            stop(glue::glue("Please specify one valid identifier for {name}."),
                call. = FALSE
            )
        }
        validateID(name, ...)
    })
    endpoint <- "datasets/{encode(dataset)}/platforms"
    if (memoised) {
        out <- memgetDatasetPlatforms(
            dataset = dataset, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getDatasetPlatforms
#'
#' @noRd
memgetDatasetPlatforms <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getDatasetPlatforms, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Dataset annotations
#'
#' Retrieves the annotations for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of annotations (annotation value objects) attached to the given
#' dataset.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' getDatasetAnnotations("GSE2018")
getDatasetAnnotations <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "getDatasetAnnotations"
    preprocessor <- processAnnotations
    validators <- list(dataset = function(name, ...) {
        ID <- unlist(list(...))
        if (length(ID) > 1) {
            stop(glue::glue("Please specify one valid identifier for {name}."),
                call. = FALSE
            )
        }
        validateID(name, ...)
    })
    endpoint <- "datasets/{encode(dataset)}/annotations"
    if (memoised) {
        out <- memgetDatasetAnnotations(
            dataset = dataset, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getDatasetAnnotations
#'
#' @noRd
memgetDatasetAnnotations <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getDatasetAnnotations, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Dataset design
#'
#' Retrieves the design for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return The design file for the given dataset.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' dat <- getDatasetDesign("GSE2018")
#' str(dat)
getDatasetDesign <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- TRUE
    fname <- "getDatasetDesign"
    preprocessor <- processFile
    validators <- list(dataset = function(name, ...) {
        ID <- unlist(list(...))
        if (length(ID) > 1) {
            stop(glue::glue("Please specify one valid identifier for {name}."),
                call. = FALSE
            )
        }
        validateID(name, ...)
    })
    endpoint <- "datasets/{encode(dataset)}/design"
    if (memoised) {
        out <- memgetDatasetDesign(
            dataset = dataset, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getDatasetDesign
#'
#' @noRd
memgetDatasetDesign <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getDatasetDesign, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Dataset differential analysis
#'
#' Retrieves the differential analysis results for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of analyses (differential expression value objects) in the
#' given dataset.
#' A `404 error` if the given identifier does not map to any object.
#' A `400 error` if required parameters are missing.
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' getDatasetDEA("GSE2018")
getDatasetDEA <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "getDatasetDEA"
    preprocessor <- processDEA
    validators <- list(dataset = function(name, ...) {
        ID <- unlist(list(...))
        if (length(ID) > 1) {
            stop(glue::glue("Please specify one valid identifier for {name}."),
                call. = FALSE
            )
        }
        validateID(name, ...)
    })
    endpoint <- "datasets/{encode(dataset)}/analyses/differential"
    if (memoised) {
        out <- memgetDatasetDEA(
            dataset = dataset, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getDatasetDEA
#'
#' @noRd
memgetDatasetDEA <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getDatasetDEA, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Platforms
#'
#' List platforms filtered and organized by given parameters
#'
#' @param platforms Optional, defaults to `empty`.
#' Limits the result to entities with given identifiers.
#' A list of identifiers, separated by commas (e.g:
#' `GPL96,GPL1355,GPL1261`). Identifiers can either be the Platform ID or
#' its short name. Retrieval by ID is more efficient.
#' Only platforms that user has access to will be available.
#' Do not combine different identifiers in one query.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to 20. Limits the result to specified amount of
#' objects.
#' @param sort Optional, defaults to `+id`.
#' Sets the ordering property and direction.
#' Format is `[+,-][property name]`. E.g. `-accession` will translate to
#' descending ordering by the 'Accession' property.
#' Note that this does [not guarantee the order of the returned
#' entities!]{.description-imp} This is merely a signal to how the data
#' should be pre-sorted before the limit and offset are applied.
#' Nested properties are also supported (recursively).
#' E.g: `+curationDetails.lastTroubledEvent.date`
#' When
#' using in scripts, remember to URL-encode the '+' plus character (see
#' the compiled URL below).
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of value objects representing the objects that matched the
#' query.
#' Empty array if no objects matched.
#' @export
#'
#' @keywords platform
#'
#' @examples
#' getPlatformsInfo("GPL1355")
#' getPlatformsInfo(c("GPL1355", "GPL96"))
getPlatformsInfo <- function(platforms = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    internal <- FALSE
    keyword <- "platform"
    header <- ""
    isFile <- FALSE
    fname <- "getPlatformsInfo"
    preprocessor <- processPlatforms
    validators <- list(
        platforms = validateOptionalID, offset = validatePositiveInteger,
        limit = validateLimit, sort = validateSort
    )
    endpoint <- "platforms/{encode(platforms)}?&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        out <- memgetPlatformsInfo(
            platforms = platforms, offset = offset,
            limit = limit, sort = sort, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getPlatformsInfo
#'
#' @noRd
memgetPlatformsInfo <- function(platforms = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    mem_call <- memoise::memoise(getPlatformsInfo, cache = gemmaCache())
    mem_call(
        platforms = platforms, offset = offset, limit = limit,
        sort = sort, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Platform datasets
#'
#' Retrieves experiments in the given platform
#'
#' @param platform Required, part of the URL path.
#' Can either be the platform ID or its short name (e.g: `GPL1355`)
#' Retrieval by ID is more efficient.
#' Only platforms that user has access to will be available.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to 20. Limits the result to specified amount of
#' objects.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of datasets (expression experiment value objects) that are on
#' the given platform.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords platform
#'
#' @examples
#' dat <- getPlatformDatasets("GPL1355")
#' str(dat, vec.len = 1)
getPlatformDatasets <- function(platform, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "platform"
    header <- ""
    isFile <- FALSE
    fname <- "getPlatformDatasets"
    preprocessor <- processDatasets
    validators <- list(
        platform = validateSingleID, offset = validatePositiveInteger,
        limit = validateLimit
    )
    endpoint <- "platforms/{encode(platform)}/datasets?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        out <- memgetPlatformDatasets(
            platform = platform, offset = offset,
            limit = limit, raw = raw, memoised = FALSE, file = file,
            overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getPlatformDatasets
#'
#' @noRd
memgetPlatformDatasets <- function(platform, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getPlatformDatasets, cache = gemmaCache())
    mem_call(
        platform = platform, offset = offset, limit = limit,
        raw = raw, memoised = FALSE, file = file, overwrite = overwrite
    )
}

#' Platform elements
#'
#' Retrieves the composite sequences (elements) for the given platform
#'
#' @param platform Required, part of the URL path.
#' Can either be the platform ID or its short name (e.g: `GPL1355`)
#' Retrieval by ID is more efficient.
#' Only platforms that user has access to will be available.
#' @param element Required, part of the URL path. Can either be the probe name or ID.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to 20. Limits the result to specified amount of
#' objects.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of elements (composite sequence value objects) of the given
#' platform.
#' Empty collection, if no elements matched the `elements` parameter.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords platform
#'
#' @examples
#' dat <- getPlatformElements("GPL1355")
#' str(dat, vec.len = 1, max.level = 1)
getPlatformElements <- function(platform, element = NA_character_, offset = 0L, limit = 20L,
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    internal <- FALSE
    keyword <- "platform"
    header <- ""
    isFile <- FALSE
    fname <- "getPlatformElements"
    preprocessor <- processElements
    validators <- list(
        platform = validateSingleID, element = validateOptionalID,
        offset = validatePositiveInteger, limit = validateLimit
    )
    endpoint <- "platforms/{encode(platform)}/elements/{encode(element)}?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        out <- memgetPlatformElements(
            platform = platform, element = element,
            offset = offset, limit = limit, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getPlatformElements
#'
#' @noRd
memgetPlatformElements <- function(platform, element = NA_character_, offset = 0L, limit = 20L,
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    mem_call <- memoise::memoise(getPlatformElements, cache = gemmaCache())
    mem_call(
        platform = platform, element = element, offset = offset,
        limit = limit, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Platform element genes
#'
#' Retrieves the genes on the given platform element
#'
#' @param platform Required, part of the URL path.
#' Can either be the platform ID or its short name (e.g: `GPL1355`)
#' Retrieval by ID is more efficient.
#' Only platforms that user has access to will be available.
#' @param element Required, part of the URL path. Can either be the probe name or ID.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to 20. Limits the result to specified amount of
#' objects.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of genes (gene value objects) aligned with the the given
#' platform element.
#' All identifiers must be valid.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords platform
#'
#' @examples
#' getPlatformElementGenes("GPL1355", "AFFX_Rat_beta-actin_M_at")
getPlatformElementGenes <- function(platform, element, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "platform"
    header <- ""
    isFile <- FALSE
    fname <- "getPlatformElementGenes"
    preprocessor <- processGenes
    validators <- list(
        platform = validateSingleID, element = validateSingleID,
        offset = validatePositiveInteger, limit = validateLimit
    )
    endpoint <- "platforms/{encode(platform)}/elements/{encode(element)}/genes?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        out <- memgetPlatformElementGenes(
            platform = platform,
            element = element, offset = offset, limit = limit,
            raw = raw, memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getPlatformElementGenes
#'
#' @noRd
memgetPlatformElementGenes <- function(platform, element, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getPlatformElementGenes, cache = gemmaCache())
    mem_call(
        platform = platform, element = element, offset = offset,
        limit = limit, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Genes
#'
#' Retrieves all genes matching the identifiers
#'
#' @param genes Required, part of the URL path.
#' A list of identifiers, separated by commas (e.g: `1859, 5728`).
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' Do not combine different identifiers in one query.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of value objects representing the objects that matched the
#' query.
#' Empty array if no objects matched.
#' A `400 error` if required parameters are missing.
#' @export
#'
#' @keywords gene
#'
#' @examples
#' getGenesInfo("DYRK1A")
#' getGenesInfo(c("DYRK1A", "PTEN"))
getGenesInfo <- function(genes, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "getGenesInfo"
    preprocessor <- processGenes
    validators <- list(genes = validateID)
    endpoint <- "genes/{encode((genes))}/"
    if (memoised) {
        out <- memgetGenesInfo(
            genes = genes, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getGenesInfo
#'
#' @noRd
memgetGenesInfo <- function(genes, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getGenesInfo, cache = gemmaCache())
    mem_call(
        genes = genes, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Gene locations
#'
#' Retrieves the physical location of the given gene
#'
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of locations (physical location value objects) of the given
#' gene or gene homologues.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords gene
#'
#' @examples
#' getGeneLocation("DYRK1A")
getGeneLocation <- function(gene, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "getGeneLocation"
    preprocessor <- processGeneLocation
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/locations"
    if (memoised) {
        out <- memgetGeneLocation(
            gene = gene, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getGeneLocation
#'
#' @noRd
memgetGeneLocation <- function(gene, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getGeneLocation, cache = gemmaCache())
    mem_call(
        gene = gene, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Gene probes
#'
#' Retrieves the probes (composite sequences) with this gene
#'
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to 20. Limits the result to specified amount of
#' objects.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of probes (composite sequence value objects) that are mapped to
#' this gene. Note, that it is possible for probes to map to multiple
#' genes.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords gene
#'
#' @examples
#' dat <- getGeneProbes("DYRK1A")
#' str(dat, vec.len = 2)
getGeneProbes <- function(gene, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "getGeneProbes"
    preprocessor <- processElements
    validators <- list(
        gene = validateSingleID, offset = validatePositiveInteger,
        limit = validateLimit
    )
    endpoint <- "genes/{encode(gene)}/probes?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        out <- memgetGeneProbes(
            gene = gene, offset = offset,
            limit = limit, raw = raw, memoised = FALSE, file = file,
            overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getGeneProbes
#'
#' @noRd
memgetGeneProbes <- function(gene, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getGeneProbes, cache = gemmaCache())
    mem_call(
        gene = gene, offset = offset, limit = limit, raw = raw,
        memoised = FALSE, file = file, overwrite = overwrite
    )
}

#' Gene goTerms
#'
#' Retrieves the GO terms of the given gene
#'
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of GO terms (gene ontology term value objects) associated with
#' the given gene.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords gene
#'
#' @examples
#' getGeneGO("DYRK1A")
getGeneGO <- function(gene, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "getGeneGO"
    preprocessor <- processGO
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/goTerms"
    if (memoised) {
        out <- memgetGeneGO(
            gene = gene, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise getGeneGO
#'
#' @noRd
memgetGeneGO <- function(gene, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(getGeneGO, cache = gemmaCache())
    mem_call(
        gene = gene, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Dataset search
#'
#' Does a search for datasets containing annotations, short name or full
#' name matching the given string
#'
#' @param query Required, defaults to `empty`.
#' The search query. Either plain text ('traumatic'), or an ontology term
#' URI ('http://purl.obolibrary.org/obo/UBERON_0002048'). Datasets that
#' contain the given string in their short of full name will also be
#' matched ('GSE201', 'Bronchoalveolar lavage samples'.
#' Can be multiple identifiers separated by commas.
#' When
#' using in scripts, remember to URL-encode any forward slashes in the
#' phenotype value URIs (see the compiled URL below).
#' @param taxon Not required, part of the URL path. can either be Taxon ID, Taxon NCBI
#' ID, or one of its string identifiers: scientific name, common name.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to 20. Limits the result to specified amount of
#' objects.
#' @param sort Optional, defaults to `+id`.
#' Sets the ordering property and direction.
#' Format is `[+,-][property name]`. E.g. `-accession` will translate to
#' descending ordering by the 'Accession' property.
#' Note that this does [not guarantee the order of the returned
#' entities!]{.description-imp} This is merely a signal to how the data
#' should be pre-sorted before the limit and offset are applied.
#' Nested properties are also supported (recursively).
#' E.g: `+curationDetails.lastTroubledEvent.date`
#' When
#' using in scripts, remember to URL-encode the '+' plus character (see
#' the compiled URL below).
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of datasets (expression experiment value objects) that are
#' annotated with the given ontology terms, or, in case of plaintext query,
#' experiments that contain the given words (name, short name, accession,
#' tags)
#' If an ontology term URI is given, the results will also include datasets
#' that are associated with the descendants of the term.
#' The search only only checks the annotations value, not the category
#' (which is also an ontology term).
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' dat <- searchDatasets("bipolar")
#' str(dat)
searchDatasets <- function(query, taxon = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "searchDatasets"
    preprocessor <- processDatasets
    validators <- list(
        query = validateQuery, taxon = validateOptionalTaxon,
        limit = validateLimit, sort = validateSort
    )
    endpoint <- "annotations/{encode(taxon)}/search/{encode(query)}/datasets?&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        out <- memsearchDatasets(
            query = query, taxon = taxon,
            offset = offset, limit = limit, sort = sort, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise searchDatasets
#'
#' @noRd
memsearchDatasets <- function(query, taxon = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    mem_call <- memoise::memoise(searchDatasets, cache = gemmaCache())
    mem_call(
        query = query, taxon = taxon, offset = offset, limit = limit,
        sort = sort, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Annotation search
#'
#' Does a search for annotations based on the given string
#'
#' @param query Required, defaults to `empty`.
#' The search query. Either plain text ('traumatic'), or an ontology term
#' URI ('http://purl.obolibrary.org/obo/UBERON_0002048'). Datasets that
#' contain the given string in their short of full name will also be
#' matched ('GSE201', 'Bronchoalveolar lavage samples'.
#' Can be multiple identifiers separated by commas.
#' When
#' using in scripts, remember to URL-encode any forward slashes in the
#' phenotype value URIs (see the compiled URL below).
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that
#' are omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of annotations (annotation search result value objects)
#' matching the given identifiers.
#' A `400 error` if required parameters are missing.
#' @export
#'
#' @keywords misc
#'
#' @examples
#' \donttest{
#' searchAnnotations("traumatic")
#' }
searchAnnotations <- function(query, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "misc"
    header <- ""
    isFile <- FALSE
    fname <- "searchAnnotations"
    preprocessor <- processSearchAnnotations
    validators <- list(query = validateQuery)
    endpoint <- "annotations/search/{encode(query)}"
    if (memoised) {
        out <- memsearchAnnotations(
            query = query, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, match.call()
    )
}

#' Memoise searchAnnotations
#'
#' @noRd
memsearchAnnotations <- function(query, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(searchAnnotations, cache = gemmaCache())
    mem_call(
        query = query, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}


#' Clear gemma.R cache
#'
#' Forget past results from memoised calls to the Gemma API (ie. using functions with memoised = `TRUE`)
#'
#' @return TRUE to indicate cache was cleared.
#' @examples
#' forgetGemmaMemoised()
#' @export
#'
#' @keywords misc
forgetGemmaMemoised <- forgetGemmaMemoised <- function() {
    mem <- memoise::memoise(function() {}, cache = gemmaCache())
    memoise::forget(mem)
}
