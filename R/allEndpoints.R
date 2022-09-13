#' Get datasets by ids
#'
#' Retrieve datasets by their identifiers
#'
#' @param datasets Numerical dataset identifiers or dataset short names
#' @param offset The offset of the first retrieved result.
#' @param limit Optional, defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param sort Order results by the given property and direction. The '+' sign indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @inherit processDatasets return
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' get_datasets_by_ids("GSE2018")
#' get_datasets_by_ids(c("GSE2018", "GSE2872"))
get_datasets_by_ids <- function(datasets = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE), attributes = getOption(
        "gemma.attributes",
        TRUE
    )) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "get_datasets_by_ids"
    preprocessor <- processDatasets
    validators <- list(
        datasets = validateOptionalID, offset = validatePositiveInteger,
        limit = validateLimit, sort = validateSort
    )
    endpoint <- "datasets/{encode(datasets)}?&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_datasets_by_ids(
            datasets = datasets, offset = offset,
            limit = limit, sort = sort, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite, attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_datasets_by_ids
#'
#' @noRd
memget_datasets_by_ids <- function(datasets = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE), attributes = getOption(
        "gemma.attributes",
        TRUE
    )) {
    mem_call <- memoise::memoise(get_datasets_by_ids, cache = gemmaCache())
    mem_call(
        datasets = datasets, offset = offset, limit = limit,
        sort = sort, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite, attributes = attributes
    )
}

#' Get result set
#'
#' Retrieve a single analysis result set by its identifier
#'
#' @param resultSet An expression analysis result set numerical identifier.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
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
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- TRUE
    header <- "text/tab-separated-values"
    isFile <- TRUE
    fname <- ".getResultSets"
    preprocessor <- processFile
    validators <- list(resultSet = validateOptionalID)
    endpoint <- "resultSets/{encode(resultSet)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- mem.getResultSets(
            resultSet = resultSet, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
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
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(.getResultSets, cache = gemmaCache())
    mem_call(
        resultSet = resultSet, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite, attributes = attributes
    )
}

#' Get result set
#'
#' Retrieve a single analysis result set by its identifier
#'
#' @param resultSet An expression analysis result set numerical identifier.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return Varies
#' @keywords internal
#'
#' @examples
.getResultSetFactors <- function(resultSet = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- TRUE
    header <- ""
    isFile <- FALSE
    fname <- ".getResultSetFactors"
    preprocessor <- processResultSetFactors
    validators <- list(resultSet = validateOptionalID)
    endpoint <- "resultSets/{encode(resultSet)}?excludeResults=true"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- mem.getResultSetFactors(
            resultSet = resultSet,
            raw = raw, memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise .getResultSetFactors
#'
#' @noRd
mem.getResultSetFactors <- function(resultSet = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(.getResultSetFactors, cache = gemmaCache())
    mem_call(
        resultSet = resultSet, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite, attributes = attributes
    )
}

#' Get result sets
#'
#' Retrieve all result sets matching the provided criteria
#'
#' @param datasets A numerical dataset identifier or a dataset short name
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @inherit processDatasetResultSets return
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' resultSets <- get_result_sets("GSE2018")
#' get_differential_expression_values(resultSet = resultSets$resultSet.id)
get_result_sets <- function(datasets, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "get_result_sets"
    preprocessor <- processDatasetResultSets
    validators <- list(datasets = validateID)
    endpoint <- "resultSets?datasets={encode(datasets)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_result_sets(
            datasets = datasets, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_result_sets
#'
#' @noRd
memget_result_sets <- function(datasets, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_result_sets, cache = gemmaCache())
    mem_call(
        datasets = datasets, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite, attributes = attributes
    )
}

#' Get dataset expression
#'
#' Retrieve the expression data of a dataset
#'
#' @param dataset A numerical dataset identifier or a dataset short name
#' @param filter The filtered version (\code{filter = TRUE}) corresponds to what is
#' used in most Gemma analyses, removing some probes/elements. Unfiltered
#' includes all elements.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return If raw is FALSE (default), a data table of the expression matrix for
#' the queried dataset. If raw is TRUE, returns the binary file in raw form.
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' get_dataset_expression("GSE2018")
get_dataset_expression <- function(dataset, filter = FALSE, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- TRUE
    fname <- "get_dataset_expression"
    preprocessor <- processFile
    validators <- list(dataset = validateID, filter = validateBoolean)
    endpoint <- "datasets/{encode(dataset)}/data?filter={encode(filter)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_dataset_expression(
            dataset = dataset, filter = filter,
            raw = raw, memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_dataset_expression
#'
#' @noRd
memget_dataset_expression <- function(dataset, filter = FALSE, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_dataset_expression, cache = gemmaCache())
    mem_call(
        dataset = dataset, filter = filter, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite, attributes = attributes
    )
}

#' Get dataset samples
#'
#' Retrieve the samples of a dataset
#'
#' @param dataset A numerical dataset identifier or a dataset short name
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @inherit processSamples return
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' head(get_dataset_samples("GSE2018"))
get_dataset_samples <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "get_dataset_samples"
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
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_dataset_samples(
            dataset = dataset, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_dataset_samples
#'
#' @noRd
memget_dataset_samples <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_dataset_samples, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite, attributes = attributes
    )
}

#' Get dataset platforms
#'
#' Retrieve the platform of a dataset
#'
#' @param dataset A numerical dataset identifier or a dataset short name
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @inherit processPlatforms return
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' get_dataset_platforms("GSE2018")
get_dataset_platforms <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "get_dataset_platforms"
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
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_dataset_platforms(
            dataset = dataset, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_dataset_platforms
#'
#' @noRd
memget_dataset_platforms <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_dataset_platforms, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite, attributes = attributes
    )
}

#' Get dataset annotations
#'
#' Retrieve the annotations analysis of a dataset
#'
#' @param dataset A numerical dataset identifier or a dataset short name
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with information about the annotations of the queried
#' dataset.A \code{404 error} if the given identifier does not map to any object
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' get_dataset_annotations("GSE2018")
get_dataset_annotations <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "get_dataset_annotations"
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
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_dataset_annotations(
            dataset = dataset,
            raw = raw, memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_dataset_annotations
#'
#' @noRd
memget_dataset_annotations <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_dataset_annotations, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite, attributes = attributes
    )
}

#' Get dataset design
#'
#' Retrieve the design of a dataset
#'
#' @param dataset A numerical dataset identifier or a dataset short name
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table of the design matrix for the queried dataset.
#' A \code{404 error} if the given identifier does not map to any object
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' head(get_dataset_design("GSE2018"))
get_dataset_design <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- TRUE
    fname <- "get_dataset_design"
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
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_dataset_design(
            dataset = dataset, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_dataset_design
#'
#' @noRd
memget_dataset_design <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_dataset_design, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite, attributes = attributes
    )
}

#' Get dataset differential expression analyses
#'
#' Retrieve the differential analyses of a dataset
#'
#' @param dataset A numerical dataset identifier or a dataset short name
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with information about the differential expression
#' analysis of the queried dataset. Note that this funciton does not return
#' differential expression values themselves. Use \code{\link{get_differential_expression_values}}
#' to get differential expression values (see examples).
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' result <- get_dataset_differential_expression_analyses("GSE2018")
#' get_differential_expression_values(resultSet = result$result.ID)
get_dataset_differential_expression_analyses <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "get_dataset_differential_expression_analyses"
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
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_dataset_differential_expression_analyses(
            dataset = dataset,
            raw = raw, memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_dataset_differential_expression_analyses
#'
#' @noRd
memget_dataset_differential_expression_analyses <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_dataset_differential_expression_analyses,
        cache = gemmaCache()
    )
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite, attributes = attributes
    )
}

#' Get platforms by ids
#'
#' Retrieve all platforms matching a set of platform identifiers
#'
#' @param platforms Platform numerical identifiers or platform short names
#' @param offset The offset of the first retrieved result.
#' @param limit Optional, defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param sort Order results by the given property and direction. The '+' sign indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @inherit processPlatforms return
#' @export
#'
#' @keywords platform
#'
#' @examples
#' get_platforms("GPL1355")
#' get_platforms(c("GPL1355", "GPL96"))
get_platforms_by_ids <- function(platforms = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE), attributes = getOption(
        "gemma.attributes",
        TRUE
    )) {
    internal <- FALSE
    keyword <- "platform"
    header <- ""
    isFile <- FALSE
    fname <- "get_platforms_by_ids"
    preprocessor <- processPlatforms
    validators <- list(
        platforms = validateOptionalID, offset = validatePositiveInteger,
        limit = validateLimit, sort = validateSort
    )
    endpoint <- "platforms/{encode(platforms)}?&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_platforms_by_ids(
            platforms = platforms,
            offset = offset, limit = limit, sort = sort, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_platforms_by_ids
#'
#' @noRd
memget_platforms_by_ids <- function(platforms = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE), attributes = getOption(
        "gemma.attributes",
        TRUE
    )) {
    mem_call <- memoise::memoise(get_platforms_by_ids, cache = gemmaCache())
    mem_call(
        platforms = platforms, offset = offset, limit = limit,
        sort = sort, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite, attributes = attributes
    )
}

#' Get platform datasets
#'
#' Retrieve all experiments within a given platform
#'
#' @param platform A platform numerical identifier or a platform short name
#' @param offset The offset of the first retrieved result.
#' @param limit Optional, defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with information about the datasets associated with the
#' queried platform. A \code{404 error} if the given identifier does not map to
#' any object
#' @export
#'
#' @keywords platform
#'
#' @examples
#' head(get_platform_datasets("GPL1355"))
get_platform_datasets <- function(platform, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "platform"
    header <- ""
    isFile <- FALSE
    fname <- "get_platform_datasets"
    preprocessor <- processDatasets
    validators <- list(
        platform = validateSingleID, offset = validatePositiveInteger,
        limit = validateLimit
    )
    endpoint <- "platforms/{encode(platform)}/datasets?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_platform_datasets(
            platform = platform,
            offset = offset, limit = limit, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite, attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_platform_datasets
#'
#' @noRd
memget_platform_datasets <- function(platform, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_platform_datasets, cache = gemmaCache())
    mem_call(
        platform = platform, offset = offset, limit = limit,
        raw = raw, memoised = FALSE, file = file, overwrite = overwrite,
        attributes = attributes
    )
}

#' Get platform element genes
#'
#' Retrieve the genes associated to a probe in a given platform
#'
#' @param platform A platform numerical identifier or a platform short name
#' @param probe A probe name or it's numerical identifier
#' @param offset The offset of the first retrieved result.
#' @param limit Optional, defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with information about the gene(s) on the queried
#' platform element. A \code{404 error} if the given identifier does not map to
#' any object
#' @export
#'
#' @keywords platform
#'
#' @examples
#' get_platform_element_genes("GPL1355", "AFFX_Rat_beta-actin_M_at")
get_platform_element_genes <- function(platform, probe, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "platform"
    header <- ""
    isFile <- FALSE
    fname <- "get_platform_element_genes"
    preprocessor <- processGenes
    validators <- list(
        platform = validateSingleID, probe = validateSingleID,
        offset = validatePositiveInteger, limit = validateLimit
    )
    endpoint <- "platforms/{encode(platform)}/elements/{encode(probe)}/genes?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_platform_element_genes(
            platform = platform,
            probe = probe, offset = offset, limit = limit, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_platform_element_genes
#'
#' @noRd
memget_platform_element_genes <- function(platform, probe, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_platform_element_genes,
        cache = gemmaCache()
    )
    mem_call(
        platform = platform, probe = probe, offset = offset,
        limit = limit, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite, attributes = attributes
    )
}

#' Get genes
#'
#' Retrieve genes matching a gene identifier
#'
#' @param genes An ensembl gene identifier which typically starts with ensg or an ncbi gene identifier or an official gene symbol approved by hgnc
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with information about the queried gene(s).
#' @export
#'
#' @keywords gene
#'
#' @examples
#' get_genes("DYRK1A")
#' get_genes(c("DYRK1A", "PTEN"))
get_genes <- function(genes, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "get_genes"
    preprocessor <- processGenes
    validators <- list(genes = validateID)
    endpoint <- "genes/{encode((genes))}/"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_genes(
            genes = genes, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite, attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_genes
#'
#' @noRd
memget_genes <- function(genes, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_genes, cache = gemmaCache())
    mem_call(
        genes = genes, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite, attributes = attributes
    )
}

#' Get gene locations
#'
#' Retrieve the physical locations of a given gene
#'
#' @param gene An ensembl gene identifier which typically starts with ensg or an ncbi gene identifier or an official gene symbol approved by hgnc
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with information about the physical location of the
#' queried gene. A \code{404 error} if the given identifier does not map to any object
#' @export
#'
#' @keywords gene
#'
#' @examples
#' get_gene_locations("DYRK1A")
get_gene_locations <- function(gene, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "get_gene_locations"
    preprocessor <- processGeneLocation
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/locations"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_gene_locations(
            gene = gene, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_gene_locations
#'
#' @noRd
memget_gene_locations <- function(gene, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_gene_locations, cache = gemmaCache())
    mem_call(
        gene = gene, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite, attributes = attributes
    )
}

#' Get gene probes
#'
#' Retrieve the probes associated to a genes
#'
#' @param gene An ensembl gene identifier which typically starts with ensg or an ncbi gene identifier or an official gene symbol approved by hgnc
#' @param offset The offset of the first retrieved result.
#' @param limit Optional, defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with information about the physical location of the
#' queried gene. A \code{404 error} if the given identifier does not map to any
#' object.
#' @export
#'
#' @keywords gene
#'
#' @examples
#' get_gene_probes("DYRK1A")
get_gene_probes <- function(gene, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "get_gene_probes"
    preprocessor <- processElements
    validators <- list(
        gene = validateSingleID, offset = validatePositiveInteger,
        limit = validateLimit
    )
    endpoint <- "genes/{encode(gene)}/probes?offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_gene_probes(
            gene = gene, offset = offset,
            limit = limit, raw = raw, memoised = FALSE, file = file,
            overwrite = overwrite, attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_gene_probes
#'
#' @noRd
memget_gene_probes <- function(gene, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_gene_probes, cache = gemmaCache())
    mem_call(
        gene = gene, offset = offset, limit = limit, raw = raw,
        memoised = FALSE, file = file, overwrite = overwrite,
        attributes = attributes
    )
}

#' Get gene go terms
#'
#' Retrieve the GO terms associated to a gene
#'
#' @param gene An ensembl gene identifier which typically starts with ensg or an ncbi gene identifier or an official gene symbol approved by hgnc
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with information about the GO terms assigned to the
#' queried gene. A \code{404 error} if the given identifier does not map to any
#' object. Go terms were updated on June 10 2022
#' @export
#'
#' @keywords gene
#'
#' @examples
#' get_gene_go_terms("DYRK1A")
get_gene_go_terms <- function(gene, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "get_gene_go_terms"
    preprocessor <- processGO
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/goTerms"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_gene_go_terms(
            gene = gene, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite, attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_gene_go_terms
#'
#' @noRd
memget_gene_go_terms <- function(gene, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_gene_go_terms, cache = gemmaCache())
    mem_call(
        gene = gene, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite, attributes = attributes
    )
}

#' Search taxon datasets
#'
#' Retrieve datasets within a given taxa associated to an annotation tags search
#'
#' @param query The search query. Either plain text ('traumatic'), or an ontology
#' term (UBERON_0002048). Datasets that contain the given string in their short
#' or full name will also be matched. Can be multiple identifiers separated by commas.
#' @param taxon A numerical taxon identifier or an ncbi taxon identifier or a taxon identifier that matches either its scientific or common name
#' @param offset The offset of the first retrieved result.
#' @param limit Optional, defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param sort Order results by the given property and direction. The '+' sign indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with information about matching datasets. Returns an
#' empty list if no datasets found. Lists dataset (expression experiment value
#' objects) that are annotated with the given ontology terms, or, in case of
#' plaintext query, experiments that contain the given words (name, short name,
#' accession, tags) If an ontology term URI is given, the results will also
#' include datasets that are associated with the descendants of the term. The
#' search only only checks the annotations value, not the category (which is
#' also an ontology term).
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' search_datasets("bipolar")
search_datasets <- function(query, taxon = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE), attributes = getOption(
        "gemma.attributes",
        TRUE
    )) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "search_datasets"
    preprocessor <- processDatasets
    validators <- list(
        query = validateQuery, taxon = validateOptionalTaxon,
        limit = validateLimit, sort = validateSort
    )
    endpoint <- "annotations/{encode(taxon)}/search/{encode(query)}/datasets?&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memsearch_datasets(
            query = query, taxon = taxon,
            offset = offset, limit = limit, sort = sort, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise search_datasets
#'
#' @noRd
memsearch_datasets <- function(query, taxon = NA_character_, offset = 0L, limit = 20L,
    sort = "+id", raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE), attributes = getOption(
        "gemma.attributes",
        TRUE
    )) {
    mem_call <- memoise::memoise(search_datasets, cache = gemmaCache())
    mem_call(
        query = query, taxon = taxon, offset = offset, limit = limit,
        sort = sort, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite, attributes = attributes
    )
}

#' Search annotations
#'
#' Search for annotation tags
#'
#' @param query The search query
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with annotations (annotation search result value objects)
#' matching the given identifiers. A \code{400 error} if required parameters are missing.
#' @export
#'
#' @keywords misc
#'
#' @examples
#' search_annotations("traumatic")
search_annotations <- function(query, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "misc"
    header <- ""
    isFile <- FALSE
    fname <- "search_annotations"
    preprocessor <- processSearchAnnotations
    validators <- list(query = validateQuery)
    endpoint <- "annotations/search/{encode(query)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memsearch_annotations(
            query = query, raw = raw,
            memoised = FALSE, file = file, overwrite = overwrite,
            attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise search_annotations
#'
#' @noRd
memsearch_annotations <- function(query, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(search_annotations, cache = gemmaCache())
    mem_call(
        query = query, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite, attributes = attributes
    )
}

#' Get taxa by ids
#'
#' Retrieve taxa by their identifiers
#'
#' @param taxa Limits the result to entities with given identifiers.
#' A vector of identifiers.
#' Identifiers can be the any of the following:
#' \itemize{
#' \item taxon ID
#' \item scientific name
#' \item common name
#' Retrieval by ID is more efficient.
#' Do not combine different identifiers in one query.
#' For convenience, below is a list of officially supported taxa
#' \tabular{rllr}{
#' \strong{ID} \tab \strong{Comm.name} \tab \strong{Scient.name}    \tab \strong{NcbiID}\cr
#' 1            \tab human               \tab Homo sapiens             \tab 9606            \cr
#' 2            \tab mouse               \tab Mus musculus             \tab 10090           \cr
#' 3            \tab rat                 \tab Rattus norvegicus        \tab 10116           \cr
#' 11           \tab yeast               \tab Saccharomyces cerevisiae \tab 4932            \cr
#' 12           \tab zebrafish           \tab Danio rerio              \tab 7955            \cr
#' 13           \tab fly                 \tab Drosophila melanogaster  \tab 7227            \cr
#' 14           \tab worm                \tab Caenorhabditis elegans   \tab 6239
#' }
#' }
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return A data table with the queried taxa's details.
#' @keywords internal
#'
#' @examples
#' gemma.R:::get_taxa_by_ids(c("mouse", "human"))
get_taxa_by_ids <- function(taxa, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- TRUE
    header <- ""
    isFile <- FALSE
    fname <- "get_taxa_by_ids"
    preprocessor <- processTaxon
    validators <- list(taxa = validateTaxa)
    endpoint <- "taxa/{encode(taxa)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_taxa_by_ids(
            taxa = taxa, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite, attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_taxa_by_ids
#'
#' @noRd
memget_taxa_by_ids <- function(taxa, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_taxa_by_ids, cache = gemmaCache())
    mem_call(
        taxa = taxa, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite, attributes = attributes
    )
}

#' Get taxon datasets
#'
#' Retrieve the datasets for a given taxon
#'
#' @param taxon can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers: scientific name, common name.
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers available.
#' Use the \code{\link{get_taxa_by_ids}} function to retrieve the necessary information. For convenience, below is a list of officially supported taxa:
#' \tabular{rllr}{
#' \strong{ID} \tab \strong{Comm.name} \tab \strong{Scient.name}    \tab \strong{NcbiID}\cr
#' 1            \tab human               \tab Homo sapiens             \tab 9606            \cr
#' 2            \tab mouse               \tab Mus musculus             \tab 10090           \cr
#' 3            \tab rat                 \tab Rattus norvegicus        \tab 10116           \cr
#' 11           \tab yeast               \tab Saccharomyces cerevisiae \tab 4932            \cr
#' 12           \tab zebrafish           \tab Danio rerio              \tab 7955            \cr
#' 13           \tab fly                 \tab Drosophila melanogaster  \tab 7227            \cr
#' 14           \tab worm                \tab Caenorhabditis elegans   \tab 6239
#' }
#' @param offset The offset of the first retrieved result.
#' @param limit Optional, defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param sort Order results by the given property and direction. The '+' sign indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @return "A data table with information about the datasets associated with
#' the queried taxon. A \verb{404 error} if the given identifier does not map
#' to any object."
#' @export
#'
#' @keywords taxon
#'
#' @examples
#' get_taxon_datasets("human")
get_taxon_datasets <- function(taxon, offset = 0L, limit = 20, sort = "+id", raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    internal <- FALSE
    keyword <- "taxon"
    header <- ""
    isFile <- FALSE
    fname <- "get_taxon_datasets"
    preprocessor <- processDatasets
    validators <- list(
        taxon = validateTaxon, offset = validatePositiveInteger,
        limit = validatePositiveInteger, sort = validateSort
    )
    endpoint <- "taxa/{encode(taxon)}/datasets/?offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memget_taxon_datasets(
            taxon = taxon, offset = offset,
            limit = limit, sort = sort, raw = raw, memoised = FALSE,
            file = file, overwrite = overwrite, attributes = attributes
        )
        return(out)
    }
    .body(
        fname, validators, endpoint, environment(), isFile,
        header, raw, overwrite, file, attributes, match.call()
    )
}

#' Memoise get_taxon_datasets
#'
#' @noRd
memget_taxon_datasets <- function(taxon, offset = 0L, limit = 20, sort = "+id", raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    ), attributes = getOption("gemma.attributes", TRUE)) {
    mem_call <- memoise::memoise(get_taxon_datasets, cache = gemmaCache())
    mem_call(
        taxon = taxon, offset = offset, limit = limit, sort = sort,
        raw = raw, memoised = FALSE, file = file, overwrite = overwrite,
        attributes = attributes
    )
}


#' Clear gemma.R cache
#'
#' Forget past results from memoised calls to the Gemma API (ie. using functions with memoised = `TRUE`)
#'
#' @return TRUE to indicate cache was cleared.
#' @examples
#' forget_gemma_memoised()
#' @export
#'
#' @keywords misc
forget_gemma_memoised <- forget_gemma_memoised <- function() {
    mem <- memoise::memoise(function() {}, cache = gemmaCache())
    memoise::forget(mem)
}
