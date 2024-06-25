#' Retrieve a single analysis result set by its identifier
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @return Varies
#' @keywords internal
#'
#' @examples
#' # gemma.R:::.getResultSets(523099)
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
    compressibles <- NULL
    open_api_name <- "get_result_set"
    internal <- TRUE
    keyword <- NULL
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache(".getResultSets",
                resultSet = resultSet,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            ))
        } else {
            out <- mem.getResultSets(
                resultSet = resultSet, raw = raw,
                memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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

#' Retrieve all result sets matching the provided criteria
#'
#' Returns queried result set
#'
#' @details Output and usage of this function is mostly identical to \code{\link{get_dataset_differential_expression_analyses}}.
#' The principal difference being the ability to restrict your result sets, being able to
#' query across multiple datasets and being able to use the filter argument
#' to search based on result set properties.
#'
#' @param datasets A vector of dataset IDs or short names
#' @param resultSets A resultSet identifier. Note that result set identifiers
#' are not static and can change when Gemma re-runs analyses internally. Whem
#' using these as inputs, try to make sure you access a currently existing
#' result set ID by basing them on result sets returned for a particular dataset or
#' filter used in \code{\link{get_result_sets}}
#' @param filter Filter results by matching expression. Use \code{\link{filter_properties}}
#' function to get a list of all available parameters. These properties can be
#' combined using "and" "or" clauses and may contain common operators such as "=", "<" or "in".
#' (e.g. "taxon.commonName = human", "taxon.commonName in (human,mouse), "id < 1000")
#' @param offset The offset of the first retrieved result.
#' @param limit Defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param sort Order results by the given property and direction. The '+' sign
#' indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processDifferentialExpressionAnalysisResultSetValueObject return
#' @export
#'
#' @keywords misc
#'
#' @examples
#' get_result_sets(dataset = 1)
#' # get all contrasts comparing disease states. use filter_properties to see avaialble options
#' get_result_sets(filter = "baselineGroup.characteristics.value = disease")
get_result_sets <- function(datasets = NA_character_, resultSets = NA_character_,
    filter = NA_character_, offset = 0, limit = 20, sort = "+id",
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    compressibles <- "filter"
    open_api_name <- "get_result_sets"
    internal <- FALSE
    keyword <- "misc"
    header <- ""
    isFile <- FALSE
    fname <- "get_result_sets"
    preprocessor <- processDifferentialExpressionAnalysisResultSetValueObject
    validators <- list(
        datasets = validateOptionalID, resultSets = validateOptionalID,
        filter = validateFilter, offset = validatePositiveInteger,
        limit = validateLimit, sort = validateSort
    )
    endpoint <- "resultSets?datasets={encode(datasets)}&filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_result_sets",
                datasets = datasets,
                resultSets = resultSets, filter = filter, offset = offset,
                limit = limit, sort = sort, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_result_sets(
                datasets = datasets, resultSets = resultSets,
                filter = filter, offset = offset, limit = limit,
                sort = sort, raw = raw, memoised = FALSE, file = file,
                overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
    )
}

#' Memoise get_result_sets
#'
#' @noRd
memget_result_sets <- function(datasets = NA_character_, resultSets = NA_character_,
    filter = NA_character_, offset = 0, limit = 20, sort = "+id",
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    mem_call <- memoise::memoise(get_result_sets, cache = gemmaCache())
    mem_call(
        datasets = datasets, resultSets = resultSets, filter = filter,
        offset = offset, limit = limit, sort = sort, raw = raw,
        memoised = FALSE, file = file, overwrite = overwrite
    )
}

#' Search for annotation tags
#'
#'
#'
#' @param query The search query. Queries can include plain text or ontology
#' terms They also support conjunctions ("alpha AND beta"), disjunctions ("alpha OR beta")
#' grouping ("(alpha OR beta) AND gamma"), prefixing ("alpha*"), wildcard characters
#' ("BRCA?") and fuzzy matches ("alpha~").
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processSearchAnnotations return
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
    )) {
    compressibles <- "query"
    open_api_name <- "search_annotations"
    internal <- FALSE
    keyword <- "misc"
    header <- ""
    isFile <- FALSE
    fname <- "search_annotations"
    preprocessor <- processSearchAnnotations
    validators <- list(query = validateQuery)
    endpoint <- "annotations/search?query={encode(query)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("search_annotations",
                query = query, raw = raw, memoised = FALSE, file = file,
                overwrite = overwrite
            ))
        } else {
            out <- memsearch_annotations(
                query = query, raw = raw,
                memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(search_annotations, cache = gemmaCache())
    mem_call(
        query = query, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Retrieve the annotations of a dataset
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processAnnotations return
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
    )) {
    compressibles <- NULL
    open_api_name <- "get_dataset_annotations"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_dataset_annotations",
                dataset = dataset, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_dataset_annotations(
                dataset = dataset,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_dataset_annotations, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Retrieve the design of a dataset
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
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
    )) {
    compressibles <- NULL
    open_api_name <- "get_dataset_design"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_dataset_design",
                dataset = dataset, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_dataset_design(
                dataset = dataset, raw = raw,
                memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_dataset_design, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Retrieve annotations and surface level stats for a dataset's differential analyses
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processDEA return
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' result <- get_dataset_differential_expression_analyses("GSE2872")
#' get_differential_expression_values(resultSet = result$result.ID[1])
get_dataset_differential_expression_analyses <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    compressibles <- NULL
    open_api_name <- "get_dataset_differential_expression_analyses"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_dataset_differential_expression_analyses",
                dataset = dataset, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_dataset_differential_expression_analyses(
                dataset = dataset,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_dataset_differential_expression_analyses,
        cache = gemmaCache()
    )
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Retrieve the expression data matrix of a set of datasets and genes
#'
#'
#'
#' @param datasets A vector of dataset IDs or short names
#' @param genes A vector of NCBI IDs, Ensembl IDs or gene symbols.
#' @param keepNonSpecific logical. \code{FALSE} by default. If \code{TRUE}, results
#' from probesets that are not specific to the gene will also be returned.
#' @param consolidate An option for gene expression level consolidation. If empty,
#' will return every probe for the genes. "pickmax" to
#' pick the probe with the highest expression, "pickvar" to pick the prove with
#' the highest variance and "average" for returning the average expression
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @return A list of data frames
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' get_dataset_expression_for_genes("GSE2018", genes = c(10225, 2841))
get_dataset_expression_for_genes <- function(datasets, genes, keepNonSpecific = FALSE, consolidate = NA_character_,
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    compressibles <- NULL
    open_api_name <- "get_dataset_expression_for_genes"
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "get_dataset_expression_for_genes"
    preprocessor <- process_dataset_gene_expression
    validators <- list(datasets = function(name, ...) {
        ID <- unlist(list(...))
        isID <- grepl("^\\d+$", ID)
        if (any(is.na(ID)) || (any(isID) && !all(isID)) || any(ID ==
            "")) {
            stop(glue::glue("Please specify valid identifiers for {name} and do not combine different types of identifiers."),
                call. = FALSE
            )
        }
        paste0(ID, collapse = ",")
    }, genes = function(name, ...) {
        ID <- unlist(list(...))
        isID <- grepl("^\\d+$", ID)
        if (any(is.na(ID)) || (any(isID) && !all(isID)) || any(ID ==
            "")) {
            stop(glue::glue("Please specify valid identifiers for {name} and do not combine different types of identifiers."),
                call. = FALSE
            )
        }
        paste0(ID, collapse = ",")
    }, keepNonSpecific = function(name, ...) {
        args <- unlist(list(...))
        if (length(args) != 1 || !is.logical(args)) {
            stop(glue::glue("Please only specify boolean values for {name}."),
                call. = FALSE
            )
        }
        tolower(as.character(args))
    }, consolidate = function(name, ...) {
        consolidate <- unlist(list(...))
        if (length(consolidate) > 1 | (!consolidate %in% c(
            NA_character_,
            "pickmax", "pickvar", "average"
        ))) {
            stop("consolidate must be NA, \"pickmax\", \"pickmax\" or \"average\"")
        }
        return(consolidate)
    })
    endpoint <- "datasets/{encode(datasets)}/expressions/genes/{encode(genes)}?keepNonSpecific={encode(keepNonSpecific)}&consolidate={encode(consolidate)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_dataset_expression_for_genes",
                datasets = datasets, genes = genes, keepNonSpecific = keepNonSpecific,
                consolidate = consolidate, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_dataset_expression_for_genes(
                datasets = datasets,
                genes = genes, keepNonSpecific = keepNonSpecific,
                consolidate = consolidate, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
    )
}

#' Memoise get_dataset_expression_for_genes
#'
#' @noRd
memget_dataset_expression_for_genes <- function(datasets, genes, keepNonSpecific = FALSE, consolidate = NA_character_,
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    mem_call <- memoise::memoise(get_dataset_expression_for_genes,
        cache = gemmaCache()
    )
    mem_call(
        datasets = datasets, genes = genes, keepNonSpecific = keepNonSpecific,
        consolidate = consolidate, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Retrieve the platforms of a dataset
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
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
    )) {
    compressibles <- NULL
    open_api_name <- "get_dataset_platforms"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_dataset_platforms",
                dataset = dataset, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_dataset_platforms(
                dataset = dataset,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_dataset_platforms, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Retrieve processed expression data of a dataset
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @return If raw is FALSE (default), a data table of the expression matrix for
#' the queried dataset. If raw is TRUE, returns the binary file in raw form.
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' get_dataset_processed_expression("GSE2018")
get_dataset_processed_expression <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    compressibles <- NULL
    open_api_name <- "get_dataset_processed_expression"
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- TRUE
    fname <- "get_dataset_processed_expression"
    preprocessor <- processFile
    validators <- list(dataset = validateID)
    endpoint <- "datasets/{encode(dataset)}/data/processed"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_dataset_processed_expression",
                dataset = dataset, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_dataset_processed_expression(
                dataset = dataset,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
    )
}

#' Memoise get_dataset_processed_expression
#'
#' @noRd
memget_dataset_processed_expression <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(get_dataset_processed_expression,
        cache = gemmaCache()
    )
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Retrieve quantitation types of a dataset
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processQuantitationTypeValueObject return
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' get_dataset_quantitation_types("GSE59918")
get_dataset_quantitation_types <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    compressibles <- NULL
    open_api_name <- "get_dataset_quantitation_types"
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "get_dataset_quantitation_types"
    preprocessor <- processQuantitationTypeValueObject
    validators <- list(dataset = validateID)
    endpoint <- "datasets/{encode(dataset)}/quantitationTypes"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_dataset_quantitation_types",
                dataset = dataset, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_dataset_quantitation_types(
                dataset = dataset,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
    )
}

#' Memoise get_dataset_quantitation_types
#'
#' @noRd
memget_dataset_quantitation_types <- function(dataset, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(get_dataset_quantitation_types,
        cache = gemmaCache()
    )
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Retrieve raw expression data of a dataset
#'
#'
#'
#' @param dataset A numerical dataset identifier or a dataset short name
#' @param quantitationType Quantitation type id. These can be acquired
#' using \code{\link{get_dataset_quantitation_types}} function. This endpoint can
#' only return non-processed quantitation types.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @return If raw is FALSE (default), a data table of the expression matrix for
#' the queried dataset. If raw is TRUE, returns the binary file in raw form.
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' q_types <- get_dataset_quantitation_types("GSE59918")
#' get_dataset_raw_expression("GSE59918", q_types$id[q_types$name == "Counts"])
get_dataset_raw_expression <- function(dataset, quantitationType, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    compressibles <- NULL
    open_api_name <- "get_dataset_raw_expression"
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- TRUE
    fname <- "get_dataset_raw_expression"
    preprocessor <- processFile
    validators <- list(dataset = validateID, quantitationType = validateID)
    endpoint <- "datasets/{encode(dataset)}/data/raw?quantitationType={encode(quantitationType)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_dataset_raw_expression",
                dataset = dataset, quantitationType = quantitationType,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_dataset_raw_expression(
                dataset = dataset,
                quantitationType = quantitationType, raw = raw,
                memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
    )
}

#' Memoise get_dataset_raw_expression
#'
#' @noRd
memget_dataset_raw_expression <- function(dataset, quantitationType, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(get_dataset_raw_expression,
        cache = gemmaCache()
    )
    mem_call(
        dataset = dataset, quantitationType = quantitationType,
        raw = raw, memoised = FALSE, file = file, overwrite = overwrite
    )
}

#' Retrieve the samples of a dataset
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
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
    )) {
    compressibles <- NULL
    open_api_name <- "get_dataset_samples"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_dataset_samples",
                dataset = dataset, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_dataset_samples(
                dataset = dataset,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_dataset_samples, cache = gemmaCache())
    mem_call(
        dataset = dataset, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Retrieve all datasets
#'
#'
#'
#' @param query The search query. Queries can include plain text or ontology
#' terms They also support conjunctions ("alpha AND beta"), disjunctions ("alpha OR beta")
#' grouping ("(alpha OR beta) AND gamma"), prefixing ("alpha*"), wildcard characters
#' ("BRCA?") and fuzzy matches ("alpha~").
#' @param filter Filter results by matching expression. Use \code{\link{filter_properties}}
#' function to get a list of all available parameters. These properties can be
#' combined using "and" "or" clauses and may contain common operators such as "=", "<" or "in".
#' (e.g. "taxon.commonName = human", "taxon.commonName in (human,mouse), "id < 1000")
#' @param taxa A vector of taxon common names (e.g. human, mouse, rat). Providing multiple
#' species will return results for all species. These are appended
#' to the filter and equivalent to filtering for \code{taxon.commonName} property
#' @param uris A vector of ontology term URIs. Providing multiple terms will
#' return results containing any of the terms and their children. These are
#' appended to the filter and equivalent to filtering for \code{allCharacteristics.valueUri}
#' @param offset The offset of the first retrieved result.
#' @param limit Defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param sort Order results by the given property and direction. The '+' sign
#' indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processDatasets return
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' get_datasets()
#' get_datasets(taxa = c("mouse", "human"), uris = "http://purl.obolibrary.org/obo/UBERON_0002048")
#' # filter below is equivalent to the call above
#' get_datasets(filter = "taxon.commonName in (mouse,human) and allCharacteristics.valueUri = http://purl.obolibrary.org/obo/UBERON_0002048")
#' get_datasets(query = "lung")
get_datasets <- function(query = NA_character_, filter = NA_character_, taxa = NA_character_,
    uris = NA_character_, offset = 0L, limit = 20L, sort = "+id",
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    compressibles <- "filter"
    open_api_name <- "get_datasets"
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "get_datasets"
    preprocessor <- processDatasets
    validators <- list(
        query = validateOptionalQuery, filter = validateFilter,
        offset = validatePositiveInteger, limit = validateLimit,
        sort = validateSort
    )
    endpoint <- "datasets/?&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}&filter={encode(filter)}&query={encode(query)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_datasets",
                query = query,
                filter = filter, taxa = taxa, uris = uris, offset = offset,
                limit = limit, sort = sort, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_datasets(
                query = query, filter = filter,
                taxa = taxa, uris = uris, offset = offset, limit = limit,
                sort = sort, raw = raw, memoised = FALSE, file = file,
                overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
    )
}

#' Memoise get_datasets
#'
#' @noRd
memget_datasets <- function(query = NA_character_, filter = NA_character_, taxa = NA_character_,
    uris = NA_character_, offset = 0L, limit = 20L, sort = "+id",
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    mem_call <- memoise::memoise(get_datasets, cache = gemmaCache())
    mem_call(
        query = query, filter = filter, taxa = taxa, uris = uris,
        offset = offset, limit = limit, sort = sort, raw = raw,
        memoised = FALSE, file = file, overwrite = overwrite
    )
}

#' Retrieve datasets by their identifiers
#'
#'
#'
#' @param datasets Numerical dataset identifiers or dataset short names. If not
#' specified, all datasets will be returned instead
#' @param filter Filter results by matching expression. Use \code{\link{filter_properties}}
#' function to get a list of all available parameters. These properties can be
#' combined using "and" "or" clauses and may contain common operators such as "=", "<" or "in".
#' (e.g. "taxon.commonName = human", "taxon.commonName in (human,mouse), "id < 1000")
#' @param taxa A vector of taxon common names (e.g. human, mouse, rat). Providing multiple
#' species will return results for all species. These are appended
#' to the filter and equivalent to filtering for \code{taxon.commonName} property
#' @param uris A vector of ontology term URIs. Providing multiple terms will
#' return results containing any of the terms and their children. These are
#' appended to the filter and equivalent to filtering for \code{allCharacteristics.valueUri}
#' @param offset The offset of the first retrieved result.
#' @param limit Defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param sort Order results by the given property and direction. The '+' sign
#' indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processDatasets return
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' get_datasets_by_ids("GSE2018")
#' get_datasets_by_ids(c("GSE2018", "GSE2872"))
get_datasets_by_ids <- function(datasets = NA_character_, filter = NA_character_, taxa = NA_character_,
    uris = NA_character_, offset = 0L, limit = 20L, sort = "+id",
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    compressibles <- "filter"
    open_api_name <- "get_datasets_by_ids"
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "get_datasets_by_ids"
    preprocessor <- processDatasets
    validators <- list(
        datasets = validateOptionalID, filter = validateFilter,
        offset = validatePositiveInteger, limit = validateLimit,
        sort = validateSort
    )
    endpoint <- "datasets/{encode(datasets)}?&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}&filter={encode(filter)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_datasets_by_ids",
                datasets = datasets, filter = filter, taxa = taxa,
                uris = uris, offset = offset, limit = limit,
                sort = sort, raw = raw, memoised = FALSE, file = file,
                overwrite = overwrite
            ))
        } else {
            out <- memget_datasets_by_ids(
                datasets = datasets,
                filter = filter, taxa = taxa, uris = uris, offset = offset,
                limit = limit, sort = sort, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
    )
}

#' Memoise get_datasets_by_ids
#'
#' @noRd
memget_datasets_by_ids <- function(datasets = NA_character_, filter = NA_character_, taxa = NA_character_,
    uris = NA_character_, offset = 0L, limit = 20L, sort = "+id",
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    mem_call <- memoise::memoise(get_datasets_by_ids, cache = gemmaCache())
    mem_call(
        datasets = datasets, filter = filter, taxa = taxa,
        uris = uris, offset = offset, limit = limit, sort = sort,
        raw = raw, memoised = FALSE, file = file, overwrite = overwrite
    )
}

#' get_gene_differential_expression_values
#'
#'
#'
#' @param gene
#' @param query The search query. Queries can include plain text or ontology
#' terms They also support conjunctions ("alpha AND beta"), disjunctions ("alpha OR beta")
#' grouping ("(alpha OR beta) AND gamma"), prefixing ("alpha*"), wildcard characters
#' ("BRCA?") and fuzzy matches ("alpha~").
#' @param filter Filter results by matching expression. Use \code{\link{filter_properties}}
#' function to get a list of all available parameters. These properties can be
#' combined using "and" "or" clauses and may contain common operators such as "=", "<" or "in".
#' (e.g. "taxon.commonName = human", "taxon.commonName in (human,mouse), "id < 1000")
#' @param offset
#' @param limit Defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param threshold
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @return Varies
#' @export
#'
#' @keywords gene
#'
#' @examples
get_gene_differential_expression_values <- function(gene, query = NA_character_, filter = NA_character_,
    offset = 0L, limit = 20L, threshold = 1, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE),
    file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    compressibles <- "filter"
    open_api_name <- "get_datasets_differential_analysis_results_expression_for_gene"
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "get_gene_differential_expression_values"
    preprocessor <- processDifferentialExpressionAnalysisResultByGeneValueObject
    validators <- list(gene = function(name, ...) {
        ID <- unlist(list(...))
        if (length(ID) > 1) {
            stop(glue::glue("Please specify one valid identifier for {name}."),
                call. = FALSE
            )
        }
        validateID(name, ...)
    }, query = function(name, ...) {
        if (all(is.na(as.character(unlist(list(...)))))) {
            ""
        } else {
            validateSingleQuery(name, ...)
        }
    }, filter = function(name, ...) {
        filter <- unlist(list(...))
        assertthat::assert_that(is.null(filter) || is.na(filter) ||
            assertthat::is.string(filter), msg = "filter must be a string of length one")
        if (is.null(filter) || is.na(filter)) {
            filter <- ""
        }
        env <- parent.frame()
        if (!(all(is.na(env$original_env$taxa)) || is.null(env$original_env$taxa))) {
            filter <- addToFilter(
                filter, "taxon.commonName",
                env$original_env$taxa
            )
        }
        if (!(all(is.na(env$original_env$uris)) || is.null(env$original_env$uris))) {
            filter <- addToFilter(
                filter, "allCharacteristics.valueUri",
                env$original_env$uris
            )
        }
        if (!(all(is.na(env$original_env$resultSets)) || is.null(env$original_env$resultSets)) &&
            env$fname == "get_result_sets") {
            filter <- addToFilter(filter, "id", env$original_env$resultSets)
        }
        return(filter)
    }, offset = function(name, ...) {
        args <- list(...)
        if (length(unlist(args)) != 1 || any(is.na(unlist(args))) ||
            !is.numeric(unlist(args)) || any(vapply(args, "%%",
            1,
            FUN.VALUE = numeric(1)
        ) != 0) || any(vapply(args,
            sign,
            FUN.VALUE = numeric(1)
        ) < 0)) {
            stop(glue::glue("Please only specify positive integer values for {name}."),
                call. = FALSE
            )
        }
        unlist(args)
    }, limit = function(name, ...) {
        validatePositiveInteger(name, ...)
        args <- list(...)
        if (unlist(args) <= 0 || unlist(args) > 100) {
            stop(glue::glue("Please specify a limit between 1 and 100 (inclusive)"),
                call. = FALSE
            )
        }
        unlist(args)
    }, threshold = function(name, ...) {
        number <- unlist(list(...))
        if (length(number) > 1 || typeof(number) != "double") {
            stop(glue::glue("{name} must be a double of length one"))
        }
        return(number)
    })
    endpoint <- "datasets/analyses/differential/results/genes/{encode(gene)}?&query={encode(query)}&filter={encode(filter)}&threshold={encode(threshold)}&offset={encode(offset)}&limit={encode(limit)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_gene_differential_expression_values",
                gene = gene, query = query, filter = filter,
                offset = offset, limit = limit, threshold = threshold,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_gene_differential_expression_values(
                gene = gene,
                query = query, filter = filter, offset = offset,
                limit = limit, threshold = threshold, raw = raw,
                memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
    )
}

#' Memoise get_gene_differential_expression_values
#'
#' @noRd
memget_gene_differential_expression_values <- function(gene, query = NA_character_, filter = NA_character_,
    offset = 0L, limit = 20L, threshold = 1, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE),
    file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(get_gene_differential_expression_values,
        cache = gemmaCache()
    )
    mem_call(
        gene = gene, query = query, filter = filter, offset = offset,
        limit = limit, threshold = threshold, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Retrieve the GO terms associated to a gene
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processGO return
#' @export
#'
#' @keywords gene
#'
#' @examples
#' get_gene_go_terms(3091)
get_gene_go_terms <- function(gene, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    compressibles <- NULL
    open_api_name <- "get_gene_go_terms"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_gene_go_terms",
                gene = gene,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_gene_go_terms(
                gene = gene, raw = raw,
                memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_gene_go_terms, cache = gemmaCache())
    mem_call(
        gene = gene, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Retrieve the physical locations of a given gene
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processGeneLocation return
#' @export
#'
#' @keywords gene
#'
#' @examples
#' get_gene_locations("DYRK1A")
#' get_gene_locations(1859)
get_gene_locations <- function(gene, raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    compressibles <- NULL
    open_api_name <- "get_gene_locations"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_gene_locations",
                gene = gene, raw = raw, memoised = FALSE, file = file,
                overwrite = overwrite
            ))
        } else {
            out <- memget_gene_locations(
                gene = gene, raw = raw,
                memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_gene_locations, cache = gemmaCache())
    mem_call(
        gene = gene, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Retrieve the probes associated to a genes across all platforms
#'
#'
#'
#' @param gene An ensembl gene identifier which typically starts with ensg or an ncbi gene identifier or an official gene symbol approved by hgnc
#' @param offset The offset of the first retrieved result.
#' @param limit Defaults to 20. Limits the result to specified amount
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processElements return
#' @export
#'
#' @keywords gene
#'
#' @examples
#' get_gene_probes(1859)
get_gene_probes <- function(gene, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE), file = getOption(
        "gemma.file",
        NA_character_
    ), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    compressibles <- NULL
    open_api_name <- "get_gene_probes"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_gene_probes",
                gene = gene,
                offset = offset, limit = limit, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_gene_probes(
                gene = gene, offset = offset,
                limit = limit, raw = raw, memoised = FALSE, file = file,
                overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_gene_probes, cache = gemmaCache())
    mem_call(
        gene = gene, offset = offset, limit = limit, raw = raw,
        memoised = FALSE, file = file, overwrite = overwrite
    )
}

#' Retrieve genes matching gene identifiers
#'
#'
#'
#' @param genes A vector of NCBI IDs, Ensembl IDs or gene symbols.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processGenes return
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
    )) {
    compressibles <- NULL
    open_api_name <- "get_genes"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_genes",
                genes = genes,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_genes(
                genes = genes, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_genes, cache = gemmaCache())
    mem_call(
        genes = genes, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Retrieve all experiments using a given platform
#'
#'
#'
#' @param platform A platform numerical identifier or a platform short name
#' @param offset The offset of the first retrieved result.
#' @param limit Defaults to 20. Limits the result to specified amount
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processDatasets return
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
    )) {
    compressibles <- NULL
    open_api_name <- "get_platform_datasets"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_platform_datasets",
                platform = platform, offset = offset, limit = limit,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_platform_datasets(
                platform = platform,
                offset = offset, limit = limit, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_platform_datasets, cache = gemmaCache())
    mem_call(
        platform = platform, offset = offset, limit = limit,
        raw = raw, memoised = FALSE, file = file, overwrite = overwrite
    )
}

#' Retrieve the genes associated to a probe in a given platform
#'
#'
#'
#' @param platform A platform numerical identifier or a platform short name
#' @param probe A probe name or it's numerical identifier
#' @param offset The offset of the first retrieved result.
#' @param limit Defaults to 20. Limits the result to specified amount
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processGenes return
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
    )) {
    compressibles <- NULL
    open_api_name <- "get_platform_element_genes"
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_platform_element_genes",
                platform = platform, probe = probe, offset = offset,
                limit = limit, raw = raw, memoised = FALSE, file = file,
                overwrite = overwrite
            ))
        } else {
            out <- memget_platform_element_genes(
                platform = platform,
                probe = probe, offset = offset, limit = limit,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_platform_element_genes,
        cache = gemmaCache()
    )
    mem_call(
        platform = platform, probe = probe, offset = offset,
        limit = limit, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
    )
}

#' Retrieve all platforms matching a set of platform identifiers
#'
#'
#'
#' @param platforms Platform numerical identifiers or platform short names.  If not
#' specified, all platforms will be returned instead
#' @param filter Filter results by matching expression. Use \code{\link{filter_properties}}
#' function to get a list of all available parameters. These properties can be
#' combined using "and" "or" clauses and may contain common operators such as "=", "<" or "in".
#' (e.g. "taxon.commonName = human", "taxon.commonName in (human,mouse), "id < 1000")
#' @param taxa A vector of taxon common names (e.g. human, mouse, rat). Providing multiple
#' species will return results for all species. These are appended
#' to the filter and equivalent to filtering for \code{taxon.commonName} property
#' @param offset The offset of the first retrieved result.
#' @param limit Defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param sort Order results by the given property and direction. The '+' sign
#' indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processPlatforms return
#' @export
#'
#' @keywords platform
#'
#' @examples
#' get_platforms_by_ids("GPL1355")
#' get_platforms_by_ids(c("GPL1355", "GPL96"))
get_platforms_by_ids <- function(platforms = NA_character_, filter = NA_character_,
    taxa = NA_character_, offset = 0L, limit = 20L, sort = "+id",
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    compressibles <- "filter"
    open_api_name <- "get_platforms_by_ids"
    internal <- FALSE
    keyword <- "platform"
    header <- ""
    isFile <- FALSE
    fname <- "get_platforms_by_ids"
    preprocessor <- processPlatforms
    validators <- list(
        platforms = validateOptionalID, filter = validateFilter,
        offset = validatePositiveInteger, limit = validateLimit,
        sort = validateSort
    )
    endpoint <- "platforms/{encode(platforms)}?&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}&filter={encode(filter)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_platforms_by_ids",
                platforms = platforms, filter = filter, taxa = taxa,
                offset = offset, limit = limit, sort = sort,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_platforms_by_ids(
                platforms = platforms,
                filter = filter, taxa = taxa, offset = offset,
                limit = limit, sort = sort, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
    )
}

#' Memoise get_platforms_by_ids
#'
#' @noRd
memget_platforms_by_ids <- function(platforms = NA_character_, filter = NA_character_,
    taxa = NA_character_, offset = 0L, limit = 20L, sort = "+id",
    raw = getOption("gemma.raw", FALSE), memoised = getOption(
        "gemma.memoised",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    mem_call <- memoise::memoise(get_platforms_by_ids, cache = gemmaCache())
    mem_call(
        platforms = platforms, filter = filter, taxa = taxa,
        offset = offset, limit = limit, sort = sort, raw = raw,
        memoised = FALSE, file = file, overwrite = overwrite
    )
}

#' Search everything in Gemma
#'
#'
#'
#' @param query The search query. Queries can include plain text or ontology
#' terms They also support conjunctions ("alpha AND beta"), disjunctions ("alpha OR beta")
#' grouping ("(alpha OR beta) AND gamma"), prefixing ("alpha*"), wildcard characters
#' ("BRCA?") and fuzzy matches ("alpha~").
#' @param taxon A numerical taxon identifier or an ncbi taxon identifier or a taxon identifier that matches either its scientific or common name
#' @param platform A platform numerical identifier or a platform short name
#' @param limit Defaults to 100 with a maximum value of 2000.
#' Limits the number of returned results. Note
#' that this function does not support pagination.
#' @param resultType The kind of results that should be included in the output. Can be experiment, gene, platform or a long object type name, documented in the API documentation.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @return If \code{raw = FALSE} and resultType is experiment, gene or platform,
#' a data.table containing the search results. If it is any other type, a list
#' of results. A list with additional details about the search if \code{raw = TRUE}
#' @export
#'
#' @keywords misc
#'
#' @examples
#' search_gemma("bipolar")
search_gemma <- function(query, taxon = NA_character_, platform = NA_character_,
    limit = 100, resultType = "experiment", raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE),
    file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    compressibles <- NULL
    open_api_name <- "search"
    internal <- FALSE
    keyword <- "misc"
    header <- ""
    isFile <- FALSE
    fname <- "search_gemma"
    preprocessor <- process_search
    validators <- list(
        query = validateQuery, taxon = validateOptionalTaxon,
        platform = validateOptionalID, limit = validatePositiveInteger,
        resultType = validateResultType
    )
    endpoint <- "search?query={encode(query)}&taxon={encode(taxon)}&platform={encode(platform)}&limit={encode(limit)}&resultTypes={encode(resultType)}"
    if (memoised) {
        if (!is.na(file)) {
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("search_gemma",
                query = query,
                taxon = taxon, platform = platform, limit = limit,
                resultType = resultType, raw = raw, memoised = FALSE,
                file = file, overwrite = overwrite
            ))
        } else {
            out <- memsearch_gemma(
                query = query, taxon = taxon,
                platform = platform, limit = limit, resultType = resultType,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
    )
}

#' Memoise search_gemma
#'
#' @noRd
memsearch_gemma <- function(query, taxon = NA_character_, platform = NA_character_,
    limit = 100, resultType = "experiment", raw = getOption(
        "gemma.raw",
        FALSE
    ), memoised = getOption("gemma.memoised", FALSE),
    file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    mem_call <- memoise::memoise(search_gemma, cache = gemmaCache())
    mem_call(
        query = query, taxon = taxon, platform = platform,
        limit = limit, resultType = resultType, raw = raw, memoised = FALSE,
        file = file, overwrite = overwrite
    )
}

#' Retrieve taxa by their identifiers
#'
#'
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
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
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
    )) {
    compressibles <- NULL
    open_api_name <- "get_taxa_by_ids"
    internal <- TRUE
    keyword <- NULL
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
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_taxa_by_ids",
                taxa = taxa,
                raw = raw, memoised = FALSE, file = file, overwrite = overwrite
            ))
        } else {
            out <- memget_taxa_by_ids(
                taxa = taxa, raw = raw,
                memoised = FALSE, file = file, overwrite = overwrite
            )
            return(out)
        }
    }
    .body(
        fname = fname, validators = validators, endpoint = endpoint,
        envWhere = environment(), isFile = isFile, header = header,
        raw = raw, overwrite = overwrite, file = file, attributes = TRUE,
        open_api_name = open_api_name, .call = match.call()
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
    )) {
    mem_call <- memoise::memoise(get_taxa_by_ids, cache = gemmaCache())
    mem_call(
        taxa = taxa, raw = raw, memoised = FALSE, file = file,
        overwrite = overwrite
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
forget_gemma_memoised <- forget_gemma_memoised <-
    function() {
        if ("character" %in% class(gemmaCache()) && gemmaCache() == "cache_in_memory") {
            memoise::forget(mem_in_memory_cache)
        } else {
            mem <- memoise::memoise(function() {}, cache = gemmaCache())
            memoise::forget(mem)
        }
    }
