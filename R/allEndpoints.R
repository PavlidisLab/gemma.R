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
#' @param filter Optional, defaults to `empty`.
#' Filtering can be done on any* property or nested property that the
#' appropriate object class defines or inherits (and that is mapped by
#' hibernate). [These do not correspond to the properties of the objects
#' returned by the API calls.]{.description-imp}
#' Class definitions:
#' -   Datasets:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java)
#' -   Platforms:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java)
#' E.g: `curationDetails` or `curationDetails.lastTroubledEvent.date`.
#' * Any property of a supported type. Currently supported types are:
#' -   String - property of String type, required value can be any String.
#' -   Number - any Number implementation. Required value must be a string
#'     parseable to the specific Number type.
#' -   Boolean - required value will be parsed to true only if the string
#'     matches 'true', ignoring case.
#' Accepted operator keywords are:
#' -   '=' - equality
#' -   '!=' - non-equality
#' -   '<' - smaller than
#' -   '>' - larger than
#' -   '<=' - smaller or equal
#' -   '=>' - larger or equal
#' -   'like' - similar string, effectively means 'contains',
#'     translates to the sql 'LIKE' operator (given value will be
#'     surrounded by % signs)
#' Multiple filters can be chained using `AND` and `OR` keywords.
#' Leave space between the keywords and the previous/next word!
#' E.g: `?filter=property1 < value1 AND property2 like value2`
#' If chained filters are mixed conjunctions and disjunctions, the query
#' must be in conjunctive normal form (CNF). Parentheses are not necessary
#' - every AND keyword separates blocks of disjunctions.
#' Example:
#' `?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4`
#' Above query will translate to:
#' `(p1 = v1 OR p1 != v2) AND (p2 <= v2) AND (p3 > v3 OR p3 < v4;)`
#' Breaking the CNF results in an error.
#' Filter `curationDetails.troubled` will be ignored if user is not an
#' administrator.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
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
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
#' getDatasets("GSE2018")
#' getDatasets(c("GSE2018", "GSE2872"))
getDatasets <- function(datasets = NA_character_, filter = NA_character_, offset = 0L,
    limit = 20L, sort = "+id", raw = getOption("gemma.raw", FALSE),
    async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- FALSE
    fname <- "getDatasets"
    preprocessor <- processDatasets
    validators <- list(
        datasets = validateOptionalID, filter = validateFilter,
        offset = validatePositiveInteger, limit = validatePositiveInteger,
        sort = validateSort
    )
    endpoint <- "datasets/{encode(datasets)}?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getDatasets
#'
#' @noRd
memgetDatasets <- memoise::memoise(getDatasets)

#' getResultSets
#'
#' Lists resultSets filtered and organized by given parameters.
#'
#' @param resultSet Optional, defaults to empty. A single resultSet identifier (ex. 423176)
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param filter Optional, defaults to `empty`.
#' Filtering can be done on any* property or nested property that the
#' appropriate object class defines or inherits (and that is mapped by
#' hibernate). [These do not correspond to the properties of the objects
#' returned by the API calls.]{.description-imp}
#' Class definitions:
#' -   Datasets:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java)
#' -   Platforms:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java)
#' E.g: `curationDetails` or `curationDetails.lastTroubledEvent.date`.
#' * Any property of a supported type. Currently supported types are:
#' -   String - property of String type, required value can be any String.
#' -   Number - any Number implementation. Required value must be a string
#'     parseable to the specific Number type.
#' -   Boolean - required value will be parsed to true only if the string
#'     matches 'true', ignoring case.
#' Accepted operator keywords are:
#' -   '=' - equality
#' -   '!=' - non-equality
#' -   '<' - smaller than
#' -   '>' - larger than
#' -   '<=' - smaller or equal
#' -   '=>' - larger or equal
#' -   'like' - similar string, effectively means 'contains',
#'     translates to the sql 'LIKE' operator (given value will be
#'     surrounded by % signs)
#' Multiple filters can be chained using `AND` and `OR` keywords.
#' Leave space between the keywords and the previous/next word!
#' E.g: `?filter=property1 < value1 AND property2 like value2`
#' If chained filters are mixed conjunctions and disjunctions, the query
#' must be in conjunctive normal form (CNF). Parentheses are not necessary
#' - every AND keyword separates blocks of disjunctions.
#' Example:
#' `?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4`
#' Above query will translate to:
#' `(p1 = v1 OR p1 != v2) AND (p2 <= v2) AND (p3 > v3 OR p3 < v4;)`
#' Breaking the CNF results in an error.
#' Filter `curationDetails.troubled` will be ignored if user is not an
#' administrator.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
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
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return Varies
#' @keywords internal
#'
#' @examples
getResultSets <- function(resultSet = NA_character_, dataset = NA_character_,
    filter = NA_character_, offset = 0L, limit = 20L, sort = "+id",
    raw = getOption("gemma.raw", FALSE), async = getOption(
        "gemma.async",
        FALSE
    ), memoised = getOption("gemma.memoise", FALSE),
    file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- TRUE
    header <- "text/tab-separated-values"
    isFile <- TRUE
    fname <- "getResultSets"
    preprocessor <- processFile
    validators <- list(
        resultSet = validateOptionalID, dataset = validateOptionalID,
        filter = validateFilter, offset = validatePositiveInteger,
        limit = validatePositiveInteger, sort = validateSort
    )
    endpoint <- "resultSets/{encode(resultSet)}?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getResultSets
#'
#' @noRd
memgetResultSets <- memoise::memoise(getResultSets)

#' getResultSetFactors
#'
#' Returns the factor values for the queried resultSet.
#'
#' @param resultSet Optional, defaults to empty. A single resultSet identifier (ex. 423176)
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param filter Optional, defaults to `empty`.
#' Filtering can be done on any* property or nested property that the
#' appropriate object class defines or inherits (and that is mapped by
#' hibernate). [These do not correspond to the properties of the objects
#' returned by the API calls.]{.description-imp}
#' Class definitions:
#' -   Datasets:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java)
#' -   Platforms:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java)
#' E.g: `curationDetails` or `curationDetails.lastTroubledEvent.date`.
#' * Any property of a supported type. Currently supported types are:
#' -   String - property of String type, required value can be any String.
#' -   Number - any Number implementation. Required value must be a string
#'     parseable to the specific Number type.
#' -   Boolean - required value will be parsed to true only if the string
#'     matches 'true', ignoring case.
#' Accepted operator keywords are:
#' -   '=' - equality
#' -   '!=' - non-equality
#' -   '<' - smaller than
#' -   '>' - larger than
#' -   '<=' - smaller or equal
#' -   '=>' - larger or equal
#' -   'like' - similar string, effectively means 'contains',
#'     translates to the sql 'LIKE' operator (given value will be
#'     surrounded by % signs)
#' Multiple filters can be chained using `AND` and `OR` keywords.
#' Leave space between the keywords and the previous/next word!
#' E.g: `?filter=property1 < value1 AND property2 like value2`
#' If chained filters are mixed conjunctions and disjunctions, the query
#' must be in conjunctive normal form (CNF). Parentheses are not necessary
#' - every AND keyword separates blocks of disjunctions.
#' Example:
#' `?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4`
#' Above query will translate to:
#' `(p1 = v1 OR p1 != v2) AND (p2 <= v2) AND (p3 > v3 OR p3 < v4;)`
#' Breaking the CNF results in an error.
#' Filter `curationDetails.troubled` will be ignored if user is not an
#' administrator.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
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
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return Varies
#' @keywords internal
#'
#' @examples
getResultSetFactors <- function(resultSet = NA_character_, dataset = NA_character_,
    filter = NA_character_, offset = 0L, limit = 20L, sort = "+id",
    excludeResults = TRUE, raw = getOption("gemma.raw", FALSE),
    async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    internal <- TRUE
    header <- ""
    isFile <- FALSE
    fname <- "getResultSetFactors"
    preprocessor <- processResultSetFactors
    validators <- list(
        resultSet = validateOptionalID, dataset = validateOptionalID,
        filter = validateFilter, offset = validatePositiveInteger,
        limit = validatePositiveInteger, sort = validateSort
    )
    endpoint <- "resultSets/{encode(resultSet)}?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}&excludeResults={encode(excludeResults)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getResultSetFactors
#'
#' @noRd
memgetResultSetFactors <- memoise::memoise(getResultSetFactors)

#' getDatasetResultSets
#'
#' foobar
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getDatasetResultSets <- function(dataset = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getDatasetResultSets
#'
#' @noRd
memgetDatasetResultSets <- memoise::memoise(getDatasetResultSets)

#' Dataset data
#'
#' Retrieves the data for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param filter Optional, defaults to `empty`.
#' Filtering can be done on any* property or nested property that the
#' appropriate object class defines or inherits (and that is mapped by
#' hibernate). [These do not correspond to the properties of the objects
#' returned by the API calls.]{.description-imp}
#' Class definitions:
#' -   Datasets:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java)
#' -   Platforms:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java)
#' E.g: `curationDetails` or `curationDetails.lastTroubledEvent.date`.
#' * Any property of a supported type. Currently supported types are:
#' -   String - property of String type, required value can be any String.
#' -   Number - any Number implementation. Required value must be a string
#'     parseable to the specific Number type.
#' -   Boolean - required value will be parsed to true only if the string
#'     matches 'true', ignoring case.
#' Accepted operator keywords are:
#' -   '=' - equality
#' -   '!=' - non-equality
#' -   '<' - smaller than
#' -   '>' - larger than
#' -   '<=' - smaller or equal
#' -   '=>' - larger or equal
#' -   'like' - similar string, effectively means 'contains',
#'     translates to the sql 'LIKE' operator (given value will be
#'     surrounded by % signs)
#' Multiple filters can be chained using `AND` and `OR` keywords.
#' Leave space between the keywords and the previous/next word!
#' E.g: `?filter=property1 < value1 AND property2 like value2`
#' If chained filters are mixed conjunctions and disjunctions, the query
#' must be in conjunctive normal form (CNF). Parentheses are not necessary
#' - every AND keyword separates blocks of disjunctions.
#' Example:
#' `?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4`
#' Above query will translate to:
#' `(p1 = v1 OR p1 != v2) AND (p2 <= v2) AND (p3 > v3 OR p3 < v4;)`
#' Breaking the CNF results in an error.
#' Filter `curationDetails.troubled` will be ignored if user is not an
#' administrator.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return The data file for the given dataset.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords dataset
#'
#' @examples
#' dat <- getDatasetData("GSE2018")
#' str(dat)
getDatasetData <- function(dataset = NA_character_, filter = FALSE, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "dataset"
    header <- ""
    isFile <- TRUE
    fname <- "getDatasetData"
    preprocessor <- processFile
    validators <- list(dataset = validateID, filter = validateBoolean)
    endpoint <- "datasets/{encode(dataset)}/data?filter={encode(filter)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getDatasetData
#'
#' @noRd
memgetDatasetData <- memoise::memoise(getDatasetData)

#' Dataset samples
#'
#' Retrieves samples for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getDatasetSamples <- function(dataset = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/samples"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getDatasetSamples
#'
#' @noRd
memgetDatasetSamples <- memoise::memoise(getDatasetSamples)

#' Dataset platforms
#'
#' Retrieves platforms for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getDatasetPlatforms <- function(dataset = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/platforms"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getDatasetPlatforms
#'
#' @noRd
memgetDatasetPlatforms <- memoise::memoise(getDatasetPlatforms)

#' Dataset annotations
#'
#' Retrieves the annotations for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getDatasetAnnotations <- function(dataset = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/annotations"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getDatasetAnnotations
#'
#' @noRd
memgetDatasetAnnotations <- memoise::memoise(getDatasetAnnotations)

#' Dataset design
#'
#' Retrieves the design for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getDatasetDesign <- function(dataset = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/design"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getDatasetDesign
#'
#' @noRd
memgetDatasetDesign <- memoise::memoise(getDatasetDesign)

#' Dataset differential analysis
#'
#' Retrieves the differential analysis results for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getDatasetDEA <- function(dataset = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/analyses/differential"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getDatasetDEA
#'
#' @noRd
memgetDatasetDEA <- memoise::memoise(getDatasetDEA)

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
#' @param filter Optional, defaults to `empty`.
#' Filtering can be done on any* property or nested property that the
#' appropriate object class defines or inherits (and that is mapped by
#' hibernate). [These do not correspond to the properties of the objects
#' returned by the API calls.]{.description-imp}
#' Class definitions:
#' -   Datasets:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java)
#' -   Platforms:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java)
#' E.g: `curationDetails` or `curationDetails.lastTroubledEvent.date`.
#' * Any property of a supported type. Currently supported types are:
#' -   String - property of String type, required value can be any String.
#' -   Number - any Number implementation. Required value must be a string
#'     parseable to the specific Number type.
#' -   Boolean - required value will be parsed to true only if the string
#'     matches 'true', ignoring case.
#' Accepted operator keywords are:
#' -   '=' - equality
#' -   '!=' - non-equality
#' -   '<' - smaller than
#' -   '>' - larger than
#' -   '<=' - smaller or equal
#' -   '=>' - larger or equal
#' -   'like' - similar string, effectively means 'contains',
#'     translates to the sql 'LIKE' operator (given value will be
#'     surrounded by % signs)
#' Multiple filters can be chained using `AND` and `OR` keywords.
#' Leave space between the keywords and the previous/next word!
#' E.g: `?filter=property1 < value1 AND property2 like value2`
#' If chained filters are mixed conjunctions and disjunctions, the query
#' must be in conjunctive normal form (CNF). Parentheses are not necessary
#' - every AND keyword separates blocks of disjunctions.
#' Example:
#' `?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4`
#' Above query will translate to:
#' `(p1 = v1 OR p1 != v2) AND (p2 <= v2) AND (p3 > v3 OR p3 < v4;)`
#' Breaking the CNF results in an error.
#' Filter `curationDetails.troubled` will be ignored if user is not an
#' administrator.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
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
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
#' getPlatforms("GPL1355")
#' getPlatforms(c("GPL1355", "GPL96"))
getPlatforms <- function(platforms = NA_character_, filter = NA_character_,
    offset = 0L, limit = 20L, sort = "+id", raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    internal <- FALSE
    keyword <- "platform"
    header <- ""
    isFile <- FALSE
    fname <- "getPlatforms"
    preprocessor <- processPlatforms
    validators <- list(
        platforms = validateOptionalID, filter = validateFilter,
        offset = validatePositiveInteger, limit = validatePositiveInteger,
        sort = validateSort
    )
    endpoint <- "platforms/{encode(platforms)}?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getPlatforms
#'
#' @noRd
memgetPlatforms <- memoise::memoise(getPlatforms)

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
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getPlatformDatasets <- function(platform = NA_character_, offset = 0L, limit = 20L,
    raw = getOption("gemma.raw", FALSE), async = getOption(
        "gemma.async",
        FALSE
    ), memoised = getOption("gemma.memoise", FALSE),
    file = getOption("gemma.file", NA_character_), overwrite = getOption(
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
        limit = validatePositiveInteger
    )
    endpoint <- "platforms/{encode(platform)}/datasets?offset={encode(offset)}&limit={encode(limit)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getPlatformDatasets
#'
#' @noRd
memgetPlatformDatasets <- memoise::memoise(getPlatformDatasets)

#' Platform elements
#'
#' Retrieves the composite sequences (elements) for the given platform
#'
#' @param platform Required, part of the URL path.
#' Can either be the platform ID or its short name (e.g: `GPL1355`)
#' Retrieval by ID is more efficient.
#' Only platforms that user has access to will be available.
#' @param element Optional, defaults to `empty`.
#' Limits the result to entities with given identifiers.
#' A list of identifiers, separated by commas (e.g:
#' `AFFX_Rat_beta-actin_M_at, AFFX_Rat_Hexokinase_M_at`).
#' Can either be probes name or IDs.
#' Do not combine different identifiers in one query.
#' When
#' using in scripts, remember to URL-encode any forward slashes in the
#' probe name (see the compiled URL below).
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getPlatformElements <- function(platform = NA_character_, element = NA_character_,
    offset = 0L, limit = 20L, raw = getOption("gemma.raw", FALSE),
    async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
        offset = validatePositiveInteger, limit = validatePositiveInteger
    )
    endpoint <- "platforms/{encode(platform)}/elements/{encode(element)}?offset={encode(offset)}&limit={encode(limit)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getPlatformElements
#'
#' @noRd
memgetPlatformElements <- memoise::memoise(getPlatformElements)

#' Platform element genes
#'
#' Retrieves the genes on the given platform element
#'
#' @param platform Required, part of the URL path.
#' Can either be the platform ID or its short name (e.g: `GPL1355`)
#' Retrieval by ID is more efficient.
#' Only platforms that user has access to will be available.
#' @param element Optional, defaults to `empty`.
#' Limits the result to entities with given identifiers.
#' A list of identifiers, separated by commas (e.g:
#' `AFFX_Rat_beta-actin_M_at, AFFX_Rat_Hexokinase_M_at`).
#' Can either be probes name or IDs.
#' Do not combine different identifiers in one query.
#' When
#' using in scripts, remember to URL-encode any forward slashes in the
#' probe name (see the compiled URL below).
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getPlatformElementGenes <- function(platform = NA_character_, element = NA_character_,
    offset = 0L, limit = 20L, raw = getOption("gemma.raw", FALSE),
    async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
        FALSE
    ), file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE)) {
    internal <- FALSE
    keyword <- "platform"
    header <- ""
    isFile <- FALSE
    fname <- "getPlatformElementGenes"
    preprocessor <- processGenes
    validators <- list(
        platform = validateSingleID, element = validateSingleID,
        offset = validatePositiveInteger, limit = validatePositiveInteger
    )
    endpoint <- "platforms/{encode(platform)}/elements/{encode(element)}/genes?offset={encode(offset)}&limit={encode(limit)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getPlatformElementGenes
#'
#' @noRd
memgetPlatformElementGenes <- memoise::memoise(getPlatformElementGenes)

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
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
#' getGenes("DYRK1A")
#' getGenes(c("DYRK1A", "PTEN"))
getGenes <- function(genes = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "getGenes"
    preprocessor <- processGenes
    validators <- list(genes = validateID)
    endpoint <- "genes/{encode(genes)}/"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getGenes
#'
#' @noRd
memgetGenes <- memoise::memoise(getGenes)

#' Gene evidence
#'
#' Retrieves gene evidence for the given gene
#'
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file.
#' Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified filename.
#'
#' @return An array of gene evidence value objects for the given gene or gene
#' homologues.
#' A `404 error` if the given identifier does not map to any object.
#' @export
#'
#' @keywords gene
#'
#' @examples
#' getGeneEvidence("DYRK1A")
getGeneEvidence <- function(gene = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
        "gemma.overwrite",
        FALSE
    )) {
    internal <- FALSE
    keyword <- "gene"
    header <- ""
    isFile <- FALSE
    fname <- "getGeneEvidence"
    preprocessor <- processGeneEvidence
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/evidence"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getGeneEvidence
#'
#' @noRd
memgetGeneEvidence <- memoise::memoise(getGeneEvidence)

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
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getGeneLocation <- function(gene = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getGeneLocation
#'
#' @noRd
memgetGeneLocation <- memoise::memoise(getGeneLocation)

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
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getGeneProbes <- function(gene = NA_character_, offset = 0L, limit = 20L, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
        FALSE
    ), file = getOption("gemma.file", NA_character_), overwrite = getOption(
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
        limit = validatePositiveInteger
    )
    endpoint <- "genes/{encode(gene)}/probes?offset={encode(offset)}&limit={encode(limit)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getGeneProbes
#'
#' @noRd
memgetGeneProbes <- memoise::memoise(getGeneProbes)

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
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
getGeneGO <- function(gene = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise getGeneGO
#'
#' @noRd
memgetGeneGO <- memoise::memoise(getGeneGO)

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
#' @param taxon Not required, part of the URL path.
#' can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:
#' scientific name, common name
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers
#' available.
#' Use the 'All Taxa' endpoint to retrieve the necessary information. For
#' convenience, below is a list of officially supported taxa:
#'   ID   Comm.name   Scient.name                NcbiID
#'   ---- ----------- -------------------------- --------
#'   1    human       Homo sapiens               9606
#'   2    mouse       Mus musculus               10090
#'   3    rat         Rattus norvegicus          10116
#'   11   yeast       Saccharomyces cerevisiae   4932
#'   12   zebrafish   Danio rerio                7955
#'   13   fly         Drosophila melanogaster    7227
#'   14   worm        Caenorhabditis elegans     6239
#' @param filter Optional, defaults to `empty`.
#' Filtering can be done on any* property or nested property that the
#' appropriate object class defines or inherits (and that is mapped by
#' hibernate). [These do not correspond to the properties of the objects
#' returned by the API calls.]{.description-imp}
#' Class definitions:
#' -   Datasets:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java)
#' -   Platforms:
#'     [javaDoc](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html)     [gitHub](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java)
#' E.g: `curationDetails` or `curationDetails.lastTroubledEvent.date`.
#' * Any property of a supported type. Currently supported types are:
#' -   String - property of String type, required value can be any String.
#' -   Number - any Number implementation. Required value must be a string
#'     parseable to the specific Number type.
#' -   Boolean - required value will be parsed to true only if the string
#'     matches 'true', ignoring case.
#' Accepted operator keywords are:
#' -   '=' - equality
#' -   '!=' - non-equality
#' -   '<' - smaller than
#' -   '>' - larger than
#' -   '<=' - smaller or equal
#' -   '=>' - larger or equal
#' -   'like' - similar string, effectively means 'contains',
#'     translates to the sql 'LIKE' operator (given value will be
#'     surrounded by % signs)
#' Multiple filters can be chained using `AND` and `OR` keywords.
#' Leave space between the keywords and the previous/next word!
#' E.g: `?filter=property1 < value1 AND property2 like value2`
#' If chained filters are mixed conjunctions and disjunctions, the query
#' must be in conjunctive normal form (CNF). Parentheses are not necessary
#' - every AND keyword separates blocks of disjunctions.
#' Example:
#' `?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4`
#' Above query will translate to:
#' `(p1 = v1 OR p1 != v2) AND (p2 <= v2) AND (p3 > v3 OR p3 < v4;)`
#' Breaking the CNF results in an error.
#' Filter `curationDetails.troubled` will be ignored if user is not an
#' administrator.
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
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
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
searchDatasets <- function(query = NA_character_, taxon = NA_character_, filter = NA_character_,
    offset = 0L, limit = 20L, sort = "+id", raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
        filter = validateFilter, offset = validatePositiveInteger,
        limit = validatePositiveInteger, sort = validateSort
    )
    endpoint <- "annotations/{encode(taxon)}/search/{encode(query)}/datasets?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise searchDatasets
#'
#' @noRd
memsearchDatasets <- memoise::memoise(searchDatasets)

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
#' parsing.
#' @param async `TRUE` to run the API query on a separate worker, or `FALSE` to run
#' synchronously. See the `async` package for details.
#' @param memoised Whether or not to cache results so future requests for the same data
#' will be faster. Use `forgetGemmaMemoised` to clear the cache.
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
#' searchAnnotations("traumatic")
searchAnnotations <- function(query = NA_character_, raw = getOption(
        "gemma.raw",
        FALSE
    ), async = getOption("gemma.async", FALSE), memoised = getOption(
        "gemma.memoise",
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
    preprocessor <- processAnnotations
    validators <- list(query = validateQuery)
    endpoint <- "annotations/search/{encode(query)}"
    .body(
        memoised, fname, validators, endpoint, environment(),
        isFile, header, raw, overwrite, file, async, match.call()
    )
}

#' Memoise searchAnnotations
#'
#' @noRd
memsearchAnnotations <- memoise::memoise(searchAnnotations)


#' Clear Gemma API cache
#'
#' Forget past results from memoised calls to the Gemma API (ie. using functions with memoised = `TRUE`)
#'
#' @return TRUE to indicate cache was cleared.
#' @examples
#' forgetGemmaMemoised()
#' @export
#'
#' @keywords misc
forgetGemmaMemoised <- function() {
    memoise::forget(memgetDatasets)
    memoise::forget(memgetResultSets)
    memoise::forget(memgetResultSetFactors)
    memoise::forget(memgetDatasetResultSets)
    memoise::forget(memgetDatasetData)
    memoise::forget(memgetDatasetSamples)
    memoise::forget(memgetDatasetPlatforms)
    memoise::forget(memgetDatasetAnnotations)
    memoise::forget(memgetDatasetDesign)
    memoise::forget(memgetDatasetDEA)
    memoise::forget(memgetPlatforms)
    memoise::forget(memgetPlatformDatasets)
    memoise::forget(memgetPlatformElements)
    memoise::forget(memgetPlatformElementGenes)
    memoise::forget(memgetGenes)
    memoise::forget(memgetGeneEvidence)
    memoise::forget(memgetGeneLocation)
    memoise::forget(memgetGeneProbes)
    memoise::forget(memgetGeneGO)
    memoise::forget(memsearchDatasets)
    memoise::forget(memsearchAnnotations)
}
