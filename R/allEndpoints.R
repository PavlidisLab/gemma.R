#' getDatasets
#' Lists datasets filtered and organized by given parameters
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param filter Optional, defaults to `empty`.
#' Filtering can be done on any\* property or nested property that the
#' appropriate object class defines or inherits (and that is mapped by
#' hibernate). [These do not correspond to the properties of the objects
#' returned by the API calls.]{.description-imp}
#' Class definitions:
#' -   Datasets:
#'     [\[javaDoc\]](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html)
#'     [\[gitHub\]](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java)
#' -   Platforms:
#'     [\[javaDoc\]](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html)
#'     [\[gitHub\]](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java)
#' E.g: `curationDetails` or `curationDetails.lastTroubledEvent.date`.
#' \* Any property of a supported type. Currently supported types are:
#' -   String - property of String type, required value can be any String.
#' -   Number - any Number implementation. Required value must be a string
#'     parseable to the specific Number type.
#' -   Boolean - required value will be parsed to true only if the string
#'     matches \'true\', ignoring case.
#' Accepted operator keywords are:
#' -   \'=\' - equality
#' -   \'!=\' - non-equality
#' -   \'\<\' - smaller than
#' -   \'\>\' - larger than
#' -   \'\<=\' - smaller or equal
#' -   \'=\>\' - larger or equal
#' -   \'like\' - similar string, effectively means \'contains\',
#'     translates to the sql \'LIKE\' operator (given value will be
#'     surrounded by % signs)
#' Multiple filters can be chained using `AND` and `OR` keywords.\
#' Leave space between the keywords and the previous/next word!\
#' E.g: `?filter=property1 < value1 AND property2 like value2`
#' If chained filters are mixed conjunctions and disjunctions, the query
#' must be in conjunctive normal form (CNF). Parentheses are not necessary
#' - every AND keyword separates blocks of disjunctions.
#' Example:\
#' `?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4`\
#' Above query will translate to:\
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
#' descending ordering by the \'Accession\' property.
#' Note that this does [not guarantee the order of the returned
#' entities!]{.description-imp} This is merely a signal to how the data
#' should be pre-sorted before the limit and offset are applied.
#' Nested properties are also supported (recursively).\
#' E.g: `+curationDetails.lastTroubledEvent.date`
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} When
#' using in scripts, remember to URL-encode the \'+\' plus character (see
#' the compiled URL below).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' A successful response may contain a sub-element with \'Geeq\'
#' information, which aims to provide a unified metric to measure
#' experiments by the quality of their data, and their suitability for use
#' in Gemma. You can [read more about the geeq properties
#' here](https://pavlidislab.github.io/Gemma/geeq.html).
#' @export
getDatasets <- function (dataset = NA_character_, filter = NA_character_, offset = 0L, 
    limit = 20L, sort = "+id", raw = getOption("gemma.raw", F), 
    async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
        F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getDatasets"
    preprocessor <- processDatasets
    validators <- list(dataset = validateOptionalID, filter = validateFilter, 
        offset = validatePositiveInteger, limit = validatePositiveInteger, 
        sort = validateSort)
    endpoint <- "datasets/{encode(dataset)}?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getDatasets
#'
memgetDatasets <- memoise::memoise(getDatasets)

#' getDatasetDEA
#' Retrieves the differential analysis results for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getDatasetDEA <- function (dataset = NA_character_, offset = 0L, limit = 20L, 
    raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getDatasetDEA"
    preprocessor <- processDEA
    validators <- list(dataset = validateSingleID, offset = validatePositiveInteger, 
        limit = validatePositiveInteger)
    endpoint <- "datasets/{encode(dataset)}/analyses/differential?offset={encode(offset)}&limit={encode(limit)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getDatasetDEA
#'
memgetDatasetDEA <- memoise::memoise(getDatasetDEA)

#' getDatasetPCA
#' Retrieves the expression levels most correlated with the given principal
#' component
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param component Required, defaults to `empty`.
#' The pca component to limit the results to.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
#' @param keepNonSpecific Optional, defaults to `false`.
#' If set to `false`, the response will only include elements that map
#' exclusively to each queried gene
#' If set to `true`, the response will include all elements that map to
#' each queried gene, even if they also map to other genes.
#' @param consolidate Optional, defaults to `empty`.
#' What action to take when there is more than one element per gene in a
#' dataset.
#' The choices are:
#' -   `[empty]` - will list all vectors separately
#' -   `pickmax` - only return the vector that has the highest expression
#'     (mean over all its bioAssays)
#' -   `pickvar` - only return the vector with highest variance of
#'     expression across its bioAssays
#' -   `average` - create a new vector that will average the bioAssay
#'     values from all vectors
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return The expression levels for each given experiment (experiment expression
#' levels value object) of genes that are most correlated with the given
#' principal component.
#' A `404 error` if the given identifier does not map to any object.
#' @export
getDatasetPCA <- function (dataset = NA_character_, component = 1L, limit = 100L, 
    keepNonSpecific = FALSE, consolidate = NA_character_, raw = getOption("gemma.raw", 
        F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
        F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getDatasetPCA"
    preprocessor <- processExpression
    validators <- list(dataset = validateID, component = validatePositiveInteger, 
        limit = validatePositiveInteger, keepNonSpecific = validateBoolean, 
        consolidate = validateConsolidate)
    endpoint <- "datasets/{encode(dataset)}/expressions/pca?component={encode(component)}&limit={encode(limit)}&keepNonSpecific={encode(keepNonSpecific)}&consolidate={encode(consolidate)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getDatasetPCA
#'
memgetDatasetPCA <- memoise::memoise(getDatasetPCA)

#' getDatasetDE
#' Retrieves differential expression levels for given datasets
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param keepNonSpecific Optional, defaults to `false`.
#' If set to `false`, the response will only include elements that map
#' exclusively to each queried gene
#' If set to `true`, the response will include all elements that map to
#' each queried gene, even if they also map to other genes.
#' @param diffExSet Required, defaults to `empty`.
#' The ID of the differential expression set to retrieve the data from.
#' This value can be obtained through the \'Dataset differential analysis\'
#' endpoint in the \'Dataset endpoints\' category. See the `resultSetId` in
#' one of the response objects in said endpoint.
#' @param threshold Optional, defaults to `100.0`.
#' The threshold that the differential expression has to meet to be
#' included in the response.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
#' @param consolidate Optional, defaults to `empty`.
#' What action to take when there is more than one element per gene in a
#' dataset.
#' The choices are:
#' -   `[empty]` - will list all vectors separately
#' -   `pickmax` - only return the vector that has the highest expression
#'     (mean over all its bioAssays)
#' -   `pickvar` - only return the vector with highest variance of
#'     expression across its bioAssays
#' -   `average` - create a new vector that will average the bioAssay
#'     values from all vectors
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return The differential expression levels for each given experiment (experiment
#' expression levels value object) in the given differential expression
#' set.
#' If the experiment is not in the given diff. exp. set, an empty array is
#' returned.
#' A `404 error` if the given identifier does not map to any object.
#' @export
getDatasetDE <- function (dataset = NA_character_, keepNonSpecific = FALSE, diffExSet = NA_integer_, 
    threshold = 100, limit = 100L, consolidate = NA_character_, 
    raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getDatasetDE"
    preprocessor <- processExpression
    validators <- list(dataset = validateID, keepNonSpecific = validateBoolean, 
        diffExSet = validatePositiveInteger, threshold = validatePositiveReal, 
        limit = validatePositiveInteger, consolidate = validateConsolidate)
    endpoint <- "datasets/{encode(dataset)}/expressions/differential?keepNonSpecific={encode(keepNonSpecific)}&diffExSet={encode(diffExSet)}&threshold={encode(threshold)}&limit={encode(limit)}&consolidate={encode(consolidate)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getDatasetDE
#'
memgetDatasetDE <- memoise::memoise(getDatasetDE)

#' getDatasetSamples
#' Retrieves samples for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getDatasetSamples <- function (dataset = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getDatasetSamples"
    preprocessor <- processSamples
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/samples"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getDatasetSamples
#'
memgetDatasetSamples <- memoise::memoise(getDatasetSamples)

#' getDatasetSVD
#' Retrieves the SVD information for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return A simple SVD value object for the given dataset, containing information
#' about SVD of expression data
#' A `404 error` if the given identifier does not map to any object.
#' Properties of the returned object are:
#' -   **bioMaterialIds** - Array of Bio Material IDs, in same order as the
#'     rows of the v matrix
#' -   **variances** - An array of values representing the fraction of the
#'     variance each component accounts for
#' -   **vMatrix** - the V Matrix (DoubleMatrix object)
#' @export
getDatasetSVD <- function (dataset = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getDatasetSVD"
    preprocessor <- processSVD
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/svd"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getDatasetSVD
#'
memgetDatasetSVD <- memoise::memoise(getDatasetSVD)

#' getDatasetPlatforms
#' Retrieves platforms for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getDatasetPlatforms <- function (dataset = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getDatasetPlatforms"
    preprocessor <- processPlatforms
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/platforms"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getDatasetPlatforms
#'
memgetDatasetPlatforms <- memoise::memoise(getDatasetPlatforms)

#' getDatasetAnnotations
#' Retrieves the annotations for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getDatasetAnnotations <- function (dataset = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getDatasetAnnotations"
    preprocessor <- processAnnotations
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/annotations"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getDatasetAnnotations
#'
memgetDatasetAnnotations <- memoise::memoise(getDatasetAnnotations)

#' getDatasetData
#' Retrieves the data for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getDatasetData <- function (dataset = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- TRUE
    fname <- "getDatasetData"
    preprocessor <- processFile
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/data"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getDatasetData
#'
memgetDatasetData <- memoise::memoise(getDatasetData)

#' getDatasetDesign
#' Retrieves the design for the given dataset
#'
#' @param dataset Required, part of the URL path.
#' Can either be the dataset ID or its short name (e.g. `GSE1234`).
#' Retrieval by ID is more efficient.
#' Only datasets that user has access to will be available
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getDatasetDesign <- function (dataset = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- TRUE
    fname <- "getDatasetDesign"
    preprocessor <- processFile
    validators <- list(dataset = validateSingleID)
    endpoint <- "datasets/{encode(dataset)}/design"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getDatasetDesign
#'
memgetDatasetDesign <- memoise::memoise(getDatasetDesign)

#' datasetInfo
#' @export
datasetInfo <- function (dataset = NA_character_, request = NA_character_, ..., 
    raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    characteristicValue <- "dataset"
    argMap <- c(datasets = "getDatasets", differential = "getDatasetDEA", 
        PCA = "getDatasetPCA", diffEx = "getDatasetDE", samples = "getDatasetSamples", 
        SVD = "getDatasetSVD", platforms = "getDatasetPlatforms", 
        annotations = "getDatasetAnnotations", data = "getDatasetData", 
        design = "getDatasetDesign")
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
#' List platforms filtered and organized by given parameters
#'
#' @param platform Required, part of the URL path.
#' Can either be the platform ID or its short name (e.g: `GPL1355`)
#' Retrieval by ID is more efficient.
#' Only platforms that user has access to will be available.
#' @param filter Optional, defaults to `empty`.
#' Filtering can be done on any\* property or nested property that the
#' appropriate object class defines or inherits (and that is mapped by
#' hibernate). [These do not correspond to the properties of the objects
#' returned by the API calls.]{.description-imp}
#' Class definitions:
#' -   Datasets:
#'     [\[javaDoc\]](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html)
#'     [\[gitHub\]](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java)
#' -   Platforms:
#'     [\[javaDoc\]](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html)
#'     [\[gitHub\]](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java)
#' E.g: `curationDetails` or `curationDetails.lastTroubledEvent.date`.
#' \* Any property of a supported type. Currently supported types are:
#' -   String - property of String type, required value can be any String.
#' -   Number - any Number implementation. Required value must be a string
#'     parseable to the specific Number type.
#' -   Boolean - required value will be parsed to true only if the string
#'     matches \'true\', ignoring case.
#' Accepted operator keywords are:
#' -   \'=\' - equality
#' -   \'!=\' - non-equality
#' -   \'\<\' - smaller than
#' -   \'\>\' - larger than
#' -   \'\<=\' - smaller or equal
#' -   \'=\>\' - larger or equal
#' -   \'like\' - similar string, effectively means \'contains\',
#'     translates to the sql \'LIKE\' operator (given value will be
#'     surrounded by % signs)
#' Multiple filters can be chained using `AND` and `OR` keywords.\
#' Leave space between the keywords and the previous/next word!\
#' E.g: `?filter=property1 < value1 AND property2 like value2`
#' If chained filters are mixed conjunctions and disjunctions, the query
#' must be in conjunctive normal form (CNF). Parentheses are not necessary
#' - every AND keyword separates blocks of disjunctions.
#' Example:\
#' `?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4`\
#' Above query will translate to:\
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
#' descending ordering by the \'Accession\' property.
#' Note that this does [not guarantee the order of the returned
#' entities!]{.description-imp} This is merely a signal to how the data
#' should be pre-sorted before the limit and offset are applied.
#' Nested properties are also supported (recursively).\
#' E.g: `+curationDetails.lastTroubledEvent.date`
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} When
#' using in scripts, remember to URL-encode the \'+\' plus character (see
#' the compiled URL below).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getPlatforms <- function (platform = NA_character_, filter = NA_character_, offset = 0L, 
    limit = 20L, sort = "+id", raw = getOption("gemma.raw", F), 
    async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
        F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getPlatforms"
    preprocessor <- processPlatforms
    validators <- list(filter = validateFilter, offset = validatePositiveInteger, 
        limit = validatePositiveInteger, sort = validateSort)
    endpoint <- "platforms/{encode(platform)}?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getPlatforms
#'
memgetPlatforms <- memoise::memoise(getPlatforms)

#' getPlatformDatasets
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
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getPlatformDatasets <- function (platform = NA_character_, offset = 0L, limit = 20L, 
    raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getPlatformDatasets"
    preprocessor <- processDatasets
    validators <- list(platform = validateSingleID, offset = validatePositiveInteger, 
        limit = validatePositiveInteger)
    endpoint <- "platforms/{encode(platform)}/datasets?offset={encode(offset)}&limit={encode(limit)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getPlatformDatasets
#'
memgetPlatformDatasets <- memoise::memoise(getPlatformDatasets)

#' getPlatformElements
#' Retrieves the composite sequences (elements) for the given platform
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
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getPlatformElements <- function (platform = NA_character_, offset = 0L, limit = 20L, 
    raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getPlatformElements"
    preprocessor <- processElements
    validators <- list(platform = validateSingleID, offset = validatePositiveInteger, 
        limit = validatePositiveInteger)
    endpoint <- "platforms/{encode(platform)}/elements?offset={encode(offset)}&limit={encode(limit)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getPlatformElements
#'
memgetPlatformElements <- memoise::memoise(getPlatformElements)

#' getPlatformElementGenes
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
#' Can either be probes name or IDs.\
#' Do not combine different identifiers in one query.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} When
#' using in scripts, remember to URL-encode any forward slashes in the
#' probe name (see the compiled URL below).
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getPlatformElementGenes <- function (platform = NA_character_, element = NA_character_, 
    offset = 0L, limit = 20L, raw = getOption("gemma.raw", F), 
    async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
        F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getPlatformElementGenes"
    preprocessor <- processGenes
    validators <- list(platform = validateSingleID, element = validateSingleID, 
        offset = validatePositiveInteger, limit = validatePositiveInteger)
    endpoint <- "platforms/{encode(platform)}/elements/{encode(element)}/genes?offset={encode(offset)}&limit={encode(limit)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getPlatformElementGenes
#'
memgetPlatformElementGenes <- memoise::memoise(getPlatformElementGenes)

#' platformInfo
#' @export
platformInfo <- function (platform = NA_character_, request = NA_character_, 
    ..., raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
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
#' Retrieves all genes matching the identifiers
#'
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getGenes <- function (gene = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getGenes"
    preprocessor <- processGenes
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getGenes
#'
memgetGenes <- memoise::memoise(getGenes)

#' getGeneEvidence
#' Retrieves gene evidence for the given gene
#'
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getGeneEvidence <- function (gene = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getGeneEvidence"
    preprocessor <- processGeneEvidence
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/evidence"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getGeneEvidence
#'
memgetGeneEvidence <- memoise::memoise(getGeneEvidence)

#' getGeneLocation
#' Retrieves the physical location of the given gene
#'
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getGeneLocation <- function (gene = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getGeneLocation"
    preprocessor <- processGeneLocation
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/locations"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getGeneLocation
#'
memgetGeneLocation <- memoise::memoise(getGeneLocation)

#' getGeneProbes
#' Retrieves the probes (composite sequences) with this gene
#'
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param offset Optional, defaults to `0`.
#' Skips the specified amount of objects when retrieving them from the
#' database.
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getGeneProbes <- function (gene = NA_character_, offset = 0L, limit = 20L, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getGeneProbes"
    preprocessor <- processElements
    validators <- list(gene = validateSingleID, offset = validatePositiveInteger, 
        limit = validatePositiveInteger)
    endpoint <- "gene/{encode(gene)}/probes?offset={encode(offset)}&limit={encode(limit)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getGeneProbes
#'
memgetGeneProbes <- memoise::memoise(getGeneProbes)

#' getGeneGO
#' Retrieves the GO terms of the given gene
#'
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
getGeneGO <- function (gene = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getGeneGO"
    preprocessor <- processGO
    validators <- list(gene = validateSingleID)
    endpoint <- "genes/{encode(gene)}/goTerms"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getGeneGO
#'
memgetGeneGO <- memoise::memoise(getGeneGO)

#' getGeneCoexpression
#' Retrieves the coexpression of two given genes
#'
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param with Required, defaults to `empty`.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param limit Optional, defaults to `20`.
#' Limits the result to specified amount of objects. Use 0 for no limit.
#' @param stringency Optional, defaults to `1`.
#' Sets the stringency of coexpression search.
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return An array of gene coexpression data (coexpression meta value objects)
#' representing the coexpression of the two given genes.
#' A `404 error` if the given identifier does not map to any object.
#' A `400 error` if required parameters are missing.
#' @export
getGeneCoexpression <- function (gene = NA_character_, with = NA_character_, limit = 20L, 
    stringency = 1L, raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getGeneCoexpression"
    preprocessor <- processCoexpression
    validators <- list(gene = validateSingleID, with = validateSingleID, 
        limit = validatePositiveInteger, stringency = validatePositiveInteger)
    endpoint <- "genes/{encode(gene)}/coexpression?with={encode(with)}&limit={encode(limit)}&stringency={encode(stringency)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getGeneCoexpression
#'
memgetGeneCoexpression <- memoise::memoise(getGeneCoexpression)

#' geneInfo
#' @export
geneInfo <- function (gene = NA_character_, request = NA_character_, ..., 
    raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
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
#' List taxa filtered by given parameters
#'
#' @param taxon Not required, part of the URL path.
#' can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:
#' scientific name, common name
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers
#' available.
#' Use the \'All Taxa\' endpoint to retrieve the necessary information. For
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
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return An array of taxa (taxon value objects) matching the given identifiers.
#' A `400 error` if all identifiers are invalid.
#' An array of all available taxa, if no identifiers were provided.
#' @export
getTaxa <- function (taxon = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getTaxa"
    preprocessor <- processTaxon
    validators <- list(taxon = validateOptionalTaxon)
    endpoint <- "taxa/{encode(taxon)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getTaxa
#'
memgetTaxa <- memoise::memoise(getTaxa)

#' getTaxonDatasets
#' Retrieves datasets for the given taxon.
#'
#' @param taxon Not required, part of the URL path.
#' can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:
#' scientific name, common name
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers
#' available.
#' Use the \'All Taxa\' endpoint to retrieve the necessary information. For
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
#' Filtering can be done on any\* property or nested property that the
#' appropriate object class defines or inherits (and that is mapped by
#' hibernate). [These do not correspond to the properties of the objects
#' returned by the API calls.]{.description-imp}
#' Class definitions:
#' -   Datasets:
#'     [\[javaDoc\]](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html)
#'     [\[gitHub\]](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java)
#' -   Platforms:
#'     [\[javaDoc\]](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html)
#'     [\[gitHub\]](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java)
#' E.g: `curationDetails` or `curationDetails.lastTroubledEvent.date`.
#' \* Any property of a supported type. Currently supported types are:
#' -   String - property of String type, required value can be any String.
#' -   Number - any Number implementation. Required value must be a string
#'     parseable to the specific Number type.
#' -   Boolean - required value will be parsed to true only if the string
#'     matches \'true\', ignoring case.
#' Accepted operator keywords are:
#' -   \'=\' - equality
#' -   \'!=\' - non-equality
#' -   \'\<\' - smaller than
#' -   \'\>\' - larger than
#' -   \'\<=\' - smaller or equal
#' -   \'=\>\' - larger or equal
#' -   \'like\' - similar string, effectively means \'contains\',
#'     translates to the sql \'LIKE\' operator (given value will be
#'     surrounded by % signs)
#' Multiple filters can be chained using `AND` and `OR` keywords.\
#' Leave space between the keywords and the previous/next word!\
#' E.g: `?filter=property1 < value1 AND property2 like value2`
#' If chained filters are mixed conjunctions and disjunctions, the query
#' must be in conjunctive normal form (CNF). Parentheses are not necessary
#' - every AND keyword separates blocks of disjunctions.
#' Example:\
#' `?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4`\
#' Above query will translate to:\
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
#' descending ordering by the \'Accession\' property.
#' Note that this does [not guarantee the order of the returned
#' entities!]{.description-imp} This is merely a signal to how the data
#' should be pre-sorted before the limit and offset are applied.
#' Nested properties are also supported (recursively).\
#' E.g: `+curationDetails.lastTroubledEvent.date`
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} When
#' using in scripts, remember to URL-encode the \'+\' plus character (see
#' the compiled URL below).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return An array of datasets (expression experiment value objects) associated
#' with the given taxon.
#' A `404 error` if the given identifier does not map to any object.
#' @export
getTaxonDatasets <- function (taxon = NA_character_, filter = NA_character_, offset = 0L, 
    limit = 20L, sort = "+id", raw = getOption("gemma.raw", F), 
    async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
        F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getTaxonDatasets"
    preprocessor <- processDatasets
    validators <- list(taxon = validateSingleID, filter = validateFilter, 
        offset = validatePositiveInteger, limit = validatePositiveInteger, 
        sort = validateSort)
    endpoint <- "taxa/{encode(taxon)}/datasets?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getTaxonDatasets
#'
memgetTaxonDatasets <- memoise::memoise(getTaxonDatasets)

#' getTaxonPhenotypes
#' Loads all phenotypes for the given taxon.
#'
#' @param taxon Not required, part of the URL path.
#' can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:
#' scientific name, common name
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers
#' available.
#' Use the \'All Taxa\' endpoint to retrieve the necessary information. For
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
#' @param editableOnly Optional, defaults to `false`.
#' Whether to only list editable objects.
#' @param tree Optional, defaults to `false`.
#' Whether the returned structure should be an actual tree (nested JSON
#' objects).
#' Default is false - the tree is flattened and the tree structure
#' information is stored as the values of the returned object.
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return If `tree = false`, an array of simple tree value objects.
#' If `tree = true`, an array of tree characteristic value objects, that
#' will represent root nodes of a phenotype tree. Each node has its child
#' nodes in the `children` array property. There will be exactly 3 root
#' nodes - for a disease ontology tree, human phenotype ontology tree and a
#' mammalian phenotype ontology tree. If there are no terms for the given
#' taxon in any of the ontologies, the relevant root node will be null.
#' A `404 error` if the given identifier does not map to any object.
#' @export
getTaxonPhenotypes <- function (taxon = NA_character_, editableOnly = FALSE, tree = FALSE, 
    raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getTaxonPhenotypes"
    preprocessor <- processPhenotypes
    validators <- list(taxon = validateSingleID, editableOnly = validateBoolean, 
        tree = validateBoolean)
    endpoint <- "taxa/{encode(taxon)}/phenotypes?editableOnly={encode(editableOnly)}&tree={encode(tree)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getTaxonPhenotypes
#'
memgetTaxonPhenotypes <- memoise::memoise(getTaxonPhenotypes)

#' getTaxonPhenotypeCandidates
#' Given a set of phenotypes, return all genes associated with them.
#'
#' @param taxon Not required, part of the URL path.
#' can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:
#' scientific name, common name
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers
#' available.
#' Use the \'All Taxa\' endpoint to retrieve the necessary information. For
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
#' @param editableOnly Optional, defaults to `false`.
#' Whether to only list editable objects.
#' @param phenotypes Required, defaults to `empty`.
#' Phenotype value URIs separated by commas.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} When
#' using in scripts, remember to URL-encode any forward slashes in the
#' phenotype value URIs (see the compiled URL below).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return An array of gene evidence value objects for genes associated with the
#' given phenotypes on the given taxon.
#' A `404 error` if the given identifier does not map to any object.
#' A `400 error` if required parameters are missing.
#' @export
getTaxonPhenotypeCandidates <- function (taxon = NA_character_, editableOnly = FALSE, phenotypes = NA_character_, 
    raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getTaxonPhenotypeCandidates"
    preprocessor <- processGeneEvidence
    validators <- list(taxon = validateSingleID, editableOnly = validateBoolean, 
        phenotypes = validateSingleID)
    endpoint <- "taxa/{encode(taxon)}/phenotypes/candidates?editableOnly={encode(editableOnly)}&phenotypes={encode(phenotypes)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getTaxonPhenotypeCandidates
#'
memgetTaxonPhenotypeCandidates <- memoise::memoise(getTaxonPhenotypeCandidates)

#' getGeneOnTaxon
#' Retrieves genes matching the identifier on the given taxon.
#'
#' @param taxon Not required, part of the URL path.
#' can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:
#' scientific name, common name
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers
#' available.
#' Use the \'All Taxa\' endpoint to retrieve the necessary information. For
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
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return An array of genes (gene value objects) on the given taxon, matching the
#' given identifier.
#' A `404 error` if the given identifier does not map to any object.
#' @export
getGeneOnTaxon <- function (taxon = NA_character_, gene = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getGeneOnTaxon"
    preprocessor <- processGenes
    validators <- list(taxon = validateSingleID, gene = validateSingleID)
    endpoint <- "taxa/{encode(taxon)}/genes/{encode(gene)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getGeneOnTaxon
#'
memgetGeneOnTaxon <- memoise::memoise(getGeneOnTaxon)

#' getEvidenceOnTaxon
#' Retrieves gene evidence for the gene on the given taxon.
#'
#' @param taxon Not required, part of the URL path.
#' can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:
#' scientific name, common name
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers
#' available.
#' Use the \'All Taxa\' endpoint to retrieve the necessary information. For
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
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return An array of gene evidence value objects for the given gene on the given
#' taxon.
#' A `404 error` if the given identifier does not map to any object.
#' @export
getEvidenceOnTaxon <- function (taxon = NA_character_, gene = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getEvidenceOnTaxon"
    preprocessor <- processGeneEvidence
    validators <- list(taxon = validateSingleID, gene = validateSingleID)
    endpoint <- "taxa/{encode(taxon)}/genes/{encode(gene)}/evidence"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getEvidenceOnTaxon
#'
memgetEvidenceOnTaxon <- memoise::memoise(getEvidenceOnTaxon)

#' getGeneLocationOnTaxon
#' Retrieves gene evidence for the gene on the given taxon.
#'
#' @param taxon Not required, part of the URL path.
#' can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:
#' scientific name, common name
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers
#' available.
#' Use the \'All Taxa\' endpoint to retrieve the necessary information. For
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
#' @param gene Required, part of the URL path.
#' Can either be the NCBI ID (`1859`), Ensembl ID (`ENSG00000157540`) or
#' official symbol (`DYRK1A`) of the gene.
#' NCBI ID is the most efficient (and guaranteed to be unique) identifier.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} Official
#' symbol represents a gene homologue for a random taxon, unless used in a
#' specific taxon (see Taxon Endpoints).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' gene on the given taxon.
#' A `404 error` if the given identifier does not map to any object.
#' @export
getGeneLocationOnTaxon <- function (taxon = NA_character_, gene = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "getGeneLocationOnTaxon"
    preprocessor <- processGeneLocation
    validators <- list(taxon = validateSingleID, gene = validateSingleID)
    endpoint <- "taxa/{encode(taxon)}/genes/{encode(gene)}/locations"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getGeneLocationOnTaxon
#'
memgetGeneLocationOnTaxon <- memoise::memoise(getGeneLocationOnTaxon)

#' getGenesAtLocation
#' Finds genes overlapping a given region.
#'
#' @param taxon Not required, part of the URL path.
#' can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:
#' scientific name, common name
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers
#' available.
#' Use the \'All Taxa\' endpoint to retrieve the necessary information. For
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
#' @param chromosome Required, defaults to `empty`.
#' The chromosome of the query location. Eg: `3`, `21`, `X`
#' @param strand Optional, defaults to `+`.
#' Can either be `+` or `-`.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} This is a
#' WIP parameter and does currently not do anything
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} When
#' using in scripts, remember to URL-encode the \'+\' plus character (see
#' the compiled URL below).
#' @param start Required, defaults to `empty`.
#' Number of the start nucleotide of the desired region.
#' @param size Required, defaults to `empty`.
#' Amount of nucleotides in the desired region (i.e. the length of the
#' region).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
#' @return An array of genes (gene value objects) that overlap the given region.
#' A `404 error` if the given identifier does not map to any object.
#' A `400 error` if required parameters are missing.
#' @export
getGenesAtLocation <- function (taxon = NA_character_, chromosome = NA_character_, 
    strand = "+", start = NA_integer_, size = NA_integer_, raw = getOption("gemma.raw", 
        F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
        F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "getGenesAtLocation"
    preprocessor <- processGenes
    validators <- list(taxon = validateSingleID, chromosome = validateSingleID, 
        strand = validateStrand, start = validatePositiveInteger, 
        size = validatePositiveInteger)
    endpoint <- "taxa/{encode(taxon)}/chromosomes/{encode(chromosome)}/genes?strand={encode(strand)}&start={encode(start)}&size={encode(size)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise getGenesAtLocation
#'
memgetGenesAtLocation <- memoise::memoise(getGenesAtLocation)

#' searchDatasets
#' Does a search for datasets containing annotations, short name or full
#' name matching the given string
#'
#' @param taxon Not required, part of the URL path.
#' can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:
#' scientific name, common name
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers
#' available.
#' Use the \'All Taxa\' endpoint to retrieve the necessary information. For
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
#' @param query Required, defaults to `empty`.
#' The search query. Either plain text (\'traumatic\'), or an ontology term
#' URI (\'http://purl.obolibrary.org/obo/UBERON_0002048\'). Datasets that
#' contain the given string in their short of full name will also be
#' matched (\'GSE201\', \'Bronchoalveolar lavage samples\'.
#' Can be multiple identifiers separated by commas.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} When
#' using in scripts, remember to URL-encode any forward slashes in the
#' phenotype value URIs (see the compiled URL below).
#' @param filter Optional, defaults to `empty`.
#' Filtering can be done on any\* property or nested property that the
#' appropriate object class defines or inherits (and that is mapped by
#' hibernate). [These do not correspond to the properties of the objects
#' returned by the API calls.]{.description-imp}
#' Class definitions:
#' -   Datasets:
#'     [\[javaDoc\]](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html)
#'     [\[gitHub\]](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java)
#' -   Platforms:
#'     [\[javaDoc\]](http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html)
#'     [\[gitHub\]](https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java)
#' E.g: `curationDetails` or `curationDetails.lastTroubledEvent.date`.
#' \* Any property of a supported type. Currently supported types are:
#' -   String - property of String type, required value can be any String.
#' -   Number - any Number implementation. Required value must be a string
#'     parseable to the specific Number type.
#' -   Boolean - required value will be parsed to true only if the string
#'     matches \'true\', ignoring case.
#' Accepted operator keywords are:
#' -   \'=\' - equality
#' -   \'!=\' - non-equality
#' -   \'\<\' - smaller than
#' -   \'\>\' - larger than
#' -   \'\<=\' - smaller or equal
#' -   \'=\>\' - larger or equal
#' -   \'like\' - similar string, effectively means \'contains\',
#'     translates to the sql \'LIKE\' operator (given value will be
#'     surrounded by % signs)
#' Multiple filters can be chained using `AND` and `OR` keywords.\
#' Leave space between the keywords and the previous/next word!\
#' E.g: `?filter=property1 < value1 AND property2 like value2`
#' If chained filters are mixed conjunctions and disjunctions, the query
#' must be in conjunctive normal form (CNF). Parentheses are not necessary
#' - every AND keyword separates blocks of disjunctions.
#' Example:\
#' `?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4`\
#' Above query will translate to:\
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
#' descending ordering by the \'Accession\' property.
#' Note that this does [not guarantee the order of the returned
#' entities!]{.description-imp} This is merely a signal to how the data
#' should be pre-sorted before the limit and offset are applied.
#' Nested properties are also supported (recursively).\
#' E.g: `+curationDetails.lastTroubledEvent.date`
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} When
#' using in scripts, remember to URL-encode the \'+\' plus character (see
#' the compiled URL below).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
searchDatasets <- function (taxon = "", query = NA_character_, filter = NA_character_, 
    offset = 0L, limit = 0L, sort = "+id", raw = getOption("gemma.raw", 
        F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
        F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
{
    isFile <- FALSE
    fname <- "searchDatasets"
    preprocessor <- processDatasets
    validators <- list(taxon = validateTaxon, query = validateQuery, 
        filter = validateFilter, offset = validatePositiveInteger, 
        limit = validatePositiveInteger, sort = validateSort)
    endpoint <- "annotations/{encode(taxon)}/search/{encode(query)}/datasets?filter={encode(filter)}&offset={encode(offset)}&limit={encode(limit)}&sort={encode(sort)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise searchDatasets
#'
memsearchDatasets <- memoise::memoise(searchDatasets)

#' taxonInfo
#' @export
taxonInfo <- function (taxon = NA_character_, request = NA_character_, ..., 
    raw = getOption("gemma.raw", F), async = getOption("gemma.async", 
        F), memoised = getOption("gemma.memoise", F), file = getOption("gemma.file", 
        NA_character_), overwrite = getOption("gemma.overwrite", 
        F)) 
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

#' searchAnnotations
#' Does a search for annotations based on the given string
#'
#' @param query Required, defaults to `empty`.
#' The search query. Either plain text (\'traumatic\'), or an ontology term
#' URI (\'http://purl.obolibrary.org/obo/UBERON_0002048\'). Datasets that
#' contain the given string in their short of full name will also be
#' matched (\'GSE201\', \'Bronchoalveolar lavage samples\'.
#' Can be multiple identifiers separated by commas.
#' []{.glyphicon .glyphicon-th-large .glyphicon-exclamation-sign} When
#' using in scripts, remember to URL-encode any forward slashes in the
#' phenotype value URIs (see the compiled URL below).
#' @param raw `FALSE` to receive results as-is from Gemma, or `TRUE` to enable
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
searchAnnotations <- function (query = NA_character_, raw = getOption("gemma.raw", 
    F), async = getOption("gemma.async", F), memoised = getOption("gemma.memoise", 
    F), file = getOption("gemma.file", NA_character_), overwrite = getOption("gemma.overwrite", 
    F)) 
{
    isFile <- FALSE
    fname <- "searchAnnotations"
    preprocessor <- processAnnotations
    validators <- list(query = validateQuery)
    endpoint <- "annotations/search/{encode(query)}"
    .body(memoised, fname, validators, endpoint, environment(), 
        isFile, raw, overwrite, file, async)
}

#' Memoise searchAnnotations
#'
memsearchAnnotations <- memoise::memoise(searchAnnotations)


#' forgetGemmaMemoised
#'
#' Forget past results from memoised calls to the Gemma API (ie. using functions with memoised = `TRUE`)
#'
#' @export
forgetGemmaMemoised <- function () 
{
    forget(memgetDatasets)
    forget(memgetDatasetDEA)
    forget(memgetDatasetPCA)
    forget(memgetDatasetDE)
    forget(memgetDatasetSamples)
    forget(memgetDatasetSVD)
    forget(memgetDatasetPlatforms)
    forget(memgetDatasetAnnotations)
    forget(memgetDatasetData)
    forget(memgetDatasetDesign)
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
    forget(memsearchAnnotations)
}