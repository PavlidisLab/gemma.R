#' getDatasets
#' Lists datasets filtered and organized by given parameters
#'
#' @param dataset <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the dataset ID or its short name (e.g. <code>GSE1234</code>).</p><p>Retrieval by ID is more efficient.</p><p>Only datasets that user has access to will be available</p>
#' @param filter <p class='description-frow'>Optional, defaults to <code>empty</code>.</p><p>   Filtering can be done on any* property or nested property that the appropriate object class defines   or inherits (and that is mapped by hibernate). <span class='description-imp'>These do not correspond to the properties of the    objects returned by the API calls.</span> </p><p>Class definitions:    <ul>       <li>Datasets:            <a href='http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html'>           [javaDoc]</a>            <a href='https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java'>           [gitHub]</a>       </li>       <li>Platforms:            <a href='http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html'>           [javaDoc]</a>            <a href='https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java'>           [gitHub]</a>       </li>   </ul></p><p>   E.g: <code>curationDetails</code> or <code>curationDetails.lastTroubledEvent.date</code>.</p><p>   * Any property of a supported type. Currently supported types are:   <ul>       <li>String - property of String type, required value can be any String.</li>       <li>Number - any Number implementation. Required value must be a string parseable to the specific Number type.</li>       <li>Boolean - required value will be parsed to true only if the string matches 'true', ignoring case.</li>   </ul></p><p>Accepted operator keywords are:   <ul>       <li> '=' - equality</li>       <li> '!=' - non-equality</li>       <li> '<' - smaller than</li>       <li> '>' - larger than</li>       <li> '<=' - smaller or equal</li>       <li> '=>' - larger or equal</li>       <li> 'like' - similar string, effectively means 'contains', translates to the sql 'LIKE' operator (given value will be surrounded by % signs)</li>   </ul>   Multiple filters can be chained using <code>AND</code> and <code>OR</code> keywords.<br/>   Leave space between the keywords and the previous/next word! <br/>   E.g: <code>?filter=property1 < value1 AND property2 like value2</code></p><p>   If chained filters are mixed conjunctions and disjunctions, the query must be in conjunctive normal   form (CNF). Parentheses are not necessary - every AND keyword separates blocks of disjunctions.</p><p>Example:<br/><code>?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4</code><br/>Above query will translate to: <br/><code>(p1 = v1 OR p1 != v2) AND (p2 <= v2) AND (p3 > v3 OR p3 < v4;)</code></p><p>Breaking the CNF results in an error.</p><p>Filter <code>curationDetails.troubled</code> will be ignored if user is not an administrator.</p>
#' @param offset <p class='description-frow'>Optional, defaults to <code>0</code>.</p><p>Skips the specified amount of objects when retrieving them from the database.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param sort <p class='description-frow'>Optional, defaults to <code>+id</code>.</p><p>Sets the ordering property and direction.</p><p>   Format is <code>[+,-][property name]</code>. E.g. <code>-accession</code> will translate to descending ordering by the   'Accession' property.</p><p>   Note that this does <span class='description-imp'>not guarantee the order of the returned entities!</span> This is merely a signal to how the data should be pre-sorted before   the limit and offset are applied.</p><p>   Nested properties are also supported (recursively).<br/>   E.g: <code>+curationDetails.lastTroubledEvent.date</code></p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   When using in scripts, remember to URL-encode the '+' plus character (see the compiled URL below).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   An array of value objects representing the objects that matched the query. </p><p>   Empty array if no objects matched.</p><p>   A successful response may contain a sub-element with 'Geeq' information, which aims to provide a    unified metric to measure experiments by the quality of their data, and their suitability for use in Gemma.   You can <a href='https://pavlidislab.github.io/Gemma/geeq.html' target='_blank'>read more about the geeq properties here</a>. </p>
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
#' Retrieves the differential analysis results for the given dataset
#'
#' @param dataset <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the dataset ID or its short name (e.g. <code>GSE1234</code>).</p><p>Retrieval by ID is more efficient.</p><p>Only datasets that user has access to will be available</p>
#' @param offset <p class='description-frow'>Optional, defaults to <code>0</code>.</p><p>Skips the specified amount of objects when retrieving them from the database.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   An array of analyses (differential expression value objects) in the given dataset.<p>A <code>404 error</code> if the given identifier does not map to any object.</p><p>A <code>400 error</code> if required parameters are missing.</p></p>
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
#' Retrieves the expression levels most correlated with the given principal component
#'
#' @param dataset <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the dataset ID or its short name (e.g. <code>GSE1234</code>).</p><p>Retrieval by ID is more efficient.</p><p>Only datasets that user has access to will be available</p>
#' @param component <p class='description-frow'>Required, defaults to <code>empty</code>.</p><p>The pca component to limit the results to.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param keepNonSpecific <p class='description-frow'>Optional, defaults to <code>false</code>.</p><p>   If set to <code>false</code>, the response will only include elements that map exclusively to each queried gene</p><p>   If set to <code>true</code>, the response will include all elements that map to each queried gene, even if they   also map to other genes.</p>
#' @param consolidate <p class='description-frow'>Optional, defaults to <code>empty</code>.</p><p>   What action to take when there is more than one element per gene in a dataset.</p><p>   The choices are:   <ul>       <li><code>[empty]</code> - will list all vectors separately</li>       <li><code>pickmax</code> - only return the vector that has the highest expression (mean over all its bioAssays)</li>       <li><code>pickvar</code> - only return the vector with highest variance of expression across its bioAssays</li>       <li><code>average</code> - create a new vector that will average the bioAssay values from all vectors</li>   </ul></p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   The expression levels for each given experiment (experiment expression levels value object) of genes that are   most correlated with the given principal component.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p></p>
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
#' Retrieves differential expression levels for given datasets
#'
#' @param dataset <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the dataset ID or its short name (e.g. <code>GSE1234</code>).</p><p>Retrieval by ID is more efficient.</p><p>Only datasets that user has access to will be available</p>
#' @param keepNonSpecific <p class='description-frow'>Optional, defaults to <code>false</code>.</p><p>   If set to <code>false</code>, the response will only include elements that map exclusively to each queried gene</p><p>   If set to <code>true</code>, the response will include all elements that map to each queried gene, even if they   also map to other genes.</p>
#' @param diffExSet <p class='description-frow'>Required, defaults to <code>empty</code>.</p><p>The ID of the differential expression set to retrieve the data from.</p><p>   This value can be obtained through the 'Dataset differential analysis' endpoint in the 'Dataset endpoints' category.    See the <code>resultSetId</code> in one of the response objects in said endpoint.</p>
#' @param threshold <p class='description-frow'>Optional, defaults to <code>100.0</code>.</p><p>The threshold that the differential expression has to meet to be included in the response.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param consolidate <p class='description-frow'>Optional, defaults to <code>empty</code>.</p><p>   What action to take when there is more than one element per gene in a dataset.</p><p>   The choices are:   <ul>       <li><code>[empty]</code> - will list all vectors separately</li>       <li><code>pickmax</code> - only return the vector that has the highest expression (mean over all its bioAssays)</li>       <li><code>pickvar</code> - only return the vector with highest variance of expression across its bioAssays</li>       <li><code>average</code> - create a new vector that will average the bioAssay values from all vectors</li>   </ul></p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   The differential expression levels for each given experiment (experiment expression levels value object) in the   given differential expression set.</p><p>   If the experiment is not in the given diff. exp. set, an empty array is returned.<p>A <code>404 error</code> if the given identifier does not map to any object.</p></p>
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
#' Retrieves samples for the given dataset
#'
#' @param dataset <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the dataset ID or its short name (e.g. <code>GSE1234</code>).</p><p>Retrieval by ID is more efficient.</p><p>Only datasets that user has access to will be available</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   An array of samples (bio assay value objects) in the given dataset.<p>A <code>404 error</code> if the given identifier does not map to any object.</p></p>
#' @export
getDatasetSamples <- function (dataset = NA_character_, raw = FALSE, async = FALSE, 
    memoised = FALSE, file = NA_character_, overwrite = FALSE) 
{
    fname <- "getDatasetSamples"
    preprocessor <- processSamples
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
#' Retrieves the SVD information for the given dataset
#'
#' @param dataset <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the dataset ID or its short name (e.g. <code>GSE1234</code>).</p><p>Retrieval by ID is more efficient.</p><p>Only datasets that user has access to will be available</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   A simple SVD value object for the given dataset, containing information about SVD of expression data <p>A <code>404 error</code> if the given identifier does not map to any object.</p><p>   Properties of the returned object are:   <ul>       <li><b>bioMaterialIds</b> - Array of Bio Material IDs, in same order as the rows of the v matrix</li>       <li><b>variances</b> - An array of values representing the fraction of the variance each component accounts for</li>       <li><b>vMatrix</b> - the V Matrix (DoubleMatrix object)</li>   </ul></p></p>
#' @export
getDatasetSVD <- function (dataset = NA_character_, raw = FALSE, async = FALSE, 
    memoised = FALSE, file = NA_character_, overwrite = FALSE) 
{
    fname <- "getDatasetSVD"
    preprocessor <- processSVD
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
#' Retrieves platforms for the given dataset
#'
#' @param dataset <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the dataset ID or its short name (e.g. <code>GSE1234</code>).</p><p>Retrieval by ID is more efficient.</p><p>Only datasets that user has access to will be available</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   An array of platforms (array design value objects) containing the given dataset.<p>A <code>404 error</code> if the given identifier does not map to any object.</p></p>
#' @export
getDatasetPlatforms <- function (dataset = NA_character_, raw = FALSE, async = FALSE, 
    memoised = FALSE, file = NA_character_, overwrite = FALSE) 
{
    fname <- "getDatasetPlatforms"
    preprocessor <- processPlatforms
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
#' Retrieves the annotations for the given dataset
#'
#' @param dataset <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the dataset ID or its short name (e.g. <code>GSE1234</code>).</p><p>Retrieval by ID is more efficient.</p><p>Only datasets that user has access to will be available</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   An array of annotations (annotation value objects) attached to the given dataset.<p>A <code>404 error</code> if the given identifier does not map to any object.</p></p>
#' @export
getDatasetAnnotations <- function (dataset = NA_character_, raw = FALSE, async = FALSE, 
    memoised = FALSE, file = NA_character_, overwrite = FALSE) 
{
    fname <- "getDatasetAnnotations"
    preprocessor <- processAnnotations
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
#' List platforms filtered and organized by given parameters
#'
#' @param platform <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the platform ID or its short name (e.g: <code>GPL1355</code>)</p><p>Retrieval by ID is more efficient. </p><p>Only platforms that user has access to will be available.</p>
#' @param filter <p class='description-frow'>Optional, defaults to <code>empty</code>.</p><p>   Filtering can be done on any* property or nested property that the appropriate object class defines   or inherits (and that is mapped by hibernate). <span class='description-imp'>These do not correspond to the properties of the    objects returned by the API calls.</span> </p><p>Class definitions:    <ul>       <li>Datasets:            <a href='http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html'>           [javaDoc]</a>            <a href='https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java'>           [gitHub]</a>       </li>       <li>Platforms:            <a href='http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html'>           [javaDoc]</a>            <a href='https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java'>           [gitHub]</a>       </li>   </ul></p><p>   E.g: <code>curationDetails</code> or <code>curationDetails.lastTroubledEvent.date</code>.</p><p>   * Any property of a supported type. Currently supported types are:   <ul>       <li>String - property of String type, required value can be any String.</li>       <li>Number - any Number implementation. Required value must be a string parseable to the specific Number type.</li>       <li>Boolean - required value will be parsed to true only if the string matches 'true', ignoring case.</li>   </ul></p><p>Accepted operator keywords are:   <ul>       <li> '=' - equality</li>       <li> '!=' - non-equality</li>       <li> '<' - smaller than</li>       <li> '>' - larger than</li>       <li> '<=' - smaller or equal</li>       <li> '=>' - larger or equal</li>       <li> 'like' - similar string, effectively means 'contains', translates to the sql 'LIKE' operator (given value will be surrounded by % signs)</li>   </ul>   Multiple filters can be chained using <code>AND</code> and <code>OR</code> keywords.<br/>   Leave space between the keywords and the previous/next word! <br/>   E.g: <code>?filter=property1 < value1 AND property2 like value2</code></p><p>   If chained filters are mixed conjunctions and disjunctions, the query must be in conjunctive normal   form (CNF). Parentheses are not necessary - every AND keyword separates blocks of disjunctions.</p><p>Example:<br/><code>?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4</code><br/>Above query will translate to: <br/><code>(p1 = v1 OR p1 != v2) AND (p2 <= v2) AND (p3 > v3 OR p3 < v4;)</code></p><p>Breaking the CNF results in an error.</p><p>Filter <code>curationDetails.troubled</code> will be ignored if user is not an administrator.</p>
#' @param offset <p class='description-frow'>Optional, defaults to <code>0</code>.</p><p>Skips the specified amount of objects when retrieving them from the database.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param sort <p class='description-frow'>Optional, defaults to <code>+id</code>.</p><p>Sets the ordering property and direction.</p><p>   Format is <code>[+,-][property name]</code>. E.g. <code>-accession</code> will translate to descending ordering by the   'Accession' property.</p><p>   Note that this does <span class='description-imp'>not guarantee the order of the returned entities!</span> This is merely a signal to how the data should be pre-sorted before   the limit and offset are applied.</p><p>   Nested properties are also supported (recursively).<br/>   E.g: <code>+curationDetails.lastTroubledEvent.date</code></p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   When using in scripts, remember to URL-encode the '+' plus character (see the compiled URL below).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   An array of value objects representing the objects that matched the query. </p><p>   Empty array if no objects matched.</p>
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
#' Retrieves experiments in the given platform
#'
#' @param platform <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the platform ID or its short name (e.g: <code>GPL1355</code>)</p><p>Retrieval by ID is more efficient. </p><p>Only platforms that user has access to will be available.</p>
#' @param offset <p class='description-frow'>Optional, defaults to <code>0</code>.</p><p>Skips the specified amount of objects when retrieving them from the database.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   An array of datasets (expression experiment value objects) that are on the given platform.<p>A <code>404 error</code> if the given identifier does not map to any object.</p></p>
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
#' Retrieves the composite sequences (elements) for the given platform
#'
#' @param platform <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the platform ID or its short name (e.g: <code>GPL1355</code>)</p><p>Retrieval by ID is more efficient. </p><p>Only platforms that user has access to will be available.</p>
#' @param offset <p class='description-frow'>Optional, defaults to <code>0</code>.</p><p>Skips the specified amount of objects when retrieving them from the database.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of elements (composite sequence value objects) of the given platform.</p><p>   Empty collection, if no elements matched the <code>elements</code> parameter.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p></p>
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
#' Retrieves the genes on the given platform element
#'
#' @param platform <p class='description-frow'>Required, part of the URL path.</p><p>Can either be the platform ID or its short name (e.g: <code>GPL1355</code>)</p><p>Retrieval by ID is more efficient. </p><p>Only platforms that user has access to will be available.</p>
#' @param element <p class='description-frow'>Optional, defaults to <code>empty</code>.</p><p>Limits the result to entities with given identifiers.</p><p>A list of identifiers, separated by commas (e.g: <code>AFFX_Rat_beta-actin_M_at, AFFX_Rat_Hexokinase_M_at</code>).</p><p>Can either be probes name or IDs. <br/><p>Do not combine different identifiers in one query.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   When using in scripts, remember to URL-encode any forward slashes in the probe name (see the compiled URL below).</p>
#' @param offset <p class='description-frow'>Optional, defaults to <code>0</code>.</p><p>Skips the specified amount of objects when retrieving them from the database.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of genes (gene value objects) aligned with the the given platform element.</p><p> All identifiers must be valid. </p><p>A <code>404 error</code> if the given identifier does not map to any object.</p></p>
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
#' Retrieves all genes matching the identifiers
#'
#' @param gene <p class='description-frow'>Required, part of the URL path.</p><p>   Can either be the NCBI ID (<code>1859</code>), Ensembl ID (<code>ENSG00000157540</code>)    or official symbol (<code>DYRK1A</code>) of the gene.</p><p>NCBI ID is the most efficient (and guaranteed to be unique) identifier.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   Official symbol represents a gene homologue for a random taxon, unless used in a specific taxon (see Taxon Endpoints).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>   An array of value objects representing the objects that matched the query. </p><p>   Empty array if no objects matched.</p><p>A <code>400 error</code> if required parameters are missing.</p>
#' @export
getGenes <- function (gene = NA_character_, raw = FALSE, async = FALSE, memoised = FALSE, 
    file = NA_character_, overwrite = FALSE) 
{
    fname <- "getGenes"
    preprocessor <- processGenes
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
#' Retrieves gene evidence for the given gene
#'
#' @param gene <p class='description-frow'>Required, part of the URL path.</p><p>   Can either be the NCBI ID (<code>1859</code>), Ensembl ID (<code>ENSG00000157540</code>)    or official symbol (<code>DYRK1A</code>) of the gene.</p><p>NCBI ID is the most efficient (and guaranteed to be unique) identifier.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   Official symbol represents a gene homologue for a random taxon, unless used in a specific taxon (see Taxon Endpoints).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of gene evidence value objects for the given gene or gene homologues.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p>
#' @export
getGeneEvidence <- function (gene = NA_character_, raw = FALSE, async = FALSE, memoised = FALSE, 
    file = NA_character_, overwrite = FALSE) 
{
    fname <- "getGeneEvidence"
    preprocessor <- processGeneEvidence
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
#' Retrieves the physical location of the given gene
#'
#' @param gene <p class='description-frow'>Required, part of the URL path.</p><p>   Can either be the NCBI ID (<code>1859</code>), Ensembl ID (<code>ENSG00000157540</code>)    or official symbol (<code>DYRK1A</code>) of the gene.</p><p>NCBI ID is the most efficient (and guaranteed to be unique) identifier.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   Official symbol represents a gene homologue for a random taxon, unless used in a specific taxon (see Taxon Endpoints).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of locations (physical location value objects) of the given gene or gene homologues.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p>
#' @export
getGeneLocation <- function (gene = NA_character_, raw = FALSE, async = FALSE, memoised = FALSE, 
    file = NA_character_, overwrite = FALSE) 
{
    fname <- "getGeneLocation"
    preprocessor <- processGeneLocation
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
#' Retrieves the probes (composite sequences) with this gene
#'
#' @param gene <p class='description-frow'>Required, part of the URL path.</p><p>   Can either be the NCBI ID (<code>1859</code>), Ensembl ID (<code>ENSG00000157540</code>)    or official symbol (<code>DYRK1A</code>) of the gene.</p><p>NCBI ID is the most efficient (and guaranteed to be unique) identifier.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   Official symbol represents a gene homologue for a random taxon, unless used in a specific taxon (see Taxon Endpoints).</p>
#' @param offset <p class='description-frow'>Optional, defaults to <code>0</code>.</p><p>Skips the specified amount of objects when retrieving them from the database.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>    An array of probes (composite sequence value objects) that are mapped to this gene. Note, that it is   possible for probes to map to multiple genes.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p>
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
#' Retrieves the GO terms of the given gene
#'
#' @param gene <p class='description-frow'>Required, part of the URL path.</p><p>   Can either be the NCBI ID (<code>1859</code>), Ensembl ID (<code>ENSG00000157540</code>)    or official symbol (<code>DYRK1A</code>) of the gene.</p><p>NCBI ID is the most efficient (and guaranteed to be unique) identifier.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   Official symbol represents a gene homologue for a random taxon, unless used in a specific taxon (see Taxon Endpoints).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of GO terms (gene ontology term value objects) associated with the given gene.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p>
#' @export
getGeneGO <- function (gene = NA_character_, raw = FALSE, async = FALSE, memoised = FALSE, 
    file = NA_character_, overwrite = FALSE) 
{
    fname <- "getGeneGO"
    preprocessor <- processGO
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
#' Retrieves the coexpression of two given genes
#'
#' @param gene <p class='description-frow'>Required, part of the URL path.</p><p>   Can either be the NCBI ID (<code>1859</code>), Ensembl ID (<code>ENSG00000157540</code>)    or official symbol (<code>DYRK1A</code>) of the gene.</p><p>NCBI ID is the most efficient (and guaranteed to be unique) identifier.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   Official symbol represents a gene homologue for a random taxon, unless used in a specific taxon (see Taxon Endpoints).</p>
#' @param with <p class='description-frow'>Required, defaults to <code>empty</code>.</p><p>   Can either be the NCBI ID (<code>1859</code>), Ensembl ID (<code>ENSG00000157540</code>)    or official symbol (<code>DYRK1A</code>) of the gene.</p><p>NCBI ID is the most efficient (and guaranteed to be unique) identifier.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   Official symbol represents a gene homologue for a random taxon, unless used in a specific taxon (see Taxon Endpoints).</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param stringency <p class='description-frow'>Optional, defaults to <code>1</code>.</p><p>Sets the stringency of coexpression search.</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>    An array of gene coexpression data (coexpression meta value objects) representing the coexpression of the   two given genes.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p><p>A <code>400 error</code> if required parameters are missing.</p>
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
#' List taxa filtered by given parameters
#'
#' @param taxon <p class='description-frow'>Not required, part of the URL path.</p><p>   can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:   scientific name, common name</p><p>It is recommended to use Taxon ID for efficiency.</p><p>Please note, that not all taxa have all the possible identifiers available.</p><p>Use the 'All Taxa' endpoint to retrieve the necessary information. For convenience, below is a list of officially supported taxa: </p><table><tr><th> ID </th><th>Comm.name</th><th>Scient.name</th><th>NcbiID</th></tr><tr><td>  1 </td><td> human           </td><td> Homo sapiens              </td><td>    9606 </td></tr><tr><td>  2 </td><td> mouse           </td><td> Mus musculus              </td><td>   10090 </td></tr><tr><td>  3 </td><td> rat             </td><td> Rattus norvegicus         </td><td>   10116 </td></tr><tr><td> 11 </td><td> yeast           </td><td> Saccharomyces cerevisiae  </td><td>    4932 </td></tr><tr><td> 12 </td><td> zebrafish       </td><td> Danio rerio               </td><td>    7955 </td></tr><tr><td> 13 </td><td> fly             </td><td> Drosophila melanogaster   </td><td>    7227 </td></tr><tr><td> 14 </td><td> worm            </td><td> Caenorhabditis elegans    </td><td>    6239 </td></tr></table>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of taxa (taxon value objects) matching the given identifiers. </p><p> A <code>400 error</code> if all identifiers are invalid.</p><p> An array of all available taxa, if no identifiers were provided.</p>
#' @export
getTaxa <- function (taxon = NA_character_, raw = FALSE, async = FALSE, 
    memoised = FALSE, file = NA_character_, overwrite = FALSE) 
{
    fname <- "getTaxa"
    preprocessor <- processTaxon
    validators <- list(taxon = validateOptionalTaxon)
    endpoint <- "taxa/{encode(taxon)}"
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
#' Retrieves datasets for the given taxon.
#'
#' @param taxon <p class='description-frow'>Not required, part of the URL path.</p><p>   can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:   scientific name, common name</p><p>It is recommended to use Taxon ID for efficiency.</p><p>Please note, that not all taxa have all the possible identifiers available.</p><p>Use the 'All Taxa' endpoint to retrieve the necessary information. For convenience, below is a list of officially supported taxa: </p><table><tr><th> ID </th><th>Comm.name</th><th>Scient.name</th><th>NcbiID</th></tr><tr><td>  1 </td><td> human           </td><td> Homo sapiens              </td><td>    9606 </td></tr><tr><td>  2 </td><td> mouse           </td><td> Mus musculus              </td><td>   10090 </td></tr><tr><td>  3 </td><td> rat             </td><td> Rattus norvegicus         </td><td>   10116 </td></tr><tr><td> 11 </td><td> yeast           </td><td> Saccharomyces cerevisiae  </td><td>    4932 </td></tr><tr><td> 12 </td><td> zebrafish       </td><td> Danio rerio               </td><td>    7955 </td></tr><tr><td> 13 </td><td> fly             </td><td> Drosophila melanogaster   </td><td>    7227 </td></tr><tr><td> 14 </td><td> worm            </td><td> Caenorhabditis elegans    </td><td>    6239 </td></tr></table>
#' @param filter <p class='description-frow'>Optional, defaults to <code>empty</code>.</p><p>   Filtering can be done on any* property or nested property that the appropriate object class defines   or inherits (and that is mapped by hibernate). <span class='description-imp'>These do not correspond to the properties of the    objects returned by the API calls.</span> </p><p>Class definitions:    <ul>       <li>Datasets:            <a href='http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html'>           [javaDoc]</a>            <a href='https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java'>           [gitHub]</a>       </li>       <li>Platforms:            <a href='http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html'>           [javaDoc]</a>            <a href='https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java'>           [gitHub]</a>       </li>   </ul></p><p>   E.g: <code>curationDetails</code> or <code>curationDetails.lastTroubledEvent.date</code>.</p><p>   * Any property of a supported type. Currently supported types are:   <ul>       <li>String - property of String type, required value can be any String.</li>       <li>Number - any Number implementation. Required value must be a string parseable to the specific Number type.</li>       <li>Boolean - required value will be parsed to true only if the string matches 'true', ignoring case.</li>   </ul></p><p>Accepted operator keywords are:   <ul>       <li> '=' - equality</li>       <li> '!=' - non-equality</li>       <li> '<' - smaller than</li>       <li> '>' - larger than</li>       <li> '<=' - smaller or equal</li>       <li> '=>' - larger or equal</li>       <li> 'like' - similar string, effectively means 'contains', translates to the sql 'LIKE' operator (given value will be surrounded by % signs)</li>   </ul>   Multiple filters can be chained using <code>AND</code> and <code>OR</code> keywords.<br/>   Leave space between the keywords and the previous/next word! <br/>   E.g: <code>?filter=property1 < value1 AND property2 like value2</code></p><p>   If chained filters are mixed conjunctions and disjunctions, the query must be in conjunctive normal   form (CNF). Parentheses are not necessary - every AND keyword separates blocks of disjunctions.</p><p>Example:<br/><code>?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4</code><br/>Above query will translate to: <br/><code>(p1 = v1 OR p1 != v2) AND (p2 <= v2) AND (p3 > v3 OR p3 < v4;)</code></p><p>Breaking the CNF results in an error.</p><p>Filter <code>curationDetails.troubled</code> will be ignored if user is not an administrator.</p>
#' @param offset <p class='description-frow'>Optional, defaults to <code>0</code>.</p><p>Skips the specified amount of objects when retrieving them from the database.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param sort <p class='description-frow'>Optional, defaults to <code>+id</code>.</p><p>Sets the ordering property and direction.</p><p>   Format is <code>[+,-][property name]</code>. E.g. <code>-accession</code> will translate to descending ordering by the   'Accession' property.</p><p>   Note that this does <span class='description-imp'>not guarantee the order of the returned entities!</span> This is merely a signal to how the data should be pre-sorted before   the limit and offset are applied.</p><p>   Nested properties are also supported (recursively).<br/>   E.g: <code>+curationDetails.lastTroubledEvent.date</code></p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   When using in scripts, remember to URL-encode the '+' plus character (see the compiled URL below).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of datasets (expression experiment value objects) associated with the given taxon.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p>
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
#' Loads all phenotypes for the given taxon.
#'
#' @param taxon <p class='description-frow'>Not required, part of the URL path.</p><p>   can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:   scientific name, common name</p><p>It is recommended to use Taxon ID for efficiency.</p><p>Please note, that not all taxa have all the possible identifiers available.</p><p>Use the 'All Taxa' endpoint to retrieve the necessary information. For convenience, below is a list of officially supported taxa: </p><table><tr><th> ID </th><th>Comm.name</th><th>Scient.name</th><th>NcbiID</th></tr><tr><td>  1 </td><td> human           </td><td> Homo sapiens              </td><td>    9606 </td></tr><tr><td>  2 </td><td> mouse           </td><td> Mus musculus              </td><td>   10090 </td></tr><tr><td>  3 </td><td> rat             </td><td> Rattus norvegicus         </td><td>   10116 </td></tr><tr><td> 11 </td><td> yeast           </td><td> Saccharomyces cerevisiae  </td><td>    4932 </td></tr><tr><td> 12 </td><td> zebrafish       </td><td> Danio rerio               </td><td>    7955 </td></tr><tr><td> 13 </td><td> fly             </td><td> Drosophila melanogaster   </td><td>    7227 </td></tr><tr><td> 14 </td><td> worm            </td><td> Caenorhabditis elegans    </td><td>    6239 </td></tr></table>
#' @param editableOnly <p class='description-frow'>Optional, defaults to <code>false</code>.</p><p>Whether to only list editable objects.</p>
#' @param tree <p class='description-frow'>Optional, defaults to <code>false</code>.</p><p>Whether the returned structure should be an actual tree (nested JSON objects).</p> <p>   Default is false - the tree is flattened and the tree structure information is stored as   the values of the returned object.</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>If <code>tree = false</code>, an array of simple tree value objects.</p><p>   If <code>tree = true</code>, an array of tree characteristic value objects, that will represent root nodes of   a phenotype tree. Each node has its child nodes in the <code>children</code> array property. There will be    exactly 3 root nodes - for a disease ontology tree, human phenotype ontology tree and a mammalian phenotype ontology tree.   If there are no terms for the given taxon in any of the ontologies, the relevant root node will be null.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p>
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
#' Given a set of phenotypes, return all genes associated with them.
#'
#' @param taxon <p class='description-frow'>Not required, part of the URL path.</p><p>   can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:   scientific name, common name</p><p>It is recommended to use Taxon ID for efficiency.</p><p>Please note, that not all taxa have all the possible identifiers available.</p><p>Use the 'All Taxa' endpoint to retrieve the necessary information. For convenience, below is a list of officially supported taxa: </p><table><tr><th> ID </th><th>Comm.name</th><th>Scient.name</th><th>NcbiID</th></tr><tr><td>  1 </td><td> human           </td><td> Homo sapiens              </td><td>    9606 </td></tr><tr><td>  2 </td><td> mouse           </td><td> Mus musculus              </td><td>   10090 </td></tr><tr><td>  3 </td><td> rat             </td><td> Rattus norvegicus         </td><td>   10116 </td></tr><tr><td> 11 </td><td> yeast           </td><td> Saccharomyces cerevisiae  </td><td>    4932 </td></tr><tr><td> 12 </td><td> zebrafish       </td><td> Danio rerio               </td><td>    7955 </td></tr><tr><td> 13 </td><td> fly             </td><td> Drosophila melanogaster   </td><td>    7227 </td></tr><tr><td> 14 </td><td> worm            </td><td> Caenorhabditis elegans    </td><td>    6239 </td></tr></table>
#' @param editableOnly <p class='description-frow'>Optional, defaults to <code>false</code>.</p><p>Whether to only list editable objects.</p>
#' @param phenotypes <p class='description-frow'>Required, defaults to <code>empty</code>.</p><p>Phenotype value URIs separated by commas.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   When using in scripts, remember to URL-encode any forward slashes in the phenotype value URIs (see the compiled URL below).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of gene evidence value objects for genes associated with the given phenotypes on the given taxon.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p><p>A <code>400 error</code> if required parameters are missing.</p>
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
#' Retrieves genes matching the identifier on the given taxon.
#'
#' @param taxon <p class='description-frow'>Not required, part of the URL path.</p><p>   can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:   scientific name, common name</p><p>It is recommended to use Taxon ID for efficiency.</p><p>Please note, that not all taxa have all the possible identifiers available.</p><p>Use the 'All Taxa' endpoint to retrieve the necessary information. For convenience, below is a list of officially supported taxa: </p><table><tr><th> ID </th><th>Comm.name</th><th>Scient.name</th><th>NcbiID</th></tr><tr><td>  1 </td><td> human           </td><td> Homo sapiens              </td><td>    9606 </td></tr><tr><td>  2 </td><td> mouse           </td><td> Mus musculus              </td><td>   10090 </td></tr><tr><td>  3 </td><td> rat             </td><td> Rattus norvegicus         </td><td>   10116 </td></tr><tr><td> 11 </td><td> yeast           </td><td> Saccharomyces cerevisiae  </td><td>    4932 </td></tr><tr><td> 12 </td><td> zebrafish       </td><td> Danio rerio               </td><td>    7955 </td></tr><tr><td> 13 </td><td> fly             </td><td> Drosophila melanogaster   </td><td>    7227 </td></tr><tr><td> 14 </td><td> worm            </td><td> Caenorhabditis elegans    </td><td>    6239 </td></tr></table>
#' @param gene <p class='description-frow'>Required, part of the URL path.</p><p>   Can either be the NCBI ID (<code>1859</code>), Ensembl ID (<code>ENSG00000157540</code>)    or official symbol (<code>DYRK1A</code>) of the gene.</p><p>NCBI ID is the most efficient (and guaranteed to be unique) identifier.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   Official symbol represents a gene homologue for a random taxon, unless used in a specific taxon (see Taxon Endpoints).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of genes (gene value objects) on the given taxon, matching the given identifier.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p>
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
#' Retrieves gene evidence for the gene on the given taxon.
#'
#' @param taxon <p class='description-frow'>Not required, part of the URL path.</p><p>   can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:   scientific name, common name</p><p>It is recommended to use Taxon ID for efficiency.</p><p>Please note, that not all taxa have all the possible identifiers available.</p><p>Use the 'All Taxa' endpoint to retrieve the necessary information. For convenience, below is a list of officially supported taxa: </p><table><tr><th> ID </th><th>Comm.name</th><th>Scient.name</th><th>NcbiID</th></tr><tr><td>  1 </td><td> human           </td><td> Homo sapiens              </td><td>    9606 </td></tr><tr><td>  2 </td><td> mouse           </td><td> Mus musculus              </td><td>   10090 </td></tr><tr><td>  3 </td><td> rat             </td><td> Rattus norvegicus         </td><td>   10116 </td></tr><tr><td> 11 </td><td> yeast           </td><td> Saccharomyces cerevisiae  </td><td>    4932 </td></tr><tr><td> 12 </td><td> zebrafish       </td><td> Danio rerio               </td><td>    7955 </td></tr><tr><td> 13 </td><td> fly             </td><td> Drosophila melanogaster   </td><td>    7227 </td></tr><tr><td> 14 </td><td> worm            </td><td> Caenorhabditis elegans    </td><td>    6239 </td></tr></table>
#' @param gene <p class='description-frow'>Required, part of the URL path.</p><p>   Can either be the NCBI ID (<code>1859</code>), Ensembl ID (<code>ENSG00000157540</code>)    or official symbol (<code>DYRK1A</code>) of the gene.</p><p>NCBI ID is the most efficient (and guaranteed to be unique) identifier.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   Official symbol represents a gene homologue for a random taxon, unless used in a specific taxon (see Taxon Endpoints).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of gene evidence value objects for the given gene on the given taxon.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p>
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
#' Retrieves gene evidence for the gene on the given taxon.
#'
#' @param taxon <p class='description-frow'>Not required, part of the URL path.</p><p>   can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:   scientific name, common name</p><p>It is recommended to use Taxon ID for efficiency.</p><p>Please note, that not all taxa have all the possible identifiers available.</p><p>Use the 'All Taxa' endpoint to retrieve the necessary information. For convenience, below is a list of officially supported taxa: </p><table><tr><th> ID </th><th>Comm.name</th><th>Scient.name</th><th>NcbiID</th></tr><tr><td>  1 </td><td> human           </td><td> Homo sapiens              </td><td>    9606 </td></tr><tr><td>  2 </td><td> mouse           </td><td> Mus musculus              </td><td>   10090 </td></tr><tr><td>  3 </td><td> rat             </td><td> Rattus norvegicus         </td><td>   10116 </td></tr><tr><td> 11 </td><td> yeast           </td><td> Saccharomyces cerevisiae  </td><td>    4932 </td></tr><tr><td> 12 </td><td> zebrafish       </td><td> Danio rerio               </td><td>    7955 </td></tr><tr><td> 13 </td><td> fly             </td><td> Drosophila melanogaster   </td><td>    7227 </td></tr><tr><td> 14 </td><td> worm            </td><td> Caenorhabditis elegans    </td><td>    6239 </td></tr></table>
#' @param gene <p class='description-frow'>Required, part of the URL path.</p><p>   Can either be the NCBI ID (<code>1859</code>), Ensembl ID (<code>ENSG00000157540</code>)    or official symbol (<code>DYRK1A</code>) of the gene.</p><p>NCBI ID is the most efficient (and guaranteed to be unique) identifier.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   Official symbol represents a gene homologue for a random taxon, unless used in a specific taxon (see Taxon Endpoints).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of locations (physical location value objects) of the given gene on the given taxon.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p>
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
#' Finds genes overlapping a given region.
#'
#' @param taxon <p class='description-frow'>Not required, part of the URL path.</p><p>   can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:   scientific name, common name</p><p>It is recommended to use Taxon ID for efficiency.</p><p>Please note, that not all taxa have all the possible identifiers available.</p><p>Use the 'All Taxa' endpoint to retrieve the necessary information. For convenience, below is a list of officially supported taxa: </p><table><tr><th> ID </th><th>Comm.name</th><th>Scient.name</th><th>NcbiID</th></tr><tr><td>  1 </td><td> human           </td><td> Homo sapiens              </td><td>    9606 </td></tr><tr><td>  2 </td><td> mouse           </td><td> Mus musculus              </td><td>   10090 </td></tr><tr><td>  3 </td><td> rat             </td><td> Rattus norvegicus         </td><td>   10116 </td></tr><tr><td> 11 </td><td> yeast           </td><td> Saccharomyces cerevisiae  </td><td>    4932 </td></tr><tr><td> 12 </td><td> zebrafish       </td><td> Danio rerio               </td><td>    7955 </td></tr><tr><td> 13 </td><td> fly             </td><td> Drosophila melanogaster   </td><td>    7227 </td></tr><tr><td> 14 </td><td> worm            </td><td> Caenorhabditis elegans    </td><td>    6239 </td></tr></table>
#' @param chromosome <p class='description-frow'>Required, defaults to <code>empty</code>.</p><p>The chromosome of the query location. Eg: <code>3</code>, <code>21</code>, <code>X</code></p>
#' @param strand <p class='description-frow'>Optional, defaults to <code>+</code>.</p><p>Can either be <code>+</code> or <code>-</code>.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   This is a WIP parameter and does currently not do anything</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   When using in scripts, remember to URL-encode the '+' plus character (see the compiled URL below).</p>
#' @param start <p class='description-frow'>Required, defaults to <code>empty</code>.</p><p>Number of the start nucleotide of the desired region.</p>
#' @param size <p class='description-frow'>Required, defaults to <code>empty</code>.</p><p>Amount of nucleotides in the desired region (i.e. the length of the region).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of genes (gene value objects) that overlap the given region.</p><p>A <code>404 error</code> if the given identifier does not map to any object.</p><p>A <code>400 error</code> if required parameters are missing.</p>
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
#' Does a search for datasets containing annotations, short name or full name matching the given string
#'
#' @param taxon <p class='description-frow'>Not required, part of the URL path.</p><p>   can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers:   scientific name, common name</p><p>It is recommended to use Taxon ID for efficiency.</p><p>Please note, that not all taxa have all the possible identifiers available.</p><p>Use the 'All Taxa' endpoint to retrieve the necessary information. For convenience, below is a list of officially supported taxa: </p><table><tr><th> ID </th><th>Comm.name</th><th>Scient.name</th><th>NcbiID</th></tr><tr><td>  1 </td><td> human           </td><td> Homo sapiens              </td><td>    9606 </td></tr><tr><td>  2 </td><td> mouse           </td><td> Mus musculus              </td><td>   10090 </td></tr><tr><td>  3 </td><td> rat             </td><td> Rattus norvegicus         </td><td>   10116 </td></tr><tr><td> 11 </td><td> yeast           </td><td> Saccharomyces cerevisiae  </td><td>    4932 </td></tr><tr><td> 12 </td><td> zebrafish       </td><td> Danio rerio               </td><td>    7955 </td></tr><tr><td> 13 </td><td> fly             </td><td> Drosophila melanogaster   </td><td>    7227 </td></tr><tr><td> 14 </td><td> worm            </td><td> Caenorhabditis elegans    </td><td>    6239 </td></tr></table>
#' @param query <p class='description-frow'>Required, defaults to <code>empty</code>.</p><p>   The search query. Either plain text ('traumatic'), or an ontology term URI ('http://purl.obolibrary.org/obo/UBERON_0002048').    Datasets that contain the given string in their short of full name will also be matched ('GSE201', 'Bronchoalveolar lavage samples'.</p><p>   Can be multiple identifiers separated by commas.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   When using in scripts, remember to URL-encode any forward slashes in the phenotype value URIs (see the compiled URL below).</p>
#' @param filter <p class='description-frow'>Optional, defaults to <code>empty</code>.</p><p>   Filtering can be done on any* property or nested property that the appropriate object class defines   or inherits (and that is mapped by hibernate). <span class='description-imp'>These do not correspond to the properties of the    objects returned by the API calls.</span> </p><p>Class definitions:    <ul>       <li>Datasets:            <a href='http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/experiment/ExpressionExperiment.html'>           [javaDoc]</a>            <a href='https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/experiment/ExpressionExperiment.java'>           [gitHub]</a>       </li>       <li>Platforms:            <a href='http://gemma.msl.ubc.ca/resources/apidocs/ubic/gemma/model/expression/arrayDesign/ArrayDesign.html'>           [javaDoc]</a>            <a href='https://github.com/ppavlidis/Gemma/blob/development/gemma-core/src/main/java/ubic/gemma/model/expression/arrayDesign/ArrayDesign.java'>           [gitHub]</a>       </li>   </ul></p><p>   E.g: <code>curationDetails</code> or <code>curationDetails.lastTroubledEvent.date</code>.</p><p>   * Any property of a supported type. Currently supported types are:   <ul>       <li>String - property of String type, required value can be any String.</li>       <li>Number - any Number implementation. Required value must be a string parseable to the specific Number type.</li>       <li>Boolean - required value will be parsed to true only if the string matches 'true', ignoring case.</li>   </ul></p><p>Accepted operator keywords are:   <ul>       <li> '=' - equality</li>       <li> '!=' - non-equality</li>       <li> '<' - smaller than</li>       <li> '>' - larger than</li>       <li> '<=' - smaller or equal</li>       <li> '=>' - larger or equal</li>       <li> 'like' - similar string, effectively means 'contains', translates to the sql 'LIKE' operator (given value will be surrounded by % signs)</li>   </ul>   Multiple filters can be chained using <code>AND</code> and <code>OR</code> keywords.<br/>   Leave space between the keywords and the previous/next word! <br/>   E.g: <code>?filter=property1 < value1 AND property2 like value2</code></p><p>   If chained filters are mixed conjunctions and disjunctions, the query must be in conjunctive normal   form (CNF). Parentheses are not necessary - every AND keyword separates blocks of disjunctions.</p><p>Example:<br/><code>?filter=p1 = v1 OR p1 != v2 AND p2 <= v2 AND p3 > v3 OR p3 < v4</code><br/>Above query will translate to: <br/><code>(p1 = v1 OR p1 != v2) AND (p2 <= v2) AND (p3 > v3 OR p3 < v4;)</code></p><p>Breaking the CNF results in an error.</p><p>Filter <code>curationDetails.troubled</code> will be ignored if user is not an administrator.</p>
#' @param offset <p class='description-frow'>Optional, defaults to <code>0</code>.</p><p>Skips the specified amount of objects when retrieving them from the database.</p>
#' @param limit <p class='description-frow'>Optional, defaults to <code>20</code>.</p><p>Limits the result to specified amount of objects. Use 0 for no limit.</p>
#' @param sort <p class='description-frow'>Optional, defaults to <code>+id</code>.</p><p>Sets the ordering property and direction.</p><p>   Format is <code>[+,-][property name]</code>. E.g. <code>-accession</code> will translate to descending ordering by the   'Accession' property.</p><p>   Note that this does <span class='description-imp'>not guarantee the order of the returned entities!</span> This is merely a signal to how the data should be pre-sorted before   the limit and offset are applied.</p><p>   Nested properties are also supported (recursively).<br/>   E.g: <code>+curationDetails.lastTroubledEvent.date</code></p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   When using in scripts, remember to URL-encode the '+' plus character (see the compiled URL below).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p>    An array of datasets (expression experiment value objects) that are annotated with the given ontology terms,   or, in case of plaintext query, experiments that contain the given words (name, short name, accession, tags)</p><p>    If an ontology term URI is given, the results will also include datasets that are associated with the   descendants of the term.</p><p>   The search only only checks the annotations value, not the category (which is also an   ontology term).</p>
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

#' searchAnnotations
#' Does a search for annotations based on the given string
#'
#' @param query <p class='description-frow'>Required, defaults to <code>empty</code>.</p><p>   The search query. Either plain text ('traumatic'), or an ontology term URI ('http://purl.obolibrary.org/obo/UBERON_0002048').    Datasets that contain the given string in their short of full name will also be matched ('GSE201', 'Bronchoalveolar lavage samples'.</p><p>   Can be multiple identifiers separated by commas.</p><p class='description-imp'>   <span class='glyphicon glyphicon-th-large glyphicon-exclamation-sign'></span>   When using in scripts, remember to URL-encode any forward slashes in the phenotype value URIs (see the compiled URL below).</p>
#' @param raw <p><code>FALSE</code> to receive results as-is from Gemma, or <code>TRUE</code> to enable parsing.</p>
#' @param async <p><code>TRUE</code> to run the API query on a separate worker, or <code>FALSE</code> to run synchronously. See the <code>async</code> package for details.</p>
#' @param memoised <p>Whether or not to cache results so future requests for the same data will be faster. Use <code>forgetGemmaMemoised</code> to clear the cache.</p>
#' @param file <p>The name of a file to save the results to, or <code>NULL</code> to not write results to a file. If <code>raw == TRUE</code>, the output will be a JSON file. Otherwise, it will be a RDS file.</p>
#' @param overwrite <p>Whether or not to overwrite if a file exists at the specified filename.</p>
#'
#' @return <p> An array of annotations (annotation search result value objects) matching the given identifiers.</p><p>A <code>400 error</code> if required parameters are missing.</p>
#' @export
searchAnnotations <- function (query = NA_character_, raw = FALSE, async = FALSE, 
    memoised = FALSE, file = NA_character_, overwrite = FALSE) 
{
    fname <- "searchAnnotations"
    preprocessor <- processAnnotations
    validators <- list(query = validateQuery)
    endpoint <- "annotations/search/{encode(query)}"
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
    forget(memsearchAnnotations)
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
    forget(memsearchAnnotations)
}