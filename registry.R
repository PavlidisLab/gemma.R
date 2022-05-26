source('registry_helpers.R')
# -------------------------------
# You should define all endpoints in this file. This ensures everything is uniform
# and prevents you from rewriting boilerplate.
# To package the wrapper, just source this file after you're done making changes.
# Functions will be written to allEndpoints.R
# -------------------------------

library(magrittr)

if (file.exists(getOption("gemmaAPI.document", "R/allEndpoints.R"))) {
    file.remove(getOption("gemmaAPI.document", "R/allEndpoints.R"))
}

file.create(getOption("gemmaAPI.document", "R/allEndpoints.R"))



# Documentation ----
`+` <- function(A, B) {
    paste0(A, B, collapse = "")
}

# Load in descriptions from the JS
eval(httr::GET("https://gemma.msl.ubc.ca/resources/restapidocs/js/vue/descriptions.js")$content %>%
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
endpoints <- httr::GET("https://gemma.msl.ubc.ca/resources/restapidocs/")$content %>%
    rawToChar() %>%
    xml2::read_html() %>%
    xml2::xml_find_all(".//endpoint")

examples <- readLines("examples.txt") %>%
    {
        gsub("# ", "", .)
    }
mExamples <- list(example = list(), value = list())
n <- 1
mTitle <- NULL
for (i in examples) {
    if (i == "") {
        mTitle <- NULL
    } else if (i %in% c("example", "value")) {
        n <- i
    } else {
        if (is.null(mTitle)) {
            mTitle <- i
        } else {
            mExamples[[n]][[mTitle]] <- c(mExamples[[n]][[mTitle]], i)
        }
    }
}


# Dataset endpoints ----
registerEndpoint("datasets/{datasets}?&offset={offset}&limit={limit}&sort={sort}",
    "getDatasetsInfo",
    logname = "datasets", roxygen = "Datasets", keyword = "dataset",
    defaults = list(
        datasets = NA_character_,
        offset = 0L,
        limit = 20L,
        sort = "+id"
    ),
    validators = alist(
        datasets = validateOptionalID,
        offset = validatePositiveInteger,
        limit = validateLimit,
        sort = validateSort
    ),
    preprocessor = quote(processDatasets)
)

registerEndpoint(
    "resultSets/{resultSet}?&offset={offset}&limit={limit}&sort={sort}",
    ".getResultSets",
    logname = "resultSets", roxygen = "Lists resultSets filtered and organized by given parameters.",
    isFile = TRUE, internal = TRUE,
    header = "text/tab-separated-values",
    defaults = list(
        resultSet = NA_character_,
        dataset = NA_character_,
        offset = 0L,
        limit = 20L,
        sort = "+id"
    ),
    validators = alist(
        resultSet = validateOptionalID,
        dataset = validateOptionalID,
        offset = validatePositiveInteger,
        limit = validateLimit,
        sort = validateSort
    ),
    preprocessor = quote(processFile)
)

registerEndpoint(
    "resultSets/{resultSet}?&offset={offset}&limit={limit}&sort={sort}&excludeResults={excludeResults}",
    ".getResultSetFactors",
    logname = "resultSetFactors",
    roxygen = "Returns the factor values for the queried resultSet.",
    internal = TRUE,
    defaults = list(
        resultSet = NA_character_,
        dataset = NA_character_,
        offset = 0L,
        limit = 20L,
        sort = "+id",
        excludeResults = TRUE
    ),
    validators = alist(
        resultSet = validateOptionalID,
        dataset = validateOptionalID,
        offset = validatePositiveInteger,
        limit = validateLimit,
        sort = validateSort
    ),
    preprocessor = quote(processResultSetFactors)
)

registerEndpoint(
    "resultSets?datasets={dataset}",
    "getDatasetResultSets",
    logname = "datasetResultSets",
    roxygen = "Lists the available resultSets for the queried dataset.",
    keyword = "dataset",
    defaults = list(
        dataset = NA_character_
    ),
    validators = alist(
        dataset = validateID
    ),
    preprocessor = quote(processDatasetResultSets)
)

registerEndpoint("datasets/{dataset}/data?filter={filter}",
    "getDatasetExpression",
    logname = "data", roxygen = "Dataset expression", keyword = "dataset",
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

registerSimpleEndpoint("dataset", "analyses/differential",
    logname = "differential", roxygen = "Dataset differential analysis",
    "getDatasetDEA",
    preprocessor = quote(processDEA)
)

# Platform endpoints ----
registerEndpoint("platforms/{platforms}?&offset={offset}&limit={limit}&sort={sort}",
    "getPlatformsInfo",
    logname = "platforms", roxygen = "Platforms", keyword = "platform",
    defaults = list(
        platforms = NA_character_,
        offset = 0L,
        limit = 20L,
        sort = "+id"
    ),
    validators = alist(
        platforms = validateOptionalID,
        offset = validatePositiveInteger,
        limit = validateLimit,
        sort = validateSort
    ),
    preprocessor = quote(processPlatforms)
)

registerEndpoint("platforms/{platform}/datasets?offset={offset}&limit={limit}",
    "getPlatformDatasets",
    logname = "datasets", roxygen = "Platform datasets", keyword = "platform",
    defaults = list(
        platform = NA_character_,
        offset = 0L,
        limit = 20L
    ),
    validators = alist(
        platform = validateSingleID,
        offset = validatePositiveInteger,
        limit = validateLimit
    ),
    preprocessor = quote(processDatasets)
)

registerEndpoint("platforms/{platform}/elements/{element}?offset={offset}&limit={limit}",
    "getPlatformElements",
    logname = "elements", roxygen = "Platform elements", keyword = "platform",
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
        limit = validateLimit
    ),
    preprocessor = quote(processElements)
)

registerEndpoint("platforms/{platform}/elements/{element}/genes?offset={offset}&limit={limit}",
    "getPlatformElementGenes",
    logname = "genes", roxygen = "Platform element genes", keyword = "platform",
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
        limit = validateLimit
    ),
    preprocessor = quote(processGenes)
)

# Gene endpoints ----
registerSimpleEndpoint("genes", "",
    logname = "genes", roxygen = "Genes", keyword = "gene",
    "getGenesInfo",
    validator = alist(genes = validateID),
    preprocessor = quote(processGenes)
)

registerSimpleEndpoint("gene", "locations",
    logname = "locations", roxygen = "Gene locations",
    "getGeneLocation",
    preprocessor = quote(processGeneLocation)
)

registerEndpoint("genes/{gene}/probes?offset={offset}&limit={limit}",
    "getGeneProbes",
    logname = "probes", roxygen = "Gene probes", keyword = "gene",
    defaults = list(
        gene = NA_character_,
        offset = 0L,
        limit = 20L
    ),
    validators = alist(
        gene = validateSingleID,
        offset = validatePositiveInteger,
        limit = validateLimit
    ),
    preprocessor = quote(processElements)
)

registerSimpleEndpoint("gene", "goTerms",
    logname = "goTerms", roxygen = "Gene goTerms",
    "getGeneGO",
    preprocessor = quote(processGO)
)

registerEndpoint("annotations/{taxon}/search/{query}/datasets?&offset={offset}&limit={limit}&sort={sort}",
    "searchDatasets",
    logname = "datasets", roxygen = "Dataset search", keyword = "dataset",
    defaults = list(
        query = NA_character_,
        taxon = NA_character_,
        offset = 0L,
        limit = 20L,
        sort = "+id"
    ),
    validators = alist(
        query = validateQuery,
        taxon = validateOptionalTaxon,
        limit = validateLimit,
        sort = validateSort
    ),
    preprocessor = quote(processDatasets)
)

registerEndpoint("annotations/search/{query}",
    "searchAnnotations",
    roxygen = "Annotation search",
    keyword = "misc",
    defaults = list(query = NA_character_),
    validators = alist(query = validateQuery),
    preprocessor = quote(processSearchAnnotations)
)

# Clean up
doFinalize <- function(document = getOption("gemmaAPI.document", "R/allEndpoints.R")) {
    cat("\n", file = document, append = TRUE)
    cat(glue::glue("#' Clear gemma.R cache\n\n"), file = document, append = TRUE)
    cat("#'\n", file = document, append = TRUE)
    cat("#' Forget past results from memoised calls to the Gemma API (ie. using functions with memoised = `TRUE`)\n#'\n", file = document, append = TRUE)
    cat("#' @return TRUE to indicate cache was cleared.\n", file = document, append = TRUE)
    cat("#' @examples\n#' forgetGemmaMemoised()\n", file = document, append = TRUE)
    cat("#' @export\n#'\n#' @keywords misc\n", file = document, append = TRUE)
    cat("forgetGemmaMemoised <- ", file = document, append = TRUE)
    cat(deparse(get("forgetGemmaMemoised", envir = globalenv(), inherits = FALSE)) %>% paste0(collapse = "\n"), file = document, append = TRUE)

    rm(list = ls(envir = globalenv(), all.names = TRUE), envir = globalenv())

    styler::style_file("./R/allEndpoints.R", transformers = biocthis::bioc_style())
    devtools::document()
    devtools::build(vignettes = FALSE)
}

doFinalize()
