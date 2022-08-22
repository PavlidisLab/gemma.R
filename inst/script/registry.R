library(here)
library(styler)
# a cleanup is needed because the script relies on environment variables to determine what is already processed
rm(list = ls(all.names = T))
options(gemmaAPI.document = 'R/allEndpoints.R')


if (file.exists(getOption("gemmaAPI.document", "R/allEndpoints.R"))) {
    file.remove(getOption("gemmaAPI.document", "R/allEndpoints.R"))
}

devtools::load_all()
setwd(here())

source('inst/script/registry_helpers.R')

# -------------------------------
# You should define all endpoints in this file. This ensures everything is uniform
# and prevents you from rewriting boilerplate.
# To package the wrapper, just source this file after you're done making changes.
# Functions will be written to allEndpoints.R
# -------------------------------
library(magrittr)


file.create(getOption("gemmaAPI.document", "R/allEndpoints.R"))



# Documentation ----
# Load in descriptions from the JS
descriptions = new.env()
descriptions$`+` <- function(A, B) {
    paste0(A, B, collapse = "")
}
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
    },envir = descriptions)

rm(`+`,envir = descriptions)

# And endpoints from the HTML
endpoints <- httr::GET("https://gemma.msl.ubc.ca/resources/restapidocs/")$content %>%
    rawToChar() %>%
    xml2::read_html() %>%
    xml2::xml_find_all(".//endpoint")

examples <- readLines("inst/script/examples.txt") %>%
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

# load overrides, for custom documentation elements.
# should replace the examples file above eventually. currently has higher priority
# than the examples file. this change is made to allow easier overrides of every
# documentation element. -ogan
overrides = roxygen2::parse_file('inst/script/overrides.R')
names(overrides) = overrides %>% sapply(function(x){
    title = x$tags %>% purrr::map(class) %>% purrr::map_lgl(function(y){'roxy_tag_title' %in% y})
    x$tags[[which(title)]]$val
})




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
    "resultSets/{resultSet}",
    ".getResultSets",
    logname = "resultSets", roxygen = "Lists resultSets filtered and organized by given parameters.",
    isFile = TRUE, internal = TRUE,
    header = "text/tab-separated-values",
    defaults = list(
        resultSet = NA_character_
    ),
    validators = alist(
        resultSet = validateOptionalID
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
        dataset = bquote()
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
        dataset = bquote(),
        filter = FALSE
    ),
    validators = alist(
        dataset = validateID,
        filter = validateBoolean
    ),
    preprocessor = quote(processFile)
)


registerEndpoint('datasets/{dataset}/samples',
                 'getDatasetSamples',
                 logname = 'samples',
                 roxygen = "Dataset samples",
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processSamples))

registerEndpoint('datasets/{dataset}/platforms',
                 'getDatasetPlatforms',
                 logname = "platforms",
                 roxygen = "Dataset platforms",
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processPlatforms))

registerEndpoint('datasets/{dataset}/annotations',
                 'getDatasetAnnotations',
                 logname = "annotations",
                 roxygen = "Dataset annotations",
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processAnnotations))


registerEndpoint('datasets/{dataset}/design',
                 'getDatasetDesign',
                 logname = "design",
                 isFile = TRUE,
                 roxygen = "Dataset design",
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processFile))

registerEndpoint('datasets/{dataset}/analyses/differential',
                 'getDatasetDEA',
                 logname = "differential",
                 roxygen = "Dataset differential analysis details",
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processDEA))

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
        platform = bquote(),
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

registerEndpoint("platforms/{platform}/elements/{elements}?offset={offset}&limit={limit}",
    "getPlatformElements",
    logname = "elements", roxygen = "Platform elements", keyword = "platform",
    defaults = list(
        platform = bquote(),
        elements = NA_character_,
        offset = 0L,
        limit = 20L
    ),
    validators = alist(
        platform = validateSingleID,
        elements = validateOptionalID,
        offset = validatePositiveInteger,
        limit = validateLimit
    ),
    preprocessor = quote(processElements)
)

registerEndpoint("platforms/{platform}/elements/{element}/genes?offset={offset}&limit={limit}",
    "getPlatformElementGenes",
    logname = "genes", roxygen = "Platform element genes", keyword = "platform",
    defaults = list(
        platform = bquote(),
        element = bquote(),
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
registerEndpoint('genes/{(genes)}/',
                 'getGenesInfo',
                 logname = "differential",
                 roxygen = "Genes",
                 keyword = 'gene',
                 defaults = list(
                     genes = bquote()
                 ),
                 validators = alist(genes = validateID),
                 preprocessor = quote(processGenes))

registerEndpoint('genes/{gene}/locations',
                 'getGeneLocation',
                 logname = "locations",
                 roxygen = "Gene locations",
                 keyword = 'gene',
                 defaults = list(
                     gene = bquote()
                 ),
                 validators = alist(gene = validateSingleID),
                 preprocessor = quote(processGeneLocation))


registerEndpoint("genes/{gene}/probes?offset={offset}&limit={limit}",
    "getGeneProbes",
    logname = "probes", roxygen = "Gene probes", keyword = "gene",
    defaults = list(
        gene = bquote(),
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


registerEndpoint('genes/{gene}/goTerms',
                 'getGeneGO',
                 logname = "goTerms",
                 roxygen = "Gene goTerms",
                 keyword = 'gene',
                 defaults = list(
                     gene = bquote()
                 ),
                 validators = alist(gene = validateSingleID),
                 preprocessor = quote(processGO))


registerEndpoint("annotations/{taxon}/search/{query}/datasets?&offset={offset}&limit={limit}&sort={sort}",
    "searchDatasets",
    logname = "datasets", roxygen = "Dataset search", keyword = "dataset",
    defaults = list(
        query = bquote(),
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
    defaults = list(query = bquote()),
    validators = alist(query = validateQuery),
    preprocessor = quote(processSearchAnnotations)
)

# taxon endpoints --------------

registerEndpoint("taxa/{taxa}",
                 "getTaxonInfo",
                 roxygen = "Taxon search",
                 keyword = "taxon",
                 defaults = list(taxa = bquote()),
                 validators = alist(taxa = validateTaxa),
                 preprocessor = quote(processTaxon)
)

registerEndpoint("taxa/{taxon}/datasets/?offset={offset}&limit={limit}&sort={sort}",
                 "getTaxonDatasets",
                 roxygen = "Taxon dataset search",
                 keyword = "taxon",
                 defaults = list(taxon = bquote(),
                                 offset = 0L,
                                 limit = 20,
                                 sort = "+id"),
                 validators = alist(taxon = validateTaxon,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    sort = validateSort),
                 preprocessor = quote(processDatasets)
)


# Clean up -----------
doFinalize <- function(document = getOption("gemmaAPI.document", "R/allEndpoints.R")) {
    cat("\n", file = document, append = TRUE)
    cat(glue::glue("#' Clear gemma.R cache\n\n"), file = document, append = TRUE)
    cat("#'\n", file = document, append = TRUE)
    cat("#' Forget past results from memoised calls to the Gemma API (ie. using functions with memoised = `TRUE`)\n#'\n", file = document, append = TRUE)
    cat("#' @return TRUE to indicate cache was cleared.\n", file = document, append = TRUE)
    cat("#' @examples\n#' forgetGemmaMemoised()\n", file = document, append = TRUE)
    cat("#' @export\n#'\n#' @keywords misc\n", file = document, append = TRUE)
    cat("forgetGemmaMemoised <- ", file = document, append = TRUE)
    cat("forgetGemmaMemoised <- function(){mem = memoise::memoise(function(){},cache = gemmaCache());memoise::forget(mem)}", file = document, append = TRUE)

    rm(list = ls(envir = globalenv(), all.names = TRUE), envir = globalenv())

    styler::style_file("./R/allEndpoints.R", transformers = biocthis::bioc_style())
    devtools::document()
    devtools::build(vignettes = FALSE)
}

doFinalize()
