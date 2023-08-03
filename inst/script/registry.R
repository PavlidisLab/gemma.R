library(here)
library(styler)
# a cleanup is needed because the script relies on environment variables to
# determine what is already processed
rm(list = ls(all.names = TRUE))
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


# load overrides, for custom documentation elements.
# should replace the examples file above eventually. currently has higher priority
# than the examples file. this change is made to allow easier overrides of every
# documentation element. -ogan
overrides = roxygen2::parse_file('inst/script/overrides.R')
names(overrides) = overrides %>% sapply(function(x){
    title = x$tags %>% purrr::map(class) %>% purrr::map_lgl(function(y){'roxy_tag_title' %in% y})
    x$tags[[which(title)]]$val
})

download.file('https://dev.gemma.msl.ubc.ca/rest/v2/openapi.json',destfile = 'inst/script/openapi.json')
api_file = jsonlite::fromJSON(readLines('inst/script/openapi.json'),simplifyVector = FALSE)

api_file_fun_names = api_file$paths %>% purrr::map('get') %>% purrr::map_chr('operationId') %>% snakecase::to_snake_case()



# Dataset endpoints ----
registerEndpoint("datasets/{datasets}?&offset={offset}&limit={limit}&sort={sort}&filter={filter}",
    "get_datasets_by_ids",open_api_name = "get_datasets_by_ids", keyword = "dataset",
    defaults = list(
        datasets = NA_character_,
        filter = NA_character_,
        offset = 0L,
        limit = 20L,
        sort = "+id"
    ),
    validators = alist(
        datasets = validateOptionalID,
        filter = validateFilter,
        offset = validatePositiveInteger,
        limit = validateLimit,
        sort = validateSort
    ),
    preprocessor = quote(processDatasets)
)


registerEndpoint("datasets/?&offset={offset}&limit={limit}&sort={sort}&filter={filter}&query={query}",
                 "get_datasets",open_api_name = "get_datasets", keyword = "dataset",
                 defaults = list(
                     query = NA_character_,
                     filter = NA_character_,
                     offset = 0L,
                     limit = 20L,
                     sort = "+id"
                 ),
                 validators = alist(
                     query = validateOptionalQuery,
                     filter = validateFilter,
                     offset = validatePositiveInteger,
                     limit = validateLimit,
                     sort = validateSort
                 ),
                 preprocessor = quote(processDatasets)
)


registerEndpoint(
    "resultSets/{resultSet}",
    ".getResultSets", open_api_name = 'get_result_set',
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

# this is the cheat endpoint that drops the data from result sets but it uses the same arguments
registerEndpoint(
    "resultSets/{resultSet}?excludeResults=true",
    ".getResultSetFactors", open_api_name = 'get_result_set',
    internal = TRUE,
    defaults = list(
        resultSet = NA_character_
    ),
    validators = alist(
        resultSet = validateOptionalID
    ),
    preprocessor = quote(processResultSetFactors)
)

registerEndpoint(
    "resultSets?datasets={datasets}",
    "get_result_sets",open_api_name = 'get_result_sets',
    keyword = "internal",
    defaults = list(
        datasets = bquote()
    ),
    validators = alist(
        datasets = validateID
    ),
    preprocessor = quote(processDatasetResultSets)
)

registerEndpoint("datasets/{dataset}/data?filter={filter}",
    "get_dataset_expression",open_api_name = 'get_dataset_expression', keyword = "dataset",
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


registerEndpoint('datasets/{datasets}/expressions/genes/{genes}?keepNonSpecific={keepNonSpecific}&consolidate={consolidate}',
                 'get_dataset_expression_for_genes', open_api_name = 'get_dataset_expression_for_genes',
                 keyword = 'dataset',
                 defaults = list(
                     datasets = bquote(),
                     genes = bquote(),
                     keepNonSpecific = FALSE,
                     consolidate = NA_character_
                 ),
                 validators = list(
                     datasets = validateID,
                     genes = validateID,
                     keepNonSpecific = validateBoolean,
                     consolidate = validateConsolidate
                 ),
                 preprocessor = quote(process_dataset_gene_expression))


registerEndpoint('datasets/{dataset}/samples',
                 'get_dataset_samples', open_api_name = 'get_dataset_samples',
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processSamples))

registerEndpoint('datasets/{dataset}/platforms',
                 'get_dataset_platforms', open_api_name = 'get_dataset_platforms',
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processPlatforms))

registerEndpoint('datasets/{dataset}/annotations',
                 'get_dataset_annotations',open_api_name = 'get_dataset_annotations',
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processAnnotations))


registerEndpoint('datasets/{dataset}/design',
                 'get_dataset_design', open_api_name = 'get_dataset_design',
                 isFile = TRUE,
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processFile))

registerEndpoint('datasets/{dataset}/analyses/differential',
                 'get_dataset_differential_expression_analyses', open_api_name = 'get_dataset_differential_expression_analyses',
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processDEA))

# probably to be deprecated
registerEndpoint("annotations/{taxon}/search/datasets?query={query}&limit={limit}&offset={offset}&sort={sort}",
                 "search_datasets",
                 open_api_name = 'search_datasets',
                 keyword = "dataset",
                 defaults = list(query = bquote(),
                                 taxon = NA_character_,
                                 filter = NA_character_,
                                 offset = 0L,
                                 limit = 20L,
                                 sort = "+id"),
                 validators = alist(query = validateQuery,
                                    taxon = validateOptionalTaxon,
                                    filter = validateFilter,
                                    offset = validatePositiveInteger,
                                    limit = validateLimit,
                                    sort = validateSort),
                 preprocessor = quote(processDatasets)
)


# registerEndpoint('datasets/{dataset}/svd',
#                  'getDatasetSVD',
#                  logname = 'svd',
#                  roxygen = "Dataset singular value decomposition",
#                  keyword = 'dataset',
#                  defaults = list(dataset = bquote()),
#                  validators = list(dataset = validateSingleID),
#                  preprocessor = quote(processSVD)
#
# )


# Platform endpoints ----
registerEndpoint("platforms/{platforms}?&offset={offset}&limit={limit}&sort={sort}",
    "get_platforms_by_ids",open_api_name = 'get_platforms_by_ids', keyword = "platform",
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
    "get_platform_datasets",open_api_name = 'get_platform_datasets', keyword = "platform",
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

# reduntant with annotation files consider removing
# registerEndpoint("platforms/{platform}/elements/{elements}?offset={offset}&limit={limit}",
#     "get_platform_element", open_api_name = 'get_platform_element', keyword = "platform",
#     defaults = list(
#         platform = bquote(),
#         probes = NA_character_,
#         offset = 0L,
#         limit = 20L
#     ),
#     validators = alist(
#         platform = validateSingleID,
#         probes = validateOptionalID,
#         offset = validatePositiveInteger,
#         limit = validateLimit
#     ),
#     preprocessor = quote(processElements)
# )

registerEndpoint("platforms/{platform}/elements/{probe}/genes?offset={offset}&limit={limit}",
    "get_platform_element_genes",
    open_api_name = 'get_platform_element_genes', keyword = "platform",
    defaults = list(
        platform = bquote(),
        probe = bquote(),
        offset = 0L,
        limit = 20L
    ),
    validators = alist(
        platform = validateSingleID,
        probe = validateSingleID,
        offset = validatePositiveInteger,
        limit = validateLimit
    ),
    preprocessor = quote(processGenes)
)

# Gene endpoints ----
registerEndpoint('genes/{(genes)}/',
                 'get_genes',
                 open_api_name = 'get_genes',
                 keyword = 'gene',
                 defaults = list(
                     genes = bquote()
                 ),
                 validators = alist(genes = validateID),
                 preprocessor = quote(processGenes))

registerEndpoint('genes/{gene}/locations',
                 'get_gene_locations', open_api_name = 'get_gene_locations',
                 keyword = 'gene',
                 defaults = list(
                     gene = bquote()
                 ),
                 validators = alist(gene = validateSingleID),
                 preprocessor = quote(processGeneLocation))


registerEndpoint("genes/{gene}/probes?offset={offset}&limit={limit}",
    "get_gene_probes", open_api_name = 'get_gene_probes', keyword = "gene",
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
                 'get_gene_go_terms', open_api_name = 'get_gene_go_terms',
                 keyword = 'gene',
                 defaults = list(
                     gene = bquote()
                 ),
                 validators = alist(gene = validateSingleID),
                 preprocessor = quote(processGO))



registerEndpoint("annotations/search?query={query}",
    "search_annotations",
    open_api_name = 'search_annotations',
    keyword = "misc",
    defaults = list(query = bquote()),
    validators = alist(query = validateQuery),
    preprocessor = quote(processSearchAnnotations)
)

# taxon endpoints --------------

registerEndpoint("taxa/{taxa}",
                 "get_taxa_by_ids",
                 open_api_name = 'get_taxa_by_ids',
                 internal = TRUE,
                 defaults = list(taxa = bquote()),
                 validators = alist(taxa = validateTaxa),
                 preprocessor = quote(processTaxon)
)

registerEndpoint("taxa/{taxon}/datasets/?offset={offset}&limit={limit}&sort={sort}",
                 "get_taxon_datasets",open_api_name = 'get_taxon_datasets',
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

# search endpoint

registerEndpoint('search?query={query}&taxon={taxon}&platform={platform}&limit={limit}&resultTypes={resultType}',
                 'search_gemma', open_api_name = 'search',
                 keyword = 'misc',
                 defaults = list(query = bquote(),
                                 taxon = NA_character_,
                                 platform = NA_character_,
                                 limit = 20,
                                 resultType = 'experiment'),
                 validators = alist(query = validateQuery,
                                    taxon = validateOptionalTaxon,
                                    platform = validateOptionalID,
                                    limit = validatePositiveInteger,
                                    resultType = validateResultType),
                 preprocessor = quote(process_search)
)


# Clean up -----------
doFinalize <- function(document = getOption("gemmaAPI.document", "R/allEndpoints.R")) {
    cat("\n", file = document, append = TRUE)
    cat(glue::glue("#' Clear gemma.R cache\n\n"), file = document, append = TRUE)
    cat("#'\n", file = document, append = TRUE)
    cat("#' Forget past results from memoised calls to the Gemma API (ie. using functions with memoised = `TRUE`)\n#'\n", file = document, append = TRUE)
    cat("#' @return TRUE to indicate cache was cleared.\n", file = document, append = TRUE)
    cat("#' @examples\n#' forget_gemma_memoised()\n", file = document, append = TRUE)
    cat("#' @export\n#'\n#' @keywords misc\n", file = document, append = TRUE)
    cat("forget_gemma_memoised <- ", file = document, append = TRUE)
    cat('forget_gemma_memoised <-
    function(){
        if ("character" %in% class(gemmaCache()) && gemmaCache() == "cache_in_memory"){
            memoise::forget(mem_in_memory_cache)
        } else {
            mem = memoise::memoise(function(){},cache = gemmaCache());
            memoise::forget(mem)
        }
    }', file = document, append = TRUE)

    rm(list = ls(envir = globalenv(), all.names = TRUE), envir = globalenv())

    styler::style_file("./R/allEndpoints.R", transformers = biocthis::bioc_style())
    devtools::document()
    devtools::build(vignettes = FALSE)
}

doFinalize()
