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


# load overrides, reading the roxygen docs included in this file
# for custom documentation elements.
# supported elements are title, description, details, examples, parameters and 
# the return value.
# add a NULL at the end of the documentation block to allow roxygen to parse
# generic_params is matched for all cases

overrides = roxygen2::parse_file('inst/script/registry.R',env = environment())

names(overrides) = overrides %>% sapply(function(x){
    title = x$tags %>% purrr::map(class) %>% purrr::map_lgl(function(y){'roxy_tag_title' %in% y})
    x$tags[[which(title)]]$val
})


res = httr::GET('https://gemma.msl.ubc.ca/rest/v2/openapi.json')
writeBin(res$content,con = 'inst/script/openapi.json')
api_file = jsonlite::fromJSON(readLines('inst/script/openapi.json'),simplifyVector = FALSE)

api_file_fun_names = api_file$paths %>% purrr::map('get') %>% purrr::map_chr('operationId') %>% snakecase::to_snake_case()


# /resultSets/count get_number_of_result_sets ------
# unimplemented
# we don't need this here, not included



# /resultSets/{resultSet} ----------
# this is the cheat endpoint that drops the data from result sets but it uses the same arguments
# it was made obsolete since get_result_sets works just as well

#' .getResultSetFactors
#' 
#' @examples
#' gemma.R:::.getResultSetFactors(523099)
NULL



# registerEndpoint(
#     "resultSets/{resultSet}?excludeResults=true",
#     ".getResultSetFactors", open_api_name = 'get_result_set',
#     internal = TRUE,
#     defaults = list(
#         resultSet = NA_character_
#     ),
#     validators = alist(
#         resultSet = validateOptionalID
#     ),
#     preprocessor = quote(processResultSetFactors)
# )


# /resultSets/{resultSet_}, get_result_set_as_tsv ------ 
# only exposed internally for the higher level function
# get_differential_expression_values

# considered exposing this as is but decided not to since it's name is easily
# confusable with the get_result_sets endpoint while doing drastically different
# things.


#' .getResultSets
#' 
#' @examples
#' # gemma.R:::.getResultSets(523099)
NULL

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


# /resultSets/count ------
# not implemented

# /resultSets/{resultSet} ----
# not implemented, redundant with result sets.

# /resultSets, get_result_sets -----
#' .get_result_sets
#' 
#' Returns queried result set
#' 
#' Output and usage of this function is mostly identical to \code{\link{get_dataset_differential_expression_analyses}}. 
#' The principal difference being the ability to restrict your result sets, being able to
#' query across multiple datasets and being able to use the filter argument
#' to search based on result set properties.
#' 
#'
#' @inherit processDifferentialExpressionAnalysisResultSetValueObject return
#'
#' @examples
#' get_result_sets(dataset = 1)
#' 
#' # get all contrasts comparing disease states. use filter_properties to see avaialble options
#' get_result_sets(filter = 'baselineGroup.characteristics.value = disease')
NULL


# get_result_set output is paginated awkardly, it is downgraded to an internal function
# with a high level processor with the same name
# registerEndpoint(
#     "resultSets?datasets={datasets}&filter={filter}&offset={offset}&limit={limit}&sort={sort}",
#     "get_result_sets",open_api_name = 'get_result_sets',
#     keyword = "misc",
#     compressibles = 'filter',
#     defaults = list(
#         datasets = NA_character_,
#         resultSets = NA_character_, 
#         # database entires not supported for now.. not sure if there's a good use for them
#         filter = NA_character_,
#         offset = 0,
#         limit = 20,
#         sort = '+id'
#     ),
#     validators = alist(
#         datasets = validateOptionalID,
#         resultSets = validateOptionalID,
#         filter = validateFilter,
#         offset = validatePositiveInteger,
#         limit = validateLimit,
#         sort = validateSort
#     ),
#     preprocessor = quote(processDifferentialExpressionAnalysisResultSetValueObject)
# )

registerEndpoint(
    "resultSets?datasets={datasets}&filter={filter}&offset={offset}&limit={limit}&sort={sort}",
    ".get_result_sets",open_api_name = 'get_result_sets',
    keyword = "misc",
    internal = TRUE,
    compressibles = 'filter',
    defaults = list(
        datasets = NA_character_,
        resultSets = NA_character_, 
        # database entires not supported for now.. not sure if there's a good use for them
        filter = NA_character_,
        offset = 0,
        limit = 20,
        sort = '+id'
    ),
    validators = alist(
        datasets = validateOptionalID,
        resultSets = validateOptionalID,
        filter = validateFilter,
        offset = validatePositiveInteger,
        limit = validateLimit,
        sort = validateSort
    ),
    preprocessor = quote(processDifferentialExpressionAnalysisResultSetValueObject)
)

# /annotations/search, search_annotations --------

#' search_annotations
#' @examples
#' search_annotations("traumatic")
#' @inherit processSearchAnnotations return
NULL

registerEndpoint("annotations/search?query={query}",
                 "search_annotations",
                 open_api_name = 'search_annotations',
                 keyword = "misc",
                 compressibles = 'query',
                 defaults = list(query = bquote()),
                 validators = alist(query = validateQuery),
                 preprocessor = quote(processSearchAnnotations)
)



# /datasets/{dataset}/refresh ------------
# not implemented

# /datasets/{dataset}/annotations, get_dataset_annotations ----------


#' get_dataset_annotations
#'
#' @examples
#' get_dataset_annotations("GSE2018")
#' @inherit processAnnotations return
NULL


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

# /datasets/{dataset}/design, get_dataset_design -----
# this endpoint is not very useful since the names it comes with
# is annoying to match names provided in the samples endpoint
# make_design replaces this consider removing

#' get_dataset_design
#'
#' @examples
#' head(get_dataset_design("GSE2018"))
#' @return A data table of the design matrix for the queried dataset.
#' A \code{404 error} if the given identifier does not map to any object
NULL

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


# /datasets/{datasets}/expressions/differential ------
# unimplemented
# not sure how the parameters for this endpoint works and doesn't seem essential


# /datasets/{dataset}/analyses/differential, get_dataset_differential_expression_analyses ------


#' get_dataset_differential_expression_analyses
#'
#' @inherit processDEA return
#'
#' @examples
#' result = get_dataset_differential_expression_analyses("GSE2872")
#' get_differential_expression_values(resultSet = result$result.ID[1])
NULL

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


# /datasets/{dataset}/analyses/differential/resultSets -----
# unimplemented
# unsure about the distinction between this and the get_dataset_differential_expression_analyses. 
# seem to contain the reduntant information


# /datasets/{dataset}/data -----
# deprecated and removed
# registerEndpoint("datasets/{dataset}/data?filter={filter}",
#                  "get_dataset_expression",open_api_name = 'get_dataset_expression', keyword = "dataset",
#                  isFile = TRUE,
#                  defaults = list(
#                      dataset = bquote(),
#                      filter = FALSE
#                  ),
#                  validators = alist(
#                      dataset = validateID,
#                      filter = validateBoolean
#                  ),
#                  preprocessor = quote(processFile)
# )


# /datasets/{datasets}/expressions/genes/{genes}, get_dataset_expression_for_genes ------

#' get_dataset_expression_for_genes
#' @param consolidate An option for gene expression level consolidation. If empty,
#' will return every probe for the genes. "pickmax" to
#' pick the probe with the highest expression, "pickvar" to pick the prove with
#' the highest variance and "average" for returning the average expression
#' @param keepNonSpecific logical. \code{FALSE} by default. If \code{TRUE}, results
#' from probesets that are not specific to the gene will also be returned.
#' @return A list of data frames
#' @examples
#' get_dataset_expression_for_genes('GSE2018',genes=c(10225,2841))
NULL


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



# /datasets/{datasets}/expressions/taxa/{taxa}/genes/{genes} ---------
# currently unimplemented


# datasets/{datasets}/expressions/pca -----
# unimplemented


# datasets/{dataset}/platforms ------

#' get_dataset_platforms
#'
#' @inherit processPlatforms return
#'
#' @examples
#' get_dataset_platforms("GSE2018")
NULL

registerEndpoint('datasets/{dataset}/platforms',
                 'get_dataset_platforms',
                 open_api_name = 'get_dataset_platforms',
                 keyword = 'dataset',
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = list(
                     dataset = validateSingleID
                 ),
                 preprocessor = quote(processPlatforms))




# datasets/{dataset}/data/processed ------
# this should be the main way to get the expression data now
# other one might be removed in next release

#' get_dataset_processed_expression
#' @return If raw is FALSE (default), a data table of the expression matrix for
#' the queried dataset. If raw is TRUE, returns the binary file in raw form.
#' @examples
#' get_dataset_processed_expression("GSE2018")
NULL


registerEndpoint("datasets/{dataset}/data/processed",
                 "get_dataset_processed_expression",open_api_name = 'get_dataset_processed_expression', keyword = "dataset",
                 isFile = TRUE,
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = alist(
                     dataset = validateID
                 ),
                 preprocessor = quote(processFile)
)

# datasets/{dataset}/quantitationTypes get_dataset_quantitation_types ----------

#' get_dataset_quantitation_types
#' 
#' @inherit processQuantitationTypeValueObject return
#' 
#' @examples 
#' get_dataset_quantitation_types('GSE59918')
NULL


registerEndpoint("datasets/{dataset}/quantitationTypes",
                 "get_dataset_quantitation_types",open_api_name = 'get_dataset_quantitation_types', keyword = "dataset",
                 defaults = list(
                     dataset = bquote()
                 ),
                 validators = alist(
                     dataset = validateID
                 ),
                 preprocessor = quote(processQuantitationTypeValueObject)
)



# datasets/{dataset}/data/raw, get_dataset_raw_expression ---------


#' get_dataset_raw_expression
#' 
#' @param quantitationType Quantitation type id. These can be acquired
#' using \code{\link{get_dataset_quantitation_types}} function. This endpoint can
#' only return non-processed quantitation types.
#' 
#' @return If raw is FALSE (default), a data table of the expression matrix for
#' the queried dataset. If raw is TRUE, returns the binary file in raw form.
#'  
#' @examples 
#' q_types <- get_dataset_quantitation_types('GSE59918')
#' get_dataset_raw_expression("GSE59918",q_types$id[q_types$name == 'Counts'])
NULL


registerEndpoint("datasets/{dataset}/data/raw?quantitationType={quantitationType}",
                 "get_dataset_raw_expression",open_api_name = 'get_dataset_raw_expression', keyword = "dataset",
                 isFile = TRUE,
                 defaults = list(
                     dataset = bquote(),
                     quantitationType = bquote()
                 ),
                 validators = alist(
                     dataset = validateID,
                     quantitationType = validateID
                 ),
                 preprocessor = quote(processFile)
)



# datasets/{dataset}/samples, get_dataset_samples --------

#' get_dataset_samples
#'
#' @inherit processSamples return
#'
#' @examples
#' head(get_dataset_samples("GSE2018"))
NULL

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


# datasets/{dataset}/svd --- 
# not implemented
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


# datasets/{dataset}/svd ------------
# unimplemented
# datasets, get_datasets ------

#' get_datasets
#' 
#' @inherit processDatasets return
#' 
#' @examples
#' get_datasets()
#' get_datasets(taxa = c('mouse','human'), uris = 'http://purl.obolibrary.org/obo/UBERON_0002048')
#' # filter below is equivalent to the call above
#' get_datasets(filter = "taxon.commonName in (mouse,human) and allCharacteristics.valueUri = http://purl.obolibrary.org/obo/UBERON_0002048")
#' get_datasets(query='lung')
NULL


registerEndpoint("datasets/?&offset={offset}&limit={limit}&sort={sort}&filter={filter}&query={query}",
                 "get_datasets",open_api_name = "get_datasets", keyword = "dataset",
                 defaults = list(
                     query = NA_character_,
                     filter = NA_character_,
                     taxa = NA_character_,
                     uris = NA_character_,
                     offset = 0L,
                     limit = 20L,
                     sort = "+id"
                 ),
                 compressibles = 'filter',
                 validators = alist(
                     query = validateOptionalQuery,
                     filter = validateFilter,
                     offset = validatePositiveInteger,
                     limit = validateLimit,
                     sort = validateSort
                 ),
                 preprocessor = quote(processDatasets)
)

# datasets/annotations -----
# currently unimplemented

# datasets/{datasets}, get_datasets_by_ids -----


#' get_datasets_by_ids
#'
#' @param datasets Numerical dataset identifiers or dataset short names. If not
#' specified, all datasets will be returned instead
#' 
#' @examples
#' get_datasets_by_ids("GSE2018")
#' get_datasets_by_ids(c("GSE2018", "GSE2872"))
#' @inherit processDatasets return
NULL

registerEndpoint("datasets/{datasets}?&offset={offset}&limit={limit}&sort={sort}&filter={filter}",
                 "get_datasets_by_ids",open_api_name = "get_datasets_by_ids", keyword = "dataset",
                 defaults = list(
                     datasets = NA_character_,
                     filter = NA_character_,
                     taxa = NA_character_,
                     uris = NA_character_,
                     offset = 0L,
                     limit = 20L,
                     sort = "+id"
                 ),
                 compressibles = 'filter',
                 validators = alist(
                     datasets = validateOptionalID,
                     filter = validateFilter,
                     offset = validatePositiveInteger,
                     limit = validateLimit,
                     sort = validateSort
                 ),
                 preprocessor = quote(processDatasets)
)


# datasets/categories -----
# currently unimplemented

# datasets/taxa -----
# currently unimplemented

# datasets/count -----
# currently unimplemented

# genes/{gene}/goTerms -------

#' get_gene_go_terms
#' @examples
#' get_gene_go_terms(3091)
#'
#' @inherit processGO return
#'
NULL


registerEndpoint('genes/{gene}/goTerms',
                 'get_gene_go_terms', open_api_name = 'get_gene_go_terms',
                 keyword = 'gene',
                 defaults = list(
                     gene = bquote()
                 ),
                 validators = alist(gene = validateSingleID),
                 preprocessor = quote(processGO))


# genes/{gene}/locations, get_gene_locations ----

#' get_gene_locations
#' @examples
#' get_gene_locations("DYRK1A")
#' get_gene_locations(1859)
#'
#' @inherit processGeneLocation return
#'
NULL

registerEndpoint('genes/{gene}/locations',
                 'get_gene_locations', open_api_name = 'get_gene_locations',
                 keyword = 'gene',
                 defaults = list(
                     gene = bquote()
                 ),
                 validators = alist(gene = validateSingleID),
                 preprocessor = quote(processGeneLocation))



# genes/{gene}/probes, get_gene_probes -----

#' get_gene_probes
#' @examples
#' get_gene_probes(1859)
#'
#' @inherit processElements return
#'
NULL

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

# genes/{genes}, get_genes-------

#' get_genes
#'
#' @inherit processGenes return
#'
#' @examples
#' get_genes("DYRK1A")
#' get_genes(c("DYRK1A", "PTEN"))
NULL

registerEndpoint('genes/{(genes)}/',
                 'get_genes',
                 open_api_name = 'get_genes',
                 keyword = 'gene',
                 defaults = list(
                     genes = bquote()
                 ),
                 validators = alist(genes = validateID),
                 preprocessor = quote(processGenes))



# platforms/count -----
# unimplemented

# platforms/{platform}/annotations -----
# unimplemented


# platform/{platform}/datasets, get_platform_datasets ----

#' get_platform_datasets
#'
#' @examples
#' head(get_platform_datasets("GPL1355"))
#'
#' @inherit processDatasets return
#'
NULL


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

# platforms/{platform}/elements/{probes} -----
# not implemented

# platforms/{platform}/elements/{probe}/genes, get_platform_element_genes ----

#' get_platform_element_genes
#' @param probe A probe name or it's numerical identifier
#'
#' @inherit processGenes return
#'
#' @examples
#' get_platform_element_genes("GPL1355", "AFFX_Rat_beta-actin_M_at")
NULL


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


# platforms/{platform}/elements ----
# unimplemented
# reduntant with annotation files

#' get_platform_element
#' @param probes Limits the result to entities with given identifiers. A vector of identifiers (e.g: AFFX_Rat_beta-actin_M_at, AFFX_Rat_Hexokinase_M_at)
#' @return A data table with information about the elements (probes or genes)
#' used by the queried platform. A \code{404 error} if the given identifier
#' does not map to any object
#'
#' @examples
#' head(get_platform_element("GPL1355"))
NULL


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

# platforms -----
# merged with platforms/{platform}
# this endpoint has no unique parameters of its own unlike get_datasets
# which is why it's not separated

# platforms/{platform}, get_platforms_by_ids ---- 

#' get_platforms_by_ids
#' @param platforms Platform numerical identifiers or platform short names.  If not
#' specified, all platforms will be returned instead
#' @inherit processPlatforms return
#'
#' @examples
#' get_platforms_by_ids("GPL1355")
#' get_platforms_by_ids(c("GPL1355", "GPL96"))
NULL

registerEndpoint("platforms/{platforms}?&offset={offset}&limit={limit}&sort={sort}&filter={filter}",
                 "get_platforms_by_ids",open_api_name = 'get_platforms_by_ids', keyword = "platform",
                 defaults = list(
                     platforms = NA_character_,
                     filter = NA_character_,
                     taxa = NA_character_,
                     offset = 0L,
                     limit = 20L,
                     sort = "+id"
                 ),
                 compressibles = 'filter',
                 validators = alist(
                     platforms = validateOptionalID,
                     filter = validateFilter,
                     offset = validatePositiveInteger,
                     limit = validateLimit,
                     sort = validateSort
                 ),
                 preprocessor = quote(processPlatforms)
)


# search -----

#' search_gemma
#' @param limit Defaults to 100 with a maximum value of 2000.
#'  Limits the number of returned results. Note 
#'  that this function does not support pagination.
#' @param resultType The kind of results that should be included in the output. Can be experiment, gene, platform or a long object type name, documented in the API documentation.
#' @return If \code{raw = FALSE} and resultType is experiment, gene or platform,
#' a data.table containing the search results. If it is any other type, a list
#' of results. A list with additional details about the search if \code{raw = TRUE}
#' @examples
#' search_gemma('bipolar')
NULL


registerEndpoint('search?query={query}&taxon={taxon}&platform={platform}&limit={limit}&resultTypes={resultType}',
                 'search_gemma', open_api_name = 'search',
                 keyword = 'misc',
                 defaults = list(query = bquote(),
                                 taxon = NA_character_,
                                 platform = NA_character_,
                                 limit = 100,
                                 resultType = 'experiment'),
                 validators = alist(query = validateQuery,
                                    taxon = validateOptionalTaxon,
                                    platform = validateOptionalID,
                                    limit = validatePositiveInteger,
                                    resultType = validateResultType),
                 preprocessor = quote(process_search)
)


# taxa ----
# use get_taxa in conveninence instead, unimplemented

# taxa/{taxa}, get_taxa_by_ids -----


#' get_taxa_by_ids
#' @param taxa Limits the result to entities with given identifiers.
#' A vector of identifiers.
#' Identifiers can be the any of the following:
#' -   taxon ID
#' -   scientific name
#' -   common name
#' Retrieval by ID is more efficient.
#' Do not combine different identifiers in one query.
#' For convenience, below is a list of officially supported taxa
#' \tabular{rllr}{
#'     \strong{ID} \tab \strong{Comm.name} \tab \strong{Scient.name}    \tab \strong{NcbiID}\cr
#'     1            \tab human               \tab Homo sapiens             \tab 9606            \cr
#'    2            \tab mouse               \tab Mus musculus             \tab 10090           \cr
#'    3            \tab rat                 \tab Rattus norvegicus        \tab 10116           \cr
#'    11           \tab yeast               \tab Saccharomyces cerevisiae \tab 4932            \cr
#'    12           \tab zebrafish           \tab Danio rerio              \tab 7955            \cr
#'    13           \tab fly                 \tab Drosophila melanogaster  \tab 7227            \cr
#'    14           \tab worm                \tab Caenorhabditis elegans   \tab 6239
#'}
#' @examples
#' gemma.R:::get_taxa_by_ids(c('mouse','human'))
#' @return A data table with the queried taxa's details.
NULL


registerEndpoint("taxa/{taxa}",
                 "get_taxa_by_ids",
                 open_api_name = 'get_taxa_by_ids',
                 internal = TRUE,
                 defaults = list(taxa = bquote()),
                 validators = alist(taxa = validateTaxa),
                 preprocessor = quote(processTaxon)
)

# taxa/{taxon}/datasets ----
# unimplemented, redundant with get_datasets
# below lacks the filter argument



#' get_taxon_datasets
#' @param taxon  can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers: scientific name, common name.
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers available.
#' Use the \code{\link{get_taxa_by_ids}} function to retrieve the necessary information. For convenience, below is a list of officially supported taxa:
#' \tabular{rllr}{
#'     \strong{ID} \tab \strong{Comm.name} \tab \strong{Scient.name}    \tab \strong{NcbiID}\cr
#'     1            \tab human               \tab Homo sapiens             \tab 9606            \cr
#'    2            \tab mouse               \tab Mus musculus             \tab 10090           \cr
#'    3            \tab rat                 \tab Rattus norvegicus        \tab 10116           \cr
#'    11           \tab yeast               \tab Saccharomyces cerevisiae \tab 4932            \cr
#'    12           \tab zebrafish           \tab Danio rerio              \tab 7955            \cr
#'    13           \tab fly                 \tab Drosophila melanogaster  \tab 7227            \cr
#'    14           \tab worm                \tab Caenorhabditis elegans   \tab 6239
#'}
#' @inherit processDatasets return
#' @examples
#' get_taxon_datasets('human')
NULL

# registerEndpoint("taxa/{taxon}/datasets/?offset={offset}&limit={limit}&sort={sort}",
#                  "get_taxon_datasets",open_api_name = 'get_taxon_datasets',
#                  keyword = "taxon",
#                  defaults = list(taxon = bquote(),
#                                  offset = 0L,
#                                  limit = 20,
#                                  sort = "+id"),
#                  validators = alist(taxon = validateTaxon,
#                                     offset = validatePositiveInteger,
#                                     limit = validatePositiveInteger,
#                                     sort = validateSort),
#                  preprocessor = quote(processDatasets)
# )



# taxa/{taxon}/genes/{gene}/goTerms ----- 
# unimplemented

# taxa/{taxon}/genes/{gene}/locations----
# unimplemented, redundant with get_gene_locations

# taxa/{taxon}/genes/{gene}/probes --------

# taxa/{taxon}/genes/{gene} ------
# unimplemented, use get_genes with ncbi ids instead



# taxa/{taxon}/chromosomes/{chromosome}/genes -----
# unimplemented




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



#' generic_params
#' @param query The search query. Queries can include plain text or ontology 
#' terms They also support conjunctions ("alpha AND beta"), disjunctions ("alpha OR beta")
#' grouping ("(alpha OR beta) AND gamma"), prefixing ("alpha*"), wildcard characters 
#' ("BRCA?") and fuzzy matches ("alpha~").
#' @param resultSets A resultSet identifier. Note that result set identifiers
#' are not static and can change when Gemma re-runs analyses internally. Whem
#' using these as inputs, try to make sure you access a currently existing
#' result set ID by basing them on result sets returned for a particular dataset or 
#' filter used in \code{\link{get_result_sets}}
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
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param sort Order results by the given property and direction. The '+' sign
#' indicate ascending order whereas the '-' indicate descending.
#' @param limit Defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
NULL
