# use this file to override automatically generated documentation elements.
# supported elements are title, description, examples, parameters and the return value.
# add a NULL at the end of the documentation block to allow roxygen to parse

#' get_datasets_by_ids
#'
#' @param datasets Numerical dataset identifiers or dataset short names. If not
#' specified, all datasets will be returned instead
#' @examples
#' get_datasets_by_ids("GSE2018")
#' get_datasets_by_ids(c("GSE2018", "GSE2872"))
#' @inherit processDatasets return
NULL


#' get_result_sets
#'
#' @inherit processDatasetResultSets return
#'
#' @examples
#' resultSets <- get_result_sets('GSE2872')
#' get_differential_expression_values(resultSet = resultSets$resultSet.id[1])
NULL

#' get_dataset_expression
#' @param filter The filtered version (`filter = TRUE`) corresponds to what is
#' used in most Gemma analyses, removing some probes/elements. Unfiltered
#' includes all elements.
#' @return If raw is FALSE (default), a data table of the expression matrix for
#' the queried dataset. If raw is TRUE, returns the binary file in raw form.
#' @examples
#' get_dataset_expression("GSE2018")
NULL

#' get_dataset_samples
#'
#' @inherit processSamples return
#'
#' @examples
#' head(get_dataset_samples("GSE2018"))
NULL

#' get_dataset_platforms
#'
#' @inherit processPlatforms return
#'
#' @examples
#' get_dataset_platforms("GSE2018")
NULL

#' get_dataset_annotations
#'
#' @examples
#' get_dataset_annotations("GSE2018")
#' @inherit processAnnotations return
NULL

#' get_dataset_design
#'
#' @examples
#' head(get_dataset_design("GSE2018"))
#' @return A data table of the design matrix for the queried dataset.
#' A \code{404 error} if the given identifier does not map to any object
NULL

#' get_dataset_differential_expression_analyses
#'
#' @inherit processDEA return
#'
#' @examples
#' result = get_dataset_differential_expression_analyses("GSE2872")
#' get_differential_expression_values(resultSet = result$result.ID[1])
NULL

#' get_platforms_by_ids
#' @param platforms Platform numerical identifiers or platform short names.  If not
#' specified, all platforms will be returned instead
#'
#' @inherit processPlatforms return
#'
#' @examples
#' get_platforms_by_ids("GPL1355")
#' get_platforms_by_ids(c("GPL1355", "GPL96"))
NULL

#' get_platform_datasets
#'
#' @examples
#' head(get_platform_datasets("GPL1355"))
#'
#' @inherit processDatasets return
#'
NULL


#' get_platform_element
#' @param probes Limits the result to entities with given identifiers. A vector of identifiers (e.g: AFFX_Rat_beta-actin_M_at, AFFX_Rat_Hexokinase_M_at)
#' @return A data table with information about the elements (probes or genes)
#' used by the queried platform. A \code{404 error} if the given identifier
#' does not map to any object
#'
#' @examples
#' head(get_platform_element("GPL1355"))
NULL

#' get_platform_element_genes
#' @param probe A probe name or it's numerical identifier
#'
#' @inherit processGenes return
#'
#' @examples
#' get_platform_element_genes("GPL1355", "AFFX_Rat_beta-actin_M_at")
NULL

#' get_genes
#'
#' @inherit processGenes return
#'
#' @examples
#' get_genes("DYRK1A")
#' get_genes(c("DYRK1A", "PTEN"))
NULL

#' get_gene_locations
#' @examples
#' get_gene_locations("DYRK1A")
#'
#' @inherit processGeneLocation return
#'
NULL

#' get_gene_probes
#' @examples
#' get_gene_probes("DYRK1A")
#'
#' @inherit processElements return
#'
NULL

#' get_gene_go_terms
#' @examples
#' get_gene_go_terms("DYRK1A")
#'
#' @inherit processGO return
#'
NULL

#' search_datasets
#' @param query The search query. Either plain text ('traumatic'), or an ontology term URI ('http://purl.obolibrary.org/obo/UBERON_0002048'). Datasets that contain the given string in their short
#'  or full name will also be matched. Can be multiple identifiers separated by commas.
#' @param taxon Can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers: scientific name, common name.
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
#'
#' @examples
#' search_datasets('bipolar',taxon = 'human')
NULL

#' search_annotations
#' @param query The search query
#' @examples
#' search_annotations("traumatic")
#' @inherit processSearchAnnotations return
NULL


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

#' search_gemma
#' @param query The search query. Either plain text ('traumatic'), or an ontology term URI ('http://purl.obolibrary.org/obo/UBERON_0002048'). Datasets that contain the given string in their short of full name will also be matched ('GSE201', 'Bronchoalveolar lavage samples'.
#' @param resultType The kind of results that should be included in the output. Can be experiment, gene, platform or a long object type name, documented in the API documentation.
#' @return If \code{raw = FALSE} and resultType is experiment, gene or platform,
#' a data.table containing the search results. If it is any other type, a list
#' of results. A list with additional details about the search if \code{raw = TRUE}
#' @examples
#' search_gemma('bipolar')
NULL

#' generic_params
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing `options(gemma.memoised = TRUE)` will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param attributes If `TRUE` additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#' @param raw `TRUE` to receive results as-is from Gemma, or `FALSE` to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param file The name of a file to save the results to, or `NULL` to not write
#' results to a file. If `raw == TRUE`, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param limit Optional, defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
NULL

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
