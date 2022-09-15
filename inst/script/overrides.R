# use this file to override automatically generated documentation elements.
# supported elements are title, description, examples, parameters and the return value.
# add a NULL at the end of the documentation block to allow roxygen to parse

#' get_datasets_by_ids
#' 
#' @param datasets Numerical dataset identifiers or dataset short names
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
#' resultSets <- get_result_sets('GSE2018')
#' get_differential_expression_values(resultSet = resultSets$resultSet.id)
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
#' @return A data table with information about the annotations of the queried 
#' dataset.A \code{404 error} if the given identifier does not map to any object
NULL

#' get_dataset_design
#' 
#' @examples 
#' head(get_dataset_design("GSE2018"))
#' @return A data table of the design matrix for the queried dataset. 
#' A \code{404 error} if the given identifier does not map to any object
NULL

#' get_dataset_differential_expression_analyses
#' @return A data table with information about the differential expression 
#' analysis of the queried dataset. Note that this funciton does not return 
#' differential expression values themselves. Use \code{\link{get_differential_expression_values}}
#' to get differential expression values (see examples).
#' @examples 
#' result = get_dataset_differential_expression_analyses("GSE2018")
#' get_differential_expression_values(resultSet = result$result.ID)
NULL

#' get_platforms_by_ids
#' @param platforms Platform numerical identifiers or platform short names
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
#' @return A data table with information about the datasets associated with the 
#' queried platform. A \code{404 error} if the given identifier does not map to 
#' any object
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
#' @return A data table with information about the gene(s) on the queried 
#' platform element. A \code{404 error} if the given identifier does not map to
#' any object
#' @examples
#' get_platform_element_genes("GPL1355", "AFFX_Rat_beta-actin_M_at")
NULL

#' get_genes
#' 
#' @examples
#' get_genes("DYRK1A")
#' get_genes(c("DYRK1A", "PTEN"))
#' @return A data table with information about the queried gene(s).
NULL

#' get_gene_locations
#' @examples 
#' get_gene_locations("DYRK1A")
#' @return A data table with information about the physical location of the
#' queried gene. A \code{404 error} if the given identifier does not map to any object
NULL

#' get_gene_probes
#' @examples
#' get_gene_probes("DYRK1A")
#' @return A data table with information about the physical location of the 
#' queried gene. A \code{404 error} if the given identifier does not map to any 
#' object.
NULL

#' get_gene_go_terms
#' @examples
#' get_gene_go_terms("DYRK1A")
#' @return A data table with information about the GO terms assigned to the 
#' queried gene. A \code{404 error} if the given identifier does not map to any 
#' object. Go terms were updated on June 10 2022
NULL

#' search_datasets
#' @param query The search query. Either plain text ('traumatic'), or an ontology
#'  term (UBERON_0002048). Datasets that contain the given string in their short 
#'  or full name will also be matched. Can be multiple identifiers separated by commas.
#' @return A data table with information about matching datasets. Returns an 
#' empty list if no datasets found. Lists dataset (expression experiment value 
#' objects) that are annotated with the given ontology terms, or, in case of 
#' plaintext query, experiments that contain the given words (name, short name,
#'  accession, tags) If an ontology term URI is given, the results will also 
#' include datasets that are associated with the descendants of the term. The
#' search only only checks the annotations value, not the category (which is 
#' also an ontology term).
#' 
#' @examples 
#' search_datasets('bipolar')
NULL

#' search_annotations
#' @param query The search query
#' @examples 
#'  search_annotations("traumatic")
#' @return A data table with annotations (annotation search result value objects)
#'  matching the given identifiers. A \code{400 error} if required parameters are missing.
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
#' @return "A data table with information about the datasets associated with
#' the queried taxon. A `404 error` if the given identifier does not map
#' to any object."
#' @examples
#' get_taxon_datasets('human')
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
