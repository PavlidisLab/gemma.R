#' Retrieve processed expression data of a dataset
#' 
#' This function is deprecated in favor of \code{\link{get_dataset_processed_expression}}
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
#' @param filter This argument is ignored due to deprecation of the function
#' 
#' @export
#' @examples 
#' get_dataset_expression("GSE2018")
get_dataset_expression <- function(dataset,
                                   filter = FALSE,
                                   raw =  getOption("gemma.raw", FALSE),
                                   memoised = getOption("gemma.memoised", FALSE),
                                   file = getOption("gemma.file", NA_character_),
                                   overwrite =  getOption("gemma.overwrite", FALSE)){
    warning('get_dataset_expression is deprecated, please use get_dataset_processed_expression instead')
    
    get_dataset_processed_expression(
        dataset = dataset,
        raw = raw,
        memoised = memoised,
        file = file,
        overwrite = overwrite
    )
}


#' Retrieve the datasets for a given taxon
#'
#' This function is deprecated in favor of \code{\link{get_datasets}}
#'
#' @param taxon can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers: scientific name, common name.
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers available.
#' Use the \code{\link{get_taxa_by_ids}} function to retrieve the necessary information. For convenience, below is a list of officially supported taxa:
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
#' @param offset The offset of the first retrieved result.
#' @param limit Optional, defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param sort Order results by the given property and direction. The '+' sign indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @inherit processDatasets return
#' @export
#'
#'
#' @examples
#' get_taxon_datasets("human")
get_taxon_datasets <- function(taxon, 
                               offset = 0L,
                               limit = 20,
                               sort = "+id", 
                               raw = getOption("gemma.raw",FALSE), 
                               memoised = getOption("gemma.memoised", FALSE),
                               file = getOption("gemma.file",NA_character_),
                               overwrite = getOption("gemma.overwrite",FALSE),...){
    warning('get_taxon_datasets is deprecated. please use get_datasets instead')
    
    get_datasets(taxa = taxon,
                 offset = offset,
                 limit = limit,
                 sort = sort,
                 raw = raw,
                 memoised = memoised,
                 file = file,
                 overwrite = overwrite)
}


#' Retrieve datasets associated to an annotation tags search
#'
#' This function is deprecated in favor of \code{\link{get_datasets}}
#'
#' @param query The search query. Either plain text ('traumatic'), or an ontology term URI ('http://purl.obolibrary.org/obo/UBERON_0002048'). Datasets that contain the given string in their short
#' or full name will also be matched. Can be multiple identifiers separated by commas.
#' @param taxon Can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers: scientific name, common name.
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers available.
#' Use the \code{\link{get_taxa_by_ids}} function to retrieve the necessary information. For convenience, below is a list of officially supported taxa:
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
#' @param offset The offset of the first retrieved result.
#' @param limit Optional, defaults to 20. Limits the result to specified amount
#' of objects. Has a maximum value of 100. Use together with \code{offset} and
#' the \code{totalElements} \link[base:attributes]{attribute} in the output to
#' compile all data if needed.
#' @param sort Order results by the given property and direction. The '+' sign indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be a JSON file. Otherwise,
#' it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @param attributes If \code{TRUE} additional information from the call will be added
#' into the output object's attributes such as offset and available elements.
#'
#' @inherit processDatasets return
#' @export
#'
#'
#' @examples
#' search_datasets("bipolar", taxon = "human")
search_datasets <- function(query, taxon = NA_character_, 
                            offset = 0L, 
                            limit = 20L,
                            sort = "+id",
                            raw = getOption("gemma.raw", FALSE), memoised = getOption(
                                "gemma.memoised",
                                FALSE
                            ),
                            file = getOption("gemma.file", NA_character_),
                            overwrite = getOption("gemma.overwrite", FALSE), attributes = getOption(
                                "gemma.attributes",
                                TRUE
                            ),...){
    warning("search_datasets is deprecated. please use get_datasets instead")
    
    get_datasets(query = query,
                 taxa = taxon,
                 limit = limit,
                 sort = sort,
                 offset = offset,
                 raw = raw,
                 file = file,
                 overwrite = overwrite)
}
