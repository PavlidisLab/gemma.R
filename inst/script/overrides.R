# use this file to override automatically generated documentation elements.
# supported elements are title, examples, parameters and the return value.
# add a NULL at the end of the documentation block to allow roxygen to parse

#' getDatasetsInfo
#' @examples
#' getDatasetsInfo("GSE2018")
#' getDatasetsInfo(c("GSE2018", "GSE2872"))
#' @return A data table with information about the queried dataset(s). Returns
#' an empty list if no datasets matched. A successful response may contain 'Geeq'
#' information, which aims to provide a unified metric to measure experiments by
#' the quality of their data, and their suitability for use in Gemma. You can
#' [read more about the geeq properties here](https://pavlidislab.github.io/Gemma/geeq.html)
NULL


#' getTaxonInfo
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
#' getTaxonInfo(c('mouse','human'))
#' @return A data table with the queried taxa's details.
NULL

#' getTaxonDatasets
#' @param taxon  can either be Taxon ID, Taxon NCBI ID, or one of its string identifiers: scientific name, common name.
#' It is recommended to use Taxon ID for efficiency.
#' Please note, that not all taxa have all the possible identifiers available.
#' Use the \code{\link{getTaxonInfo}} function to retrieve the necessary information. For convenience, below is a list of officially supported taxa:
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
#'  the queried taxon. A `404 error` if the given identifier does not map
#'  to any object."
#'  @examples
#'  getTaxonDatasets('human')
NULL

#' generic_params
NULL
