#' gemma.R package: Access curated gene expression data and differential expression analyses
#'
#' This package contains wrappers and convenience functions for Gemma's RESTful API
#' that enables access to curated expression and differential expression data
#' from over 15,000 published studies (as of mid-2022). Gemma (https://gemma.msl.ubc.ca) is a web site, database and a set
#' of tools for the meta-analysis, re-use and sharing of transcriptomics data,
#' currently primarily targeted at the analysis of gene expression profiles.
#'
#' Most users will want to start with the high-level functions like \code{\link{get_dataset_object}}, \code{\link{get_differential_expression_values}} and \code{\link{get_platform_annotations}}
#' Additional lower-level methods are available that directly map to the Gemma RESTful API methods.
#'
#' For more information and detailed usage instructions check the
#' \href{https://pavlidislab.github.io/gemma.R/index.html}{README}, the
#' \href{https://pavlidislab.github.io/gemma.R/reference/index.html}{function reference}
#' and the \href{https://pavlidislab.github.io/gemma.R/articles/gemma.R.html}{vignette}.
#'
#' All software-related questions should be posted to the Bioconductor Support Site:
#' \url{https://support.bioconductor.org}
#'
#' @references
#'
#' Lim, N. et al., Curation of over 10 000 transcriptomic studies to enable
#' data reuse, Database, 2021. \url{https://doi.org/10.1093/database/baab006}
#'
#' @author Javier Castillo-Arnemann, Jordan Sicherman, Ogan Mancarci, Guillaume Poirier-Morency
#'
#' @docType package
#' @name gemma.R
#'
#' @import data.table
#' @import bit64
#' @import digest
#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))


utils::globalVariables(c("platform.ID", "analysis.ID", "analysis.Threshold", "baseline.category", 
                         "baseline.categoryURI", "baseline.factorValue", "baseline.factorValueURI", 
                         "cf.Val", "cf.ValLongUri", "experiment.ID", "experiment.shortName", "factor.ID", "factorId", 
                         "factorValue", "genes.Analyzed", "id", "probes.Analyzed", "result.ID", 
                         "resultIds", "stats.DE", "stats.Down", "stats.Up", "subsetFactor.category", 
                         "subsetFactor.categoryURI", "subsetFactor.Enabled", "subsetFactor.factorValue", 
                         "subsetFactor.factorValueURI", "valueUri", "category", 
                         "categoryURI", "experimental.factorValue","value",
                         "contrast.ID","%$%",'baseline.factors',"ID","sample.ID"))
