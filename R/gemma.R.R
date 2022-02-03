#' gemma.R package: Access curated gene expression data
#'
#' This package containts low- and high-level wrappers for Gemma's RESTful API
#' that enable access to curated expression and differential expression data
#' from over 10,000 published studies. Gemma is a web site, database and a set
#' of tools for the meta-analysis, re-use and sharing of genomics data,
#' currently primarily targeted at the analysis of gene expression profiles.
#'
#' The endpoints are classified as follows:
#' \itemize{
#' \item Dataset endpoints: Access expression and differential expression data, and their associated properties and files.
#' \item Platform endpoints: Access microarray or sequencing platforms and their associated properties and files.
#' \item Gene endpoints: Access information about specific genes.
#' }
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
#' @author Javier Castillo-Arnemann, Jordan Sicherman, Ogan Mancarci
#'
#' @docType package
#' @name gemma.R
#'
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
