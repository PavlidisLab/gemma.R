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
