#' Gemma Cache
#'
#' @keywords internal
gemmaCache <- function(){
  dir <- getOption(x = "gemma.cache",
                   tools::R_user_dir('gemmaR', which="cache"))

  memoise::cache_filesystem(dir, algo = "xxhash64", compress = FALSE)
}

