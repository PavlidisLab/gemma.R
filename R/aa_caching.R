#' Gemma Cache
#'
#' @keywords internal
gemmaCache <- function(){
  dir <- getOption(x = "gemma.cache",
                   rappdirs::user_cache_dir(appname="gemmaR"))

  memoise::cache_filesystem(dir, algo = "xxhash64", compress = FALSE)
}

