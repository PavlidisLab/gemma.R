#' Gemma Cache
#' @return A memoise filesystem
#' @keywords internal
gemmaCache <- function(){
  dir <- getOption(x = "gemma.cache",
                   rappdirs::user_cache_dir(appname ='gemmaR'))

  dir.create(dir,recursive = TRUE,showWarnings = FALSE)
  memoise::cache_filesystem(dir, algo = "xxhash64", compress = FALSE)
}

