#' Gemma Cache
#' @return A memoise filesystem
#' @keywords internal
gemmaCache <- function(){
  dir <- getOption(x = "gemma.cache",
                   rappdirs::user_cache_dir(appname ='gemmaR'))

  if(dir == "cache_in_memory"){
      return("cache_in_memory")
  }
  dir.create(dir,recursive = TRUE,showWarnings = FALSE)
  memoise::cache_filesystem(dir, algo = "xxhash64", compress = FALSE)
}

in_memory_cache <- function(...){
    args <- list(...)
    do.call(get(args[[1]]), args[-1])
}

mem_in_memory_cache <- memoise::memoise(in_memory_cache, cache = memoise::cache_memory())
