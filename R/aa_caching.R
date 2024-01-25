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

#' Enable and disable memoisation of gemma.R functions
#' @param memoised boolean. If TRUE memoisation will be enabled
#' @return Option set for memoisation
#' @keywords internal
gemma_memoise <- function(memoised = NULL){
    options('gemma.memoised' = setting)
    getOption('gemma.memoised',FALSE)
}

mem_in_memory_cache <- memoise::memoise(in_memory_cache, cache = memoise::cache_memory())
