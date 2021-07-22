library(testthat)
library(gemmaAPI)
library(dplyr)

# Prevent certificate issues for GitHub actions
custom_curl_options <- function(options) {
    getopt <- function(nm) {
        if (!is.null(v <- options[[nm]])) {
            return(v)
        }
        anm <- paste0("async_http_", nm)
        if (!is.null(v <- getOption(anm))) {
            return(v)
        }
        if (!is.na(v <- Sys.getenv(toupper(anm), NA_character_))) {
            return(v)
        }
    }
    modifyList(
        options,
        list(
            ssl_verifyhost = 0,
            ssl_verifypeer = 0,
            timeout = as.integer(getopt("timeout") %||% 0),
            connecttimeout = as.integer(getopt("connecttimeout") %||% 300),
            low_speed_time = as.integer(getopt("low_speed_time") %||% 0),
            low_speed_limit = as.integer(getopt("low_speed_limit") %||% 0)
        )
    )
}
# environment(custom_curl_options) <- asNamespace('async')
# assignInNamespace('get_default_curl_options', custom_curl_options, ns = 'async')

test_check("gemmaAPI")
