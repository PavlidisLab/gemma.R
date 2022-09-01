#' Create a new SearchResultsResponseDataObject
#'
#' @description
#' SearchResultsResponseDataObject Class
#'
#' @docType class
#' @title SearchResultsResponseDataObject
#' @description SearchResultsResponseDataObject Class
#' @format An \code{R6Class} generator object
#' @field data  list(\link{SearchResultValueObject}) [optional]
#' @field searchSettings  \link{SearchSettingsValueObject} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
SearchResultsResponseDataObject <- R6::R6Class(
  "SearchResultsResponseDataObject",
  public = list(
    `data` = NULL,
    `searchSettings` = NULL,
    #' Initialize a new SearchResultsResponseDataObject class.
    #'
    #' @description
    #' Initialize a new SearchResultsResponseDataObject class.
    #'
    #' @param data data
    #' @param searchSettings searchSettings
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `data` = NULL, `searchSettings` = NULL, ...
    ) {
      if (!is.null(`data`)) {
        stopifnot(is.vector(`data`), length(`data`) != 0)
        sapply(`data`, function(x) stopifnot(R6::is.R6(x)))
        self$`data` <- `data`
      }
      if (!is.null(`searchSettings`)) {
        stopifnot(R6::is.R6(`searchSettings`))
        self$`searchSettings` <- `searchSettings`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return SearchResultsResponseDataObject in JSON format
    #' @keywords internal
    toJSON = function() {
      SearchResultsResponseDataObjectObject <- list()
      if (!is.null(self$`data`)) {
        SearchResultsResponseDataObjectObject[["data"]] <-
          lapply(self$`data`, function(x) x$toJSON())
      }
      if (!is.null(self$`searchSettings`)) {
        SearchResultsResponseDataObjectObject[["searchSettings"]] <-
          self$`searchSettings`$toJSON()
      }

      SearchResultsResponseDataObjectObject
    },
    #' Deserialize JSON string into an instance of SearchResultsResponseDataObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of SearchResultsResponseDataObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of SearchResultsResponseDataObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`data`)) {
        self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "array[SearchResultValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`searchSettings`)) {
        searchsettings_object <- SearchSettingsValueObject$new()
        searchsettings_object$fromJSON(jsonlite::toJSON(this_object$searchSettings, auto_unbox = TRUE, digits = NA))
        self$`searchSettings` <- searchsettings_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return SearchResultsResponseDataObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`data`)) {
          sprintf(
          '"data":
          [%s]
',
          paste(sapply(self$`data`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`searchSettings`)) {
          sprintf(
          '"searchSettings":
          %s
          ',
          jsonlite::toJSON(self$`searchSettings`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of SearchResultsResponseDataObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of SearchResultsResponseDataObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of SearchResultsResponseDataObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "array[SearchResultValueObject]", loadNamespace("gemma.R"))
      self$`searchSettings` <- SearchSettingsValueObject$new()$fromJSON(jsonlite::toJSON(this_object$searchSettings, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to SearchResultsResponseDataObject
    #'
    #' @description
    #' Validate JSON input with respect to SearchResultsResponseDataObject and throw an exception if invalid
    #'
    #' @param input the JSON input
    #' @keywords internal
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },
    #' To string (JSON format)
    #'
    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of SearchResultsResponseDataObject
    #' @keywords internal
    toString = function() {
      self$toJSONString()
    },
    #' Return true if the values in all fields are valid.
    #'
    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    #' @keywords internal
    isValid = function() {
      TRUE
    },
    #' Return a list of invalid fields (if any).
    #'
    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    #' @keywords internal
    getInvalidFields = function() {
      invalid_fields <- list()
      invalid_fields
    }
  ),
  # Lock the class to prevent modifications to the method or field
  lock_class = TRUE
)

# Unlock the class to allow modifications of the method or field
SearchResultsResponseDataObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
SearchResultsResponseDataObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
SearchResultsResponseDataObject$lock()

