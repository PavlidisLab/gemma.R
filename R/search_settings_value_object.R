#' Create a new SearchSettingsValueObject
#'
#' @description
#' SearchSettingsValueObject Class
#'
#' @docType class
#' @title SearchSettingsValueObject
#' @description SearchSettingsValueObject Class
#' @format An \code{R6Class} generator object
#' @field query  character [optional]
#' @field resultTypes  list(character) [optional]
#' @field taxon  \link{TaxonValueObject} [optional]
#' @field platform  \link{ArrayDesignValueObject} [optional]
#' @field maxResults  integer [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
SearchSettingsValueObject <- R6::R6Class(
  "SearchSettingsValueObject",
  public = list(
    `query` = NULL,
    `resultTypes` = NULL,
    `taxon` = NULL,
    `platform` = NULL,
    `maxResults` = NULL,
    #' Initialize a new SearchSettingsValueObject class.
    #'
    #' @description
    #' Initialize a new SearchSettingsValueObject class.
    #'
    #' @param query query
    #' @param resultTypes resultTypes
    #' @param taxon taxon
    #' @param platform platform
    #' @param maxResults maxResults
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `query` = NULL, `resultTypes` = NULL, `taxon` = NULL, `platform` = NULL, `maxResults` = NULL, ...
    ) {
      if (!is.null(`query`)) {
        stopifnot(is.character(`query`), length(`query`) == 1)
        self$`query` <- `query`
      }
      if (!is.null(`resultTypes`)) {
        stopifnot(is.vector(`resultTypes`), length(`resultTypes`) != 0)
        sapply(`resultTypes`, function(x) stopifnot(is.character(x)))
        self$`resultTypes` <- `resultTypes`
      }
      if (!is.null(`taxon`)) {
        stopifnot(R6::is.R6(`taxon`))
        self$`taxon` <- `taxon`
      }
      if (!is.null(`platform`)) {
        stopifnot(R6::is.R6(`platform`))
        self$`platform` <- `platform`
      }
      if (!is.null(`maxResults`)) {
        stopifnot(is.numeric(`maxResults`), length(`maxResults`) == 1)
        self$`maxResults` <- `maxResults`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return SearchSettingsValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      SearchSettingsValueObjectObject <- list()
      if (!is.null(self$`query`)) {
        SearchSettingsValueObjectObject[["query"]] <-
          self$`query`
      }
      if (!is.null(self$`resultTypes`)) {
        SearchSettingsValueObjectObject[["resultTypes"]] <-
          self$`resultTypes`
      }
      if (!is.null(self$`taxon`)) {
        SearchSettingsValueObjectObject[["taxon"]] <-
          self$`taxon`$toJSON()
      }
      if (!is.null(self$`platform`)) {
        SearchSettingsValueObjectObject[["platform"]] <-
          self$`platform`$toJSON()
      }
      if (!is.null(self$`maxResults`)) {
        SearchSettingsValueObjectObject[["maxResults"]] <-
          self$`maxResults`
      }

      SearchSettingsValueObjectObject
    },
    #' Deserialize JSON string into an instance of SearchSettingsValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of SearchSettingsValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of SearchSettingsValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`query`)) {
        self$`query` <- this_object$`query`
      }
      if (!is.null(this_object$`resultTypes`)) {
        self$`resultTypes` <- ApiClient$new()$deserializeObj(this_object$`resultTypes`, "set[character]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`taxon`)) {
        taxon_object <- TaxonValueObject$new()
        taxon_object$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
        self$`taxon` <- taxon_object
      }
      if (!is.null(this_object$`platform`)) {
        platform_object <- ArrayDesignValueObject$new()
        platform_object$fromJSON(jsonlite::toJSON(this_object$platform, auto_unbox = TRUE, digits = NA))
        self$`platform` <- platform_object
      }
      if (!is.null(this_object$`maxResults`)) {
        self$`maxResults` <- this_object$`maxResults`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return SearchSettingsValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`query`)) {
          sprintf(
          '"query":
            "%s"
                    ',
          self$`query`
          )
        },
        if (!is.null(self$`resultTypes`)) {
          sprintf(
          '"resultTypes":
             [%s]
          ',
          paste(unlist(lapply(self$`resultTypes`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`taxon`)) {
          sprintf(
          '"taxon":
          %s
          ',
          jsonlite::toJSON(self$`taxon`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`platform`)) {
          sprintf(
          '"platform":
          %s
          ',
          jsonlite::toJSON(self$`platform`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`maxResults`)) {
          sprintf(
          '"maxResults":
            %d
                    ',
          self$`maxResults`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of SearchSettingsValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of SearchSettingsValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of SearchSettingsValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`query` <- this_object$`query`
      self$`resultTypes` <- ApiClient$new()$deserializeObj(this_object$`resultTypes`, "set[character]", loadNamespace("gemma.R"))
      self$`taxon` <- TaxonValueObject$new()$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
      self$`platform` <- ArrayDesignValueObject$new()$fromJSON(jsonlite::toJSON(this_object$platform, auto_unbox = TRUE, digits = NA))
      self$`maxResults` <- this_object$`maxResults`
      self
    },
    #' Validate JSON input with respect to SearchSettingsValueObject
    #'
    #' @description
    #' Validate JSON input with respect to SearchSettingsValueObject and throw an exception if invalid
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
    #' @return String representation of SearchSettingsValueObject
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
SearchSettingsValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
SearchSettingsValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
SearchSettingsValueObject$lock()

