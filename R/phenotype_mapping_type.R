#' Create a new PhenotypeMappingType
#'
#' @description
#' PhenotypeMappingType Class
#'
#' @docType class
#' @title PhenotypeMappingType
#' @description PhenotypeMappingType Class
#' @format An \code{R6Class} generator object
#' @field value  character [optional]
#' @field mutable  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
PhenotypeMappingType <- R6::R6Class(
  "PhenotypeMappingType",
  public = list(
    `value` = NULL,
    `mutable` = NULL,
    #' Initialize a new PhenotypeMappingType class.
    #'
    #' @description
    #' Initialize a new PhenotypeMappingType class.
    #'
    #' @param value value
    #' @param mutable mutable
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `value` = NULL, `mutable` = NULL, ...
    ) {
      if (!is.null(`value`)) {
        stopifnot(is.character(`value`), length(`value`) == 1)
        self$`value` <- `value`
      }
      if (!is.null(`mutable`)) {
        stopifnot(is.logical(`mutable`), length(`mutable`) == 1)
        self$`mutable` <- `mutable`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PhenotypeMappingType in JSON format
    #' @keywords internal
    toJSON = function() {
      PhenotypeMappingTypeObject <- list()
      if (!is.null(self$`value`)) {
        PhenotypeMappingTypeObject[["value"]] <-
          self$`value`
      }
      if (!is.null(self$`mutable`)) {
        PhenotypeMappingTypeObject[["mutable"]] <-
          self$`mutable`
      }

      PhenotypeMappingTypeObject
    },
    #' Deserialize JSON string into an instance of PhenotypeMappingType
    #'
    #' @description
    #' Deserialize JSON string into an instance of PhenotypeMappingType
    #'
    #' @param input_json the JSON input
    #' @return the instance of PhenotypeMappingType
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`value`)) {
        self$`value` <- this_object$`value`
      }
      if (!is.null(this_object$`mutable`)) {
        self$`mutable` <- this_object$`mutable`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PhenotypeMappingType in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`value`)) {
          sprintf(
          '"value":
            "%s"
                    ',
          self$`value`
          )
        },
        if (!is.null(self$`mutable`)) {
          sprintf(
          '"mutable":
            %s
                    ',
          tolower(self$`mutable`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of PhenotypeMappingType
    #'
    #' @description
    #' Deserialize JSON string into an instance of PhenotypeMappingType
    #'
    #' @param input_json the JSON input
    #' @return the instance of PhenotypeMappingType
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`value` <- this_object$`value`
      self$`mutable` <- this_object$`mutable`
      self
    },
    #' Validate JSON input with respect to PhenotypeMappingType
    #'
    #' @description
    #' Validate JSON input with respect to PhenotypeMappingType and throw an exception if invalid
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
    #' @return String representation of PhenotypeMappingType
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
PhenotypeMappingType$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
PhenotypeMappingType$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
PhenotypeMappingType$lock()

