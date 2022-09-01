#' Create a new PrimitiveType
#'
#' @description
#' PrimitiveType Class
#'
#' @docType class
#' @title PrimitiveType
#' @description PrimitiveType Class
#' @format An \code{R6Class} generator object
#' @field value  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
PrimitiveType <- R6::R6Class(
  "PrimitiveType",
  public = list(
    `value` = NULL,
    #' Initialize a new PrimitiveType class.
    #'
    #' @description
    #' Initialize a new PrimitiveType class.
    #'
    #' @param value value
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `value` = NULL, ...
    ) {
      if (!is.null(`value`)) {
        stopifnot(is.character(`value`), length(`value`) == 1)
        self$`value` <- `value`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PrimitiveType in JSON format
    #' @keywords internal
    toJSON = function() {
      PrimitiveTypeObject <- list()
      if (!is.null(self$`value`)) {
        PrimitiveTypeObject[["value"]] <-
          self$`value`
      }

      PrimitiveTypeObject
    },
    #' Deserialize JSON string into an instance of PrimitiveType
    #'
    #' @description
    #' Deserialize JSON string into an instance of PrimitiveType
    #'
    #' @param input_json the JSON input
    #' @return the instance of PrimitiveType
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`value`)) {
        self$`value` <- this_object$`value`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PrimitiveType in JSON format
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of PrimitiveType
    #'
    #' @description
    #' Deserialize JSON string into an instance of PrimitiveType
    #'
    #' @param input_json the JSON input
    #' @return the instance of PrimitiveType
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`value` <- this_object$`value`
      self
    },
    #' Validate JSON input with respect to PrimitiveType
    #'
    #' @description
    #' Validate JSON input with respect to PrimitiveType and throw an exception if invalid
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
    #' @return String representation of PrimitiveType
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
PrimitiveType$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
PrimitiveType$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
PrimitiveType$lock()

