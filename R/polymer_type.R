#' Create a new PolymerType
#'
#' @description
#' PolymerType Class
#'
#' @docType class
#' @title PolymerType
#' @description PolymerType Class
#' @format An \code{R6Class} generator object
#' @field value  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
PolymerType <- R6::R6Class(
  "PolymerType",
  public = list(
    `value` = NULL,
    #' Initialize a new PolymerType class.
    #'
    #' @description
    #' Initialize a new PolymerType class.
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
    #' @return PolymerType in JSON format
    #' @keywords internal
    toJSON = function() {
      PolymerTypeObject <- list()
      if (!is.null(self$`value`)) {
        PolymerTypeObject[["value"]] <-
          self$`value`
      }

      PolymerTypeObject
    },
    #' Deserialize JSON string into an instance of PolymerType
    #'
    #' @description
    #' Deserialize JSON string into an instance of PolymerType
    #'
    #' @param input_json the JSON input
    #' @return the instance of PolymerType
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
    #' @return PolymerType in JSON format
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
    #' Deserialize JSON string into an instance of PolymerType
    #'
    #' @description
    #' Deserialize JSON string into an instance of PolymerType
    #'
    #' @param input_json the JSON input
    #' @return the instance of PolymerType
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`value` <- this_object$`value`
      self
    },
    #' Validate JSON input with respect to PolymerType
    #'
    #' @description
    #' Validate JSON input with respect to PolymerType and throw an exception if invalid
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
    #' @return String representation of PolymerType
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
PolymerType$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
PolymerType$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
PolymerType$lock()

