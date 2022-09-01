#' Create a new DatabaseType
#'
#' @description
#' DatabaseType Class
#'
#' @docType class
#' @title DatabaseType
#' @description DatabaseType Class
#' @format An \code{R6Class} generator object
#' @field value  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
DatabaseType <- R6::R6Class(
  "DatabaseType",
  public = list(
    `value` = NULL,
    #' Initialize a new DatabaseType class.
    #'
    #' @description
    #' Initialize a new DatabaseType class.
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
    #' @return DatabaseType in JSON format
    #' @keywords internal
    toJSON = function() {
      DatabaseTypeObject <- list()
      if (!is.null(self$`value`)) {
        DatabaseTypeObject[["value"]] <-
          self$`value`
      }

      DatabaseTypeObject
    },
    #' Deserialize JSON string into an instance of DatabaseType
    #'
    #' @description
    #' Deserialize JSON string into an instance of DatabaseType
    #'
    #' @param input_json the JSON input
    #' @return the instance of DatabaseType
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
    #' @return DatabaseType in JSON format
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
    #' Deserialize JSON string into an instance of DatabaseType
    #'
    #' @description
    #' Deserialize JSON string into an instance of DatabaseType
    #'
    #' @param input_json the JSON input
    #' @return the instance of DatabaseType
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`value` <- this_object$`value`
      self
    },
    #' Validate JSON input with respect to DatabaseType
    #'
    #' @description
    #' Validate JSON input with respect to DatabaseType and throw an exception if invalid
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
    #' @return String representation of DatabaseType
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
DatabaseType$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
DatabaseType$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
DatabaseType$lock()

