#' Create a new ResponseErrorObject
#'
#' @description
#' ResponseErrorObject Class
#'
#' @docType class
#' @title ResponseErrorObject
#' @description ResponseErrorObject Class
#' @format An \code{R6Class} generator object
#' @field error  \link{WellComposedErrorBody} optional
#' @field apiVersion  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ResponseErrorObject <- R6::R6Class(
  "ResponseErrorObject",
  public = list(
    `error` = NULL,
    `apiVersion` = NULL,
    #' Initialize a new ResponseErrorObject class.
    #'
    #' @description
    #' Initialize a new ResponseErrorObject class.
    #'
    #' @param error error
    #' @param apiVersion apiVersion
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `error` = NULL, `apiVersion` = NULL, ...
    ) {
      if (!is.null(`error`)) {
        stopifnot(R6::is.R6(`error`))
        self$`error` <- `error`
      }
      if (!is.null(`apiVersion`)) {
        stopifnot(is.character(`apiVersion`), length(`apiVersion`) == 1)
        self$`apiVersion` <- `apiVersion`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ResponseErrorObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ResponseErrorObjectObject <- list()
      if (!is.null(self$`error`)) {
        ResponseErrorObjectObject[["error"]] <-
          self$`error`$toJSON()
      }
      if (!is.null(self$`apiVersion`)) {
        ResponseErrorObjectObject[["apiVersion"]] <-
          self$`apiVersion`
      }

      ResponseErrorObjectObject
    },
    #' Deserialize JSON string into an instance of ResponseErrorObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ResponseErrorObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ResponseErrorObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`error`)) {
        error_object <- WellComposedErrorBody$new()
        error_object$fromJSON(jsonlite::toJSON(this_object$error, auto_unbox = TRUE, digits = NA))
        self$`error` <- error_object
      }
      if (!is.null(this_object$`apiVersion`)) {
        self$`apiVersion` <- this_object$`apiVersion`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ResponseErrorObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`error`)) {
          sprintf(
          '"error":
          %s
          ',
          jsonlite::toJSON(self$`error`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`apiVersion`)) {
          sprintf(
          '"apiVersion":
            "%s"
                    ',
          self$`apiVersion`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ResponseErrorObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ResponseErrorObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ResponseErrorObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`error` <- WellComposedErrorBody$new()$fromJSON(jsonlite::toJSON(this_object$error, auto_unbox = TRUE, digits = NA))
      self$`apiVersion` <- this_object$`apiVersion`
      self
    },
    #' Validate JSON input with respect to ResponseErrorObject
    #'
    #' @description
    #' Validate JSON input with respect to ResponseErrorObject and throw an exception if invalid
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
    #' @return String representation of ResponseErrorObject
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
ResponseErrorObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ResponseErrorObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ResponseErrorObject$lock()

