#' Create a new ResponseDataObjectExpressionAnalysisResultSetValueObject
#'
#' @description
#' ResponseDataObjectExpressionAnalysisResultSetValueObject Class
#'
#' @docType class
#' @title ResponseDataObjectExpressionAnalysisResultSetValueObject
#' @description ResponseDataObjectExpressionAnalysisResultSetValueObject Class
#' @format An \code{R6Class} generator object
#' @field data  \link{ExpressionAnalysisResultSetValueObject} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ResponseDataObjectExpressionAnalysisResultSetValueObject <- R6::R6Class(
  "ResponseDataObjectExpressionAnalysisResultSetValueObject",
  public = list(
    `data` = NULL,
    #' Initialize a new ResponseDataObjectExpressionAnalysisResultSetValueObject class.
    #'
    #' @description
    #' Initialize a new ResponseDataObjectExpressionAnalysisResultSetValueObject class.
    #'
    #' @param data data
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `data` = NULL, ...
    ) {
      if (!is.null(`data`)) {
        stopifnot(R6::is.R6(`data`))
        self$`data` <- `data`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ResponseDataObjectExpressionAnalysisResultSetValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ResponseDataObjectExpressionAnalysisResultSetValueObjectObject <- list()
      if (!is.null(self$`data`)) {
        ResponseDataObjectExpressionAnalysisResultSetValueObjectObject[["data"]] <-
          self$`data`$toJSON()
      }

      ResponseDataObjectExpressionAnalysisResultSetValueObjectObject
    },
    #' Deserialize JSON string into an instance of ResponseDataObjectExpressionAnalysisResultSetValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ResponseDataObjectExpressionAnalysisResultSetValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ResponseDataObjectExpressionAnalysisResultSetValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`data`)) {
        data_object <- ExpressionAnalysisResultSetValueObject$new()
        data_object$fromJSON(jsonlite::toJSON(this_object$data, auto_unbox = TRUE, digits = NA))
        self$`data` <- data_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ResponseDataObjectExpressionAnalysisResultSetValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`data`)) {
          sprintf(
          '"data":
          %s
          ',
          jsonlite::toJSON(self$`data`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ResponseDataObjectExpressionAnalysisResultSetValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ResponseDataObjectExpressionAnalysisResultSetValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ResponseDataObjectExpressionAnalysisResultSetValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`data` <- ExpressionAnalysisResultSetValueObject$new()$fromJSON(jsonlite::toJSON(this_object$data, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to ResponseDataObjectExpressionAnalysisResultSetValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ResponseDataObjectExpressionAnalysisResultSetValueObject and throw an exception if invalid
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
    #' @return String representation of ResponseDataObjectExpressionAnalysisResultSetValueObject
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
ResponseDataObjectExpressionAnalysisResultSetValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ResponseDataObjectExpressionAnalysisResultSetValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ResponseDataObjectExpressionAnalysisResultSetValueObject$lock()

