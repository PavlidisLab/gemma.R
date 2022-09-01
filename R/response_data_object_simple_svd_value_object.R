#' Create a new ResponseDataObjectSimpleSVDValueObject
#'
#' @description
#' ResponseDataObjectSimpleSVDValueObject Class
#'
#' @docType class
#' @title ResponseDataObjectSimpleSVDValueObject
#' @description ResponseDataObjectSimpleSVDValueObject Class
#' @format An \code{R6Class} generator object
#' @field data  \link{SimpleSVDValueObject} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ResponseDataObjectSimpleSVDValueObject <- R6::R6Class(
  "ResponseDataObjectSimpleSVDValueObject",
  public = list(
    `data` = NULL,
    #' Initialize a new ResponseDataObjectSimpleSVDValueObject class.
    #'
    #' @description
    #' Initialize a new ResponseDataObjectSimpleSVDValueObject class.
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
    #' @return ResponseDataObjectSimpleSVDValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ResponseDataObjectSimpleSVDValueObjectObject <- list()
      if (!is.null(self$`data`)) {
        ResponseDataObjectSimpleSVDValueObjectObject[["data"]] <-
          self$`data`$toJSON()
      }

      ResponseDataObjectSimpleSVDValueObjectObject
    },
    #' Deserialize JSON string into an instance of ResponseDataObjectSimpleSVDValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ResponseDataObjectSimpleSVDValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ResponseDataObjectSimpleSVDValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`data`)) {
        data_object <- SimpleSVDValueObject$new()
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
    #' @return ResponseDataObjectSimpleSVDValueObject in JSON format
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
    #' Deserialize JSON string into an instance of ResponseDataObjectSimpleSVDValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ResponseDataObjectSimpleSVDValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ResponseDataObjectSimpleSVDValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`data` <- SimpleSVDValueObject$new()$fromJSON(jsonlite::toJSON(this_object$data, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to ResponseDataObjectSimpleSVDValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ResponseDataObjectSimpleSVDValueObject and throw an exception if invalid
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
    #' @return String representation of ResponseDataObjectSimpleSVDValueObject
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
ResponseDataObjectSimpleSVDValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ResponseDataObjectSimpleSVDValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ResponseDataObjectSimpleSVDValueObject$lock()

