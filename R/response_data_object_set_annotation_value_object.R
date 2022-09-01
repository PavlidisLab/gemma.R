#' Create a new ResponseDataObjectSetAnnotationValueObject
#'
#' @description
#' ResponseDataObjectSetAnnotationValueObject Class
#'
#' @docType class
#' @title ResponseDataObjectSetAnnotationValueObject
#' @description ResponseDataObjectSetAnnotationValueObject Class
#' @format An \code{R6Class} generator object
#' @field data  list(\link{AnnotationValueObject}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ResponseDataObjectSetAnnotationValueObject <- R6::R6Class(
  "ResponseDataObjectSetAnnotationValueObject",
  public = list(
    `data` = NULL,
    #' Initialize a new ResponseDataObjectSetAnnotationValueObject class.
    #'
    #' @description
    #' Initialize a new ResponseDataObjectSetAnnotationValueObject class.
    #'
    #' @param data data
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `data` = NULL, ...
    ) {
      if (!is.null(`data`)) {
        stopifnot(is.vector(`data`), length(`data`) != 0)
        sapply(`data`, function(x) stopifnot(R6::is.R6(x)))
        self$`data` <- `data`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ResponseDataObjectSetAnnotationValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ResponseDataObjectSetAnnotationValueObjectObject <- list()
      if (!is.null(self$`data`)) {
        ResponseDataObjectSetAnnotationValueObjectObject[["data"]] <-
          lapply(self$`data`, function(x) x$toJSON())
      }

      ResponseDataObjectSetAnnotationValueObjectObject
    },
    #' Deserialize JSON string into an instance of ResponseDataObjectSetAnnotationValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ResponseDataObjectSetAnnotationValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ResponseDataObjectSetAnnotationValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`data`)) {
        self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "set[AnnotationValueObject]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ResponseDataObjectSetAnnotationValueObject in JSON format
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ResponseDataObjectSetAnnotationValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ResponseDataObjectSetAnnotationValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ResponseDataObjectSetAnnotationValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "set[AnnotationValueObject]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to ResponseDataObjectSetAnnotationValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ResponseDataObjectSetAnnotationValueObject and throw an exception if invalid
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
    #' @return String representation of ResponseDataObjectSetAnnotationValueObject
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
ResponseDataObjectSetAnnotationValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ResponseDataObjectSetAnnotationValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ResponseDataObjectSetAnnotationValueObject$lock()

