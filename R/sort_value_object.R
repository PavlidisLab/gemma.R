#' Create a new SortValueObject
#'
#' @description
#' SortValueObject Class
#'
#' @docType class
#' @title SortValueObject
#' @description SortValueObject Class
#' @format An \code{R6Class} generator object
#' @field orderBy  character [optional]
#' @field direction  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
SortValueObject <- R6::R6Class(
  "SortValueObject",
  public = list(
    `orderBy` = NULL,
    `direction` = NULL,
    #' Initialize a new SortValueObject class.
    #'
    #' @description
    #' Initialize a new SortValueObject class.
    #'
    #' @param orderBy orderBy
    #' @param direction direction
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `orderBy` = NULL, `direction` = NULL, ...
    ) {
      if (!is.null(`orderBy`)) {
        stopifnot(is.character(`orderBy`), length(`orderBy`) == 1)
        self$`orderBy` <- `orderBy`
      }
      if (!is.null(`direction`)) {
        stopifnot(is.character(`direction`), length(`direction`) == 1)
        self$`direction` <- `direction`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return SortValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      SortValueObjectObject <- list()
      if (!is.null(self$`orderBy`)) {
        SortValueObjectObject[["orderBy"]] <-
          self$`orderBy`
      }
      if (!is.null(self$`direction`)) {
        SortValueObjectObject[["direction"]] <-
          self$`direction`
      }

      SortValueObjectObject
    },
    #' Deserialize JSON string into an instance of SortValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of SortValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of SortValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`orderBy`)) {
        self$`orderBy` <- this_object$`orderBy`
      }
      if (!is.null(this_object$`direction`)) {
        self$`direction` <- this_object$`direction`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return SortValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`orderBy`)) {
          sprintf(
          '"orderBy":
            "%s"
                    ',
          self$`orderBy`
          )
        },
        if (!is.null(self$`direction`)) {
          sprintf(
          '"direction":
            "%s"
                    ',
          self$`direction`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of SortValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of SortValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of SortValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`orderBy` <- this_object$`orderBy`
      self$`direction` <- this_object$`direction`
      self
    },
    #' Validate JSON input with respect to SortValueObject
    #'
    #' @description
    #' Validate JSON input with respect to SortValueObject and throw an exception if invalid
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
    #' @return String representation of SortValueObject
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
SortValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
SortValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
SortValueObject$lock()

