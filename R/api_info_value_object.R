#' Create a new ApiInfoValueObject
#'
#' @description
#' ApiInfoValueObject Class
#'
#' @docType class
#' @title ApiInfoValueObject
#' @description ApiInfoValueObject Class
#' @format An \code{R6Class} generator object
#' @field welcome  character [optional]
#' @field version  character [optional]
#' @field docs  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ApiInfoValueObject <- R6::R6Class(
  "ApiInfoValueObject",
  public = list(
    `welcome` = NULL,
    `version` = NULL,
    `docs` = NULL,
    #' Initialize a new ApiInfoValueObject class.
    #'
    #' @description
    #' Initialize a new ApiInfoValueObject class.
    #'
    #' @param welcome welcome
    #' @param version version
    #' @param docs docs
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `welcome` = NULL, `version` = NULL, `docs` = NULL, ...
    ) {
      if (!is.null(`welcome`)) {
        stopifnot(is.character(`welcome`), length(`welcome`) == 1)
        self$`welcome` <- `welcome`
      }
      if (!is.null(`version`)) {
        stopifnot(is.character(`version`), length(`version`) == 1)
        self$`version` <- `version`
      }
      if (!is.null(`docs`)) {
        stopifnot(is.character(`docs`), length(`docs`) == 1)
        self$`docs` <- `docs`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ApiInfoValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ApiInfoValueObjectObject <- list()
      if (!is.null(self$`welcome`)) {
        ApiInfoValueObjectObject[["welcome"]] <-
          self$`welcome`
      }
      if (!is.null(self$`version`)) {
        ApiInfoValueObjectObject[["version"]] <-
          self$`version`
      }
      if (!is.null(self$`docs`)) {
        ApiInfoValueObjectObject[["docs"]] <-
          self$`docs`
      }

      ApiInfoValueObjectObject
    },
    #' Deserialize JSON string into an instance of ApiInfoValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ApiInfoValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ApiInfoValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`welcome`)) {
        self$`welcome` <- this_object$`welcome`
      }
      if (!is.null(this_object$`version`)) {
        self$`version` <- this_object$`version`
      }
      if (!is.null(this_object$`docs`)) {
        self$`docs` <- this_object$`docs`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ApiInfoValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`welcome`)) {
          sprintf(
          '"welcome":
            "%s"
                    ',
          self$`welcome`
          )
        },
        if (!is.null(self$`version`)) {
          sprintf(
          '"version":
            "%s"
                    ',
          self$`version`
          )
        },
        if (!is.null(self$`docs`)) {
          sprintf(
          '"docs":
            "%s"
                    ',
          self$`docs`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ApiInfoValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ApiInfoValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ApiInfoValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`welcome` <- this_object$`welcome`
      self$`version` <- this_object$`version`
      self$`docs` <- this_object$`docs`
      self
    },
    #' Validate JSON input with respect to ApiInfoValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ApiInfoValueObject and throw an exception if invalid
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
    #' @return String representation of ApiInfoValueObject
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
ApiInfoValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ApiInfoValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ApiInfoValueObject$lock()

