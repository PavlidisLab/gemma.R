#' Create a new VectorElementValueObject
#'
#' @description
#' VectorElementValueObject Class
#'
#' @docType class
#' @title VectorElementValueObject
#' @description VectorElementValueObject Class
#' @format An \code{R6Class} generator object
#' @field designElementName  character optional
#' @field bioAssayExpressionLevels  named list(numeric) optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
VectorElementValueObject <- R6::R6Class(
  "VectorElementValueObject",
  public = list(
    `designElementName` = NULL,
    `bioAssayExpressionLevels` = NULL,
    #' Initialize a new VectorElementValueObject class.
    #'
    #' @description
    #' Initialize a new VectorElementValueObject class.
    #'
    #' @param designElementName designElementName
    #' @param bioAssayExpressionLevels bioAssayExpressionLevels
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `designElementName` = NULL, `bioAssayExpressionLevels` = NULL, ...
    ) {
      if (!is.null(`designElementName`)) {
        stopifnot(is.character(`designElementName`), length(`designElementName`) == 1)
        self$`designElementName` <- `designElementName`
      }
      if (!is.null(`bioAssayExpressionLevels`)) {
        stopifnot(is.vector(`bioAssayExpressionLevels`), length(`bioAssayExpressionLevels`) != 0)
        sapply(`bioAssayExpressionLevels`, function(x) stopifnot(is.character(x)))
        self$`bioAssayExpressionLevels` <- `bioAssayExpressionLevels`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return VectorElementValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      VectorElementValueObjectObject <- list()
      if (!is.null(self$`designElementName`)) {
        VectorElementValueObjectObject[["designElementName"]] <-
          self$`designElementName`
      }
      if (!is.null(self$`bioAssayExpressionLevels`)) {
        VectorElementValueObjectObject[["bioAssayExpressionLevels"]] <-
          self$`bioAssayExpressionLevels`
      }

      VectorElementValueObjectObject
    },
    #' Deserialize JSON string into an instance of VectorElementValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of VectorElementValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of VectorElementValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`designElementName`)) {
        self$`designElementName` <- this_object$`designElementName`
      }
      if (!is.null(this_object$`bioAssayExpressionLevels`)) {
        self$`bioAssayExpressionLevels` <- ApiClient$new()$deserializeObj(this_object$`bioAssayExpressionLevels`, "map(numeric)", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return VectorElementValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`designElementName`)) {
          sprintf(
          '"designElementName":
            "%s"
                    ',
          self$`designElementName`
          )
        },
        if (!is.null(self$`bioAssayExpressionLevels`)) {
          sprintf(
          '"bioAssayExpressionLevels":
            "%s"
          ',
          jsonlite::toJSON(lapply(self$`bioAssayExpressionLevels`, function(x){ x }), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of VectorElementValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of VectorElementValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of VectorElementValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`designElementName` <- this_object$`designElementName`
      self$`bioAssayExpressionLevels` <- ApiClient$new()$deserializeObj(this_object$`bioAssayExpressionLevels`, "map(numeric)", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to VectorElementValueObject
    #'
    #' @description
    #' Validate JSON input with respect to VectorElementValueObject and throw an exception if invalid
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
    #' @return String representation of VectorElementValueObject
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
VectorElementValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
VectorElementValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
VectorElementValueObject$lock()

