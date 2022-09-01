#' Create a new ExperimentExpressionLevelsValueObject
#'
#' @description
#' ExperimentExpressionLevelsValueObject Class
#'
#' @docType class
#' @title ExperimentExpressionLevelsValueObject
#' @description ExperimentExpressionLevelsValueObject Class
#' @format An \code{R6Class} generator object
#' @field datasetId  integer optional
#' @field geneExpressionLevels  list(\link{GeneElementExpressionsValueObject}) optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ExperimentExpressionLevelsValueObject <- R6::R6Class(
  "ExperimentExpressionLevelsValueObject",
  public = list(
    `datasetId` = NULL,
    `geneExpressionLevels` = NULL,
    #' Initialize a new ExperimentExpressionLevelsValueObject class.
    #'
    #' @description
    #' Initialize a new ExperimentExpressionLevelsValueObject class.
    #'
    #' @param datasetId datasetId
    #' @param geneExpressionLevels geneExpressionLevels
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `datasetId` = NULL, `geneExpressionLevels` = NULL, ...
    ) {
      if (!is.null(`datasetId`)) {
        stopifnot(is.numeric(`datasetId`), length(`datasetId`) == 1)
        self$`datasetId` <- `datasetId`
      }
      if (!is.null(`geneExpressionLevels`)) {
        stopifnot(is.vector(`geneExpressionLevels`), length(`geneExpressionLevels`) != 0)
        sapply(`geneExpressionLevels`, function(x) stopifnot(R6::is.R6(x)))
        self$`geneExpressionLevels` <- `geneExpressionLevels`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExperimentExpressionLevelsValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ExperimentExpressionLevelsValueObjectObject <- list()
      if (!is.null(self$`datasetId`)) {
        ExperimentExpressionLevelsValueObjectObject[["datasetId"]] <-
          self$`datasetId`
      }
      if (!is.null(self$`geneExpressionLevels`)) {
        ExperimentExpressionLevelsValueObjectObject[["geneExpressionLevels"]] <-
          lapply(self$`geneExpressionLevels`, function(x) x$toJSON())
      }

      ExperimentExpressionLevelsValueObjectObject
    },
    #' Deserialize JSON string into an instance of ExperimentExpressionLevelsValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExperimentExpressionLevelsValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExperimentExpressionLevelsValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`datasetId`)) {
        self$`datasetId` <- this_object$`datasetId`
      }
      if (!is.null(this_object$`geneExpressionLevels`)) {
        self$`geneExpressionLevels` <- ApiClient$new()$deserializeObj(this_object$`geneExpressionLevels`, "array[GeneElementExpressionsValueObject]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExperimentExpressionLevelsValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`datasetId`)) {
          sprintf(
          '"datasetId":
            %d
                    ',
          self$`datasetId`
          )
        },
        if (!is.null(self$`geneExpressionLevels`)) {
          sprintf(
          '"geneExpressionLevels":
          [%s]
',
          paste(sapply(self$`geneExpressionLevels`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ExperimentExpressionLevelsValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExperimentExpressionLevelsValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExperimentExpressionLevelsValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`datasetId` <- this_object$`datasetId`
      self$`geneExpressionLevels` <- ApiClient$new()$deserializeObj(this_object$`geneExpressionLevels`, "array[GeneElementExpressionsValueObject]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to ExperimentExpressionLevelsValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ExperimentExpressionLevelsValueObject and throw an exception if invalid
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
    #' @return String representation of ExperimentExpressionLevelsValueObject
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
ExperimentExpressionLevelsValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ExperimentExpressionLevelsValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ExperimentExpressionLevelsValueObject$lock()

