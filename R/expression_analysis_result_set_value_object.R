#' Create a new ExpressionAnalysisResultSetValueObject
#'
#' @description
#' ExpressionAnalysisResultSetValueObject Class
#'
#' @docType class
#' @title ExpressionAnalysisResultSetValueObject
#' @description ExpressionAnalysisResultSetValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field analysis  \link{DifferentialExpressionAnalysisValueObject} [optional]
#' @field experimentalFactors  list(\link{ExperimentalFactorValueObject}) [optional]
#' @field results  list(\link{DifferentialExpressionAnalysisResultValueObject}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ExpressionAnalysisResultSetValueObject <- R6::R6Class(
  "ExpressionAnalysisResultSetValueObject",
  public = list(
    `id` = NULL,
    `analysis` = NULL,
    `experimentalFactors` = NULL,
    `results` = NULL,
    #' Initialize a new ExpressionAnalysisResultSetValueObject class.
    #'
    #' @description
    #' Initialize a new ExpressionAnalysisResultSetValueObject class.
    #'
    #' @param id id
    #' @param analysis analysis
    #' @param experimentalFactors experimentalFactors
    #' @param results results
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `analysis` = NULL, `experimentalFactors` = NULL, `results` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`analysis`)) {
        stopifnot(R6::is.R6(`analysis`))
        self$`analysis` <- `analysis`
      }
      if (!is.null(`experimentalFactors`)) {
        stopifnot(is.vector(`experimentalFactors`), length(`experimentalFactors`) != 0)
        sapply(`experimentalFactors`, function(x) stopifnot(R6::is.R6(x)))
        self$`experimentalFactors` <- `experimentalFactors`
      }
      if (!is.null(`results`)) {
        stopifnot(is.vector(`results`), length(`results`) != 0)
        sapply(`results`, function(x) stopifnot(R6::is.R6(x)))
        self$`results` <- `results`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExpressionAnalysisResultSetValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ExpressionAnalysisResultSetValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        ExpressionAnalysisResultSetValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`analysis`)) {
        ExpressionAnalysisResultSetValueObjectObject[["analysis"]] <-
          self$`analysis`$toJSON()
      }
      if (!is.null(self$`experimentalFactors`)) {
        ExpressionAnalysisResultSetValueObjectObject[["experimentalFactors"]] <-
          lapply(self$`experimentalFactors`, function(x) x$toJSON())
      }
      if (!is.null(self$`results`)) {
        ExpressionAnalysisResultSetValueObjectObject[["results"]] <-
          lapply(self$`results`, function(x) x$toJSON())
      }

      ExpressionAnalysisResultSetValueObjectObject
    },
    #' Deserialize JSON string into an instance of ExpressionAnalysisResultSetValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExpressionAnalysisResultSetValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExpressionAnalysisResultSetValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`analysis`)) {
        analysis_object <- DifferentialExpressionAnalysisValueObject$new()
        analysis_object$fromJSON(jsonlite::toJSON(this_object$analysis, auto_unbox = TRUE, digits = NA))
        self$`analysis` <- analysis_object
      }
      if (!is.null(this_object$`experimentalFactors`)) {
        self$`experimentalFactors` <- ApiClient$new()$deserializeObj(this_object$`experimentalFactors`, "array[ExperimentalFactorValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`results`)) {
        self$`results` <- ApiClient$new()$deserializeObj(this_object$`results`, "array[DifferentialExpressionAnalysisResultValueObject]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExpressionAnalysisResultSetValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`id`)) {
          sprintf(
          '"id":
            %d
                    ',
          self$`id`
          )
        },
        if (!is.null(self$`analysis`)) {
          sprintf(
          '"analysis":
          %s
          ',
          jsonlite::toJSON(self$`analysis`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`experimentalFactors`)) {
          sprintf(
          '"experimentalFactors":
          [%s]
',
          paste(sapply(self$`experimentalFactors`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`results`)) {
          sprintf(
          '"results":
          [%s]
',
          paste(sapply(self$`results`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ExpressionAnalysisResultSetValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExpressionAnalysisResultSetValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExpressionAnalysisResultSetValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`analysis` <- DifferentialExpressionAnalysisValueObject$new()$fromJSON(jsonlite::toJSON(this_object$analysis, auto_unbox = TRUE, digits = NA))
      self$`experimentalFactors` <- ApiClient$new()$deserializeObj(this_object$`experimentalFactors`, "array[ExperimentalFactorValueObject]", loadNamespace("gemma.R"))
      self$`results` <- ApiClient$new()$deserializeObj(this_object$`results`, "array[DifferentialExpressionAnalysisResultValueObject]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to ExpressionAnalysisResultSetValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ExpressionAnalysisResultSetValueObject and throw an exception if invalid
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
    #' @return String representation of ExpressionAnalysisResultSetValueObject
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
ExpressionAnalysisResultSetValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ExpressionAnalysisResultSetValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ExpressionAnalysisResultSetValueObject$lock()

