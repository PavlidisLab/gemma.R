#' Create a new DifferentialExpressionAnalysisValueObject
#'
#' @description
#' DifferentialExpressionAnalysisValueObject Class
#'
#' @docType class
#' @title DifferentialExpressionAnalysisValueObject
#' @description DifferentialExpressionAnalysisValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field factorValuesUsed  named list(list(\link{FactorValueValueObject})) optional
#' @field resultSets  list(\link{DiffExResultSetSummaryValueObject}) optional
#' @field arrayDesignsUsed  list(integer) optional
#' @field bioAssaySetId  integer optional
#' @field sourceExperiment  integer optional
#' @field subsetFactor  \link{ExperimentalFactorValueObject} optional
#' @field subsetFactorValue  \link{FactorValueValueObject} optional
#' @field subset  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
DifferentialExpressionAnalysisValueObject <- R6::R6Class(
  "DifferentialExpressionAnalysisValueObject",
  public = list(
    `id` = NULL,
    `factorValuesUsed` = NULL,
    `resultSets` = NULL,
    `arrayDesignsUsed` = NULL,
    `bioAssaySetId` = NULL,
    `sourceExperiment` = NULL,
    `subsetFactor` = NULL,
    `subsetFactorValue` = NULL,
    `subset` = NULL,
    #' Initialize a new DifferentialExpressionAnalysisValueObject class.
    #'
    #' @description
    #' Initialize a new DifferentialExpressionAnalysisValueObject class.
    #'
    #' @param id id
    #' @param factorValuesUsed factorValuesUsed
    #' @param resultSets resultSets
    #' @param arrayDesignsUsed arrayDesignsUsed
    #' @param bioAssaySetId bioAssaySetId
    #' @param sourceExperiment sourceExperiment
    #' @param subsetFactor subsetFactor
    #' @param subsetFactorValue subsetFactorValue
    #' @param subset subset
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `factorValuesUsed` = NULL, `resultSets` = NULL, `arrayDesignsUsed` = NULL, `bioAssaySetId` = NULL, `sourceExperiment` = NULL, `subsetFactor` = NULL, `subsetFactorValue` = NULL, `subset` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`factorValuesUsed`)) {
        stopifnot(is.vector(`factorValuesUsed`), length(`factorValuesUsed`) != 0)
        sapply(`factorValuesUsed`, function(x) stopifnot(R6::is.R6(x)))
        self$`factorValuesUsed` <- `factorValuesUsed`
      }
      if (!is.null(`resultSets`)) {
        stopifnot(is.vector(`resultSets`), length(`resultSets`) != 0)
        sapply(`resultSets`, function(x) stopifnot(R6::is.R6(x)))
        self$`resultSets` <- `resultSets`
      }
      if (!is.null(`arrayDesignsUsed`)) {
        stopifnot(is.vector(`arrayDesignsUsed`), length(`arrayDesignsUsed`) != 0)
        sapply(`arrayDesignsUsed`, function(x) stopifnot(is.character(x)))
        self$`arrayDesignsUsed` <- `arrayDesignsUsed`
      }
      if (!is.null(`bioAssaySetId`)) {
        stopifnot(is.numeric(`bioAssaySetId`), length(`bioAssaySetId`) == 1)
        self$`bioAssaySetId` <- `bioAssaySetId`
      }
      if (!is.null(`sourceExperiment`)) {
        stopifnot(is.numeric(`sourceExperiment`), length(`sourceExperiment`) == 1)
        self$`sourceExperiment` <- `sourceExperiment`
      }
      if (!is.null(`subsetFactor`)) {
        stopifnot(R6::is.R6(`subsetFactor`))
        self$`subsetFactor` <- `subsetFactor`
      }
      if (!is.null(`subsetFactorValue`)) {
        stopifnot(R6::is.R6(`subsetFactorValue`))
        self$`subsetFactorValue` <- `subsetFactorValue`
      }
      if (!is.null(`subset`)) {
        stopifnot(is.logical(`subset`), length(`subset`) == 1)
        self$`subset` <- `subset`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DifferentialExpressionAnalysisValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      DifferentialExpressionAnalysisValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        DifferentialExpressionAnalysisValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`factorValuesUsed`)) {
        DifferentialExpressionAnalysisValueObjectObject[["factorValuesUsed"]] <-
          lapply(self$`factorValuesUsed`, function(x) x$toJSON())
      }
      if (!is.null(self$`resultSets`)) {
        DifferentialExpressionAnalysisValueObjectObject[["resultSets"]] <-
          lapply(self$`resultSets`, function(x) x$toJSON())
      }
      if (!is.null(self$`arrayDesignsUsed`)) {
        DifferentialExpressionAnalysisValueObjectObject[["arrayDesignsUsed"]] <-
          self$`arrayDesignsUsed`
      }
      if (!is.null(self$`bioAssaySetId`)) {
        DifferentialExpressionAnalysisValueObjectObject[["bioAssaySetId"]] <-
          self$`bioAssaySetId`
      }
      if (!is.null(self$`sourceExperiment`)) {
        DifferentialExpressionAnalysisValueObjectObject[["sourceExperiment"]] <-
          self$`sourceExperiment`
      }
      if (!is.null(self$`subsetFactor`)) {
        DifferentialExpressionAnalysisValueObjectObject[["subsetFactor"]] <-
          self$`subsetFactor`$toJSON()
      }
      if (!is.null(self$`subsetFactorValue`)) {
        DifferentialExpressionAnalysisValueObjectObject[["subsetFactorValue"]] <-
          self$`subsetFactorValue`$toJSON()
      }
      if (!is.null(self$`subset`)) {
        DifferentialExpressionAnalysisValueObjectObject[["subset"]] <-
          self$`subset`
      }

      DifferentialExpressionAnalysisValueObjectObject
    },
    #' Deserialize JSON string into an instance of DifferentialExpressionAnalysisValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of DifferentialExpressionAnalysisValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of DifferentialExpressionAnalysisValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`factorValuesUsed`)) {
        self$`factorValuesUsed` <- ApiClient$new()$deserializeObj(this_object$`factorValuesUsed`, "map(array[FactorValueValueObject])", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`resultSets`)) {
        self$`resultSets` <- ApiClient$new()$deserializeObj(this_object$`resultSets`, "array[DiffExResultSetSummaryValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`arrayDesignsUsed`)) {
        self$`arrayDesignsUsed` <- ApiClient$new()$deserializeObj(this_object$`arrayDesignsUsed`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`bioAssaySetId`)) {
        self$`bioAssaySetId` <- this_object$`bioAssaySetId`
      }
      if (!is.null(this_object$`sourceExperiment`)) {
        self$`sourceExperiment` <- this_object$`sourceExperiment`
      }
      if (!is.null(this_object$`subsetFactor`)) {
        subsetfactor_object <- ExperimentalFactorValueObject$new()
        subsetfactor_object$fromJSON(jsonlite::toJSON(this_object$subsetFactor, auto_unbox = TRUE, digits = NA))
        self$`subsetFactor` <- subsetfactor_object
      }
      if (!is.null(this_object$`subsetFactorValue`)) {
        subsetfactorvalue_object <- FactorValueValueObject$new()
        subsetfactorvalue_object$fromJSON(jsonlite::toJSON(this_object$subsetFactorValue, auto_unbox = TRUE, digits = NA))
        self$`subsetFactorValue` <- subsetfactorvalue_object
      }
      if (!is.null(this_object$`subset`)) {
        self$`subset` <- this_object$`subset`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DifferentialExpressionAnalysisValueObject in JSON format
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
        if (!is.null(self$`factorValuesUsed`)) {
          sprintf(
          '"factorValuesUsed":
          %s
',
          jsonlite::toJSON(lapply(self$`factorValuesUsed`, function(x){ x$toJSON() }), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`resultSets`)) {
          sprintf(
          '"resultSets":
          [%s]
',
          paste(sapply(self$`resultSets`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`arrayDesignsUsed`)) {
          sprintf(
          '"arrayDesignsUsed":
             [%s]
          ',
          paste(unlist(lapply(self$`arrayDesignsUsed`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`bioAssaySetId`)) {
          sprintf(
          '"bioAssaySetId":
            %d
                    ',
          self$`bioAssaySetId`
          )
        },
        if (!is.null(self$`sourceExperiment`)) {
          sprintf(
          '"sourceExperiment":
            %d
                    ',
          self$`sourceExperiment`
          )
        },
        if (!is.null(self$`subsetFactor`)) {
          sprintf(
          '"subsetFactor":
          %s
          ',
          jsonlite::toJSON(self$`subsetFactor`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`subsetFactorValue`)) {
          sprintf(
          '"subsetFactorValue":
          %s
          ',
          jsonlite::toJSON(self$`subsetFactorValue`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`subset`)) {
          sprintf(
          '"subset":
            %s
                    ',
          tolower(self$`subset`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of DifferentialExpressionAnalysisValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of DifferentialExpressionAnalysisValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of DifferentialExpressionAnalysisValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`factorValuesUsed` <- ApiClient$new()$deserializeObj(this_object$`factorValuesUsed`, "map(array[FactorValueValueObject])", loadNamespace("gemma.R"))
      self$`resultSets` <- ApiClient$new()$deserializeObj(this_object$`resultSets`, "array[DiffExResultSetSummaryValueObject]", loadNamespace("gemma.R"))
      self$`arrayDesignsUsed` <- ApiClient$new()$deserializeObj(this_object$`arrayDesignsUsed`, "array[integer]", loadNamespace("gemma.R"))
      self$`bioAssaySetId` <- this_object$`bioAssaySetId`
      self$`sourceExperiment` <- this_object$`sourceExperiment`
      self$`subsetFactor` <- ExperimentalFactorValueObject$new()$fromJSON(jsonlite::toJSON(this_object$subsetFactor, auto_unbox = TRUE, digits = NA))
      self$`subsetFactorValue` <- FactorValueValueObject$new()$fromJSON(jsonlite::toJSON(this_object$subsetFactorValue, auto_unbox = TRUE, digits = NA))
      self$`subset` <- this_object$`subset`
      self
    },
    #' Validate JSON input with respect to DifferentialExpressionAnalysisValueObject
    #'
    #' @description
    #' Validate JSON input with respect to DifferentialExpressionAnalysisValueObject and throw an exception if invalid
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
    #' @return String representation of DifferentialExpressionAnalysisValueObject
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
DifferentialExpressionAnalysisValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
DifferentialExpressionAnalysisValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
DifferentialExpressionAnalysisValueObject$lock()

