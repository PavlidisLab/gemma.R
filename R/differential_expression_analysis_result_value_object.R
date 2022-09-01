#' Create a new DifferentialExpressionAnalysisResultValueObject
#'
#' @description
#' DifferentialExpressionAnalysisResultValueObject Class
#'
#' @docType class
#' @title DifferentialExpressionAnalysisResultValueObject
#' @description DifferentialExpressionAnalysisResultValueObject Class
#' @format An \code{R6Class} generator object
#' @field probeId  integer [optional]
#' @field probeName  character [optional]
#' @field genes  list(\link{GeneValueObject}) [optional]
#' @field correctedPvalue  numeric [optional]
#' @field rank  numeric [optional]
#' @field contrasts  list(\link{ContrastResultValueObject}) [optional]
#' @field pvalue  numeric [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
DifferentialExpressionAnalysisResultValueObject <- R6::R6Class(
  "DifferentialExpressionAnalysisResultValueObject",
  public = list(
    `probeId` = NULL,
    `probeName` = NULL,
    `genes` = NULL,
    `correctedPvalue` = NULL,
    `rank` = NULL,
    `contrasts` = NULL,
    `pvalue` = NULL,
    #' Initialize a new DifferentialExpressionAnalysisResultValueObject class.
    #'
    #' @description
    #' Initialize a new DifferentialExpressionAnalysisResultValueObject class.
    #'
    #' @param probeId probeId
    #' @param probeName probeName
    #' @param genes genes
    #' @param correctedPvalue correctedPvalue
    #' @param rank rank
    #' @param contrasts contrasts
    #' @param pvalue pvalue
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `probeId` = NULL, `probeName` = NULL, `genes` = NULL, `correctedPvalue` = NULL, `rank` = NULL, `contrasts` = NULL, `pvalue` = NULL, ...
    ) {
      if (!is.null(`probeId`)) {
        stopifnot(is.numeric(`probeId`), length(`probeId`) == 1)
        self$`probeId` <- `probeId`
      }
      if (!is.null(`probeName`)) {
        stopifnot(is.character(`probeName`), length(`probeName`) == 1)
        self$`probeName` <- `probeName`
      }
      if (!is.null(`genes`)) {
        stopifnot(is.vector(`genes`), length(`genes`) != 0)
        sapply(`genes`, function(x) stopifnot(R6::is.R6(x)))
        self$`genes` <- `genes`
      }
      if (!is.null(`correctedPvalue`)) {
        stopifnot(is.numeric(`correctedPvalue`), length(`correctedPvalue`) == 1)
        self$`correctedPvalue` <- `correctedPvalue`
      }
      if (!is.null(`rank`)) {
        stopifnot(is.numeric(`rank`), length(`rank`) == 1)
        self$`rank` <- `rank`
      }
      if (!is.null(`contrasts`)) {
        stopifnot(is.vector(`contrasts`), length(`contrasts`) != 0)
        sapply(`contrasts`, function(x) stopifnot(R6::is.R6(x)))
        self$`contrasts` <- `contrasts`
      }
      if (!is.null(`pvalue`)) {
        stopifnot(is.numeric(`pvalue`), length(`pvalue`) == 1)
        self$`pvalue` <- `pvalue`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DifferentialExpressionAnalysisResultValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      DifferentialExpressionAnalysisResultValueObjectObject <- list()
      if (!is.null(self$`probeId`)) {
        DifferentialExpressionAnalysisResultValueObjectObject[["probeId"]] <-
          self$`probeId`
      }
      if (!is.null(self$`probeName`)) {
        DifferentialExpressionAnalysisResultValueObjectObject[["probeName"]] <-
          self$`probeName`
      }
      if (!is.null(self$`genes`)) {
        DifferentialExpressionAnalysisResultValueObjectObject[["genes"]] <-
          lapply(self$`genes`, function(x) x$toJSON())
      }
      if (!is.null(self$`correctedPvalue`)) {
        DifferentialExpressionAnalysisResultValueObjectObject[["correctedPvalue"]] <-
          self$`correctedPvalue`
      }
      if (!is.null(self$`rank`)) {
        DifferentialExpressionAnalysisResultValueObjectObject[["rank"]] <-
          self$`rank`
      }
      if (!is.null(self$`contrasts`)) {
        DifferentialExpressionAnalysisResultValueObjectObject[["contrasts"]] <-
          lapply(self$`contrasts`, function(x) x$toJSON())
      }
      if (!is.null(self$`pvalue`)) {
        DifferentialExpressionAnalysisResultValueObjectObject[["pvalue"]] <-
          self$`pvalue`
      }

      DifferentialExpressionAnalysisResultValueObjectObject
    },
    #' Deserialize JSON string into an instance of DifferentialExpressionAnalysisResultValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of DifferentialExpressionAnalysisResultValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of DifferentialExpressionAnalysisResultValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`probeId`)) {
        self$`probeId` <- this_object$`probeId`
      }
      if (!is.null(this_object$`probeName`)) {
        self$`probeName` <- this_object$`probeName`
      }
      if (!is.null(this_object$`genes`)) {
        self$`genes` <- ApiClient$new()$deserializeObj(this_object$`genes`, "array[GeneValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`correctedPvalue`)) {
        self$`correctedPvalue` <- this_object$`correctedPvalue`
      }
      if (!is.null(this_object$`rank`)) {
        self$`rank` <- this_object$`rank`
      }
      if (!is.null(this_object$`contrasts`)) {
        self$`contrasts` <- ApiClient$new()$deserializeObj(this_object$`contrasts`, "array[ContrastResultValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`pvalue`)) {
        self$`pvalue` <- this_object$`pvalue`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DifferentialExpressionAnalysisResultValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`probeId`)) {
          sprintf(
          '"probeId":
            %d
                    ',
          self$`probeId`
          )
        },
        if (!is.null(self$`probeName`)) {
          sprintf(
          '"probeName":
            "%s"
                    ',
          self$`probeName`
          )
        },
        if (!is.null(self$`genes`)) {
          sprintf(
          '"genes":
          [%s]
',
          paste(sapply(self$`genes`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`correctedPvalue`)) {
          sprintf(
          '"correctedPvalue":
            %d
                    ',
          self$`correctedPvalue`
          )
        },
        if (!is.null(self$`rank`)) {
          sprintf(
          '"rank":
            %d
                    ',
          self$`rank`
          )
        },
        if (!is.null(self$`contrasts`)) {
          sprintf(
          '"contrasts":
          [%s]
',
          paste(sapply(self$`contrasts`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`pvalue`)) {
          sprintf(
          '"pvalue":
            %d
                    ',
          self$`pvalue`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of DifferentialExpressionAnalysisResultValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of DifferentialExpressionAnalysisResultValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of DifferentialExpressionAnalysisResultValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`probeId` <- this_object$`probeId`
      self$`probeName` <- this_object$`probeName`
      self$`genes` <- ApiClient$new()$deserializeObj(this_object$`genes`, "array[GeneValueObject]", loadNamespace("gemma.R"))
      self$`correctedPvalue` <- this_object$`correctedPvalue`
      self$`rank` <- this_object$`rank`
      self$`contrasts` <- ApiClient$new()$deserializeObj(this_object$`contrasts`, "array[ContrastResultValueObject]", loadNamespace("gemma.R"))
      self$`pvalue` <- this_object$`pvalue`
      self
    },
    #' Validate JSON input with respect to DifferentialExpressionAnalysisResultValueObject
    #'
    #' @description
    #' Validate JSON input with respect to DifferentialExpressionAnalysisResultValueObject and throw an exception if invalid
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
    #' @return String representation of DifferentialExpressionAnalysisResultValueObject
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
DifferentialExpressionAnalysisResultValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
DifferentialExpressionAnalysisResultValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
DifferentialExpressionAnalysisResultValueObject$lock()

