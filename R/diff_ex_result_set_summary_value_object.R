#' Create a new DiffExResultSetSummaryValueObject
#'
#' @description
#' DiffExResultSetSummaryValueObject Class
#'
#' @docType class
#' @title DiffExResultSetSummaryValueObject
#' @description DiffExResultSetSummaryValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field analysisId  integer [optional]
#' @field arrayDesignsUsed  list(integer) [optional]
#' @field baselineGroup  \link{FactorValueValueObject} [optional]
#' @field downregulatedCount  integer [optional]
#' @field experimentalFactors  list(\link{ExperimentalFactorValueObject}) [optional]
#' @field factorIds  list(integer) [optional]
#' @field numberOfDiffExpressedProbes  integer [optional]
#' @field numberOfGenesAnalyzed  integer [optional]
#' @field numberOfProbesAnalyzed  integer [optional]
#' @field threshold  numeric [optional]
#' @field upregulatedCount  integer [optional]
#' @field bioAssaySetAnalyzedId  integer [optional]
#' @field resultSetId  integer [optional]
#' @field qvalue  numeric [optional]
#' @field experimentalFactorsByValueObject  list(\link{ExperimentalFactorValueObject}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
DiffExResultSetSummaryValueObject <- R6::R6Class(
  "DiffExResultSetSummaryValueObject",
  public = list(
    `id` = NULL,
    `analysisId` = NULL,
    `arrayDesignsUsed` = NULL,
    `baselineGroup` = NULL,
    `downregulatedCount` = NULL,
    `experimentalFactors` = NULL,
    `factorIds` = NULL,
    `numberOfDiffExpressedProbes` = NULL,
    `numberOfGenesAnalyzed` = NULL,
    `numberOfProbesAnalyzed` = NULL,
    `threshold` = NULL,
    `upregulatedCount` = NULL,
    `bioAssaySetAnalyzedId` = NULL,
    `resultSetId` = NULL,
    `qvalue` = NULL,
    `experimentalFactorsByValueObject` = NULL,
    #' Initialize a new DiffExResultSetSummaryValueObject class.
    #'
    #' @description
    #' Initialize a new DiffExResultSetSummaryValueObject class.
    #'
    #' @param id id
    #' @param analysisId analysisId
    #' @param arrayDesignsUsed arrayDesignsUsed
    #' @param baselineGroup baselineGroup
    #' @param downregulatedCount downregulatedCount
    #' @param experimentalFactors experimentalFactors
    #' @param factorIds factorIds
    #' @param numberOfDiffExpressedProbes numberOfDiffExpressedProbes
    #' @param numberOfGenesAnalyzed numberOfGenesAnalyzed
    #' @param numberOfProbesAnalyzed numberOfProbesAnalyzed
    #' @param threshold threshold
    #' @param upregulatedCount upregulatedCount
    #' @param bioAssaySetAnalyzedId bioAssaySetAnalyzedId
    #' @param resultSetId resultSetId
    #' @param qvalue qvalue
    #' @param experimentalFactorsByValueObject experimentalFactorsByValueObject
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `analysisId` = NULL, `arrayDesignsUsed` = NULL, `baselineGroup` = NULL, `downregulatedCount` = NULL, `experimentalFactors` = NULL, `factorIds` = NULL, `numberOfDiffExpressedProbes` = NULL, `numberOfGenesAnalyzed` = NULL, `numberOfProbesAnalyzed` = NULL, `threshold` = NULL, `upregulatedCount` = NULL, `bioAssaySetAnalyzedId` = NULL, `resultSetId` = NULL, `qvalue` = NULL, `experimentalFactorsByValueObject` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`analysisId`)) {
        stopifnot(is.numeric(`analysisId`), length(`analysisId`) == 1)
        self$`analysisId` <- `analysisId`
      }
      if (!is.null(`arrayDesignsUsed`)) {
        stopifnot(is.vector(`arrayDesignsUsed`), length(`arrayDesignsUsed`) != 0)
        sapply(`arrayDesignsUsed`, function(x) stopifnot(is.character(x)))
        self$`arrayDesignsUsed` <- `arrayDesignsUsed`
      }
      if (!is.null(`baselineGroup`)) {
        stopifnot(R6::is.R6(`baselineGroup`))
        self$`baselineGroup` <- `baselineGroup`
      }
      if (!is.null(`downregulatedCount`)) {
        stopifnot(is.numeric(`downregulatedCount`), length(`downregulatedCount`) == 1)
        self$`downregulatedCount` <- `downregulatedCount`
      }
      if (!is.null(`experimentalFactors`)) {
        stopifnot(is.vector(`experimentalFactors`), length(`experimentalFactors`) != 0)
        sapply(`experimentalFactors`, function(x) stopifnot(R6::is.R6(x)))
        self$`experimentalFactors` <- `experimentalFactors`
      }
      if (!is.null(`factorIds`)) {
        stopifnot(is.vector(`factorIds`), length(`factorIds`) != 0)
        sapply(`factorIds`, function(x) stopifnot(is.character(x)))
        self$`factorIds` <- `factorIds`
      }
      if (!is.null(`numberOfDiffExpressedProbes`)) {
        stopifnot(is.numeric(`numberOfDiffExpressedProbes`), length(`numberOfDiffExpressedProbes`) == 1)
        self$`numberOfDiffExpressedProbes` <- `numberOfDiffExpressedProbes`
      }
      if (!is.null(`numberOfGenesAnalyzed`)) {
        stopifnot(is.numeric(`numberOfGenesAnalyzed`), length(`numberOfGenesAnalyzed`) == 1)
        self$`numberOfGenesAnalyzed` <- `numberOfGenesAnalyzed`
      }
      if (!is.null(`numberOfProbesAnalyzed`)) {
        stopifnot(is.numeric(`numberOfProbesAnalyzed`), length(`numberOfProbesAnalyzed`) == 1)
        self$`numberOfProbesAnalyzed` <- `numberOfProbesAnalyzed`
      }
      if (!is.null(`threshold`)) {
        stopifnot(is.numeric(`threshold`), length(`threshold`) == 1)
        self$`threshold` <- `threshold`
      }
      if (!is.null(`upregulatedCount`)) {
        stopifnot(is.numeric(`upregulatedCount`), length(`upregulatedCount`) == 1)
        self$`upregulatedCount` <- `upregulatedCount`
      }
      if (!is.null(`bioAssaySetAnalyzedId`)) {
        stopifnot(is.numeric(`bioAssaySetAnalyzedId`), length(`bioAssaySetAnalyzedId`) == 1)
        self$`bioAssaySetAnalyzedId` <- `bioAssaySetAnalyzedId`
      }
      if (!is.null(`resultSetId`)) {
        stopifnot(is.numeric(`resultSetId`), length(`resultSetId`) == 1)
        self$`resultSetId` <- `resultSetId`
      }
      if (!is.null(`qvalue`)) {
        stopifnot(is.numeric(`qvalue`), length(`qvalue`) == 1)
        self$`qvalue` <- `qvalue`
      }
      if (!is.null(`experimentalFactorsByValueObject`)) {
        stopifnot(is.vector(`experimentalFactorsByValueObject`), length(`experimentalFactorsByValueObject`) != 0)
        sapply(`experimentalFactorsByValueObject`, function(x) stopifnot(R6::is.R6(x)))
        self$`experimentalFactorsByValueObject` <- `experimentalFactorsByValueObject`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DiffExResultSetSummaryValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      DiffExResultSetSummaryValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        DiffExResultSetSummaryValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`analysisId`)) {
        DiffExResultSetSummaryValueObjectObject[["analysisId"]] <-
          self$`analysisId`
      }
      if (!is.null(self$`arrayDesignsUsed`)) {
        DiffExResultSetSummaryValueObjectObject[["arrayDesignsUsed"]] <-
          self$`arrayDesignsUsed`
      }
      if (!is.null(self$`baselineGroup`)) {
        DiffExResultSetSummaryValueObjectObject[["baselineGroup"]] <-
          self$`baselineGroup`$toJSON()
      }
      if (!is.null(self$`downregulatedCount`)) {
        DiffExResultSetSummaryValueObjectObject[["downregulatedCount"]] <-
          self$`downregulatedCount`
      }
      if (!is.null(self$`experimentalFactors`)) {
        DiffExResultSetSummaryValueObjectObject[["experimentalFactors"]] <-
          lapply(self$`experimentalFactors`, function(x) x$toJSON())
      }
      if (!is.null(self$`factorIds`)) {
        DiffExResultSetSummaryValueObjectObject[["factorIds"]] <-
          self$`factorIds`
      }
      if (!is.null(self$`numberOfDiffExpressedProbes`)) {
        DiffExResultSetSummaryValueObjectObject[["numberOfDiffExpressedProbes"]] <-
          self$`numberOfDiffExpressedProbes`
      }
      if (!is.null(self$`numberOfGenesAnalyzed`)) {
        DiffExResultSetSummaryValueObjectObject[["numberOfGenesAnalyzed"]] <-
          self$`numberOfGenesAnalyzed`
      }
      if (!is.null(self$`numberOfProbesAnalyzed`)) {
        DiffExResultSetSummaryValueObjectObject[["numberOfProbesAnalyzed"]] <-
          self$`numberOfProbesAnalyzed`
      }
      if (!is.null(self$`threshold`)) {
        DiffExResultSetSummaryValueObjectObject[["threshold"]] <-
          self$`threshold`
      }
      if (!is.null(self$`upregulatedCount`)) {
        DiffExResultSetSummaryValueObjectObject[["upregulatedCount"]] <-
          self$`upregulatedCount`
      }
      if (!is.null(self$`bioAssaySetAnalyzedId`)) {
        DiffExResultSetSummaryValueObjectObject[["bioAssaySetAnalyzedId"]] <-
          self$`bioAssaySetAnalyzedId`
      }
      if (!is.null(self$`resultSetId`)) {
        DiffExResultSetSummaryValueObjectObject[["resultSetId"]] <-
          self$`resultSetId`
      }
      if (!is.null(self$`qvalue`)) {
        DiffExResultSetSummaryValueObjectObject[["qvalue"]] <-
          self$`qvalue`
      }
      if (!is.null(self$`experimentalFactorsByValueObject`)) {
        DiffExResultSetSummaryValueObjectObject[["experimentalFactorsByValueObject"]] <-
          lapply(self$`experimentalFactorsByValueObject`, function(x) x$toJSON())
      }

      DiffExResultSetSummaryValueObjectObject
    },
    #' Deserialize JSON string into an instance of DiffExResultSetSummaryValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of DiffExResultSetSummaryValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of DiffExResultSetSummaryValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`analysisId`)) {
        self$`analysisId` <- this_object$`analysisId`
      }
      if (!is.null(this_object$`arrayDesignsUsed`)) {
        self$`arrayDesignsUsed` <- ApiClient$new()$deserializeObj(this_object$`arrayDesignsUsed`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`baselineGroup`)) {
        baselinegroup_object <- FactorValueValueObject$new()
        baselinegroup_object$fromJSON(jsonlite::toJSON(this_object$baselineGroup, auto_unbox = TRUE, digits = NA))
        self$`baselineGroup` <- baselinegroup_object
      }
      if (!is.null(this_object$`downregulatedCount`)) {
        self$`downregulatedCount` <- this_object$`downregulatedCount`
      }
      if (!is.null(this_object$`experimentalFactors`)) {
        self$`experimentalFactors` <- ApiClient$new()$deserializeObj(this_object$`experimentalFactors`, "array[ExperimentalFactorValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`factorIds`)) {
        self$`factorIds` <- ApiClient$new()$deserializeObj(this_object$`factorIds`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`numberOfDiffExpressedProbes`)) {
        self$`numberOfDiffExpressedProbes` <- this_object$`numberOfDiffExpressedProbes`
      }
      if (!is.null(this_object$`numberOfGenesAnalyzed`)) {
        self$`numberOfGenesAnalyzed` <- this_object$`numberOfGenesAnalyzed`
      }
      if (!is.null(this_object$`numberOfProbesAnalyzed`)) {
        self$`numberOfProbesAnalyzed` <- this_object$`numberOfProbesAnalyzed`
      }
      if (!is.null(this_object$`threshold`)) {
        self$`threshold` <- this_object$`threshold`
      }
      if (!is.null(this_object$`upregulatedCount`)) {
        self$`upregulatedCount` <- this_object$`upregulatedCount`
      }
      if (!is.null(this_object$`bioAssaySetAnalyzedId`)) {
        self$`bioAssaySetAnalyzedId` <- this_object$`bioAssaySetAnalyzedId`
      }
      if (!is.null(this_object$`resultSetId`)) {
        self$`resultSetId` <- this_object$`resultSetId`
      }
      if (!is.null(this_object$`qvalue`)) {
        self$`qvalue` <- this_object$`qvalue`
      }
      if (!is.null(this_object$`experimentalFactorsByValueObject`)) {
        self$`experimentalFactorsByValueObject` <- ApiClient$new()$deserializeObj(this_object$`experimentalFactorsByValueObject`, "array[ExperimentalFactorValueObject]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DiffExResultSetSummaryValueObject in JSON format
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
        if (!is.null(self$`analysisId`)) {
          sprintf(
          '"analysisId":
            %d
                    ',
          self$`analysisId`
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
        if (!is.null(self$`baselineGroup`)) {
          sprintf(
          '"baselineGroup":
          %s
          ',
          jsonlite::toJSON(self$`baselineGroup`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`downregulatedCount`)) {
          sprintf(
          '"downregulatedCount":
            %d
                    ',
          self$`downregulatedCount`
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
        if (!is.null(self$`factorIds`)) {
          sprintf(
          '"factorIds":
             [%s]
          ',
          paste(unlist(lapply(self$`factorIds`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`numberOfDiffExpressedProbes`)) {
          sprintf(
          '"numberOfDiffExpressedProbes":
            %d
                    ',
          self$`numberOfDiffExpressedProbes`
          )
        },
        if (!is.null(self$`numberOfGenesAnalyzed`)) {
          sprintf(
          '"numberOfGenesAnalyzed":
            %d
                    ',
          self$`numberOfGenesAnalyzed`
          )
        },
        if (!is.null(self$`numberOfProbesAnalyzed`)) {
          sprintf(
          '"numberOfProbesAnalyzed":
            %d
                    ',
          self$`numberOfProbesAnalyzed`
          )
        },
        if (!is.null(self$`threshold`)) {
          sprintf(
          '"threshold":
            %d
                    ',
          self$`threshold`
          )
        },
        if (!is.null(self$`upregulatedCount`)) {
          sprintf(
          '"upregulatedCount":
            %d
                    ',
          self$`upregulatedCount`
          )
        },
        if (!is.null(self$`bioAssaySetAnalyzedId`)) {
          sprintf(
          '"bioAssaySetAnalyzedId":
            %d
                    ',
          self$`bioAssaySetAnalyzedId`
          )
        },
        if (!is.null(self$`resultSetId`)) {
          sprintf(
          '"resultSetId":
            %d
                    ',
          self$`resultSetId`
          )
        },
        if (!is.null(self$`qvalue`)) {
          sprintf(
          '"qvalue":
            %d
                    ',
          self$`qvalue`
          )
        },
        if (!is.null(self$`experimentalFactorsByValueObject`)) {
          sprintf(
          '"experimentalFactorsByValueObject":
          [%s]
',
          paste(sapply(self$`experimentalFactorsByValueObject`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of DiffExResultSetSummaryValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of DiffExResultSetSummaryValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of DiffExResultSetSummaryValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`analysisId` <- this_object$`analysisId`
      self$`arrayDesignsUsed` <- ApiClient$new()$deserializeObj(this_object$`arrayDesignsUsed`, "array[integer]", loadNamespace("gemma.R"))
      self$`baselineGroup` <- FactorValueValueObject$new()$fromJSON(jsonlite::toJSON(this_object$baselineGroup, auto_unbox = TRUE, digits = NA))
      self$`downregulatedCount` <- this_object$`downregulatedCount`
      self$`experimentalFactors` <- ApiClient$new()$deserializeObj(this_object$`experimentalFactors`, "array[ExperimentalFactorValueObject]", loadNamespace("gemma.R"))
      self$`factorIds` <- ApiClient$new()$deserializeObj(this_object$`factorIds`, "array[integer]", loadNamespace("gemma.R"))
      self$`numberOfDiffExpressedProbes` <- this_object$`numberOfDiffExpressedProbes`
      self$`numberOfGenesAnalyzed` <- this_object$`numberOfGenesAnalyzed`
      self$`numberOfProbesAnalyzed` <- this_object$`numberOfProbesAnalyzed`
      self$`threshold` <- this_object$`threshold`
      self$`upregulatedCount` <- this_object$`upregulatedCount`
      self$`bioAssaySetAnalyzedId` <- this_object$`bioAssaySetAnalyzedId`
      self$`resultSetId` <- this_object$`resultSetId`
      self$`qvalue` <- this_object$`qvalue`
      self$`experimentalFactorsByValueObject` <- ApiClient$new()$deserializeObj(this_object$`experimentalFactorsByValueObject`, "array[ExperimentalFactorValueObject]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to DiffExResultSetSummaryValueObject
    #'
    #' @description
    #' Validate JSON input with respect to DiffExResultSetSummaryValueObject and throw an exception if invalid
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
    #' @return String representation of DiffExResultSetSummaryValueObject
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
DiffExResultSetSummaryValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
DiffExResultSetSummaryValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
DiffExResultSetSummaryValueObject$lock()

