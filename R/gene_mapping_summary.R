#' Create a new GeneMappingSummary
#'
#' @description
#' GeneMappingSummary Class
#'
#' @docType class
#' @title GeneMappingSummary
#' @description GeneMappingSummary Class
#' @format An \code{R6Class} generator object
#' @field geneProductIdMap  named list(\link{GeneProductValueObject}) optional
#' @field geneProductIdGeneMap  named list(\link{GeneValueObject}) optional
#' @field blatResult  \link{BlatResultValueObject} optional
#' @field geneProductMap  named list(\link{GeneValueObject}) optional
#' @field compositeSequence  \link{CompositeSequenceValueObject} optional
#' @field identity  numeric optional
#' @field score  numeric optional
#' @field blatResultId  character optional
#' @field geneProducts  list(\link{GeneProductValueObject}) optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
GeneMappingSummary <- R6::R6Class(
  "GeneMappingSummary",
  public = list(
    `geneProductIdMap` = NULL,
    `geneProductIdGeneMap` = NULL,
    `blatResult` = NULL,
    `geneProductMap` = NULL,
    `compositeSequence` = NULL,
    `identity` = NULL,
    `score` = NULL,
    `blatResultId` = NULL,
    `geneProducts` = NULL,
    #' Initialize a new GeneMappingSummary class.
    #'
    #' @description
    #' Initialize a new GeneMappingSummary class.
    #'
    #' @param geneProductIdMap geneProductIdMap
    #' @param geneProductIdGeneMap geneProductIdGeneMap
    #' @param blatResult blatResult
    #' @param geneProductMap geneProductMap
    #' @param compositeSequence compositeSequence
    #' @param identity identity
    #' @param score score
    #' @param blatResultId blatResultId
    #' @param geneProducts geneProducts
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `geneProductIdMap` = NULL, `geneProductIdGeneMap` = NULL, `blatResult` = NULL, `geneProductMap` = NULL, `compositeSequence` = NULL, `identity` = NULL, `score` = NULL, `blatResultId` = NULL, `geneProducts` = NULL, ...
    ) {
      if (!is.null(`geneProductIdMap`)) {
        stopifnot(is.vector(`geneProductIdMap`), length(`geneProductIdMap`) != 0)
        sapply(`geneProductIdMap`, function(x) stopifnot(R6::is.R6(x)))
        self$`geneProductIdMap` <- `geneProductIdMap`
      }
      if (!is.null(`geneProductIdGeneMap`)) {
        stopifnot(is.vector(`geneProductIdGeneMap`), length(`geneProductIdGeneMap`) != 0)
        sapply(`geneProductIdGeneMap`, function(x) stopifnot(R6::is.R6(x)))
        self$`geneProductIdGeneMap` <- `geneProductIdGeneMap`
      }
      if (!is.null(`blatResult`)) {
        stopifnot(R6::is.R6(`blatResult`))
        self$`blatResult` <- `blatResult`
      }
      if (!is.null(`geneProductMap`)) {
        stopifnot(is.vector(`geneProductMap`), length(`geneProductMap`) != 0)
        sapply(`geneProductMap`, function(x) stopifnot(R6::is.R6(x)))
        self$`geneProductMap` <- `geneProductMap`
      }
      if (!is.null(`compositeSequence`)) {
        stopifnot(R6::is.R6(`compositeSequence`))
        self$`compositeSequence` <- `compositeSequence`
      }
      if (!is.null(`identity`)) {
        stopifnot(is.numeric(`identity`), length(`identity`) == 1)
        self$`identity` <- `identity`
      }
      if (!is.null(`score`)) {
        stopifnot(is.numeric(`score`), length(`score`) == 1)
        self$`score` <- `score`
      }
      if (!is.null(`blatResultId`)) {
        stopifnot(is.character(`blatResultId`), length(`blatResultId`) == 1)
        self$`blatResultId` <- `blatResultId`
      }
      if (!is.null(`geneProducts`)) {
        stopifnot(is.vector(`geneProducts`), length(`geneProducts`) != 0)
        sapply(`geneProducts`, function(x) stopifnot(R6::is.R6(x)))
        self$`geneProducts` <- `geneProducts`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneMappingSummary in JSON format
    #' @keywords internal
    toJSON = function() {
      GeneMappingSummaryObject <- list()
      if (!is.null(self$`geneProductIdMap`)) {
        GeneMappingSummaryObject[["geneProductIdMap"]] <-
          lapply(self$`geneProductIdMap`, function(x) x$toJSON())
      }
      if (!is.null(self$`geneProductIdGeneMap`)) {
        GeneMappingSummaryObject[["geneProductIdGeneMap"]] <-
          lapply(self$`geneProductIdGeneMap`, function(x) x$toJSON())
      }
      if (!is.null(self$`blatResult`)) {
        GeneMappingSummaryObject[["blatResult"]] <-
          self$`blatResult`$toJSON()
      }
      if (!is.null(self$`geneProductMap`)) {
        GeneMappingSummaryObject[["geneProductMap"]] <-
          lapply(self$`geneProductMap`, function(x) x$toJSON())
      }
      if (!is.null(self$`compositeSequence`)) {
        GeneMappingSummaryObject[["compositeSequence"]] <-
          self$`compositeSequence`$toJSON()
      }
      if (!is.null(self$`identity`)) {
        GeneMappingSummaryObject[["identity"]] <-
          self$`identity`
      }
      if (!is.null(self$`score`)) {
        GeneMappingSummaryObject[["score"]] <-
          self$`score`
      }
      if (!is.null(self$`blatResultId`)) {
        GeneMappingSummaryObject[["blatResultId"]] <-
          self$`blatResultId`
      }
      if (!is.null(self$`geneProducts`)) {
        GeneMappingSummaryObject[["geneProducts"]] <-
          lapply(self$`geneProducts`, function(x) x$toJSON())
      }

      GeneMappingSummaryObject
    },
    #' Deserialize JSON string into an instance of GeneMappingSummary
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneMappingSummary
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneMappingSummary
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`geneProductIdMap`)) {
        self$`geneProductIdMap` <- ApiClient$new()$deserializeObj(this_object$`geneProductIdMap`, "map(GeneProductValueObject)", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`geneProductIdGeneMap`)) {
        self$`geneProductIdGeneMap` <- ApiClient$new()$deserializeObj(this_object$`geneProductIdGeneMap`, "map(GeneValueObject)", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`blatResult`)) {
        blatresult_object <- BlatResultValueObject$new()
        blatresult_object$fromJSON(jsonlite::toJSON(this_object$blatResult, auto_unbox = TRUE, digits = NA))
        self$`blatResult` <- blatresult_object
      }
      if (!is.null(this_object$`geneProductMap`)) {
        self$`geneProductMap` <- ApiClient$new()$deserializeObj(this_object$`geneProductMap`, "map(GeneValueObject)", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`compositeSequence`)) {
        compositesequence_object <- CompositeSequenceValueObject$new()
        compositesequence_object$fromJSON(jsonlite::toJSON(this_object$compositeSequence, auto_unbox = TRUE, digits = NA))
        self$`compositeSequence` <- compositesequence_object
      }
      if (!is.null(this_object$`identity`)) {
        self$`identity` <- this_object$`identity`
      }
      if (!is.null(this_object$`score`)) {
        self$`score` <- this_object$`score`
      }
      if (!is.null(this_object$`blatResultId`)) {
        self$`blatResultId` <- this_object$`blatResultId`
      }
      if (!is.null(this_object$`geneProducts`)) {
        self$`geneProducts` <- ApiClient$new()$deserializeObj(this_object$`geneProducts`, "array[GeneProductValueObject]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneMappingSummary in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`geneProductIdMap`)) {
          sprintf(
          '"geneProductIdMap":
          %s
',
          jsonlite::toJSON(lapply(self$`geneProductIdMap`, function(x){ x$toJSON() }), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`geneProductIdGeneMap`)) {
          sprintf(
          '"geneProductIdGeneMap":
          %s
',
          jsonlite::toJSON(lapply(self$`geneProductIdGeneMap`, function(x){ x$toJSON() }), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`blatResult`)) {
          sprintf(
          '"blatResult":
          %s
          ',
          jsonlite::toJSON(self$`blatResult`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`geneProductMap`)) {
          sprintf(
          '"geneProductMap":
          %s
',
          jsonlite::toJSON(lapply(self$`geneProductMap`, function(x){ x$toJSON() }), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`compositeSequence`)) {
          sprintf(
          '"compositeSequence":
          %s
          ',
          jsonlite::toJSON(self$`compositeSequence`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`identity`)) {
          sprintf(
          '"identity":
            %d
                    ',
          self$`identity`
          )
        },
        if (!is.null(self$`score`)) {
          sprintf(
          '"score":
            %d
                    ',
          self$`score`
          )
        },
        if (!is.null(self$`blatResultId`)) {
          sprintf(
          '"blatResultId":
            "%s"
                    ',
          self$`blatResultId`
          )
        },
        if (!is.null(self$`geneProducts`)) {
          sprintf(
          '"geneProducts":
          [%s]
',
          paste(sapply(self$`geneProducts`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of GeneMappingSummary
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneMappingSummary
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneMappingSummary
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`geneProductIdMap` <- ApiClient$new()$deserializeObj(this_object$`geneProductIdMap`, "map(GeneProductValueObject)", loadNamespace("gemma.R"))
      self$`geneProductIdGeneMap` <- ApiClient$new()$deserializeObj(this_object$`geneProductIdGeneMap`, "map(GeneValueObject)", loadNamespace("gemma.R"))
      self$`blatResult` <- BlatResultValueObject$new()$fromJSON(jsonlite::toJSON(this_object$blatResult, auto_unbox = TRUE, digits = NA))
      self$`geneProductMap` <- ApiClient$new()$deserializeObj(this_object$`geneProductMap`, "map(GeneValueObject)", loadNamespace("gemma.R"))
      self$`compositeSequence` <- CompositeSequenceValueObject$new()$fromJSON(jsonlite::toJSON(this_object$compositeSequence, auto_unbox = TRUE, digits = NA))
      self$`identity` <- this_object$`identity`
      self$`score` <- this_object$`score`
      self$`blatResultId` <- this_object$`blatResultId`
      self$`geneProducts` <- ApiClient$new()$deserializeObj(this_object$`geneProducts`, "array[GeneProductValueObject]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to GeneMappingSummary
    #'
    #' @description
    #' Validate JSON input with respect to GeneMappingSummary and throw an exception if invalid
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
    #' @return String representation of GeneMappingSummary
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
GeneMappingSummary$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
GeneMappingSummary$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
GeneMappingSummary$lock()

