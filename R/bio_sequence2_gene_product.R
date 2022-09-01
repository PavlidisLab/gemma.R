#' Create a new BioSequence2GeneProduct
#'
#' @description
#' BioSequence2GeneProduct Class
#'
#' @docType class
#' @title BioSequence2GeneProduct
#' @description BioSequence2GeneProduct Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field sourceAnalysis  \link{Analysis} optional
#' @field overlap  integer optional
#' @field score  numeric optional
#' @field threePrimeDistance  integer optional
#' @field threePrimeDistanceMeasurementMethod  \link{ThreePrimeDistanceMethod} optional
#' @field specificity  numeric optional
#' @field bioSequence  \link{BioSequence} optional
#' @field geneProduct  \link{GeneProduct} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
BioSequence2GeneProduct <- R6::R6Class(
  "BioSequence2GeneProduct",
  public = list(
    `id` = NULL,
    `sourceAnalysis` = NULL,
    `overlap` = NULL,
    `score` = NULL,
    `threePrimeDistance` = NULL,
    `threePrimeDistanceMeasurementMethod` = NULL,
    `specificity` = NULL,
    `bioSequence` = NULL,
    `geneProduct` = NULL,
    #' Initialize a new BioSequence2GeneProduct class.
    #'
    #' @description
    #' Initialize a new BioSequence2GeneProduct class.
    #'
    #' @param id id
    #' @param sourceAnalysis sourceAnalysis
    #' @param overlap overlap
    #' @param score score
    #' @param threePrimeDistance threePrimeDistance
    #' @param threePrimeDistanceMeasurementMethod threePrimeDistanceMeasurementMethod
    #' @param specificity specificity
    #' @param bioSequence bioSequence
    #' @param geneProduct geneProduct
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `sourceAnalysis` = NULL, `overlap` = NULL, `score` = NULL, `threePrimeDistance` = NULL, `threePrimeDistanceMeasurementMethod` = NULL, `specificity` = NULL, `bioSequence` = NULL, `geneProduct` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`sourceAnalysis`)) {
        stopifnot(R6::is.R6(`sourceAnalysis`))
        self$`sourceAnalysis` <- `sourceAnalysis`
      }
      if (!is.null(`overlap`)) {
        stopifnot(is.numeric(`overlap`), length(`overlap`) == 1)
        self$`overlap` <- `overlap`
      }
      if (!is.null(`score`)) {
        stopifnot(is.numeric(`score`), length(`score`) == 1)
        self$`score` <- `score`
      }
      if (!is.null(`threePrimeDistance`)) {
        stopifnot(is.numeric(`threePrimeDistance`), length(`threePrimeDistance`) == 1)
        self$`threePrimeDistance` <- `threePrimeDistance`
      }
      if (!is.null(`threePrimeDistanceMeasurementMethod`)) {
        stopifnot(R6::is.R6(`threePrimeDistanceMeasurementMethod`))
        self$`threePrimeDistanceMeasurementMethod` <- `threePrimeDistanceMeasurementMethod`
      }
      if (!is.null(`specificity`)) {
        stopifnot(is.numeric(`specificity`), length(`specificity`) == 1)
        self$`specificity` <- `specificity`
      }
      if (!is.null(`bioSequence`)) {
        stopifnot(R6::is.R6(`bioSequence`))
        self$`bioSequence` <- `bioSequence`
      }
      if (!is.null(`geneProduct`)) {
        stopifnot(R6::is.R6(`geneProduct`))
        self$`geneProduct` <- `geneProduct`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BioSequence2GeneProduct in JSON format
    #' @keywords internal
    toJSON = function() {
      BioSequence2GeneProductObject <- list()
      if (!is.null(self$`id`)) {
        BioSequence2GeneProductObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`sourceAnalysis`)) {
        BioSequence2GeneProductObject[["sourceAnalysis"]] <-
          self$`sourceAnalysis`$toJSON()
      }
      if (!is.null(self$`overlap`)) {
        BioSequence2GeneProductObject[["overlap"]] <-
          self$`overlap`
      }
      if (!is.null(self$`score`)) {
        BioSequence2GeneProductObject[["score"]] <-
          self$`score`
      }
      if (!is.null(self$`threePrimeDistance`)) {
        BioSequence2GeneProductObject[["threePrimeDistance"]] <-
          self$`threePrimeDistance`
      }
      if (!is.null(self$`threePrimeDistanceMeasurementMethod`)) {
        BioSequence2GeneProductObject[["threePrimeDistanceMeasurementMethod"]] <-
          self$`threePrimeDistanceMeasurementMethod`$toJSON()
      }
      if (!is.null(self$`specificity`)) {
        BioSequence2GeneProductObject[["specificity"]] <-
          self$`specificity`
      }
      if (!is.null(self$`bioSequence`)) {
        BioSequence2GeneProductObject[["bioSequence"]] <-
          self$`bioSequence`$toJSON()
      }
      if (!is.null(self$`geneProduct`)) {
        BioSequence2GeneProductObject[["geneProduct"]] <-
          self$`geneProduct`$toJSON()
      }

      BioSequence2GeneProductObject
    },
    #' Deserialize JSON string into an instance of BioSequence2GeneProduct
    #'
    #' @description
    #' Deserialize JSON string into an instance of BioSequence2GeneProduct
    #'
    #' @param input_json the JSON input
    #' @return the instance of BioSequence2GeneProduct
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`sourceAnalysis`)) {
        sourceanalysis_object <- Analysis$new()
        sourceanalysis_object$fromJSON(jsonlite::toJSON(this_object$sourceAnalysis, auto_unbox = TRUE, digits = NA))
        self$`sourceAnalysis` <- sourceanalysis_object
      }
      if (!is.null(this_object$`overlap`)) {
        self$`overlap` <- this_object$`overlap`
      }
      if (!is.null(this_object$`score`)) {
        self$`score` <- this_object$`score`
      }
      if (!is.null(this_object$`threePrimeDistance`)) {
        self$`threePrimeDistance` <- this_object$`threePrimeDistance`
      }
      if (!is.null(this_object$`threePrimeDistanceMeasurementMethod`)) {
        threeprimedistancemeasurementmethod_object <- ThreePrimeDistanceMethod$new()
        threeprimedistancemeasurementmethod_object$fromJSON(jsonlite::toJSON(this_object$threePrimeDistanceMeasurementMethod, auto_unbox = TRUE, digits = NA))
        self$`threePrimeDistanceMeasurementMethod` <- threeprimedistancemeasurementmethod_object
      }
      if (!is.null(this_object$`specificity`)) {
        self$`specificity` <- this_object$`specificity`
      }
      if (!is.null(this_object$`bioSequence`)) {
        biosequence_object <- BioSequence$new()
        biosequence_object$fromJSON(jsonlite::toJSON(this_object$bioSequence, auto_unbox = TRUE, digits = NA))
        self$`bioSequence` <- biosequence_object
      }
      if (!is.null(this_object$`geneProduct`)) {
        geneproduct_object <- GeneProduct$new()
        geneproduct_object$fromJSON(jsonlite::toJSON(this_object$geneProduct, auto_unbox = TRUE, digits = NA))
        self$`geneProduct` <- geneproduct_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BioSequence2GeneProduct in JSON format
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
        if (!is.null(self$`sourceAnalysis`)) {
          sprintf(
          '"sourceAnalysis":
          %s
          ',
          jsonlite::toJSON(self$`sourceAnalysis`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`overlap`)) {
          sprintf(
          '"overlap":
            %d
                    ',
          self$`overlap`
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
        if (!is.null(self$`threePrimeDistance`)) {
          sprintf(
          '"threePrimeDistance":
            %d
                    ',
          self$`threePrimeDistance`
          )
        },
        if (!is.null(self$`threePrimeDistanceMeasurementMethod`)) {
          sprintf(
          '"threePrimeDistanceMeasurementMethod":
          %s
          ',
          jsonlite::toJSON(self$`threePrimeDistanceMeasurementMethod`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`specificity`)) {
          sprintf(
          '"specificity":
            %d
                    ',
          self$`specificity`
          )
        },
        if (!is.null(self$`bioSequence`)) {
          sprintf(
          '"bioSequence":
          %s
          ',
          jsonlite::toJSON(self$`bioSequence`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`geneProduct`)) {
          sprintf(
          '"geneProduct":
          %s
          ',
          jsonlite::toJSON(self$`geneProduct`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of BioSequence2GeneProduct
    #'
    #' @description
    #' Deserialize JSON string into an instance of BioSequence2GeneProduct
    #'
    #' @param input_json the JSON input
    #' @return the instance of BioSequence2GeneProduct
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`sourceAnalysis` <- Analysis$new()$fromJSON(jsonlite::toJSON(this_object$sourceAnalysis, auto_unbox = TRUE, digits = NA))
      self$`overlap` <- this_object$`overlap`
      self$`score` <- this_object$`score`
      self$`threePrimeDistance` <- this_object$`threePrimeDistance`
      self$`threePrimeDistanceMeasurementMethod` <- ThreePrimeDistanceMethod$new()$fromJSON(jsonlite::toJSON(this_object$threePrimeDistanceMeasurementMethod, auto_unbox = TRUE, digits = NA))
      self$`specificity` <- this_object$`specificity`
      self$`bioSequence` <- BioSequence$new()$fromJSON(jsonlite::toJSON(this_object$bioSequence, auto_unbox = TRUE, digits = NA))
      self$`geneProduct` <- GeneProduct$new()$fromJSON(jsonlite::toJSON(this_object$geneProduct, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to BioSequence2GeneProduct
    #'
    #' @description
    #' Validate JSON input with respect to BioSequence2GeneProduct and throw an exception if invalid
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
    #' @return String representation of BioSequence2GeneProduct
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
BioSequence2GeneProduct$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
BioSequence2GeneProduct$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
BioSequence2GeneProduct$lock()

