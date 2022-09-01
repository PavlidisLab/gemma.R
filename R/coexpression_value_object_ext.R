#' Create a new CoexpressionValueObjectExt
#'
#' @description
#' CoexpressionValueObjectExt Class
#'
#' @docType class
#' @title CoexpressionValueObjectExt
#' @description CoexpressionValueObjectExt Class
#' @format An \code{R6Class} generator object
#' @field containsMyData  character [optional]
#' @field foundGene  \link{GeneValueObject} [optional]
#' @field foundGeneNodeDegree  integer [optional]
#' @field foundGeneNodeDegreeRank  numeric [optional]
#' @field negSupp  integer [optional]
#' @field numTestedIn  integer [optional]
#' @field posSupp  integer [optional]
#' @field queryGene  \link{GeneValueObject} [optional]
#' @field queryGeneNodeDegree  integer [optional]
#' @field queryGeneNodeDegreeRank  numeric [optional]
#' @field sortKey  character [optional]
#' @field supportingExperiments  list(integer) [optional]
#' @field support  integer [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
CoexpressionValueObjectExt <- R6::R6Class(
  "CoexpressionValueObjectExt",
  public = list(
    `containsMyData` = NULL,
    `foundGene` = NULL,
    `foundGeneNodeDegree` = NULL,
    `foundGeneNodeDegreeRank` = NULL,
    `negSupp` = NULL,
    `numTestedIn` = NULL,
    `posSupp` = NULL,
    `queryGene` = NULL,
    `queryGeneNodeDegree` = NULL,
    `queryGeneNodeDegreeRank` = NULL,
    `sortKey` = NULL,
    `supportingExperiments` = NULL,
    `support` = NULL,
    #' Initialize a new CoexpressionValueObjectExt class.
    #'
    #' @description
    #' Initialize a new CoexpressionValueObjectExt class.
    #'
    #' @param containsMyData containsMyData
    #' @param foundGene foundGene
    #' @param foundGeneNodeDegree foundGeneNodeDegree
    #' @param foundGeneNodeDegreeRank foundGeneNodeDegreeRank
    #' @param negSupp negSupp
    #' @param numTestedIn numTestedIn
    #' @param posSupp posSupp
    #' @param queryGene queryGene
    #' @param queryGeneNodeDegree queryGeneNodeDegree
    #' @param queryGeneNodeDegreeRank queryGeneNodeDegreeRank
    #' @param sortKey sortKey
    #' @param supportingExperiments supportingExperiments
    #' @param support support
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `containsMyData` = NULL, `foundGene` = NULL, `foundGeneNodeDegree` = NULL, `foundGeneNodeDegreeRank` = NULL, `negSupp` = NULL, `numTestedIn` = NULL, `posSupp` = NULL, `queryGene` = NULL, `queryGeneNodeDegree` = NULL, `queryGeneNodeDegreeRank` = NULL, `sortKey` = NULL, `supportingExperiments` = NULL, `support` = NULL, ...
    ) {
      if (!is.null(`containsMyData`)) {
        stopifnot(is.logical(`containsMyData`), length(`containsMyData`) == 1)
        self$`containsMyData` <- `containsMyData`
      }
      if (!is.null(`foundGene`)) {
        stopifnot(R6::is.R6(`foundGene`))
        self$`foundGene` <- `foundGene`
      }
      if (!is.null(`foundGeneNodeDegree`)) {
        stopifnot(is.numeric(`foundGeneNodeDegree`), length(`foundGeneNodeDegree`) == 1)
        self$`foundGeneNodeDegree` <- `foundGeneNodeDegree`
      }
      if (!is.null(`foundGeneNodeDegreeRank`)) {
        stopifnot(is.numeric(`foundGeneNodeDegreeRank`), length(`foundGeneNodeDegreeRank`) == 1)
        self$`foundGeneNodeDegreeRank` <- `foundGeneNodeDegreeRank`
      }
      if (!is.null(`negSupp`)) {
        stopifnot(is.numeric(`negSupp`), length(`negSupp`) == 1)
        self$`negSupp` <- `negSupp`
      }
      if (!is.null(`numTestedIn`)) {
        stopifnot(is.numeric(`numTestedIn`), length(`numTestedIn`) == 1)
        self$`numTestedIn` <- `numTestedIn`
      }
      if (!is.null(`posSupp`)) {
        stopifnot(is.numeric(`posSupp`), length(`posSupp`) == 1)
        self$`posSupp` <- `posSupp`
      }
      if (!is.null(`queryGene`)) {
        stopifnot(R6::is.R6(`queryGene`))
        self$`queryGene` <- `queryGene`
      }
      if (!is.null(`queryGeneNodeDegree`)) {
        stopifnot(is.numeric(`queryGeneNodeDegree`), length(`queryGeneNodeDegree`) == 1)
        self$`queryGeneNodeDegree` <- `queryGeneNodeDegree`
      }
      if (!is.null(`queryGeneNodeDegreeRank`)) {
        stopifnot(is.numeric(`queryGeneNodeDegreeRank`), length(`queryGeneNodeDegreeRank`) == 1)
        self$`queryGeneNodeDegreeRank` <- `queryGeneNodeDegreeRank`
      }
      if (!is.null(`sortKey`)) {
        stopifnot(is.character(`sortKey`), length(`sortKey`) == 1)
        self$`sortKey` <- `sortKey`
      }
      if (!is.null(`supportingExperiments`)) {
        stopifnot(is.vector(`supportingExperiments`), length(`supportingExperiments`) != 0)
        sapply(`supportingExperiments`, function(x) stopifnot(is.character(x)))
        self$`supportingExperiments` <- `supportingExperiments`
      }
      if (!is.null(`support`)) {
        stopifnot(is.numeric(`support`), length(`support`) == 1)
        self$`support` <- `support`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CoexpressionValueObjectExt in JSON format
    #' @keywords internal
    toJSON = function() {
      CoexpressionValueObjectExtObject <- list()
      if (!is.null(self$`containsMyData`)) {
        CoexpressionValueObjectExtObject[["containsMyData"]] <-
          self$`containsMyData`
      }
      if (!is.null(self$`foundGene`)) {
        CoexpressionValueObjectExtObject[["foundGene"]] <-
          self$`foundGene`$toJSON()
      }
      if (!is.null(self$`foundGeneNodeDegree`)) {
        CoexpressionValueObjectExtObject[["foundGeneNodeDegree"]] <-
          self$`foundGeneNodeDegree`
      }
      if (!is.null(self$`foundGeneNodeDegreeRank`)) {
        CoexpressionValueObjectExtObject[["foundGeneNodeDegreeRank"]] <-
          self$`foundGeneNodeDegreeRank`
      }
      if (!is.null(self$`negSupp`)) {
        CoexpressionValueObjectExtObject[["negSupp"]] <-
          self$`negSupp`
      }
      if (!is.null(self$`numTestedIn`)) {
        CoexpressionValueObjectExtObject[["numTestedIn"]] <-
          self$`numTestedIn`
      }
      if (!is.null(self$`posSupp`)) {
        CoexpressionValueObjectExtObject[["posSupp"]] <-
          self$`posSupp`
      }
      if (!is.null(self$`queryGene`)) {
        CoexpressionValueObjectExtObject[["queryGene"]] <-
          self$`queryGene`$toJSON()
      }
      if (!is.null(self$`queryGeneNodeDegree`)) {
        CoexpressionValueObjectExtObject[["queryGeneNodeDegree"]] <-
          self$`queryGeneNodeDegree`
      }
      if (!is.null(self$`queryGeneNodeDegreeRank`)) {
        CoexpressionValueObjectExtObject[["queryGeneNodeDegreeRank"]] <-
          self$`queryGeneNodeDegreeRank`
      }
      if (!is.null(self$`sortKey`)) {
        CoexpressionValueObjectExtObject[["sortKey"]] <-
          self$`sortKey`
      }
      if (!is.null(self$`supportingExperiments`)) {
        CoexpressionValueObjectExtObject[["supportingExperiments"]] <-
          self$`supportingExperiments`
      }
      if (!is.null(self$`support`)) {
        CoexpressionValueObjectExtObject[["support"]] <-
          self$`support`
      }

      CoexpressionValueObjectExtObject
    },
    #' Deserialize JSON string into an instance of CoexpressionValueObjectExt
    #'
    #' @description
    #' Deserialize JSON string into an instance of CoexpressionValueObjectExt
    #'
    #' @param input_json the JSON input
    #' @return the instance of CoexpressionValueObjectExt
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`containsMyData`)) {
        self$`containsMyData` <- this_object$`containsMyData`
      }
      if (!is.null(this_object$`foundGene`)) {
        foundgene_object <- GeneValueObject$new()
        foundgene_object$fromJSON(jsonlite::toJSON(this_object$foundGene, auto_unbox = TRUE, digits = NA))
        self$`foundGene` <- foundgene_object
      }
      if (!is.null(this_object$`foundGeneNodeDegree`)) {
        self$`foundGeneNodeDegree` <- this_object$`foundGeneNodeDegree`
      }
      if (!is.null(this_object$`foundGeneNodeDegreeRank`)) {
        self$`foundGeneNodeDegreeRank` <- this_object$`foundGeneNodeDegreeRank`
      }
      if (!is.null(this_object$`negSupp`)) {
        self$`negSupp` <- this_object$`negSupp`
      }
      if (!is.null(this_object$`numTestedIn`)) {
        self$`numTestedIn` <- this_object$`numTestedIn`
      }
      if (!is.null(this_object$`posSupp`)) {
        self$`posSupp` <- this_object$`posSupp`
      }
      if (!is.null(this_object$`queryGene`)) {
        querygene_object <- GeneValueObject$new()
        querygene_object$fromJSON(jsonlite::toJSON(this_object$queryGene, auto_unbox = TRUE, digits = NA))
        self$`queryGene` <- querygene_object
      }
      if (!is.null(this_object$`queryGeneNodeDegree`)) {
        self$`queryGeneNodeDegree` <- this_object$`queryGeneNodeDegree`
      }
      if (!is.null(this_object$`queryGeneNodeDegreeRank`)) {
        self$`queryGeneNodeDegreeRank` <- this_object$`queryGeneNodeDegreeRank`
      }
      if (!is.null(this_object$`sortKey`)) {
        self$`sortKey` <- this_object$`sortKey`
      }
      if (!is.null(this_object$`supportingExperiments`)) {
        self$`supportingExperiments` <- ApiClient$new()$deserializeObj(this_object$`supportingExperiments`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`support`)) {
        self$`support` <- this_object$`support`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CoexpressionValueObjectExt in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`containsMyData`)) {
          sprintf(
          '"containsMyData":
            %s
                    ',
          tolower(self$`containsMyData`)
          )
        },
        if (!is.null(self$`foundGene`)) {
          sprintf(
          '"foundGene":
          %s
          ',
          jsonlite::toJSON(self$`foundGene`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`foundGeneNodeDegree`)) {
          sprintf(
          '"foundGeneNodeDegree":
            %d
                    ',
          self$`foundGeneNodeDegree`
          )
        },
        if (!is.null(self$`foundGeneNodeDegreeRank`)) {
          sprintf(
          '"foundGeneNodeDegreeRank":
            %d
                    ',
          self$`foundGeneNodeDegreeRank`
          )
        },
        if (!is.null(self$`negSupp`)) {
          sprintf(
          '"negSupp":
            %d
                    ',
          self$`negSupp`
          )
        },
        if (!is.null(self$`numTestedIn`)) {
          sprintf(
          '"numTestedIn":
            %d
                    ',
          self$`numTestedIn`
          )
        },
        if (!is.null(self$`posSupp`)) {
          sprintf(
          '"posSupp":
            %d
                    ',
          self$`posSupp`
          )
        },
        if (!is.null(self$`queryGene`)) {
          sprintf(
          '"queryGene":
          %s
          ',
          jsonlite::toJSON(self$`queryGene`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`queryGeneNodeDegree`)) {
          sprintf(
          '"queryGeneNodeDegree":
            %d
                    ',
          self$`queryGeneNodeDegree`
          )
        },
        if (!is.null(self$`queryGeneNodeDegreeRank`)) {
          sprintf(
          '"queryGeneNodeDegreeRank":
            %d
                    ',
          self$`queryGeneNodeDegreeRank`
          )
        },
        if (!is.null(self$`sortKey`)) {
          sprintf(
          '"sortKey":
            "%s"
                    ',
          self$`sortKey`
          )
        },
        if (!is.null(self$`supportingExperiments`)) {
          sprintf(
          '"supportingExperiments":
             [%s]
          ',
          paste(unlist(lapply(self$`supportingExperiments`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`support`)) {
          sprintf(
          '"support":
            %d
                    ',
          self$`support`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of CoexpressionValueObjectExt
    #'
    #' @description
    #' Deserialize JSON string into an instance of CoexpressionValueObjectExt
    #'
    #' @param input_json the JSON input
    #' @return the instance of CoexpressionValueObjectExt
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`containsMyData` <- this_object$`containsMyData`
      self$`foundGene` <- GeneValueObject$new()$fromJSON(jsonlite::toJSON(this_object$foundGene, auto_unbox = TRUE, digits = NA))
      self$`foundGeneNodeDegree` <- this_object$`foundGeneNodeDegree`
      self$`foundGeneNodeDegreeRank` <- this_object$`foundGeneNodeDegreeRank`
      self$`negSupp` <- this_object$`negSupp`
      self$`numTestedIn` <- this_object$`numTestedIn`
      self$`posSupp` <- this_object$`posSupp`
      self$`queryGene` <- GeneValueObject$new()$fromJSON(jsonlite::toJSON(this_object$queryGene, auto_unbox = TRUE, digits = NA))
      self$`queryGeneNodeDegree` <- this_object$`queryGeneNodeDegree`
      self$`queryGeneNodeDegreeRank` <- this_object$`queryGeneNodeDegreeRank`
      self$`sortKey` <- this_object$`sortKey`
      self$`supportingExperiments` <- ApiClient$new()$deserializeObj(this_object$`supportingExperiments`, "array[integer]", loadNamespace("gemma.R"))
      self$`support` <- this_object$`support`
      self
    },
    #' Validate JSON input with respect to CoexpressionValueObjectExt
    #'
    #' @description
    #' Validate JSON input with respect to CoexpressionValueObjectExt and throw an exception if invalid
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
    #' @return String representation of CoexpressionValueObjectExt
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
CoexpressionValueObjectExt$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
CoexpressionValueObjectExt$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
CoexpressionValueObjectExt$lock()

