#' Create a new GeneElementExpressionsValueObject
#'
#' @description
#' GeneElementExpressionsValueObject Class
#'
#' @docType class
#' @title GeneElementExpressionsValueObject
#' @description GeneElementExpressionsValueObject Class
#' @format An \code{R6Class} generator object
#' @field geneOfficialSymbol  character optional
#' @field geneNcbiId  integer optional
#' @field vectors  list(\link{VectorElementValueObject}) optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
GeneElementExpressionsValueObject <- R6::R6Class(
  "GeneElementExpressionsValueObject",
  public = list(
    `geneOfficialSymbol` = NULL,
    `geneNcbiId` = NULL,
    `vectors` = NULL,
    #' Initialize a new GeneElementExpressionsValueObject class.
    #'
    #' @description
    #' Initialize a new GeneElementExpressionsValueObject class.
    #'
    #' @param geneOfficialSymbol geneOfficialSymbol
    #' @param geneNcbiId geneNcbiId
    #' @param vectors vectors
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `geneOfficialSymbol` = NULL, `geneNcbiId` = NULL, `vectors` = NULL, ...
    ) {
      if (!is.null(`geneOfficialSymbol`)) {
        stopifnot(is.character(`geneOfficialSymbol`), length(`geneOfficialSymbol`) == 1)
        self$`geneOfficialSymbol` <- `geneOfficialSymbol`
      }
      if (!is.null(`geneNcbiId`)) {
        stopifnot(is.numeric(`geneNcbiId`), length(`geneNcbiId`) == 1)
        self$`geneNcbiId` <- `geneNcbiId`
      }
      if (!is.null(`vectors`)) {
        stopifnot(is.vector(`vectors`), length(`vectors`) != 0)
        sapply(`vectors`, function(x) stopifnot(R6::is.R6(x)))
        self$`vectors` <- `vectors`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneElementExpressionsValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      GeneElementExpressionsValueObjectObject <- list()
      if (!is.null(self$`geneOfficialSymbol`)) {
        GeneElementExpressionsValueObjectObject[["geneOfficialSymbol"]] <-
          self$`geneOfficialSymbol`
      }
      if (!is.null(self$`geneNcbiId`)) {
        GeneElementExpressionsValueObjectObject[["geneNcbiId"]] <-
          self$`geneNcbiId`
      }
      if (!is.null(self$`vectors`)) {
        GeneElementExpressionsValueObjectObject[["vectors"]] <-
          lapply(self$`vectors`, function(x) x$toJSON())
      }

      GeneElementExpressionsValueObjectObject
    },
    #' Deserialize JSON string into an instance of GeneElementExpressionsValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneElementExpressionsValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneElementExpressionsValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`geneOfficialSymbol`)) {
        self$`geneOfficialSymbol` <- this_object$`geneOfficialSymbol`
      }
      if (!is.null(this_object$`geneNcbiId`)) {
        self$`geneNcbiId` <- this_object$`geneNcbiId`
      }
      if (!is.null(this_object$`vectors`)) {
        self$`vectors` <- ApiClient$new()$deserializeObj(this_object$`vectors`, "array[VectorElementValueObject]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneElementExpressionsValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`geneOfficialSymbol`)) {
          sprintf(
          '"geneOfficialSymbol":
            "%s"
                    ',
          self$`geneOfficialSymbol`
          )
        },
        if (!is.null(self$`geneNcbiId`)) {
          sprintf(
          '"geneNcbiId":
            %d
                    ',
          self$`geneNcbiId`
          )
        },
        if (!is.null(self$`vectors`)) {
          sprintf(
          '"vectors":
          [%s]
',
          paste(sapply(self$`vectors`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of GeneElementExpressionsValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneElementExpressionsValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneElementExpressionsValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`geneOfficialSymbol` <- this_object$`geneOfficialSymbol`
      self$`geneNcbiId` <- this_object$`geneNcbiId`
      self$`vectors` <- ApiClient$new()$deserializeObj(this_object$`vectors`, "array[VectorElementValueObject]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to GeneElementExpressionsValueObject
    #'
    #' @description
    #' Validate JSON input with respect to GeneElementExpressionsValueObject and throw an exception if invalid
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
    #' @return String representation of GeneElementExpressionsValueObject
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
GeneElementExpressionsValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
GeneElementExpressionsValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
GeneElementExpressionsValueObject$lock()

