#' Create a new SimpleSVDValueObject
#'
#' @description
#' SimpleSVDValueObject Class
#'
#' @docType class
#' @title SimpleSVDValueObject
#' @description SimpleSVDValueObject Class
#' @format An \code{R6Class} generator object
#' @field bioMaterialIds  list(integer) optional
#' @field variances  list(numeric) optional
#' @field getvMatrix  \link{DoubleMatrixLongInteger} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
SimpleSVDValueObject <- R6::R6Class(
  "SimpleSVDValueObject",
  public = list(
    `bioMaterialIds` = NULL,
    `variances` = NULL,
    `getvMatrix` = NULL,
    #' Initialize a new SimpleSVDValueObject class.
    #'
    #' @description
    #' Initialize a new SimpleSVDValueObject class.
    #'
    #' @param bioMaterialIds bioMaterialIds
    #' @param variances variances
    #' @param getvMatrix getvMatrix
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `bioMaterialIds` = NULL, `variances` = NULL, `getvMatrix` = NULL, ...
    ) {
      if (!is.null(`bioMaterialIds`)) {
        stopifnot(is.vector(`bioMaterialIds`), length(`bioMaterialIds`) != 0)
        sapply(`bioMaterialIds`, function(x) stopifnot(is.character(x)))
        self$`bioMaterialIds` <- `bioMaterialIds`
      }
      if (!is.null(`variances`)) {
        stopifnot(is.vector(`variances`), length(`variances`) != 0)
        sapply(`variances`, function(x) stopifnot(is.character(x)))
        self$`variances` <- `variances`
      }
      if (!is.null(`getvMatrix`)) {
        stopifnot(R6::is.R6(`getvMatrix`))
        self$`getvMatrix` <- `getvMatrix`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return SimpleSVDValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      SimpleSVDValueObjectObject <- list()
      if (!is.null(self$`bioMaterialIds`)) {
        SimpleSVDValueObjectObject[["bioMaterialIds"]] <-
          self$`bioMaterialIds`
      }
      if (!is.null(self$`variances`)) {
        SimpleSVDValueObjectObject[["variances"]] <-
          self$`variances`
      }
      if (!is.null(self$`getvMatrix`)) {
        SimpleSVDValueObjectObject[["getvMatrix"]] <-
          self$`getvMatrix`$toJSON()
      }

      SimpleSVDValueObjectObject
    },
    #' Deserialize JSON string into an instance of SimpleSVDValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of SimpleSVDValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of SimpleSVDValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`bioMaterialIds`)) {
        self$`bioMaterialIds` <- ApiClient$new()$deserializeObj(this_object$`bioMaterialIds`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`variances`)) {
        self$`variances` <- ApiClient$new()$deserializeObj(this_object$`variances`, "array[numeric]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`getvMatrix`)) {
        getvmatrix_object <- DoubleMatrixLongInteger$new()
        getvmatrix_object$fromJSON(jsonlite::toJSON(this_object$getvMatrix, auto_unbox = TRUE, digits = NA))
        self$`getvMatrix` <- getvmatrix_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return SimpleSVDValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`bioMaterialIds`)) {
          sprintf(
          '"bioMaterialIds":
             [%s]
          ',
          paste(unlist(lapply(self$`bioMaterialIds`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`variances`)) {
          sprintf(
          '"variances":
             [%s]
          ',
          paste(unlist(lapply(self$`variances`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`getvMatrix`)) {
          sprintf(
          '"getvMatrix":
          %s
          ',
          jsonlite::toJSON(self$`getvMatrix`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of SimpleSVDValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of SimpleSVDValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of SimpleSVDValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`bioMaterialIds` <- ApiClient$new()$deserializeObj(this_object$`bioMaterialIds`, "array[integer]", loadNamespace("gemma.R"))
      self$`variances` <- ApiClient$new()$deserializeObj(this_object$`variances`, "array[numeric]", loadNamespace("gemma.R"))
      self$`getvMatrix` <- DoubleMatrixLongInteger$new()$fromJSON(jsonlite::toJSON(this_object$getvMatrix, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to SimpleSVDValueObject
    #'
    #' @description
    #' Validate JSON input with respect to SimpleSVDValueObject and throw an exception if invalid
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
    #' @return String representation of SimpleSVDValueObject
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
SimpleSVDValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
SimpleSVDValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
SimpleSVDValueObject$lock()

