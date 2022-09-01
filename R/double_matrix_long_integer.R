#' Create a new DoubleMatrixLongInteger
#'
#' @description
#' DoubleMatrixLongInteger Class
#'
#' @docType class
#' @title DoubleMatrixLongInteger
#' @description DoubleMatrixLongInteger Class
#' @format An \code{R6Class} generator object
#' @field colNames  list(integer) optional
#' @field rowNames  list(integer) optional
#' @field rawMatrix  list(list(numeric)) optional
#' @field columnNames  list(integer) optional
#' @field rowNameMapIterator  object optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
DoubleMatrixLongInteger <- R6::R6Class(
  "DoubleMatrixLongInteger",
  public = list(
    `colNames` = NULL,
    `rowNames` = NULL,
    `rawMatrix` = NULL,
    `columnNames` = NULL,
    `rowNameMapIterator` = NULL,
    #' Initialize a new DoubleMatrixLongInteger class.
    #'
    #' @description
    #' Initialize a new DoubleMatrixLongInteger class.
    #'
    #' @param colNames colNames
    #' @param rowNames rowNames
    #' @param rawMatrix rawMatrix
    #' @param columnNames columnNames
    #' @param rowNameMapIterator rowNameMapIterator
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `colNames` = NULL, `rowNames` = NULL, `rawMatrix` = NULL, `columnNames` = NULL, `rowNameMapIterator` = NULL, ...
    ) {
      if (!is.null(`colNames`)) {
        stopifnot(is.vector(`colNames`), length(`colNames`) != 0)
        sapply(`colNames`, function(x) stopifnot(is.character(x)))
        self$`colNames` <- `colNames`
      }
      if (!is.null(`rowNames`)) {
        stopifnot(is.vector(`rowNames`), length(`rowNames`) != 0)
        sapply(`rowNames`, function(x) stopifnot(is.character(x)))
        self$`rowNames` <- `rowNames`
      }
      if (!is.null(`rawMatrix`)) {
        stopifnot(is.vector(`rawMatrix`), length(`rawMatrix`) != 0)
        sapply(`rawMatrix`, function(x) stopifnot(R6::is.R6(x)))
        self$`rawMatrix` <- `rawMatrix`
      }
      if (!is.null(`columnNames`)) {
        stopifnot(is.vector(`columnNames`), length(`columnNames`) != 0)
        sapply(`columnNames`, function(x) stopifnot(is.character(x)))
        self$`columnNames` <- `columnNames`
      }
      if (!is.null(`rowNameMapIterator`)) {
        self$`rowNameMapIterator` <- `rowNameMapIterator`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DoubleMatrixLongInteger in JSON format
    #' @keywords internal
    toJSON = function() {
      DoubleMatrixLongIntegerObject <- list()
      if (!is.null(self$`colNames`)) {
        DoubleMatrixLongIntegerObject[["colNames"]] <-
          self$`colNames`
      }
      if (!is.null(self$`rowNames`)) {
        DoubleMatrixLongIntegerObject[["rowNames"]] <-
          self$`rowNames`
      }
      if (!is.null(self$`rawMatrix`)) {
        DoubleMatrixLongIntegerObject[["rawMatrix"]] <-
          lapply(self$`rawMatrix`, function(x) x$toJSON())
      }
      if (!is.null(self$`columnNames`)) {
        DoubleMatrixLongIntegerObject[["columnNames"]] <-
          self$`columnNames`
      }
      if (!is.null(self$`rowNameMapIterator`)) {
        DoubleMatrixLongIntegerObject[["rowNameMapIterator"]] <-
          self$`rowNameMapIterator`
      }

      DoubleMatrixLongIntegerObject
    },
    #' Deserialize JSON string into an instance of DoubleMatrixLongInteger
    #'
    #' @description
    #' Deserialize JSON string into an instance of DoubleMatrixLongInteger
    #'
    #' @param input_json the JSON input
    #' @return the instance of DoubleMatrixLongInteger
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`colNames`)) {
        self$`colNames` <- ApiClient$new()$deserializeObj(this_object$`colNames`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`rowNames`)) {
        self$`rowNames` <- ApiClient$new()$deserializeObj(this_object$`rowNames`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`rawMatrix`)) {
        self$`rawMatrix` <- ApiClient$new()$deserializeObj(this_object$`rawMatrix`, "array[array[numeric]]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`columnNames`)) {
        self$`columnNames` <- ApiClient$new()$deserializeObj(this_object$`columnNames`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`rowNameMapIterator`)) {
        self$`rowNameMapIterator` <- this_object$`rowNameMapIterator`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DoubleMatrixLongInteger in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`colNames`)) {
          sprintf(
          '"colNames":
             [%s]
          ',
          paste(unlist(lapply(self$`colNames`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`rowNames`)) {
          sprintf(
          '"rowNames":
             [%s]
          ',
          paste(unlist(lapply(self$`rowNames`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`rawMatrix`)) {
          sprintf(
          '"rawMatrix":
          [%s]
',
          paste(sapply(self$`rawMatrix`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`columnNames`)) {
          sprintf(
          '"columnNames":
             [%s]
          ',
          paste(unlist(lapply(self$`columnNames`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`rowNameMapIterator`)) {
          sprintf(
          '"rowNameMapIterator":
            "%s"
                    ',
          self$`rowNameMapIterator`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of DoubleMatrixLongInteger
    #'
    #' @description
    #' Deserialize JSON string into an instance of DoubleMatrixLongInteger
    #'
    #' @param input_json the JSON input
    #' @return the instance of DoubleMatrixLongInteger
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`colNames` <- ApiClient$new()$deserializeObj(this_object$`colNames`, "array[integer]", loadNamespace("gemma.R"))
      self$`rowNames` <- ApiClient$new()$deserializeObj(this_object$`rowNames`, "array[integer]", loadNamespace("gemma.R"))
      self$`rawMatrix` <- ApiClient$new()$deserializeObj(this_object$`rawMatrix`, "array[array[numeric]]", loadNamespace("gemma.R"))
      self$`columnNames` <- ApiClient$new()$deserializeObj(this_object$`columnNames`, "array[integer]", loadNamespace("gemma.R"))
      self$`rowNameMapIterator` <- this_object$`rowNameMapIterator`
      self
    },
    #' Validate JSON input with respect to DoubleMatrixLongInteger
    #'
    #' @description
    #' Validate JSON input with respect to DoubleMatrixLongInteger and throw an exception if invalid
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
    #' @return String representation of DoubleMatrixLongInteger
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
DoubleMatrixLongInteger$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
DoubleMatrixLongInteger$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
DoubleMatrixLongInteger$lock()

