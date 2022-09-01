#' Create a new ContrastResultValueObject
#'
#' @description
#' ContrastResultValueObject Class
#'
#' @docType class
#' @title ContrastResultValueObject
#' @description ContrastResultValueObject Class
#' @format An \code{R6Class} generator object
#' @field pvalue  numeric [optional]
#' @field coefficient  numeric [optional]
#' @field logFoldChange  numeric [optional]
#' @field factorValue  \link{FactorValueBasicValueObject} [optional]
#' @field secondFactorValue  \link{FactorValueBasicValueObject} [optional]
#' @field tstat  numeric [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ContrastResultValueObject <- R6::R6Class(
  "ContrastResultValueObject",
  public = list(
    `pvalue` = NULL,
    `coefficient` = NULL,
    `logFoldChange` = NULL,
    `factorValue` = NULL,
    `secondFactorValue` = NULL,
    `tstat` = NULL,
    #' Initialize a new ContrastResultValueObject class.
    #'
    #' @description
    #' Initialize a new ContrastResultValueObject class.
    #'
    #' @param pvalue pvalue
    #' @param coefficient coefficient
    #' @param logFoldChange logFoldChange
    #' @param factorValue factorValue
    #' @param secondFactorValue secondFactorValue
    #' @param tstat tstat
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `pvalue` = NULL, `coefficient` = NULL, `logFoldChange` = NULL, `factorValue` = NULL, `secondFactorValue` = NULL, `tstat` = NULL, ...
    ) {
      if (!is.null(`pvalue`)) {
        stopifnot(is.numeric(`pvalue`), length(`pvalue`) == 1)
        self$`pvalue` <- `pvalue`
      }
      if (!is.null(`coefficient`)) {
        stopifnot(is.numeric(`coefficient`), length(`coefficient`) == 1)
        self$`coefficient` <- `coefficient`
      }
      if (!is.null(`logFoldChange`)) {
        stopifnot(is.numeric(`logFoldChange`), length(`logFoldChange`) == 1)
        self$`logFoldChange` <- `logFoldChange`
      }
      if (!is.null(`factorValue`)) {
        stopifnot(R6::is.R6(`factorValue`))
        self$`factorValue` <- `factorValue`
      }
      if (!is.null(`secondFactorValue`)) {
        stopifnot(R6::is.R6(`secondFactorValue`))
        self$`secondFactorValue` <- `secondFactorValue`
      }
      if (!is.null(`tstat`)) {
        stopifnot(is.numeric(`tstat`), length(`tstat`) == 1)
        self$`tstat` <- `tstat`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ContrastResultValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ContrastResultValueObjectObject <- list()
      if (!is.null(self$`pvalue`)) {
        ContrastResultValueObjectObject[["pvalue"]] <-
          self$`pvalue`
      }
      if (!is.null(self$`coefficient`)) {
        ContrastResultValueObjectObject[["coefficient"]] <-
          self$`coefficient`
      }
      if (!is.null(self$`logFoldChange`)) {
        ContrastResultValueObjectObject[["logFoldChange"]] <-
          self$`logFoldChange`
      }
      if (!is.null(self$`factorValue`)) {
        ContrastResultValueObjectObject[["factorValue"]] <-
          self$`factorValue`$toJSON()
      }
      if (!is.null(self$`secondFactorValue`)) {
        ContrastResultValueObjectObject[["secondFactorValue"]] <-
          self$`secondFactorValue`$toJSON()
      }
      if (!is.null(self$`tstat`)) {
        ContrastResultValueObjectObject[["tstat"]] <-
          self$`tstat`
      }

      ContrastResultValueObjectObject
    },
    #' Deserialize JSON string into an instance of ContrastResultValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ContrastResultValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ContrastResultValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`pvalue`)) {
        self$`pvalue` <- this_object$`pvalue`
      }
      if (!is.null(this_object$`coefficient`)) {
        self$`coefficient` <- this_object$`coefficient`
      }
      if (!is.null(this_object$`logFoldChange`)) {
        self$`logFoldChange` <- this_object$`logFoldChange`
      }
      if (!is.null(this_object$`factorValue`)) {
        factorvalue_object <- FactorValueBasicValueObject$new()
        factorvalue_object$fromJSON(jsonlite::toJSON(this_object$factorValue, auto_unbox = TRUE, digits = NA))
        self$`factorValue` <- factorvalue_object
      }
      if (!is.null(this_object$`secondFactorValue`)) {
        secondfactorvalue_object <- FactorValueBasicValueObject$new()
        secondfactorvalue_object$fromJSON(jsonlite::toJSON(this_object$secondFactorValue, auto_unbox = TRUE, digits = NA))
        self$`secondFactorValue` <- secondfactorvalue_object
      }
      if (!is.null(this_object$`tstat`)) {
        self$`tstat` <- this_object$`tstat`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ContrastResultValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`pvalue`)) {
          sprintf(
          '"pvalue":
            %d
                    ',
          self$`pvalue`
          )
        },
        if (!is.null(self$`coefficient`)) {
          sprintf(
          '"coefficient":
            %d
                    ',
          self$`coefficient`
          )
        },
        if (!is.null(self$`logFoldChange`)) {
          sprintf(
          '"logFoldChange":
            %d
                    ',
          self$`logFoldChange`
          )
        },
        if (!is.null(self$`factorValue`)) {
          sprintf(
          '"factorValue":
          %s
          ',
          jsonlite::toJSON(self$`factorValue`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`secondFactorValue`)) {
          sprintf(
          '"secondFactorValue":
          %s
          ',
          jsonlite::toJSON(self$`secondFactorValue`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`tstat`)) {
          sprintf(
          '"tstat":
            %d
                    ',
          self$`tstat`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ContrastResultValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ContrastResultValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ContrastResultValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`pvalue` <- this_object$`pvalue`
      self$`coefficient` <- this_object$`coefficient`
      self$`logFoldChange` <- this_object$`logFoldChange`
      self$`factorValue` <- FactorValueBasicValueObject$new()$fromJSON(jsonlite::toJSON(this_object$factorValue, auto_unbox = TRUE, digits = NA))
      self$`secondFactorValue` <- FactorValueBasicValueObject$new()$fromJSON(jsonlite::toJSON(this_object$secondFactorValue, auto_unbox = TRUE, digits = NA))
      self$`tstat` <- this_object$`tstat`
      self
    },
    #' Validate JSON input with respect to ContrastResultValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ContrastResultValueObject and throw an exception if invalid
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
    #' @return String representation of ContrastResultValueObject
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
ContrastResultValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ContrastResultValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ContrastResultValueObject$lock()

