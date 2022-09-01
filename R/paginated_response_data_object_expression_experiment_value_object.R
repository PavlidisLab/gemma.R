#' Create a new PaginatedResponseDataObjectExpressionExperimentValueObject
#'
#' @description
#' PaginatedResponseDataObjectExpressionExperimentValueObject Class
#'
#' @docType class
#' @title PaginatedResponseDataObjectExpressionExperimentValueObject
#' @description PaginatedResponseDataObjectExpressionExperimentValueObject Class
#' @format An \code{R6Class} generator object
#' @field data  list(\link{ExpressionExperimentValueObject}) [optional]
#' @field offset  integer [optional]
#' @field limit  integer [optional]
#' @field sort  \link{SortValueObject} [optional]
#' @field totalElements  integer [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
PaginatedResponseDataObjectExpressionExperimentValueObject <- R6::R6Class(
  "PaginatedResponseDataObjectExpressionExperimentValueObject",
  public = list(
    `data` = NULL,
    `offset` = NULL,
    `limit` = NULL,
    `sort` = NULL,
    `totalElements` = NULL,
    #' Initialize a new PaginatedResponseDataObjectExpressionExperimentValueObject class.
    #'
    #' @description
    #' Initialize a new PaginatedResponseDataObjectExpressionExperimentValueObject class.
    #'
    #' @param data data
    #' @param offset offset
    #' @param limit limit
    #' @param sort sort
    #' @param totalElements totalElements
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `data` = NULL, `offset` = NULL, `limit` = NULL, `sort` = NULL, `totalElements` = NULL, ...
    ) {
      if (!is.null(`data`)) {
        stopifnot(is.vector(`data`), length(`data`) != 0)
        sapply(`data`, function(x) stopifnot(R6::is.R6(x)))
        self$`data` <- `data`
      }
      if (!is.null(`offset`)) {
        stopifnot(is.numeric(`offset`), length(`offset`) == 1)
        self$`offset` <- `offset`
      }
      if (!is.null(`limit`)) {
        stopifnot(is.numeric(`limit`), length(`limit`) == 1)
        self$`limit` <- `limit`
      }
      if (!is.null(`sort`)) {
        stopifnot(R6::is.R6(`sort`))
        self$`sort` <- `sort`
      }
      if (!is.null(`totalElements`)) {
        stopifnot(is.numeric(`totalElements`), length(`totalElements`) == 1)
        self$`totalElements` <- `totalElements`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PaginatedResponseDataObjectExpressionExperimentValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      PaginatedResponseDataObjectExpressionExperimentValueObjectObject <- list()
      if (!is.null(self$`data`)) {
        PaginatedResponseDataObjectExpressionExperimentValueObjectObject[["data"]] <-
          lapply(self$`data`, function(x) x$toJSON())
      }
      if (!is.null(self$`offset`)) {
        PaginatedResponseDataObjectExpressionExperimentValueObjectObject[["offset"]] <-
          self$`offset`
      }
      if (!is.null(self$`limit`)) {
        PaginatedResponseDataObjectExpressionExperimentValueObjectObject[["limit"]] <-
          self$`limit`
      }
      if (!is.null(self$`sort`)) {
        PaginatedResponseDataObjectExpressionExperimentValueObjectObject[["sort"]] <-
          self$`sort`$toJSON()
      }
      if (!is.null(self$`totalElements`)) {
        PaginatedResponseDataObjectExpressionExperimentValueObjectObject[["totalElements"]] <-
          self$`totalElements`
      }

      PaginatedResponseDataObjectExpressionExperimentValueObjectObject
    },
    #' Deserialize JSON string into an instance of PaginatedResponseDataObjectExpressionExperimentValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of PaginatedResponseDataObjectExpressionExperimentValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of PaginatedResponseDataObjectExpressionExperimentValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`data`)) {
        self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "array[ExpressionExperimentValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`offset`)) {
        self$`offset` <- this_object$`offset`
      }
      if (!is.null(this_object$`limit`)) {
        self$`limit` <- this_object$`limit`
      }
      if (!is.null(this_object$`sort`)) {
        sort_object <- SortValueObject$new()
        sort_object$fromJSON(jsonlite::toJSON(this_object$sort, auto_unbox = TRUE, digits = NA))
        self$`sort` <- sort_object
      }
      if (!is.null(this_object$`totalElements`)) {
        self$`totalElements` <- this_object$`totalElements`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PaginatedResponseDataObjectExpressionExperimentValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`data`)) {
          sprintf(
          '"data":
          [%s]
',
          paste(sapply(self$`data`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`offset`)) {
          sprintf(
          '"offset":
            %d
                    ',
          self$`offset`
          )
        },
        if (!is.null(self$`limit`)) {
          sprintf(
          '"limit":
            %d
                    ',
          self$`limit`
          )
        },
        if (!is.null(self$`sort`)) {
          sprintf(
          '"sort":
          %s
          ',
          jsonlite::toJSON(self$`sort`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`totalElements`)) {
          sprintf(
          '"totalElements":
            %d
                    ',
          self$`totalElements`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of PaginatedResponseDataObjectExpressionExperimentValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of PaginatedResponseDataObjectExpressionExperimentValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of PaginatedResponseDataObjectExpressionExperimentValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "array[ExpressionExperimentValueObject]", loadNamespace("gemma.R"))
      self$`offset` <- this_object$`offset`
      self$`limit` <- this_object$`limit`
      self$`sort` <- SortValueObject$new()$fromJSON(jsonlite::toJSON(this_object$sort, auto_unbox = TRUE, digits = NA))
      self$`totalElements` <- this_object$`totalElements`
      self
    },
    #' Validate JSON input with respect to PaginatedResponseDataObjectExpressionExperimentValueObject
    #'
    #' @description
    #' Validate JSON input with respect to PaginatedResponseDataObjectExpressionExperimentValueObject and throw an exception if invalid
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
    #' @return String representation of PaginatedResponseDataObjectExpressionExperimentValueObject
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
PaginatedResponseDataObjectExpressionExperimentValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
PaginatedResponseDataObjectExpressionExperimentValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
PaginatedResponseDataObjectExpressionExperimentValueObject$lock()

