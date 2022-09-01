#' Create a new SearchResultValueObject
#'
#' @description
#' SearchResultValueObject Class
#'
#' @docType class
#' @title SearchResultValueObject
#' @description SearchResultValueObject Class
#' @format An \code{R6Class} generator object
#' @field resultId  integer optional
#' @field resultType  character optional
#' @field score  numeric optional
#' @field resultObject  \link{IdentifiableValueObjectIdentifiable} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
SearchResultValueObject <- R6::R6Class(
  "SearchResultValueObject",
  public = list(
    `resultId` = NULL,
    `resultType` = NULL,
    `score` = NULL,
    `resultObject` = NULL,
    #' Initialize a new SearchResultValueObject class.
    #'
    #' @description
    #' Initialize a new SearchResultValueObject class.
    #'
    #' @param resultId resultId
    #' @param resultType resultType
    #' @param score score
    #' @param resultObject resultObject
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `resultId` = NULL, `resultType` = NULL, `score` = NULL, `resultObject` = NULL, ...
    ) {
      if (!is.null(`resultId`)) {
        stopifnot(is.numeric(`resultId`), length(`resultId`) == 1)
        self$`resultId` <- `resultId`
      }
      if (!is.null(`resultType`)) {
        stopifnot(is.character(`resultType`), length(`resultType`) == 1)
        self$`resultType` <- `resultType`
      }
      if (!is.null(`score`)) {
        stopifnot(is.numeric(`score`), length(`score`) == 1)
        self$`score` <- `score`
      }
      if (!is.null(`resultObject`)) {
        stopifnot(R6::is.R6(`resultObject`))
        self$`resultObject` <- `resultObject`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return SearchResultValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      SearchResultValueObjectObject <- list()
      if (!is.null(self$`resultId`)) {
        SearchResultValueObjectObject[["resultId"]] <-
          self$`resultId`
      }
      if (!is.null(self$`resultType`)) {
        SearchResultValueObjectObject[["resultType"]] <-
          self$`resultType`
      }
      if (!is.null(self$`score`)) {
        SearchResultValueObjectObject[["score"]] <-
          self$`score`
      }
      if (!is.null(self$`resultObject`)) {
        SearchResultValueObjectObject[["resultObject"]] <-
          self$`resultObject`$toJSON()
      }

      SearchResultValueObjectObject
    },
    #' Deserialize JSON string into an instance of SearchResultValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of SearchResultValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of SearchResultValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`resultId`)) {
        self$`resultId` <- this_object$`resultId`
      }
      if (!is.null(this_object$`resultType`)) {
        self$`resultType` <- this_object$`resultType`
      }
      if (!is.null(this_object$`score`)) {
        self$`score` <- this_object$`score`
      }
      if (!is.null(this_object$`resultObject`)) {
        resultobject_object <- IdentifiableValueObjectIdentifiable$new()
        resultobject_object$fromJSON(jsonlite::toJSON(this_object$resultObject, auto_unbox = TRUE, digits = NA))
        self$`resultObject` <- resultobject_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return SearchResultValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`resultId`)) {
          sprintf(
          '"resultId":
            %d
                    ',
          self$`resultId`
          )
        },
        if (!is.null(self$`resultType`)) {
          sprintf(
          '"resultType":
            "%s"
                    ',
          self$`resultType`
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
        if (!is.null(self$`resultObject`)) {
          sprintf(
          '"resultObject":
          %s
          ',
          jsonlite::toJSON(self$`resultObject`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of SearchResultValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of SearchResultValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of SearchResultValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`resultId` <- this_object$`resultId`
      self$`resultType` <- this_object$`resultType`
      self$`score` <- this_object$`score`
      self$`resultObject` <- IdentifiableValueObjectIdentifiable$new()$fromJSON(jsonlite::toJSON(this_object$resultObject, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to SearchResultValueObject
    #'
    #' @description
    #' Validate JSON input with respect to SearchResultValueObject and throw an exception if invalid
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
    #' @return String representation of SearchResultValueObject
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
SearchResultValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
SearchResultValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
SearchResultValueObject$lock()

