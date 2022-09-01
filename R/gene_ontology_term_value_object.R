#' Create a new GeneOntologyTermValueObject
#'
#' @description
#' GeneOntologyTermValueObject Class
#'
#' @docType class
#' @title GeneOntologyTermValueObject
#' @description GeneOntologyTermValueObject Class
#' @format An \code{R6Class} generator object
#' @field goId  character [optional]
#' @field label  character [optional]
#' @field uri  character [optional]
#' @field comment  character [optional]
#' @field localName  character [optional]
#' @field term  character [optional]
#' @field obsolete  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
GeneOntologyTermValueObject <- R6::R6Class(
  "GeneOntologyTermValueObject",
  public = list(
    `goId` = NULL,
    `label` = NULL,
    `uri` = NULL,
    `comment` = NULL,
    `localName` = NULL,
    `term` = NULL,
    `obsolete` = NULL,
    #' Initialize a new GeneOntologyTermValueObject class.
    #'
    #' @description
    #' Initialize a new GeneOntologyTermValueObject class.
    #'
    #' @param goId goId
    #' @param label label
    #' @param uri uri
    #' @param comment comment
    #' @param localName localName
    #' @param term term
    #' @param obsolete obsolete
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `goId` = NULL, `label` = NULL, `uri` = NULL, `comment` = NULL, `localName` = NULL, `term` = NULL, `obsolete` = NULL, ...
    ) {
      if (!is.null(`goId`)) {
        stopifnot(is.character(`goId`), length(`goId`) == 1)
        self$`goId` <- `goId`
      }
      if (!is.null(`label`)) {
        stopifnot(is.character(`label`), length(`label`) == 1)
        self$`label` <- `label`
      }
      if (!is.null(`uri`)) {
        stopifnot(is.character(`uri`), length(`uri`) == 1)
        self$`uri` <- `uri`
      }
      if (!is.null(`comment`)) {
        stopifnot(is.character(`comment`), length(`comment`) == 1)
        self$`comment` <- `comment`
      }
      if (!is.null(`localName`)) {
        stopifnot(is.character(`localName`), length(`localName`) == 1)
        self$`localName` <- `localName`
      }
      if (!is.null(`term`)) {
        stopifnot(is.character(`term`), length(`term`) == 1)
        self$`term` <- `term`
      }
      if (!is.null(`obsolete`)) {
        stopifnot(is.logical(`obsolete`), length(`obsolete`) == 1)
        self$`obsolete` <- `obsolete`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneOntologyTermValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      GeneOntologyTermValueObjectObject <- list()
      if (!is.null(self$`goId`)) {
        GeneOntologyTermValueObjectObject[["goId"]] <-
          self$`goId`
      }
      if (!is.null(self$`label`)) {
        GeneOntologyTermValueObjectObject[["label"]] <-
          self$`label`
      }
      if (!is.null(self$`uri`)) {
        GeneOntologyTermValueObjectObject[["uri"]] <-
          self$`uri`
      }
      if (!is.null(self$`comment`)) {
        GeneOntologyTermValueObjectObject[["comment"]] <-
          self$`comment`
      }
      if (!is.null(self$`localName`)) {
        GeneOntologyTermValueObjectObject[["localName"]] <-
          self$`localName`
      }
      if (!is.null(self$`term`)) {
        GeneOntologyTermValueObjectObject[["term"]] <-
          self$`term`
      }
      if (!is.null(self$`obsolete`)) {
        GeneOntologyTermValueObjectObject[["obsolete"]] <-
          self$`obsolete`
      }

      GeneOntologyTermValueObjectObject
    },
    #' Deserialize JSON string into an instance of GeneOntologyTermValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneOntologyTermValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneOntologyTermValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`goId`)) {
        self$`goId` <- this_object$`goId`
      }
      if (!is.null(this_object$`label`)) {
        self$`label` <- this_object$`label`
      }
      if (!is.null(this_object$`uri`)) {
        self$`uri` <- this_object$`uri`
      }
      if (!is.null(this_object$`comment`)) {
        self$`comment` <- this_object$`comment`
      }
      if (!is.null(this_object$`localName`)) {
        self$`localName` <- this_object$`localName`
      }
      if (!is.null(this_object$`term`)) {
        self$`term` <- this_object$`term`
      }
      if (!is.null(this_object$`obsolete`)) {
        self$`obsolete` <- this_object$`obsolete`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneOntologyTermValueObject in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`goId`)) {
          sprintf(
          '"goId":
            "%s"
                    ',
          self$`goId`
          )
        },
        if (!is.null(self$`label`)) {
          sprintf(
          '"label":
            "%s"
                    ',
          self$`label`
          )
        },
        if (!is.null(self$`uri`)) {
          sprintf(
          '"uri":
            "%s"
                    ',
          self$`uri`
          )
        },
        if (!is.null(self$`comment`)) {
          sprintf(
          '"comment":
            "%s"
                    ',
          self$`comment`
          )
        },
        if (!is.null(self$`localName`)) {
          sprintf(
          '"localName":
            "%s"
                    ',
          self$`localName`
          )
        },
        if (!is.null(self$`term`)) {
          sprintf(
          '"term":
            "%s"
                    ',
          self$`term`
          )
        },
        if (!is.null(self$`obsolete`)) {
          sprintf(
          '"obsolete":
            %s
                    ',
          tolower(self$`obsolete`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of GeneOntologyTermValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneOntologyTermValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneOntologyTermValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`goId` <- this_object$`goId`
      self$`label` <- this_object$`label`
      self$`uri` <- this_object$`uri`
      self$`comment` <- this_object$`comment`
      self$`localName` <- this_object$`localName`
      self$`term` <- this_object$`term`
      self$`obsolete` <- this_object$`obsolete`
      self
    },
    #' Validate JSON input with respect to GeneOntologyTermValueObject
    #'
    #' @description
    #' Validate JSON input with respect to GeneOntologyTermValueObject and throw an exception if invalid
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
    #' @return String representation of GeneOntologyTermValueObject
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
GeneOntologyTermValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
GeneOntologyTermValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
GeneOntologyTermValueObject$lock()

