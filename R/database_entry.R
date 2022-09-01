#' Create a new DatabaseEntry
#'
#' @description
#' DatabaseEntry Class
#'
#' @docType class
#' @title DatabaseEntry
#' @description DatabaseEntry Class
#' @format An \code{R6Class} generator object
#' @field accession  character [optional]
#' @field accessionVersion  character [optional]
#' @field id  integer [optional]
#' @field externalDatabase  \link{ExternalDatabase} [optional]
#' @field uri  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
DatabaseEntry <- R6::R6Class(
  "DatabaseEntry",
  public = list(
    `accession` = NULL,
    `accessionVersion` = NULL,
    `id` = NULL,
    `externalDatabase` = NULL,
    `uri` = NULL,
    #' Initialize a new DatabaseEntry class.
    #'
    #' @description
    #' Initialize a new DatabaseEntry class.
    #'
    #' @param accession accession
    #' @param accessionVersion accessionVersion
    #' @param id id
    #' @param externalDatabase externalDatabase
    #' @param uri uri
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `accession` = NULL, `accessionVersion` = NULL, `id` = NULL, `externalDatabase` = NULL, `uri` = NULL, ...
    ) {
      if (!is.null(`accession`)) {
        stopifnot(is.character(`accession`), length(`accession`) == 1)
        self$`accession` <- `accession`
      }
      if (!is.null(`accessionVersion`)) {
        stopifnot(is.character(`accessionVersion`), length(`accessionVersion`) == 1)
        self$`accessionVersion` <- `accessionVersion`
      }
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`externalDatabase`)) {
        stopifnot(R6::is.R6(`externalDatabase`))
        self$`externalDatabase` <- `externalDatabase`
      }
      if (!is.null(`uri`)) {
        stopifnot(is.character(`uri`), length(`uri`) == 1)
        self$`uri` <- `uri`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DatabaseEntry in JSON format
    #' @keywords internal
    toJSON = function() {
      DatabaseEntryObject <- list()
      if (!is.null(self$`accession`)) {
        DatabaseEntryObject[["accession"]] <-
          self$`accession`
      }
      if (!is.null(self$`accessionVersion`)) {
        DatabaseEntryObject[["accessionVersion"]] <-
          self$`accessionVersion`
      }
      if (!is.null(self$`id`)) {
        DatabaseEntryObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`externalDatabase`)) {
        DatabaseEntryObject[["externalDatabase"]] <-
          self$`externalDatabase`$toJSON()
      }
      if (!is.null(self$`uri`)) {
        DatabaseEntryObject[["uri"]] <-
          self$`uri`
      }

      DatabaseEntryObject
    },
    #' Deserialize JSON string into an instance of DatabaseEntry
    #'
    #' @description
    #' Deserialize JSON string into an instance of DatabaseEntry
    #'
    #' @param input_json the JSON input
    #' @return the instance of DatabaseEntry
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`accession`)) {
        self$`accession` <- this_object$`accession`
      }
      if (!is.null(this_object$`accessionVersion`)) {
        self$`accessionVersion` <- this_object$`accessionVersion`
      }
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`externalDatabase`)) {
        externaldatabase_object <- ExternalDatabase$new()
        externaldatabase_object$fromJSON(jsonlite::toJSON(this_object$externalDatabase, auto_unbox = TRUE, digits = NA))
        self$`externalDatabase` <- externaldatabase_object
      }
      if (!is.null(this_object$`uri`)) {
        self$`uri` <- this_object$`uri`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DatabaseEntry in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`accession`)) {
          sprintf(
          '"accession":
            "%s"
                    ',
          self$`accession`
          )
        },
        if (!is.null(self$`accessionVersion`)) {
          sprintf(
          '"accessionVersion":
            "%s"
                    ',
          self$`accessionVersion`
          )
        },
        if (!is.null(self$`id`)) {
          sprintf(
          '"id":
            %d
                    ',
          self$`id`
          )
        },
        if (!is.null(self$`externalDatabase`)) {
          sprintf(
          '"externalDatabase":
          %s
          ',
          jsonlite::toJSON(self$`externalDatabase`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`uri`)) {
          sprintf(
          '"uri":
            "%s"
                    ',
          self$`uri`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of DatabaseEntry
    #'
    #' @description
    #' Deserialize JSON string into an instance of DatabaseEntry
    #'
    #' @param input_json the JSON input
    #' @return the instance of DatabaseEntry
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`accession` <- this_object$`accession`
      self$`accessionVersion` <- this_object$`accessionVersion`
      self$`id` <- this_object$`id`
      self$`externalDatabase` <- ExternalDatabase$new()$fromJSON(jsonlite::toJSON(this_object$externalDatabase, auto_unbox = TRUE, digits = NA))
      self$`uri` <- this_object$`uri`
      self
    },
    #' Validate JSON input with respect to DatabaseEntry
    #'
    #' @description
    #' Validate JSON input with respect to DatabaseEntry and throw an exception if invalid
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
    #' @return String representation of DatabaseEntry
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
DatabaseEntry$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
DatabaseEntry$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
DatabaseEntry$lock()

