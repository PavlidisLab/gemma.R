#' Create a new DatabaseEntryValueObject
#'
#' @description
#' DatabaseEntryValueObject Class
#'
#' @docType class
#' @title DatabaseEntryValueObject
#' @description DatabaseEntryValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field accession  character optional
#' @field externalDatabase  \link{ExternalDatabaseValueObject} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
DatabaseEntryValueObject <- R6::R6Class(
  "DatabaseEntryValueObject",
  public = list(
    `id` = NULL,
    `accession` = NULL,
    `externalDatabase` = NULL,
    #' Initialize a new DatabaseEntryValueObject class.
    #'
    #' @description
    #' Initialize a new DatabaseEntryValueObject class.
    #'
    #' @param id id
    #' @param accession accession
    #' @param externalDatabase externalDatabase
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `accession` = NULL, `externalDatabase` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`accession`)) {
        stopifnot(is.character(`accession`), length(`accession`) == 1)
        self$`accession` <- `accession`
      }
      if (!is.null(`externalDatabase`)) {
        stopifnot(R6::is.R6(`externalDatabase`))
        self$`externalDatabase` <- `externalDatabase`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DatabaseEntryValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      DatabaseEntryValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        DatabaseEntryValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`accession`)) {
        DatabaseEntryValueObjectObject[["accession"]] <-
          self$`accession`
      }
      if (!is.null(self$`externalDatabase`)) {
        DatabaseEntryValueObjectObject[["externalDatabase"]] <-
          self$`externalDatabase`$toJSON()
      }

      DatabaseEntryValueObjectObject
    },
    #' Deserialize JSON string into an instance of DatabaseEntryValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of DatabaseEntryValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of DatabaseEntryValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`accession`)) {
        self$`accession` <- this_object$`accession`
      }
      if (!is.null(this_object$`externalDatabase`)) {
        externaldatabase_object <- ExternalDatabaseValueObject$new()
        externaldatabase_object$fromJSON(jsonlite::toJSON(this_object$externalDatabase, auto_unbox = TRUE, digits = NA))
        self$`externalDatabase` <- externaldatabase_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DatabaseEntryValueObject in JSON format
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
        if (!is.null(self$`accession`)) {
          sprintf(
          '"accession":
            "%s"
                    ',
          self$`accession`
          )
        },
        if (!is.null(self$`externalDatabase`)) {
          sprintf(
          '"externalDatabase":
          %s
          ',
          jsonlite::toJSON(self$`externalDatabase`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of DatabaseEntryValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of DatabaseEntryValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of DatabaseEntryValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`accession` <- this_object$`accession`
      self$`externalDatabase` <- ExternalDatabaseValueObject$new()$fromJSON(jsonlite::toJSON(this_object$externalDatabase, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to DatabaseEntryValueObject
    #'
    #' @description
    #' Validate JSON input with respect to DatabaseEntryValueObject and throw an exception if invalid
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
    #' @return String representation of DatabaseEntryValueObject
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
DatabaseEntryValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
DatabaseEntryValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
DatabaseEntryValueObject$lock()

