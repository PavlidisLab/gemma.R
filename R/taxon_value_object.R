#' Create a new TaxonValueObject
#'
#' @description
#' TaxonValueObject Class
#'
#' @docType class
#' @title TaxonValueObject
#' @description TaxonValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field scientificName  character [optional]
#' @field commonName  character [optional]
#' @field ncbiId  integer [optional]
#' @field isSpecies  character [optional]
#' @field isGenesUsable  character [optional]
#' @field externalDatabase  \link{ExternalDatabaseValueObject} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
TaxonValueObject <- R6::R6Class(
  "TaxonValueObject",
  public = list(
    `id` = NULL,
    `scientificName` = NULL,
    `commonName` = NULL,
    `ncbiId` = NULL,
    `isSpecies` = NULL,
    `isGenesUsable` = NULL,
    `externalDatabase` = NULL,
    #' Initialize a new TaxonValueObject class.
    #'
    #' @description
    #' Initialize a new TaxonValueObject class.
    #'
    #' @param id id
    #' @param scientificName scientificName
    #' @param commonName commonName
    #' @param ncbiId ncbiId
    #' @param isSpecies isSpecies
    #' @param isGenesUsable isGenesUsable
    #' @param externalDatabase externalDatabase
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `scientificName` = NULL, `commonName` = NULL, `ncbiId` = NULL, `isSpecies` = NULL, `isGenesUsable` = NULL, `externalDatabase` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`scientificName`)) {
        stopifnot(is.character(`scientificName`), length(`scientificName`) == 1)
        self$`scientificName` <- `scientificName`
      }
      if (!is.null(`commonName`)) {
        stopifnot(is.character(`commonName`), length(`commonName`) == 1)
        self$`commonName` <- `commonName`
      }
      if (!is.null(`ncbiId`)) {
        stopifnot(is.numeric(`ncbiId`), length(`ncbiId`) == 1)
        self$`ncbiId` <- `ncbiId`
      }
      if (!is.null(`isSpecies`)) {
        stopifnot(is.logical(`isSpecies`), length(`isSpecies`) == 1)
        self$`isSpecies` <- `isSpecies`
      }
      if (!is.null(`isGenesUsable`)) {
        stopifnot(is.logical(`isGenesUsable`), length(`isGenesUsable`) == 1)
        self$`isGenesUsable` <- `isGenesUsable`
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
    #' @return TaxonValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      TaxonValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        TaxonValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`scientificName`)) {
        TaxonValueObjectObject[["scientificName"]] <-
          self$`scientificName`
      }
      if (!is.null(self$`commonName`)) {
        TaxonValueObjectObject[["commonName"]] <-
          self$`commonName`
      }
      if (!is.null(self$`ncbiId`)) {
        TaxonValueObjectObject[["ncbiId"]] <-
          self$`ncbiId`
      }
      if (!is.null(self$`isSpecies`)) {
        TaxonValueObjectObject[["isSpecies"]] <-
          self$`isSpecies`
      }
      if (!is.null(self$`isGenesUsable`)) {
        TaxonValueObjectObject[["isGenesUsable"]] <-
          self$`isGenesUsable`
      }
      if (!is.null(self$`externalDatabase`)) {
        TaxonValueObjectObject[["externalDatabase"]] <-
          self$`externalDatabase`$toJSON()
      }

      TaxonValueObjectObject
    },
    #' Deserialize JSON string into an instance of TaxonValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of TaxonValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of TaxonValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`scientificName`)) {
        self$`scientificName` <- this_object$`scientificName`
      }
      if (!is.null(this_object$`commonName`)) {
        self$`commonName` <- this_object$`commonName`
      }
      if (!is.null(this_object$`ncbiId`)) {
        self$`ncbiId` <- this_object$`ncbiId`
      }
      if (!is.null(this_object$`isSpecies`)) {
        self$`isSpecies` <- this_object$`isSpecies`
      }
      if (!is.null(this_object$`isGenesUsable`)) {
        self$`isGenesUsable` <- this_object$`isGenesUsable`
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
    #' @return TaxonValueObject in JSON format
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
        if (!is.null(self$`scientificName`)) {
          sprintf(
          '"scientificName":
            "%s"
                    ',
          self$`scientificName`
          )
        },
        if (!is.null(self$`commonName`)) {
          sprintf(
          '"commonName":
            "%s"
                    ',
          self$`commonName`
          )
        },
        if (!is.null(self$`ncbiId`)) {
          sprintf(
          '"ncbiId":
            %d
                    ',
          self$`ncbiId`
          )
        },
        if (!is.null(self$`isSpecies`)) {
          sprintf(
          '"isSpecies":
            %s
                    ',
          tolower(self$`isSpecies`)
          )
        },
        if (!is.null(self$`isGenesUsable`)) {
          sprintf(
          '"isGenesUsable":
            %s
                    ',
          tolower(self$`isGenesUsable`)
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
    #' Deserialize JSON string into an instance of TaxonValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of TaxonValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of TaxonValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`scientificName` <- this_object$`scientificName`
      self$`commonName` <- this_object$`commonName`
      self$`ncbiId` <- this_object$`ncbiId`
      self$`isSpecies` <- this_object$`isSpecies`
      self$`isGenesUsable` <- this_object$`isGenesUsable`
      self$`externalDatabase` <- ExternalDatabaseValueObject$new()$fromJSON(jsonlite::toJSON(this_object$externalDatabase, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to TaxonValueObject
    #'
    #' @description
    #' Validate JSON input with respect to TaxonValueObject and throw an exception if invalid
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
    #' @return String representation of TaxonValueObject
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
TaxonValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
TaxonValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
TaxonValueObject$lock()

