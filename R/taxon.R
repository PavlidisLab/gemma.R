#' Create a new Taxon
#'
#' @description
#' Taxon Class
#'
#' @docType class
#' @title Taxon
#' @description Taxon Class
#' @format An \code{R6Class} generator object
#' @field scientificName  character [optional]
#' @field commonName  character [optional]
#' @field ncbiId  integer [optional]
#' @field isGenesUsable  character [optional]
#' @field secondaryNcbiId  integer [optional]
#' @field id  integer [optional]
#' @field externalDatabase  \link{ExternalDatabase} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Taxon <- R6::R6Class(
  "Taxon",
  public = list(
    `scientificName` = NULL,
    `commonName` = NULL,
    `ncbiId` = NULL,
    `isGenesUsable` = NULL,
    `secondaryNcbiId` = NULL,
    `id` = NULL,
    `externalDatabase` = NULL,
    #' Initialize a new Taxon class.
    #'
    #' @description
    #' Initialize a new Taxon class.
    #'
    #' @param scientificName scientificName
    #' @param commonName commonName
    #' @param ncbiId ncbiId
    #' @param isGenesUsable isGenesUsable
    #' @param secondaryNcbiId secondaryNcbiId
    #' @param id id
    #' @param externalDatabase externalDatabase
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `scientificName` = NULL, `commonName` = NULL, `ncbiId` = NULL, `isGenesUsable` = NULL, `secondaryNcbiId` = NULL, `id` = NULL, `externalDatabase` = NULL, ...
    ) {
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
      if (!is.null(`isGenesUsable`)) {
        stopifnot(is.logical(`isGenesUsable`), length(`isGenesUsable`) == 1)
        self$`isGenesUsable` <- `isGenesUsable`
      }
      if (!is.null(`secondaryNcbiId`)) {
        stopifnot(is.numeric(`secondaryNcbiId`), length(`secondaryNcbiId`) == 1)
        self$`secondaryNcbiId` <- `secondaryNcbiId`
      }
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
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
    #' @return Taxon in JSON format
    #' @keywords internal
    toJSON = function() {
      TaxonObject <- list()
      if (!is.null(self$`scientificName`)) {
        TaxonObject[["scientificName"]] <-
          self$`scientificName`
      }
      if (!is.null(self$`commonName`)) {
        TaxonObject[["commonName"]] <-
          self$`commonName`
      }
      if (!is.null(self$`ncbiId`)) {
        TaxonObject[["ncbiId"]] <-
          self$`ncbiId`
      }
      if (!is.null(self$`isGenesUsable`)) {
        TaxonObject[["isGenesUsable"]] <-
          self$`isGenesUsable`
      }
      if (!is.null(self$`secondaryNcbiId`)) {
        TaxonObject[["secondaryNcbiId"]] <-
          self$`secondaryNcbiId`
      }
      if (!is.null(self$`id`)) {
        TaxonObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`externalDatabase`)) {
        TaxonObject[["externalDatabase"]] <-
          self$`externalDatabase`$toJSON()
      }

      TaxonObject
    },
    #' Deserialize JSON string into an instance of Taxon
    #'
    #' @description
    #' Deserialize JSON string into an instance of Taxon
    #'
    #' @param input_json the JSON input
    #' @return the instance of Taxon
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`scientificName`)) {
        self$`scientificName` <- this_object$`scientificName`
      }
      if (!is.null(this_object$`commonName`)) {
        self$`commonName` <- this_object$`commonName`
      }
      if (!is.null(this_object$`ncbiId`)) {
        self$`ncbiId` <- this_object$`ncbiId`
      }
      if (!is.null(this_object$`isGenesUsable`)) {
        self$`isGenesUsable` <- this_object$`isGenesUsable`
      }
      if (!is.null(this_object$`secondaryNcbiId`)) {
        self$`secondaryNcbiId` <- this_object$`secondaryNcbiId`
      }
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`externalDatabase`)) {
        externaldatabase_object <- ExternalDatabase$new()
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
    #' @return Taxon in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
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
        if (!is.null(self$`isGenesUsable`)) {
          sprintf(
          '"isGenesUsable":
            %s
                    ',
          tolower(self$`isGenesUsable`)
          )
        },
        if (!is.null(self$`secondaryNcbiId`)) {
          sprintf(
          '"secondaryNcbiId":
            %d
                    ',
          self$`secondaryNcbiId`
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Taxon
    #'
    #' @description
    #' Deserialize JSON string into an instance of Taxon
    #'
    #' @param input_json the JSON input
    #' @return the instance of Taxon
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`scientificName` <- this_object$`scientificName`
      self$`commonName` <- this_object$`commonName`
      self$`ncbiId` <- this_object$`ncbiId`
      self$`isGenesUsable` <- this_object$`isGenesUsable`
      self$`secondaryNcbiId` <- this_object$`secondaryNcbiId`
      self$`id` <- this_object$`id`
      self$`externalDatabase` <- ExternalDatabase$new()$fromJSON(jsonlite::toJSON(this_object$externalDatabase, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to Taxon
    #'
    #' @description
    #' Validate JSON input with respect to Taxon and throw an exception if invalid
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
    #' @return String representation of Taxon
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
Taxon$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
Taxon$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
Taxon$lock()

