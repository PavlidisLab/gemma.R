#' Create a new PhenotypeAssociationPublication
#'
#' @description
#' PhenotypeAssociationPublication Class
#'
#' @docType class
#' @title PhenotypeAssociationPublication
#' @description PhenotypeAssociationPublication Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field type  character optional
#' @field citation  \link{BibliographicReference} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
PhenotypeAssociationPublication <- R6::R6Class(
  "PhenotypeAssociationPublication",
  public = list(
    `id` = NULL,
    `type` = NULL,
    `citation` = NULL,
    #' Initialize a new PhenotypeAssociationPublication class.
    #'
    #' @description
    #' Initialize a new PhenotypeAssociationPublication class.
    #'
    #' @param id id
    #' @param type type
    #' @param citation citation
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `type` = NULL, `citation` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
        self$`type` <- `type`
      }
      if (!is.null(`citation`)) {
        stopifnot(R6::is.R6(`citation`))
        self$`citation` <- `citation`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PhenotypeAssociationPublication in JSON format
    #' @keywords internal
    toJSON = function() {
      PhenotypeAssociationPublicationObject <- list()
      if (!is.null(self$`id`)) {
        PhenotypeAssociationPublicationObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`type`)) {
        PhenotypeAssociationPublicationObject[["type"]] <-
          self$`type`
      }
      if (!is.null(self$`citation`)) {
        PhenotypeAssociationPublicationObject[["citation"]] <-
          self$`citation`$toJSON()
      }

      PhenotypeAssociationPublicationObject
    },
    #' Deserialize JSON string into an instance of PhenotypeAssociationPublication
    #'
    #' @description
    #' Deserialize JSON string into an instance of PhenotypeAssociationPublication
    #'
    #' @param input_json the JSON input
    #' @return the instance of PhenotypeAssociationPublication
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`type`)) {
        self$`type` <- this_object$`type`
      }
      if (!is.null(this_object$`citation`)) {
        citation_object <- BibliographicReference$new()
        citation_object$fromJSON(jsonlite::toJSON(this_object$citation, auto_unbox = TRUE, digits = NA))
        self$`citation` <- citation_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PhenotypeAssociationPublication in JSON format
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
        if (!is.null(self$`type`)) {
          sprintf(
          '"type":
            "%s"
                    ',
          self$`type`
          )
        },
        if (!is.null(self$`citation`)) {
          sprintf(
          '"citation":
          %s
          ',
          jsonlite::toJSON(self$`citation`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of PhenotypeAssociationPublication
    #'
    #' @description
    #' Deserialize JSON string into an instance of PhenotypeAssociationPublication
    #'
    #' @param input_json the JSON input
    #' @return the instance of PhenotypeAssociationPublication
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`type` <- this_object$`type`
      self$`citation` <- BibliographicReference$new()$fromJSON(jsonlite::toJSON(this_object$citation, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to PhenotypeAssociationPublication
    #'
    #' @description
    #' Validate JSON input with respect to PhenotypeAssociationPublication and throw an exception if invalid
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
    #' @return String representation of PhenotypeAssociationPublication
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
PhenotypeAssociationPublication$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
PhenotypeAssociationPublication$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
PhenotypeAssociationPublication$lock()

