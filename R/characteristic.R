#' Create a new Characteristic
#'
#' @description
#' Characteristic Class
#'
#' @docType class
#' @title Characteristic
#' @description Characteristic Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field description  character [optional]
#' @field id  integer [optional]
#' @field category  character [optional]
#' @field categoryUri  character [optional]
#' @field evidenceCode  \link{GOEvidenceCode} [optional]
#' @field originalValue  character [optional]
#' @field value  character [optional]
#' @field valueUri  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Characteristic <- R6::R6Class(
  "Characteristic",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `category` = NULL,
    `categoryUri` = NULL,
    `evidenceCode` = NULL,
    `originalValue` = NULL,
    `value` = NULL,
    `valueUri` = NULL,
    #' Initialize a new Characteristic class.
    #'
    #' @description
    #' Initialize a new Characteristic class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param category category
    #' @param categoryUri categoryUri
    #' @param evidenceCode evidenceCode
    #' @param originalValue originalValue
    #' @param value value
    #' @param valueUri valueUri
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `category` = NULL, `categoryUri` = NULL, `evidenceCode` = NULL, `originalValue` = NULL, `value` = NULL, `valueUri` = NULL, ...
    ) {
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`category`)) {
        stopifnot(is.character(`category`), length(`category`) == 1)
        self$`category` <- `category`
      }
      if (!is.null(`categoryUri`)) {
        stopifnot(is.character(`categoryUri`), length(`categoryUri`) == 1)
        self$`categoryUri` <- `categoryUri`
      }
      if (!is.null(`evidenceCode`)) {
        stopifnot(R6::is.R6(`evidenceCode`))
        self$`evidenceCode` <- `evidenceCode`
      }
      if (!is.null(`originalValue`)) {
        stopifnot(is.character(`originalValue`), length(`originalValue`) == 1)
        self$`originalValue` <- `originalValue`
      }
      if (!is.null(`value`)) {
        stopifnot(is.character(`value`), length(`value`) == 1)
        self$`value` <- `value`
      }
      if (!is.null(`valueUri`)) {
        stopifnot(is.character(`valueUri`), length(`valueUri`) == 1)
        self$`valueUri` <- `valueUri`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Characteristic in JSON format
    #' @keywords internal
    toJSON = function() {
      CharacteristicObject <- list()
      if (!is.null(self$`name`)) {
        CharacteristicObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        CharacteristicObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        CharacteristicObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`category`)) {
        CharacteristicObject[["category"]] <-
          self$`category`
      }
      if (!is.null(self$`categoryUri`)) {
        CharacteristicObject[["categoryUri"]] <-
          self$`categoryUri`
      }
      if (!is.null(self$`evidenceCode`)) {
        CharacteristicObject[["evidenceCode"]] <-
          self$`evidenceCode`$toJSON()
      }
      if (!is.null(self$`originalValue`)) {
        CharacteristicObject[["originalValue"]] <-
          self$`originalValue`
      }
      if (!is.null(self$`value`)) {
        CharacteristicObject[["value"]] <-
          self$`value`
      }
      if (!is.null(self$`valueUri`)) {
        CharacteristicObject[["valueUri"]] <-
          self$`valueUri`
      }

      CharacteristicObject
    },
    #' Deserialize JSON string into an instance of Characteristic
    #'
    #' @description
    #' Deserialize JSON string into an instance of Characteristic
    #'
    #' @param input_json the JSON input
    #' @return the instance of Characteristic
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`category`)) {
        self$`category` <- this_object$`category`
      }
      if (!is.null(this_object$`categoryUri`)) {
        self$`categoryUri` <- this_object$`categoryUri`
      }
      if (!is.null(this_object$`evidenceCode`)) {
        evidencecode_object <- GOEvidenceCode$new()
        evidencecode_object$fromJSON(jsonlite::toJSON(this_object$evidenceCode, auto_unbox = TRUE, digits = NA))
        self$`evidenceCode` <- evidencecode_object
      }
      if (!is.null(this_object$`originalValue`)) {
        self$`originalValue` <- this_object$`originalValue`
      }
      if (!is.null(this_object$`value`)) {
        self$`value` <- this_object$`value`
      }
      if (!is.null(this_object$`valueUri`)) {
        self$`valueUri` <- this_object$`valueUri`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Characteristic in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`name`)) {
          sprintf(
          '"name":
            "%s"
                    ',
          self$`name`
          )
        },
        if (!is.null(self$`description`)) {
          sprintf(
          '"description":
            "%s"
                    ',
          self$`description`
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
        if (!is.null(self$`category`)) {
          sprintf(
          '"category":
            "%s"
                    ',
          self$`category`
          )
        },
        if (!is.null(self$`categoryUri`)) {
          sprintf(
          '"categoryUri":
            "%s"
                    ',
          self$`categoryUri`
          )
        },
        if (!is.null(self$`evidenceCode`)) {
          sprintf(
          '"evidenceCode":
          %s
          ',
          jsonlite::toJSON(self$`evidenceCode`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`originalValue`)) {
          sprintf(
          '"originalValue":
            "%s"
                    ',
          self$`originalValue`
          )
        },
        if (!is.null(self$`value`)) {
          sprintf(
          '"value":
            "%s"
                    ',
          self$`value`
          )
        },
        if (!is.null(self$`valueUri`)) {
          sprintf(
          '"valueUri":
            "%s"
                    ',
          self$`valueUri`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Characteristic
    #'
    #' @description
    #' Deserialize JSON string into an instance of Characteristic
    #'
    #' @param input_json the JSON input
    #' @return the instance of Characteristic
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`category` <- this_object$`category`
      self$`categoryUri` <- this_object$`categoryUri`
      self$`evidenceCode` <- GOEvidenceCode$new()$fromJSON(jsonlite::toJSON(this_object$evidenceCode, auto_unbox = TRUE, digits = NA))
      self$`originalValue` <- this_object$`originalValue`
      self$`value` <- this_object$`value`
      self$`valueUri` <- this_object$`valueUri`
      self
    },
    #' Validate JSON input with respect to Characteristic
    #'
    #' @description
    #' Validate JSON input with respect to Characteristic and throw an exception if invalid
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
    #' @return String representation of Characteristic
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
Characteristic$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
Characteristic$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
Characteristic$lock()

