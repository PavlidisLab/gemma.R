#' Create a new CharacteristicBasicValueObject
#'
#' @description
#' CharacteristicBasicValueObject Class
#'
#' @docType class
#' @title CharacteristicBasicValueObject
#' @description CharacteristicBasicValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field value  character optional
#' @field valueUri  character optional
#' @field category  character optional
#' @field categoryUri  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
CharacteristicBasicValueObject <- R6::R6Class(
  "CharacteristicBasicValueObject",
  public = list(
    `id` = NULL,
    `value` = NULL,
    `valueUri` = NULL,
    `category` = NULL,
    `categoryUri` = NULL,
    #' Initialize a new CharacteristicBasicValueObject class.
    #'
    #' @description
    #' Initialize a new CharacteristicBasicValueObject class.
    #'
    #' @param id id
    #' @param value value
    #' @param valueUri valueUri
    #' @param category category
    #' @param categoryUri categoryUri
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `value` = NULL, `valueUri` = NULL, `category` = NULL, `categoryUri` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`value`)) {
        stopifnot(is.character(`value`), length(`value`) == 1)
        self$`value` <- `value`
      }
      if (!is.null(`valueUri`)) {
        stopifnot(is.character(`valueUri`), length(`valueUri`) == 1)
        self$`valueUri` <- `valueUri`
      }
      if (!is.null(`category`)) {
        stopifnot(is.character(`category`), length(`category`) == 1)
        self$`category` <- `category`
      }
      if (!is.null(`categoryUri`)) {
        stopifnot(is.character(`categoryUri`), length(`categoryUri`) == 1)
        self$`categoryUri` <- `categoryUri`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CharacteristicBasicValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      CharacteristicBasicValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        CharacteristicBasicValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`value`)) {
        CharacteristicBasicValueObjectObject[["value"]] <-
          self$`value`
      }
      if (!is.null(self$`valueUri`)) {
        CharacteristicBasicValueObjectObject[["valueUri"]] <-
          self$`valueUri`
      }
      if (!is.null(self$`category`)) {
        CharacteristicBasicValueObjectObject[["category"]] <-
          self$`category`
      }
      if (!is.null(self$`categoryUri`)) {
        CharacteristicBasicValueObjectObject[["categoryUri"]] <-
          self$`categoryUri`
      }

      CharacteristicBasicValueObjectObject
    },
    #' Deserialize JSON string into an instance of CharacteristicBasicValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of CharacteristicBasicValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of CharacteristicBasicValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`value`)) {
        self$`value` <- this_object$`value`
      }
      if (!is.null(this_object$`valueUri`)) {
        self$`valueUri` <- this_object$`valueUri`
      }
      if (!is.null(this_object$`category`)) {
        self$`category` <- this_object$`category`
      }
      if (!is.null(this_object$`categoryUri`)) {
        self$`categoryUri` <- this_object$`categoryUri`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CharacteristicBasicValueObject in JSON format
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of CharacteristicBasicValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of CharacteristicBasicValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of CharacteristicBasicValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`value` <- this_object$`value`
      self$`valueUri` <- this_object$`valueUri`
      self$`category` <- this_object$`category`
      self$`categoryUri` <- this_object$`categoryUri`
      self
    },
    #' Validate JSON input with respect to CharacteristicBasicValueObject
    #'
    #' @description
    #' Validate JSON input with respect to CharacteristicBasicValueObject and throw an exception if invalid
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
    #' @return String representation of CharacteristicBasicValueObject
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
CharacteristicBasicValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
CharacteristicBasicValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
CharacteristicBasicValueObject$lock()

