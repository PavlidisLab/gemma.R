#' Create a new ExternalDatabaseValueObject
#'
#' @description
#' ExternalDatabaseValueObject Class
#'
#' @docType class
#' @title ExternalDatabaseValueObject
#' @description ExternalDatabaseValueObject Class
#' @format An \code{R6Class} generator object
#' @field name  character optional
#' @field id  integer optional
#' @field checked  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ExternalDatabaseValueObject <- R6::R6Class(
  "ExternalDatabaseValueObject",
  public = list(
    `name` = NULL,
    `id` = NULL,
    `checked` = NULL,
    #' Initialize a new ExternalDatabaseValueObject class.
    #'
    #' @description
    #' Initialize a new ExternalDatabaseValueObject class.
    #'
    #' @param name name
    #' @param id id
    #' @param checked checked
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `id` = NULL, `checked` = NULL, ...
    ) {
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`checked`)) {
        stopifnot(is.logical(`checked`), length(`checked`) == 1)
        self$`checked` <- `checked`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExternalDatabaseValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ExternalDatabaseValueObjectObject <- list()
      if (!is.null(self$`name`)) {
        ExternalDatabaseValueObjectObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`id`)) {
        ExternalDatabaseValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`checked`)) {
        ExternalDatabaseValueObjectObject[["checked"]] <-
          self$`checked`
      }

      ExternalDatabaseValueObjectObject
    },
    #' Deserialize JSON string into an instance of ExternalDatabaseValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExternalDatabaseValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExternalDatabaseValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`checked`)) {
        self$`checked` <- this_object$`checked`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExternalDatabaseValueObject in JSON format
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
        if (!is.null(self$`id`)) {
          sprintf(
          '"id":
            %d
                    ',
          self$`id`
          )
        },
        if (!is.null(self$`checked`)) {
          sprintf(
          '"checked":
            %s
                    ',
          tolower(self$`checked`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ExternalDatabaseValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExternalDatabaseValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExternalDatabaseValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`id` <- this_object$`id`
      self$`checked` <- this_object$`checked`
      self
    },
    #' Validate JSON input with respect to ExternalDatabaseValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ExternalDatabaseValueObject and throw an exception if invalid
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
    #' @return String representation of ExternalDatabaseValueObject
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
ExternalDatabaseValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ExternalDatabaseValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ExternalDatabaseValueObject$lock()

