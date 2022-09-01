#' Create a new IdentifiableValueObject
#'
#' @description
#' IdentifiableValueObject Class
#'
#' @docType class
#' @title IdentifiableValueObject
#' @description IdentifiableValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
IdentifiableValueObject <- R6::R6Class(
  "IdentifiableValueObject",
  public = list(
    `id` = NULL,
    #' Initialize a new IdentifiableValueObject class.
    #'
    #' @description
    #' Initialize a new IdentifiableValueObject class.
    #'
    #' @param id id
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return IdentifiableValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      IdentifiableValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        IdentifiableValueObjectObject[["id"]] <-
          self$`id`
      }

      IdentifiableValueObjectObject
    },
    #' Deserialize JSON string into an instance of IdentifiableValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of IdentifiableValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of IdentifiableValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return IdentifiableValueObject in JSON format
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of IdentifiableValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of IdentifiableValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of IdentifiableValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self
    },
    #' Validate JSON input with respect to IdentifiableValueObject
    #'
    #' @description
    #' Validate JSON input with respect to IdentifiableValueObject and throw an exception if invalid
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
    #' @return String representation of IdentifiableValueObject
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
IdentifiableValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
IdentifiableValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
IdentifiableValueObject$lock()

