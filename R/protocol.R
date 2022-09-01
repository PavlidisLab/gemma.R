#' Create a new Protocol
#'
#' @description
#' Protocol Class
#'
#' @docType class
#' @title Protocol
#' @description Protocol Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field description  character [optional]
#' @field id  integer [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Protocol <- R6::R6Class(
  "Protocol",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    #' Initialize a new Protocol class.
    #'
    #' @description
    #' Initialize a new Protocol class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, ...
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
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Protocol in JSON format
    #' @keywords internal
    toJSON = function() {
      ProtocolObject <- list()
      if (!is.null(self$`name`)) {
        ProtocolObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        ProtocolObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        ProtocolObject[["id"]] <-
          self$`id`
      }

      ProtocolObject
    },
    #' Deserialize JSON string into an instance of Protocol
    #'
    #' @description
    #' Deserialize JSON string into an instance of Protocol
    #'
    #' @param input_json the JSON input
    #' @return the instance of Protocol
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
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Protocol in JSON format
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Protocol
    #'
    #' @description
    #' Deserialize JSON string into an instance of Protocol
    #'
    #' @param input_json the JSON input
    #' @return the instance of Protocol
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self
    },
    #' Validate JSON input with respect to Protocol
    #'
    #' @description
    #' Validate JSON input with respect to Protocol and throw an exception if invalid
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
    #' @return String representation of Protocol
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
Protocol$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
Protocol$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
Protocol$lock()

