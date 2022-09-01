#' Create a new Contact
#'
#' @description
#' Contact Class
#'
#' @docType class
#' @title Contact
#' @description Contact Class
#' @format An \code{R6Class} generator object
#' @field name  character optional
#' @field description  character optional
#' @field id  integer optional
#' @field email  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Contact <- R6::R6Class(
  "Contact",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `email` = NULL,
    #' Initialize a new Contact class.
    #'
    #' @description
    #' Initialize a new Contact class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param email email
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `email` = NULL, ...
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
      if (!is.null(`email`)) {
        stopifnot(is.character(`email`), length(`email`) == 1)
        self$`email` <- `email`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Contact in JSON format
    #' @keywords internal
    toJSON = function() {
      ContactObject <- list()
      if (!is.null(self$`name`)) {
        ContactObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        ContactObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        ContactObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`email`)) {
        ContactObject[["email"]] <-
          self$`email`
      }

      ContactObject
    },
    #' Deserialize JSON string into an instance of Contact
    #'
    #' @description
    #' Deserialize JSON string into an instance of Contact
    #'
    #' @param input_json the JSON input
    #' @return the instance of Contact
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
      if (!is.null(this_object$`email`)) {
        self$`email` <- this_object$`email`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Contact in JSON format
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
        if (!is.null(self$`email`)) {
          sprintf(
          '"email":
            "%s"
                    ',
          self$`email`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Contact
    #'
    #' @description
    #' Deserialize JSON string into an instance of Contact
    #'
    #' @param input_json the JSON input
    #' @return the instance of Contact
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`email` <- this_object$`email`
      self
    },
    #' Validate JSON input with respect to Contact
    #'
    #' @description
    #' Validate JSON input with respect to Contact and throw an exception if invalid
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
    #' @return String representation of Contact
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
Contact$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
Contact$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
Contact$lock()

