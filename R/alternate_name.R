#' Create a new AlternateName
#'
#' @description
#' AlternateName Class
#'
#' @docType class
#' @title AlternateName
#' @description AlternateName Class
#' @format An \code{R6Class} generator object
#' @field name  character optional
#' @field id  integer optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
AlternateName <- R6::R6Class(
  "AlternateName",
  public = list(
    `name` = NULL,
    `id` = NULL,
    #' Initialize a new AlternateName class.
    #'
    #' @description
    #' Initialize a new AlternateName class.
    #'
    #' @param name name
    #' @param id id
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `id` = NULL, ...
    ) {
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
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
    #' @return AlternateName in JSON format
    #' @keywords internal
    toJSON = function() {
      AlternateNameObject <- list()
      if (!is.null(self$`name`)) {
        AlternateNameObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`id`)) {
        AlternateNameObject[["id"]] <-
          self$`id`
      }

      AlternateNameObject
    },
    #' Deserialize JSON string into an instance of AlternateName
    #'
    #' @description
    #' Deserialize JSON string into an instance of AlternateName
    #'
    #' @param input_json the JSON input
    #' @return the instance of AlternateName
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
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
    #' @return AlternateName in JSON format
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of AlternateName
    #'
    #' @description
    #' Deserialize JSON string into an instance of AlternateName
    #'
    #' @param input_json the JSON input
    #' @return the instance of AlternateName
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`id` <- this_object$`id`
      self
    },
    #' Validate JSON input with respect to AlternateName
    #'
    #' @description
    #' Validate JSON input with respect to AlternateName and throw an exception if invalid
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
    #' @return String representation of AlternateName
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
AlternateName$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
AlternateName$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
AlternateName$lock()

