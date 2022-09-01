#' Create a new GeneAlias
#'
#' @description
#' GeneAlias Class
#'
#' @docType class
#' @title GeneAlias
#' @description GeneAlias Class
#' @format An \code{R6Class} generator object
#' @field alias  character [optional]
#' @field id  integer [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
GeneAlias <- R6::R6Class(
  "GeneAlias",
  public = list(
    `alias` = NULL,
    `id` = NULL,
    #' Initialize a new GeneAlias class.
    #'
    #' @description
    #' Initialize a new GeneAlias class.
    #'
    #' @param alias alias
    #' @param id id
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `alias` = NULL, `id` = NULL, ...
    ) {
      if (!is.null(`alias`)) {
        stopifnot(is.character(`alias`), length(`alias`) == 1)
        self$`alias` <- `alias`
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
    #' @return GeneAlias in JSON format
    #' @keywords internal
    toJSON = function() {
      GeneAliasObject <- list()
      if (!is.null(self$`alias`)) {
        GeneAliasObject[["alias"]] <-
          self$`alias`
      }
      if (!is.null(self$`id`)) {
        GeneAliasObject[["id"]] <-
          self$`id`
      }

      GeneAliasObject
    },
    #' Deserialize JSON string into an instance of GeneAlias
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneAlias
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneAlias
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`alias`)) {
        self$`alias` <- this_object$`alias`
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
    #' @return GeneAlias in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`alias`)) {
          sprintf(
          '"alias":
            "%s"
                    ',
          self$`alias`
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
    #' Deserialize JSON string into an instance of GeneAlias
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneAlias
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneAlias
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`alias` <- this_object$`alias`
      self$`id` <- this_object$`id`
      self
    },
    #' Validate JSON input with respect to GeneAlias
    #'
    #' @description
    #' Validate JSON input with respect to GeneAlias and throw an exception if invalid
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
    #' @return String representation of GeneAlias
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
GeneAlias$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
GeneAlias$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
GeneAlias$lock()

