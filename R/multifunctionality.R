#' Create a new Multifunctionality
#'
#' @description
#' Multifunctionality Class
#'
#' @docType class
#' @title Multifunctionality
#' @description Multifunctionality Class
#' @format An \code{R6Class} generator object
#' @field score  numeric [optional]
#' @field rank  numeric [optional]
#' @field numGoTerms  integer [optional]
#' @field id  integer [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Multifunctionality <- R6::R6Class(
  "Multifunctionality",
  public = list(
    `score` = NULL,
    `rank` = NULL,
    `numGoTerms` = NULL,
    `id` = NULL,
    #' Initialize a new Multifunctionality class.
    #'
    #' @description
    #' Initialize a new Multifunctionality class.
    #'
    #' @param score score
    #' @param rank rank
    #' @param numGoTerms numGoTerms
    #' @param id id
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `score` = NULL, `rank` = NULL, `numGoTerms` = NULL, `id` = NULL, ...
    ) {
      if (!is.null(`score`)) {
        stopifnot(is.numeric(`score`), length(`score`) == 1)
        self$`score` <- `score`
      }
      if (!is.null(`rank`)) {
        stopifnot(is.numeric(`rank`), length(`rank`) == 1)
        self$`rank` <- `rank`
      }
      if (!is.null(`numGoTerms`)) {
        stopifnot(is.numeric(`numGoTerms`), length(`numGoTerms`) == 1)
        self$`numGoTerms` <- `numGoTerms`
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
    #' @return Multifunctionality in JSON format
    #' @keywords internal
    toJSON = function() {
      MultifunctionalityObject <- list()
      if (!is.null(self$`score`)) {
        MultifunctionalityObject[["score"]] <-
          self$`score`
      }
      if (!is.null(self$`rank`)) {
        MultifunctionalityObject[["rank"]] <-
          self$`rank`
      }
      if (!is.null(self$`numGoTerms`)) {
        MultifunctionalityObject[["numGoTerms"]] <-
          self$`numGoTerms`
      }
      if (!is.null(self$`id`)) {
        MultifunctionalityObject[["id"]] <-
          self$`id`
      }

      MultifunctionalityObject
    },
    #' Deserialize JSON string into an instance of Multifunctionality
    #'
    #' @description
    #' Deserialize JSON string into an instance of Multifunctionality
    #'
    #' @param input_json the JSON input
    #' @return the instance of Multifunctionality
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`score`)) {
        self$`score` <- this_object$`score`
      }
      if (!is.null(this_object$`rank`)) {
        self$`rank` <- this_object$`rank`
      }
      if (!is.null(this_object$`numGoTerms`)) {
        self$`numGoTerms` <- this_object$`numGoTerms`
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
    #' @return Multifunctionality in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`score`)) {
          sprintf(
          '"score":
            %d
                    ',
          self$`score`
          )
        },
        if (!is.null(self$`rank`)) {
          sprintf(
          '"rank":
            %d
                    ',
          self$`rank`
          )
        },
        if (!is.null(self$`numGoTerms`)) {
          sprintf(
          '"numGoTerms":
            %d
                    ',
          self$`numGoTerms`
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
    #' Deserialize JSON string into an instance of Multifunctionality
    #'
    #' @description
    #' Deserialize JSON string into an instance of Multifunctionality
    #'
    #' @param input_json the JSON input
    #' @return the instance of Multifunctionality
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`score` <- this_object$`score`
      self$`rank` <- this_object$`rank`
      self$`numGoTerms` <- this_object$`numGoTerms`
      self$`id` <- this_object$`id`
      self
    },
    #' Validate JSON input with respect to Multifunctionality
    #'
    #' @description
    #' Validate JSON input with respect to Multifunctionality and throw an exception if invalid
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
    #' @return String representation of Multifunctionality
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
Multifunctionality$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
Multifunctionality$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
Multifunctionality$lock()

