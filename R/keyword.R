#' Create a new Keyword
#'
#' @description
#' Keyword Class
#'
#' @docType class
#' @title Keyword
#' @description Keyword Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field isMajorTopic  character optional
#' @field term  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Keyword <- R6::R6Class(
  "Keyword",
  public = list(
    `id` = NULL,
    `isMajorTopic` = NULL,
    `term` = NULL,
    #' Initialize a new Keyword class.
    #'
    #' @description
    #' Initialize a new Keyword class.
    #'
    #' @param id id
    #' @param isMajorTopic isMajorTopic
    #' @param term term
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `isMajorTopic` = NULL, `term` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`isMajorTopic`)) {
        stopifnot(is.logical(`isMajorTopic`), length(`isMajorTopic`) == 1)
        self$`isMajorTopic` <- `isMajorTopic`
      }
      if (!is.null(`term`)) {
        stopifnot(is.character(`term`), length(`term`) == 1)
        self$`term` <- `term`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Keyword in JSON format
    #' @keywords internal
    toJSON = function() {
      KeywordObject <- list()
      if (!is.null(self$`id`)) {
        KeywordObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`isMajorTopic`)) {
        KeywordObject[["isMajorTopic"]] <-
          self$`isMajorTopic`
      }
      if (!is.null(self$`term`)) {
        KeywordObject[["term"]] <-
          self$`term`
      }

      KeywordObject
    },
    #' Deserialize JSON string into an instance of Keyword
    #'
    #' @description
    #' Deserialize JSON string into an instance of Keyword
    #'
    #' @param input_json the JSON input
    #' @return the instance of Keyword
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`isMajorTopic`)) {
        self$`isMajorTopic` <- this_object$`isMajorTopic`
      }
      if (!is.null(this_object$`term`)) {
        self$`term` <- this_object$`term`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Keyword in JSON format
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
        if (!is.null(self$`isMajorTopic`)) {
          sprintf(
          '"isMajorTopic":
            %s
                    ',
          tolower(self$`isMajorTopic`)
          )
        },
        if (!is.null(self$`term`)) {
          sprintf(
          '"term":
            "%s"
                    ',
          self$`term`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Keyword
    #'
    #' @description
    #' Deserialize JSON string into an instance of Keyword
    #'
    #' @param input_json the JSON input
    #' @return the instance of Keyword
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`isMajorTopic` <- this_object$`isMajorTopic`
      self$`term` <- this_object$`term`
      self
    },
    #' Validate JSON input with respect to Keyword
    #'
    #' @description
    #' Validate JSON input with respect to Keyword and throw an exception if invalid
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
    #' @return String representation of Keyword
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
Keyword$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
Keyword$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
Keyword$lock()

