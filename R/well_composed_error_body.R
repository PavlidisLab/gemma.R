#' Create a new WellComposedErrorBody
#'
#' @description
#' WellComposedErrorBody Class
#'
#' @docType class
#' @title WellComposedErrorBody
#' @description WellComposedErrorBody Class
#' @format An \code{R6Class} generator object
#' @field code  integer [optional]
#' @field message  character [optional]
#' @field errors  named list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
WellComposedErrorBody <- R6::R6Class(
  "WellComposedErrorBody",
  public = list(
    `code` = NULL,
    `message` = NULL,
    `errors` = NULL,
    #' Initialize a new WellComposedErrorBody class.
    #'
    #' @description
    #' Initialize a new WellComposedErrorBody class.
    #'
    #' @param code code
    #' @param message message
    #' @param errors errors
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `code` = NULL, `message` = NULL, `errors` = NULL, ...
    ) {
      if (!is.null(`code`)) {
        stopifnot(is.numeric(`code`), length(`code`) == 1)
        self$`code` <- `code`
      }
      if (!is.null(`message`)) {
        stopifnot(is.character(`message`), length(`message`) == 1)
        self$`message` <- `message`
      }
      if (!is.null(`errors`)) {
        stopifnot(is.vector(`errors`), length(`errors`) != 0)
        sapply(`errors`, function(x) stopifnot(is.character(x)))
        self$`errors` <- `errors`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return WellComposedErrorBody in JSON format
    #' @keywords internal
    toJSON = function() {
      WellComposedErrorBodyObject <- list()
      if (!is.null(self$`code`)) {
        WellComposedErrorBodyObject[["code"]] <-
          self$`code`
      }
      if (!is.null(self$`message`)) {
        WellComposedErrorBodyObject[["message"]] <-
          self$`message`
      }
      if (!is.null(self$`errors`)) {
        WellComposedErrorBodyObject[["errors"]] <-
          self$`errors`
      }

      WellComposedErrorBodyObject
    },
    #' Deserialize JSON string into an instance of WellComposedErrorBody
    #'
    #' @description
    #' Deserialize JSON string into an instance of WellComposedErrorBody
    #'
    #' @param input_json the JSON input
    #' @return the instance of WellComposedErrorBody
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`code`)) {
        self$`code` <- this_object$`code`
      }
      if (!is.null(this_object$`message`)) {
        self$`message` <- this_object$`message`
      }
      if (!is.null(this_object$`errors`)) {
        self$`errors` <- ApiClient$new()$deserializeObj(this_object$`errors`, "map(character)", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return WellComposedErrorBody in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`code`)) {
          sprintf(
          '"code":
            %d
                    ',
          self$`code`
          )
        },
        if (!is.null(self$`message`)) {
          sprintf(
          '"message":
            "%s"
                    ',
          self$`message`
          )
        },
        if (!is.null(self$`errors`)) {
          sprintf(
          '"errors":
            "%s"
          ',
          jsonlite::toJSON(lapply(self$`errors`, function(x){ x }), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of WellComposedErrorBody
    #'
    #' @description
    #' Deserialize JSON string into an instance of WellComposedErrorBody
    #'
    #' @param input_json the JSON input
    #' @return the instance of WellComposedErrorBody
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`code` <- this_object$`code`
      self$`message` <- this_object$`message`
      self$`errors` <- ApiClient$new()$deserializeObj(this_object$`errors`, "map(character)", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to WellComposedErrorBody
    #'
    #' @description
    #' Validate JSON input with respect to WellComposedErrorBody and throw an exception if invalid
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
    #' @return String representation of WellComposedErrorBody
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
WellComposedErrorBody$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
WellComposedErrorBody$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
WellComposedErrorBody$lock()

