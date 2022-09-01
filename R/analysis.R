#' Create a new Analysis
#'
#' @description
#' Analysis Class
#'
#' @docType class
#' @title Analysis
#' @description Analysis Class
#' @format An \code{R6Class} generator object
#' @field name  character optional
#' @field description  character optional
#' @field id  integer optional
#' @field protocol  \link{Protocol} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Analysis <- R6::R6Class(
  "Analysis",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `protocol` = NULL,
    #' Initialize a new Analysis class.
    #'
    #' @description
    #' Initialize a new Analysis class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param protocol protocol
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `protocol` = NULL, ...
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
      if (!is.null(`protocol`)) {
        stopifnot(R6::is.R6(`protocol`))
        self$`protocol` <- `protocol`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Analysis in JSON format
    #' @keywords internal
    toJSON = function() {
      AnalysisObject <- list()
      if (!is.null(self$`name`)) {
        AnalysisObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        AnalysisObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        AnalysisObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`protocol`)) {
        AnalysisObject[["protocol"]] <-
          self$`protocol`$toJSON()
      }

      AnalysisObject
    },
    #' Deserialize JSON string into an instance of Analysis
    #'
    #' @description
    #' Deserialize JSON string into an instance of Analysis
    #'
    #' @param input_json the JSON input
    #' @return the instance of Analysis
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
      if (!is.null(this_object$`protocol`)) {
        protocol_object <- Protocol$new()
        protocol_object$fromJSON(jsonlite::toJSON(this_object$protocol, auto_unbox = TRUE, digits = NA))
        self$`protocol` <- protocol_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Analysis in JSON format
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
        if (!is.null(self$`protocol`)) {
          sprintf(
          '"protocol":
          %s
          ',
          jsonlite::toJSON(self$`protocol`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Analysis
    #'
    #' @description
    #' Deserialize JSON string into an instance of Analysis
    #'
    #' @param input_json the JSON input
    #' @return the instance of Analysis
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`protocol` <- Protocol$new()$fromJSON(jsonlite::toJSON(this_object$protocol, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to Analysis
    #'
    #' @description
    #' Validate JSON input with respect to Analysis and throw an exception if invalid
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
    #' @return String representation of Analysis
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
Analysis$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
Analysis$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
Analysis$lock()

