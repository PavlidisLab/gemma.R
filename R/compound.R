#' Create a new Compound
#'
#' @description
#' Compound Class
#'
#' @docType class
#' @title Compound
#' @description Compound Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field description  character [optional]
#' @field id  integer [optional]
#' @field registryNumber  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Compound <- R6::R6Class(
  "Compound",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `registryNumber` = NULL,
    #' Initialize a new Compound class.
    #'
    #' @description
    #' Initialize a new Compound class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param registryNumber registryNumber
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `registryNumber` = NULL, ...
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
      if (!is.null(`registryNumber`)) {
        stopifnot(is.character(`registryNumber`), length(`registryNumber`) == 1)
        self$`registryNumber` <- `registryNumber`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Compound in JSON format
    #' @keywords internal
    toJSON = function() {
      CompoundObject <- list()
      if (!is.null(self$`name`)) {
        CompoundObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        CompoundObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        CompoundObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`registryNumber`)) {
        CompoundObject[["registryNumber"]] <-
          self$`registryNumber`
      }

      CompoundObject
    },
    #' Deserialize JSON string into an instance of Compound
    #'
    #' @description
    #' Deserialize JSON string into an instance of Compound
    #'
    #' @param input_json the JSON input
    #' @return the instance of Compound
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
      if (!is.null(this_object$`registryNumber`)) {
        self$`registryNumber` <- this_object$`registryNumber`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Compound in JSON format
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
        if (!is.null(self$`registryNumber`)) {
          sprintf(
          '"registryNumber":
            "%s"
                    ',
          self$`registryNumber`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Compound
    #'
    #' @description
    #' Deserialize JSON string into an instance of Compound
    #'
    #' @param input_json the JSON input
    #' @return the instance of Compound
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`registryNumber` <- this_object$`registryNumber`
      self
    },
    #' Validate JSON input with respect to Compound
    #'
    #' @description
    #' Validate JSON input with respect to Compound and throw an exception if invalid
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
    #' @return String representation of Compound
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
Compound$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
Compound$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
Compound$lock()

