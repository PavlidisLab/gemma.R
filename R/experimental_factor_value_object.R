#' Create a new ExperimentalFactorValueObject
#'
#' @description
#' ExperimentalFactorValueObject Class
#'
#' @docType class
#' @title ExperimentalFactorValueObject
#' @description ExperimentalFactorValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field category  character [optional]
#' @field categoryUri  character [optional]
#' @field description  character [optional]
#' @field factorValues  character [optional]
#' @field name  character [optional]
#' @field numValues  integer [optional]
#' @field type  character [optional]
#' @field values  list(\link{FactorValueValueObject}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ExperimentalFactorValueObject <- R6::R6Class(
  "ExperimentalFactorValueObject",
  public = list(
    `id` = NULL,
    `category` = NULL,
    `categoryUri` = NULL,
    `description` = NULL,
    `factorValues` = NULL,
    `name` = NULL,
    `numValues` = NULL,
    `type` = NULL,
    `values` = NULL,
    #' Initialize a new ExperimentalFactorValueObject class.
    #'
    #' @description
    #' Initialize a new ExperimentalFactorValueObject class.
    #'
    #' @param id id
    #' @param category category
    #' @param categoryUri categoryUri
    #' @param description description
    #' @param factorValues factorValues
    #' @param name name
    #' @param numValues numValues
    #' @param type type
    #' @param values values
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `category` = NULL, `categoryUri` = NULL, `description` = NULL, `factorValues` = NULL, `name` = NULL, `numValues` = NULL, `type` = NULL, `values` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`category`)) {
        stopifnot(is.character(`category`), length(`category`) == 1)
        self$`category` <- `category`
      }
      if (!is.null(`categoryUri`)) {
        stopifnot(is.character(`categoryUri`), length(`categoryUri`) == 1)
        self$`categoryUri` <- `categoryUri`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`factorValues`)) {
        stopifnot(is.character(`factorValues`), length(`factorValues`) == 1)
        self$`factorValues` <- `factorValues`
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`numValues`)) {
        stopifnot(is.numeric(`numValues`), length(`numValues`) == 1)
        self$`numValues` <- `numValues`
      }
      if (!is.null(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
        self$`type` <- `type`
      }
      if (!is.null(`values`)) {
        stopifnot(is.vector(`values`), length(`values`) != 0)
        sapply(`values`, function(x) stopifnot(R6::is.R6(x)))
        self$`values` <- `values`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExperimentalFactorValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ExperimentalFactorValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        ExperimentalFactorValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`category`)) {
        ExperimentalFactorValueObjectObject[["category"]] <-
          self$`category`
      }
      if (!is.null(self$`categoryUri`)) {
        ExperimentalFactorValueObjectObject[["categoryUri"]] <-
          self$`categoryUri`
      }
      if (!is.null(self$`description`)) {
        ExperimentalFactorValueObjectObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`factorValues`)) {
        ExperimentalFactorValueObjectObject[["factorValues"]] <-
          self$`factorValues`
      }
      if (!is.null(self$`name`)) {
        ExperimentalFactorValueObjectObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`numValues`)) {
        ExperimentalFactorValueObjectObject[["numValues"]] <-
          self$`numValues`
      }
      if (!is.null(self$`type`)) {
        ExperimentalFactorValueObjectObject[["type"]] <-
          self$`type`
      }
      if (!is.null(self$`values`)) {
        ExperimentalFactorValueObjectObject[["values"]] <-
          lapply(self$`values`, function(x) x$toJSON())
      }

      ExperimentalFactorValueObjectObject
    },
    #' Deserialize JSON string into an instance of ExperimentalFactorValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExperimentalFactorValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExperimentalFactorValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`category`)) {
        self$`category` <- this_object$`category`
      }
      if (!is.null(this_object$`categoryUri`)) {
        self$`categoryUri` <- this_object$`categoryUri`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`factorValues`)) {
        self$`factorValues` <- this_object$`factorValues`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`numValues`)) {
        self$`numValues` <- this_object$`numValues`
      }
      if (!is.null(this_object$`type`)) {
        self$`type` <- this_object$`type`
      }
      if (!is.null(this_object$`values`)) {
        self$`values` <- ApiClient$new()$deserializeObj(this_object$`values`, "array[FactorValueValueObject]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExperimentalFactorValueObject in JSON format
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
        if (!is.null(self$`category`)) {
          sprintf(
          '"category":
            "%s"
                    ',
          self$`category`
          )
        },
        if (!is.null(self$`categoryUri`)) {
          sprintf(
          '"categoryUri":
            "%s"
                    ',
          self$`categoryUri`
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
        if (!is.null(self$`factorValues`)) {
          sprintf(
          '"factorValues":
            "%s"
                    ',
          self$`factorValues`
          )
        },
        if (!is.null(self$`name`)) {
          sprintf(
          '"name":
            "%s"
                    ',
          self$`name`
          )
        },
        if (!is.null(self$`numValues`)) {
          sprintf(
          '"numValues":
            %d
                    ',
          self$`numValues`
          )
        },
        if (!is.null(self$`type`)) {
          sprintf(
          '"type":
            "%s"
                    ',
          self$`type`
          )
        },
        if (!is.null(self$`values`)) {
          sprintf(
          '"values":
          [%s]
',
          paste(sapply(self$`values`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ExperimentalFactorValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExperimentalFactorValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExperimentalFactorValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`category` <- this_object$`category`
      self$`categoryUri` <- this_object$`categoryUri`
      self$`description` <- this_object$`description`
      self$`factorValues` <- this_object$`factorValues`
      self$`name` <- this_object$`name`
      self$`numValues` <- this_object$`numValues`
      self$`type` <- this_object$`type`
      self$`values` <- ApiClient$new()$deserializeObj(this_object$`values`, "array[FactorValueValueObject]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to ExperimentalFactorValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ExperimentalFactorValueObject and throw an exception if invalid
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
    #' @return String representation of ExperimentalFactorValueObject
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
ExperimentalFactorValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ExperimentalFactorValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ExperimentalFactorValueObject$lock()

