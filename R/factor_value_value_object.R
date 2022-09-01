#' Create a new FactorValueValueObject
#'
#' @description
#' FactorValueValueObject Class
#'
#' @docType class
#' @title FactorValueValueObject
#' @description FactorValueValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field category  character [optional]
#' @field categoryUri  character [optional]
#' @field description  character [optional]
#' @field value  character [optional]
#' @field valueUri  character [optional]
#' @field charId  integer [optional]
#' @field factorId  integer [optional]
#' @field isBaseline  character [optional]
#' @field measurement  character [optional]
#' @field factorValue  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
FactorValueValueObject <- R6::R6Class(
  "FactorValueValueObject",
  public = list(
    `id` = NULL,
    `category` = NULL,
    `categoryUri` = NULL,
    `description` = NULL,
    `value` = NULL,
    `valueUri` = NULL,
    `charId` = NULL,
    `factorId` = NULL,
    `isBaseline` = NULL,
    `measurement` = NULL,
    `factorValue` = NULL,
    #' Initialize a new FactorValueValueObject class.
    #'
    #' @description
    #' Initialize a new FactorValueValueObject class.
    #'
    #' @param id id
    #' @param category category
    #' @param categoryUri categoryUri
    #' @param description description
    #' @param value value
    #' @param valueUri valueUri
    #' @param charId charId
    #' @param factorId factorId
    #' @param isBaseline isBaseline
    #' @param measurement measurement
    #' @param factorValue factorValue
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `category` = NULL, `categoryUri` = NULL, `description` = NULL, `value` = NULL, `valueUri` = NULL, `charId` = NULL, `factorId` = NULL, `isBaseline` = NULL, `measurement` = NULL, `factorValue` = NULL, ...
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
      if (!is.null(`value`)) {
        stopifnot(is.character(`value`), length(`value`) == 1)
        self$`value` <- `value`
      }
      if (!is.null(`valueUri`)) {
        stopifnot(is.character(`valueUri`), length(`valueUri`) == 1)
        self$`valueUri` <- `valueUri`
      }
      if (!is.null(`charId`)) {
        stopifnot(is.numeric(`charId`), length(`charId`) == 1)
        self$`charId` <- `charId`
      }
      if (!is.null(`factorId`)) {
        stopifnot(is.numeric(`factorId`), length(`factorId`) == 1)
        self$`factorId` <- `factorId`
      }
      if (!is.null(`isBaseline`)) {
        stopifnot(is.logical(`isBaseline`), length(`isBaseline`) == 1)
        self$`isBaseline` <- `isBaseline`
      }
      if (!is.null(`measurement`)) {
        stopifnot(is.logical(`measurement`), length(`measurement`) == 1)
        self$`measurement` <- `measurement`
      }
      if (!is.null(`factorValue`)) {
        stopifnot(is.character(`factorValue`), length(`factorValue`) == 1)
        self$`factorValue` <- `factorValue`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return FactorValueValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      FactorValueValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        FactorValueValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`category`)) {
        FactorValueValueObjectObject[["category"]] <-
          self$`category`
      }
      if (!is.null(self$`categoryUri`)) {
        FactorValueValueObjectObject[["categoryUri"]] <-
          self$`categoryUri`
      }
      if (!is.null(self$`description`)) {
        FactorValueValueObjectObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`value`)) {
        FactorValueValueObjectObject[["value"]] <-
          self$`value`
      }
      if (!is.null(self$`valueUri`)) {
        FactorValueValueObjectObject[["valueUri"]] <-
          self$`valueUri`
      }
      if (!is.null(self$`charId`)) {
        FactorValueValueObjectObject[["charId"]] <-
          self$`charId`
      }
      if (!is.null(self$`factorId`)) {
        FactorValueValueObjectObject[["factorId"]] <-
          self$`factorId`
      }
      if (!is.null(self$`isBaseline`)) {
        FactorValueValueObjectObject[["isBaseline"]] <-
          self$`isBaseline`
      }
      if (!is.null(self$`measurement`)) {
        FactorValueValueObjectObject[["measurement"]] <-
          self$`measurement`
      }
      if (!is.null(self$`factorValue`)) {
        FactorValueValueObjectObject[["factorValue"]] <-
          self$`factorValue`
      }

      FactorValueValueObjectObject
    },
    #' Deserialize JSON string into an instance of FactorValueValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of FactorValueValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of FactorValueValueObject
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
      if (!is.null(this_object$`value`)) {
        self$`value` <- this_object$`value`
      }
      if (!is.null(this_object$`valueUri`)) {
        self$`valueUri` <- this_object$`valueUri`
      }
      if (!is.null(this_object$`charId`)) {
        self$`charId` <- this_object$`charId`
      }
      if (!is.null(this_object$`factorId`)) {
        self$`factorId` <- this_object$`factorId`
      }
      if (!is.null(this_object$`isBaseline`)) {
        self$`isBaseline` <- this_object$`isBaseline`
      }
      if (!is.null(this_object$`measurement`)) {
        self$`measurement` <- this_object$`measurement`
      }
      if (!is.null(this_object$`factorValue`)) {
        self$`factorValue` <- this_object$`factorValue`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return FactorValueValueObject in JSON format
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
        if (!is.null(self$`value`)) {
          sprintf(
          '"value":
            "%s"
                    ',
          self$`value`
          )
        },
        if (!is.null(self$`valueUri`)) {
          sprintf(
          '"valueUri":
            "%s"
                    ',
          self$`valueUri`
          )
        },
        if (!is.null(self$`charId`)) {
          sprintf(
          '"charId":
            %d
                    ',
          self$`charId`
          )
        },
        if (!is.null(self$`factorId`)) {
          sprintf(
          '"factorId":
            %d
                    ',
          self$`factorId`
          )
        },
        if (!is.null(self$`isBaseline`)) {
          sprintf(
          '"isBaseline":
            %s
                    ',
          tolower(self$`isBaseline`)
          )
        },
        if (!is.null(self$`measurement`)) {
          sprintf(
          '"measurement":
            %s
                    ',
          tolower(self$`measurement`)
          )
        },
        if (!is.null(self$`factorValue`)) {
          sprintf(
          '"factorValue":
            "%s"
                    ',
          self$`factorValue`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of FactorValueValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of FactorValueValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of FactorValueValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`category` <- this_object$`category`
      self$`categoryUri` <- this_object$`categoryUri`
      self$`description` <- this_object$`description`
      self$`value` <- this_object$`value`
      self$`valueUri` <- this_object$`valueUri`
      self$`charId` <- this_object$`charId`
      self$`factorId` <- this_object$`factorId`
      self$`isBaseline` <- this_object$`isBaseline`
      self$`measurement` <- this_object$`measurement`
      self$`factorValue` <- this_object$`factorValue`
      self
    },
    #' Validate JSON input with respect to FactorValueValueObject
    #'
    #' @description
    #' Validate JSON input with respect to FactorValueValueObject and throw an exception if invalid
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
    #' @return String representation of FactorValueValueObject
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
FactorValueValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
FactorValueValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
FactorValueValueObject$lock()

