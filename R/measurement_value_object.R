#' Create a new MeasurementValueObject
#'
#' @description
#' MeasurementValueObject Class
#'
#' @docType class
#' @title MeasurementValueObject
#' @description MeasurementValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field value  character [optional]
#' @field unit  character [optional]
#' @field unitId  integer [optional]
#' @field type  character [optional]
#' @field representation  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
MeasurementValueObject <- R6::R6Class(
  "MeasurementValueObject",
  public = list(
    `id` = NULL,
    `value` = NULL,
    `unit` = NULL,
    `unitId` = NULL,
    `type` = NULL,
    `representation` = NULL,
    #' Initialize a new MeasurementValueObject class.
    #'
    #' @description
    #' Initialize a new MeasurementValueObject class.
    #'
    #' @param id id
    #' @param value value
    #' @param unit unit
    #' @param unitId unitId
    #' @param type type
    #' @param representation representation
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `value` = NULL, `unit` = NULL, `unitId` = NULL, `type` = NULL, `representation` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`value`)) {
        stopifnot(is.character(`value`), length(`value`) == 1)
        self$`value` <- `value`
      }
      if (!is.null(`unit`)) {
        stopifnot(is.character(`unit`), length(`unit`) == 1)
        self$`unit` <- `unit`
      }
      if (!is.null(`unitId`)) {
        stopifnot(is.numeric(`unitId`), length(`unitId`) == 1)
        self$`unitId` <- `unitId`
      }
      if (!is.null(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
        self$`type` <- `type`
      }
      if (!is.null(`representation`)) {
        stopifnot(is.character(`representation`), length(`representation`) == 1)
        self$`representation` <- `representation`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return MeasurementValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      MeasurementValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        MeasurementValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`value`)) {
        MeasurementValueObjectObject[["value"]] <-
          self$`value`
      }
      if (!is.null(self$`unit`)) {
        MeasurementValueObjectObject[["unit"]] <-
          self$`unit`
      }
      if (!is.null(self$`unitId`)) {
        MeasurementValueObjectObject[["unitId"]] <-
          self$`unitId`
      }
      if (!is.null(self$`type`)) {
        MeasurementValueObjectObject[["type"]] <-
          self$`type`
      }
      if (!is.null(self$`representation`)) {
        MeasurementValueObjectObject[["representation"]] <-
          self$`representation`
      }

      MeasurementValueObjectObject
    },
    #' Deserialize JSON string into an instance of MeasurementValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of MeasurementValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of MeasurementValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`value`)) {
        self$`value` <- this_object$`value`
      }
      if (!is.null(this_object$`unit`)) {
        self$`unit` <- this_object$`unit`
      }
      if (!is.null(this_object$`unitId`)) {
        self$`unitId` <- this_object$`unitId`
      }
      if (!is.null(this_object$`type`)) {
        self$`type` <- this_object$`type`
      }
      if (!is.null(this_object$`representation`)) {
        self$`representation` <- this_object$`representation`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return MeasurementValueObject in JSON format
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
        if (!is.null(self$`value`)) {
          sprintf(
          '"value":
            "%s"
                    ',
          self$`value`
          )
        },
        if (!is.null(self$`unit`)) {
          sprintf(
          '"unit":
            "%s"
                    ',
          self$`unit`
          )
        },
        if (!is.null(self$`unitId`)) {
          sprintf(
          '"unitId":
            %d
                    ',
          self$`unitId`
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
        if (!is.null(self$`representation`)) {
          sprintf(
          '"representation":
            "%s"
                    ',
          self$`representation`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of MeasurementValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of MeasurementValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of MeasurementValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`value` <- this_object$`value`
      self$`unit` <- this_object$`unit`
      self$`unitId` <- this_object$`unitId`
      self$`type` <- this_object$`type`
      self$`representation` <- this_object$`representation`
      self
    },
    #' Validate JSON input with respect to MeasurementValueObject
    #'
    #' @description
    #' Validate JSON input with respect to MeasurementValueObject and throw an exception if invalid
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
    #' @return String representation of MeasurementValueObject
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
MeasurementValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
MeasurementValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
MeasurementValueObject$lock()

