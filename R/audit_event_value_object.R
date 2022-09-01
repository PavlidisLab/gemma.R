#' Create a new AuditEventValueObject
#'
#' @description
#' AuditEventValueObject Class
#'
#' @docType class
#' @title AuditEventValueObject
#' @description AuditEventValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field performer  character optional
#' @field date  character optional
#' @field action  character optional
#' @field note  character optional
#' @field detail  character optional
#' @field eventType  \link{AuditEventType} optional
#' @field actionName  character optional
#' @field eventTypeName  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
AuditEventValueObject <- R6::R6Class(
  "AuditEventValueObject",
  public = list(
    `id` = NULL,
    `performer` = NULL,
    `date` = NULL,
    `action` = NULL,
    `note` = NULL,
    `detail` = NULL,
    `eventType` = NULL,
    `actionName` = NULL,
    `eventTypeName` = NULL,
    #' Initialize a new AuditEventValueObject class.
    #'
    #' @description
    #' Initialize a new AuditEventValueObject class.
    #'
    #' @param id id
    #' @param performer performer
    #' @param date date
    #' @param action action
    #' @param note note
    #' @param detail detail
    #' @param eventType eventType
    #' @param actionName actionName
    #' @param eventTypeName eventTypeName
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `performer` = NULL, `date` = NULL, `action` = NULL, `note` = NULL, `detail` = NULL, `eventType` = NULL, `actionName` = NULL, `eventTypeName` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`performer`)) {
        stopifnot(is.character(`performer`), length(`performer`) == 1)
        self$`performer` <- `performer`
      }
      if (!is.null(`date`)) {
        stopifnot(is.character(`date`), length(`date`) == 1)
        self$`date` <- `date`
      }
      if (!is.null(`action`)) {
        stopifnot(is.character(`action`), length(`action`) == 1)
        self$`action` <- `action`
      }
      if (!is.null(`note`)) {
        stopifnot(is.character(`note`), length(`note`) == 1)
        self$`note` <- `note`
      }
      if (!is.null(`detail`)) {
        stopifnot(is.character(`detail`), length(`detail`) == 1)
        self$`detail` <- `detail`
      }
      if (!is.null(`eventType`)) {
        stopifnot(R6::is.R6(`eventType`))
        self$`eventType` <- `eventType`
      }
      if (!is.null(`actionName`)) {
        stopifnot(is.character(`actionName`), length(`actionName`) == 1)
        self$`actionName` <- `actionName`
      }
      if (!is.null(`eventTypeName`)) {
        stopifnot(is.character(`eventTypeName`), length(`eventTypeName`) == 1)
        self$`eventTypeName` <- `eventTypeName`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return AuditEventValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      AuditEventValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        AuditEventValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`performer`)) {
        AuditEventValueObjectObject[["performer"]] <-
          self$`performer`
      }
      if (!is.null(self$`date`)) {
        AuditEventValueObjectObject[["date"]] <-
          self$`date`
      }
      if (!is.null(self$`action`)) {
        AuditEventValueObjectObject[["action"]] <-
          self$`action`
      }
      if (!is.null(self$`note`)) {
        AuditEventValueObjectObject[["note"]] <-
          self$`note`
      }
      if (!is.null(self$`detail`)) {
        AuditEventValueObjectObject[["detail"]] <-
          self$`detail`
      }
      if (!is.null(self$`eventType`)) {
        AuditEventValueObjectObject[["eventType"]] <-
          self$`eventType`$toJSON()
      }
      if (!is.null(self$`actionName`)) {
        AuditEventValueObjectObject[["actionName"]] <-
          self$`actionName`
      }
      if (!is.null(self$`eventTypeName`)) {
        AuditEventValueObjectObject[["eventTypeName"]] <-
          self$`eventTypeName`
      }

      AuditEventValueObjectObject
    },
    #' Deserialize JSON string into an instance of AuditEventValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of AuditEventValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of AuditEventValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`performer`)) {
        self$`performer` <- this_object$`performer`
      }
      if (!is.null(this_object$`date`)) {
        self$`date` <- this_object$`date`
      }
      if (!is.null(this_object$`action`)) {
        self$`action` <- this_object$`action`
      }
      if (!is.null(this_object$`note`)) {
        self$`note` <- this_object$`note`
      }
      if (!is.null(this_object$`detail`)) {
        self$`detail` <- this_object$`detail`
      }
      if (!is.null(this_object$`eventType`)) {
        eventtype_object <- AuditEventType$new()
        eventtype_object$fromJSON(jsonlite::toJSON(this_object$eventType, auto_unbox = TRUE, digits = NA))
        self$`eventType` <- eventtype_object
      }
      if (!is.null(this_object$`actionName`)) {
        self$`actionName` <- this_object$`actionName`
      }
      if (!is.null(this_object$`eventTypeName`)) {
        self$`eventTypeName` <- this_object$`eventTypeName`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return AuditEventValueObject in JSON format
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
        if (!is.null(self$`performer`)) {
          sprintf(
          '"performer":
            "%s"
                    ',
          self$`performer`
          )
        },
        if (!is.null(self$`date`)) {
          sprintf(
          '"date":
            "%s"
                    ',
          self$`date`
          )
        },
        if (!is.null(self$`action`)) {
          sprintf(
          '"action":
            "%s"
                    ',
          self$`action`
          )
        },
        if (!is.null(self$`note`)) {
          sprintf(
          '"note":
            "%s"
                    ',
          self$`note`
          )
        },
        if (!is.null(self$`detail`)) {
          sprintf(
          '"detail":
            "%s"
                    ',
          self$`detail`
          )
        },
        if (!is.null(self$`eventType`)) {
          sprintf(
          '"eventType":
          %s
          ',
          jsonlite::toJSON(self$`eventType`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`actionName`)) {
          sprintf(
          '"actionName":
            "%s"
                    ',
          self$`actionName`
          )
        },
        if (!is.null(self$`eventTypeName`)) {
          sprintf(
          '"eventTypeName":
            "%s"
                    ',
          self$`eventTypeName`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of AuditEventValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of AuditEventValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of AuditEventValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`performer` <- this_object$`performer`
      self$`date` <- this_object$`date`
      self$`action` <- this_object$`action`
      self$`note` <- this_object$`note`
      self$`detail` <- this_object$`detail`
      self$`eventType` <- AuditEventType$new()$fromJSON(jsonlite::toJSON(this_object$eventType, auto_unbox = TRUE, digits = NA))
      self$`actionName` <- this_object$`actionName`
      self$`eventTypeName` <- this_object$`eventTypeName`
      self
    },
    #' Validate JSON input with respect to AuditEventValueObject
    #'
    #' @description
    #' Validate JSON input with respect to AuditEventValueObject and throw an exception if invalid
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
    #' @return String representation of AuditEventValueObject
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
AuditEventValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
AuditEventValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
AuditEventValueObject$lock()

