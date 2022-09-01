#' Create a new AuditEvent
#'
#' @description
#' AuditEvent Class
#'
#' @docType class
#' @title AuditEvent
#' @description AuditEvent Class
#' @format An \code{R6Class} generator object
#' @field action  \link{AuditAction} [optional]
#' @field date  character [optional]
#' @field detail  character [optional]
#' @field eventType  \link{AuditEventType} [optional]
#' @field id  integer [optional]
#' @field note  character [optional]
#' @field performer  \link{User} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
AuditEvent <- R6::R6Class(
  "AuditEvent",
  public = list(
    `action` = NULL,
    `date` = NULL,
    `detail` = NULL,
    `eventType` = NULL,
    `id` = NULL,
    `note` = NULL,
    `performer` = NULL,
    #' Initialize a new AuditEvent class.
    #'
    #' @description
    #' Initialize a new AuditEvent class.
    #'
    #' @param action action
    #' @param date date
    #' @param detail detail
    #' @param eventType eventType
    #' @param id id
    #' @param note note
    #' @param performer performer
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `action` = NULL, `date` = NULL, `detail` = NULL, `eventType` = NULL, `id` = NULL, `note` = NULL, `performer` = NULL, ...
    ) {
      if (!is.null(`action`)) {
        stopifnot(R6::is.R6(`action`))
        self$`action` <- `action`
      }
      if (!is.null(`date`)) {
        stopifnot(is.character(`date`), length(`date`) == 1)
        self$`date` <- `date`
      }
      if (!is.null(`detail`)) {
        stopifnot(is.character(`detail`), length(`detail`) == 1)
        self$`detail` <- `detail`
      }
      if (!is.null(`eventType`)) {
        stopifnot(R6::is.R6(`eventType`))
        self$`eventType` <- `eventType`
      }
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`note`)) {
        stopifnot(is.character(`note`), length(`note`) == 1)
        self$`note` <- `note`
      }
      if (!is.null(`performer`)) {
        stopifnot(R6::is.R6(`performer`))
        self$`performer` <- `performer`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return AuditEvent in JSON format
    #' @keywords internal
    toJSON = function() {
      AuditEventObject <- list()
      if (!is.null(self$`action`)) {
        AuditEventObject[["action"]] <-
          self$`action`$toJSON()
      }
      if (!is.null(self$`date`)) {
        AuditEventObject[["date"]] <-
          self$`date`
      }
      if (!is.null(self$`detail`)) {
        AuditEventObject[["detail"]] <-
          self$`detail`
      }
      if (!is.null(self$`eventType`)) {
        AuditEventObject[["eventType"]] <-
          self$`eventType`$toJSON()
      }
      if (!is.null(self$`id`)) {
        AuditEventObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`note`)) {
        AuditEventObject[["note"]] <-
          self$`note`
      }
      if (!is.null(self$`performer`)) {
        AuditEventObject[["performer"]] <-
          self$`performer`$toJSON()
      }

      AuditEventObject
    },
    #' Deserialize JSON string into an instance of AuditEvent
    #'
    #' @description
    #' Deserialize JSON string into an instance of AuditEvent
    #'
    #' @param input_json the JSON input
    #' @return the instance of AuditEvent
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`action`)) {
        action_object <- AuditAction$new()
        action_object$fromJSON(jsonlite::toJSON(this_object$action, auto_unbox = TRUE, digits = NA))
        self$`action` <- action_object
      }
      if (!is.null(this_object$`date`)) {
        self$`date` <- this_object$`date`
      }
      if (!is.null(this_object$`detail`)) {
        self$`detail` <- this_object$`detail`
      }
      if (!is.null(this_object$`eventType`)) {
        eventtype_object <- AuditEventType$new()
        eventtype_object$fromJSON(jsonlite::toJSON(this_object$eventType, auto_unbox = TRUE, digits = NA))
        self$`eventType` <- eventtype_object
      }
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`note`)) {
        self$`note` <- this_object$`note`
      }
      if (!is.null(this_object$`performer`)) {
        performer_object <- User$new()
        performer_object$fromJSON(jsonlite::toJSON(this_object$performer, auto_unbox = TRUE, digits = NA))
        self$`performer` <- performer_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return AuditEvent in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`action`)) {
          sprintf(
          '"action":
          %s
          ',
          jsonlite::toJSON(self$`action`$toJSON(), auto_unbox = TRUE, digits = NA)
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
        if (!is.null(self$`id`)) {
          sprintf(
          '"id":
            %d
                    ',
          self$`id`
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
        if (!is.null(self$`performer`)) {
          sprintf(
          '"performer":
          %s
          ',
          jsonlite::toJSON(self$`performer`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of AuditEvent
    #'
    #' @description
    #' Deserialize JSON string into an instance of AuditEvent
    #'
    #' @param input_json the JSON input
    #' @return the instance of AuditEvent
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`action` <- AuditAction$new()$fromJSON(jsonlite::toJSON(this_object$action, auto_unbox = TRUE, digits = NA))
      self$`date` <- this_object$`date`
      self$`detail` <- this_object$`detail`
      self$`eventType` <- AuditEventType$new()$fromJSON(jsonlite::toJSON(this_object$eventType, auto_unbox = TRUE, digits = NA))
      self$`id` <- this_object$`id`
      self$`note` <- this_object$`note`
      self$`performer` <- User$new()$fromJSON(jsonlite::toJSON(this_object$performer, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to AuditEvent
    #'
    #' @description
    #' Validate JSON input with respect to AuditEvent and throw an exception if invalid
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
    #' @return String representation of AuditEvent
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
AuditEvent$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
AuditEvent$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
AuditEvent$lock()

