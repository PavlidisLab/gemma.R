#' Create a new AuditTrail
#'
#' @description
#' AuditTrail Class
#'
#' @docType class
#' @title AuditTrail
#' @description AuditTrail Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field events  list(\link{AuditEvent}) optional
#' @field creationEvent  \link{AuditEvent} optional
#' @field last  \link{AuditEvent} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
AuditTrail <- R6::R6Class(
  "AuditTrail",
  public = list(
    `id` = NULL,
    `events` = NULL,
    `creationEvent` = NULL,
    `last` = NULL,
    #' Initialize a new AuditTrail class.
    #'
    #' @description
    #' Initialize a new AuditTrail class.
    #'
    #' @param id id
    #' @param events events
    #' @param creationEvent creationEvent
    #' @param last last
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `events` = NULL, `creationEvent` = NULL, `last` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`events`)) {
        stopifnot(is.vector(`events`), length(`events`) != 0)
        sapply(`events`, function(x) stopifnot(R6::is.R6(x)))
        self$`events` <- `events`
      }
      if (!is.null(`creationEvent`)) {
        stopifnot(R6::is.R6(`creationEvent`))
        self$`creationEvent` <- `creationEvent`
      }
      if (!is.null(`last`)) {
        stopifnot(R6::is.R6(`last`))
        self$`last` <- `last`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return AuditTrail in JSON format
    #' @keywords internal
    toJSON = function() {
      AuditTrailObject <- list()
      if (!is.null(self$`id`)) {
        AuditTrailObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`events`)) {
        AuditTrailObject[["events"]] <-
          lapply(self$`events`, function(x) x$toJSON())
      }
      if (!is.null(self$`creationEvent`)) {
        AuditTrailObject[["creationEvent"]] <-
          self$`creationEvent`$toJSON()
      }
      if (!is.null(self$`last`)) {
        AuditTrailObject[["last"]] <-
          self$`last`$toJSON()
      }

      AuditTrailObject
    },
    #' Deserialize JSON string into an instance of AuditTrail
    #'
    #' @description
    #' Deserialize JSON string into an instance of AuditTrail
    #'
    #' @param input_json the JSON input
    #' @return the instance of AuditTrail
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`events`)) {
        self$`events` <- ApiClient$new()$deserializeObj(this_object$`events`, "array[AuditEvent]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`creationEvent`)) {
        creationevent_object <- AuditEvent$new()
        creationevent_object$fromJSON(jsonlite::toJSON(this_object$creationEvent, auto_unbox = TRUE, digits = NA))
        self$`creationEvent` <- creationevent_object
      }
      if (!is.null(this_object$`last`)) {
        last_object <- AuditEvent$new()
        last_object$fromJSON(jsonlite::toJSON(this_object$last, auto_unbox = TRUE, digits = NA))
        self$`last` <- last_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return AuditTrail in JSON format
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
        if (!is.null(self$`events`)) {
          sprintf(
          '"events":
          [%s]
',
          paste(sapply(self$`events`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`creationEvent`)) {
          sprintf(
          '"creationEvent":
          %s
          ',
          jsonlite::toJSON(self$`creationEvent`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`last`)) {
          sprintf(
          '"last":
          %s
          ',
          jsonlite::toJSON(self$`last`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of AuditTrail
    #'
    #' @description
    #' Deserialize JSON string into an instance of AuditTrail
    #'
    #' @param input_json the JSON input
    #' @return the instance of AuditTrail
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`events` <- ApiClient$new()$deserializeObj(this_object$`events`, "array[AuditEvent]", loadNamespace("gemma.R"))
      self$`creationEvent` <- AuditEvent$new()$fromJSON(jsonlite::toJSON(this_object$creationEvent, auto_unbox = TRUE, digits = NA))
      self$`last` <- AuditEvent$new()$fromJSON(jsonlite::toJSON(this_object$last, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to AuditTrail
    #'
    #' @description
    #' Validate JSON input with respect to AuditTrail and throw an exception if invalid
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
    #' @return String representation of AuditTrail
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
AuditTrail$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
AuditTrail$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
AuditTrail$lock()

