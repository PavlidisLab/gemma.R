#' Create a new CurationDetails
#'
#' @description
#' CurationDetails Class
#'
#' @docType class
#' @title CurationDetails
#' @description CurationDetails Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field lastUpdated  character optional
#' @field lastNeedsAttentionEvent  \link{AuditEvent} optional
#' @field needsAttention  character optional
#' @field lastTroubledEvent  \link{AuditEvent} optional
#' @field troubled  character optional
#' @field lastNoteUpdateEvent  \link{AuditEvent} optional
#' @field curationNote  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
CurationDetails <- R6::R6Class(
  "CurationDetails",
  public = list(
    `id` = NULL,
    `lastUpdated` = NULL,
    `lastNeedsAttentionEvent` = NULL,
    `needsAttention` = NULL,
    `lastTroubledEvent` = NULL,
    `troubled` = NULL,
    `lastNoteUpdateEvent` = NULL,
    `curationNote` = NULL,
    #' Initialize a new CurationDetails class.
    #'
    #' @description
    #' Initialize a new CurationDetails class.
    #'
    #' @param id id
    #' @param lastUpdated lastUpdated
    #' @param lastNeedsAttentionEvent lastNeedsAttentionEvent
    #' @param needsAttention needsAttention
    #' @param lastTroubledEvent lastTroubledEvent
    #' @param troubled troubled
    #' @param lastNoteUpdateEvent lastNoteUpdateEvent
    #' @param curationNote curationNote
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `lastUpdated` = NULL, `lastNeedsAttentionEvent` = NULL, `needsAttention` = NULL, `lastTroubledEvent` = NULL, `troubled` = NULL, `lastNoteUpdateEvent` = NULL, `curationNote` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`lastUpdated`)) {
        stopifnot(is.character(`lastUpdated`), length(`lastUpdated`) == 1)
        self$`lastUpdated` <- `lastUpdated`
      }
      if (!is.null(`lastNeedsAttentionEvent`)) {
        stopifnot(R6::is.R6(`lastNeedsAttentionEvent`))
        self$`lastNeedsAttentionEvent` <- `lastNeedsAttentionEvent`
      }
      if (!is.null(`needsAttention`)) {
        stopifnot(is.logical(`needsAttention`), length(`needsAttention`) == 1)
        self$`needsAttention` <- `needsAttention`
      }
      if (!is.null(`lastTroubledEvent`)) {
        stopifnot(R6::is.R6(`lastTroubledEvent`))
        self$`lastTroubledEvent` <- `lastTroubledEvent`
      }
      if (!is.null(`troubled`)) {
        stopifnot(is.logical(`troubled`), length(`troubled`) == 1)
        self$`troubled` <- `troubled`
      }
      if (!is.null(`lastNoteUpdateEvent`)) {
        stopifnot(R6::is.R6(`lastNoteUpdateEvent`))
        self$`lastNoteUpdateEvent` <- `lastNoteUpdateEvent`
      }
      if (!is.null(`curationNote`)) {
        stopifnot(is.character(`curationNote`), length(`curationNote`) == 1)
        self$`curationNote` <- `curationNote`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CurationDetails in JSON format
    #' @keywords internal
    toJSON = function() {
      CurationDetailsObject <- list()
      if (!is.null(self$`id`)) {
        CurationDetailsObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`lastUpdated`)) {
        CurationDetailsObject[["lastUpdated"]] <-
          self$`lastUpdated`
      }
      if (!is.null(self$`lastNeedsAttentionEvent`)) {
        CurationDetailsObject[["lastNeedsAttentionEvent"]] <-
          self$`lastNeedsAttentionEvent`$toJSON()
      }
      if (!is.null(self$`needsAttention`)) {
        CurationDetailsObject[["needsAttention"]] <-
          self$`needsAttention`
      }
      if (!is.null(self$`lastTroubledEvent`)) {
        CurationDetailsObject[["lastTroubledEvent"]] <-
          self$`lastTroubledEvent`$toJSON()
      }
      if (!is.null(self$`troubled`)) {
        CurationDetailsObject[["troubled"]] <-
          self$`troubled`
      }
      if (!is.null(self$`lastNoteUpdateEvent`)) {
        CurationDetailsObject[["lastNoteUpdateEvent"]] <-
          self$`lastNoteUpdateEvent`$toJSON()
      }
      if (!is.null(self$`curationNote`)) {
        CurationDetailsObject[["curationNote"]] <-
          self$`curationNote`
      }

      CurationDetailsObject
    },
    #' Deserialize JSON string into an instance of CurationDetails
    #'
    #' @description
    #' Deserialize JSON string into an instance of CurationDetails
    #'
    #' @param input_json the JSON input
    #' @return the instance of CurationDetails
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`lastUpdated`)) {
        self$`lastUpdated` <- this_object$`lastUpdated`
      }
      if (!is.null(this_object$`lastNeedsAttentionEvent`)) {
        lastneedsattentionevent_object <- AuditEvent$new()
        lastneedsattentionevent_object$fromJSON(jsonlite::toJSON(this_object$lastNeedsAttentionEvent, auto_unbox = TRUE, digits = NA))
        self$`lastNeedsAttentionEvent` <- lastneedsattentionevent_object
      }
      if (!is.null(this_object$`needsAttention`)) {
        self$`needsAttention` <- this_object$`needsAttention`
      }
      if (!is.null(this_object$`lastTroubledEvent`)) {
        lasttroubledevent_object <- AuditEvent$new()
        lasttroubledevent_object$fromJSON(jsonlite::toJSON(this_object$lastTroubledEvent, auto_unbox = TRUE, digits = NA))
        self$`lastTroubledEvent` <- lasttroubledevent_object
      }
      if (!is.null(this_object$`troubled`)) {
        self$`troubled` <- this_object$`troubled`
      }
      if (!is.null(this_object$`lastNoteUpdateEvent`)) {
        lastnoteupdateevent_object <- AuditEvent$new()
        lastnoteupdateevent_object$fromJSON(jsonlite::toJSON(this_object$lastNoteUpdateEvent, auto_unbox = TRUE, digits = NA))
        self$`lastNoteUpdateEvent` <- lastnoteupdateevent_object
      }
      if (!is.null(this_object$`curationNote`)) {
        self$`curationNote` <- this_object$`curationNote`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CurationDetails in JSON format
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
        if (!is.null(self$`lastUpdated`)) {
          sprintf(
          '"lastUpdated":
            "%s"
                    ',
          self$`lastUpdated`
          )
        },
        if (!is.null(self$`lastNeedsAttentionEvent`)) {
          sprintf(
          '"lastNeedsAttentionEvent":
          %s
          ',
          jsonlite::toJSON(self$`lastNeedsAttentionEvent`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`needsAttention`)) {
          sprintf(
          '"needsAttention":
            %s
                    ',
          tolower(self$`needsAttention`)
          )
        },
        if (!is.null(self$`lastTroubledEvent`)) {
          sprintf(
          '"lastTroubledEvent":
          %s
          ',
          jsonlite::toJSON(self$`lastTroubledEvent`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`troubled`)) {
          sprintf(
          '"troubled":
            %s
                    ',
          tolower(self$`troubled`)
          )
        },
        if (!is.null(self$`lastNoteUpdateEvent`)) {
          sprintf(
          '"lastNoteUpdateEvent":
          %s
          ',
          jsonlite::toJSON(self$`lastNoteUpdateEvent`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`curationNote`)) {
          sprintf(
          '"curationNote":
            "%s"
                    ',
          self$`curationNote`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of CurationDetails
    #'
    #' @description
    #' Deserialize JSON string into an instance of CurationDetails
    #'
    #' @param input_json the JSON input
    #' @return the instance of CurationDetails
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`lastUpdated` <- this_object$`lastUpdated`
      self$`lastNeedsAttentionEvent` <- AuditEvent$new()$fromJSON(jsonlite::toJSON(this_object$lastNeedsAttentionEvent, auto_unbox = TRUE, digits = NA))
      self$`needsAttention` <- this_object$`needsAttention`
      self$`lastTroubledEvent` <- AuditEvent$new()$fromJSON(jsonlite::toJSON(this_object$lastTroubledEvent, auto_unbox = TRUE, digits = NA))
      self$`troubled` <- this_object$`troubled`
      self$`lastNoteUpdateEvent` <- AuditEvent$new()$fromJSON(jsonlite::toJSON(this_object$lastNoteUpdateEvent, auto_unbox = TRUE, digits = NA))
      self$`curationNote` <- this_object$`curationNote`
      self
    },
    #' Validate JSON input with respect to CurationDetails
    #'
    #' @description
    #' Validate JSON input with respect to CurationDetails and throw an exception if invalid
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
    #' @return String representation of CurationDetails
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
CurationDetails$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
CurationDetails$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
CurationDetails$lock()

