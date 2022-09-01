#' Create a new JobInfo
#'
#' @description
#' JobInfo Class
#'
#' @docType class
#' @title JobInfo
#' @description JobInfo Class
#' @format An \code{R6Class} generator object
#' @field runningStatus  character optional
#' @field failedMessage  character optional
#' @field startTime  character optional
#' @field endTime  character optional
#' @field phases  integer optional
#' @field description  character optional
#' @field messages  character optional
#' @field taskId  character optional
#' @field id  integer optional
#' @field user  \link{User} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
JobInfo <- R6::R6Class(
  "JobInfo",
  public = list(
    `runningStatus` = NULL,
    `failedMessage` = NULL,
    `startTime` = NULL,
    `endTime` = NULL,
    `phases` = NULL,
    `description` = NULL,
    `messages` = NULL,
    `taskId` = NULL,
    `id` = NULL,
    `user` = NULL,
    #' Initialize a new JobInfo class.
    #'
    #' @description
    #' Initialize a new JobInfo class.
    #'
    #' @param runningStatus runningStatus
    #' @param failedMessage failedMessage
    #' @param startTime startTime
    #' @param endTime endTime
    #' @param phases phases
    #' @param description description
    #' @param messages messages
    #' @param taskId taskId
    #' @param id id
    #' @param user user
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `runningStatus` = NULL, `failedMessage` = NULL, `startTime` = NULL, `endTime` = NULL, `phases` = NULL, `description` = NULL, `messages` = NULL, `taskId` = NULL, `id` = NULL, `user` = NULL, ...
    ) {
      if (!is.null(`runningStatus`)) {
        stopifnot(is.logical(`runningStatus`), length(`runningStatus`) == 1)
        self$`runningStatus` <- `runningStatus`
      }
      if (!is.null(`failedMessage`)) {
        stopifnot(is.character(`failedMessage`), length(`failedMessage`) == 1)
        self$`failedMessage` <- `failedMessage`
      }
      if (!is.null(`startTime`)) {
        stopifnot(is.character(`startTime`), length(`startTime`) == 1)
        self$`startTime` <- `startTime`
      }
      if (!is.null(`endTime`)) {
        stopifnot(is.character(`endTime`), length(`endTime`) == 1)
        self$`endTime` <- `endTime`
      }
      if (!is.null(`phases`)) {
        stopifnot(is.numeric(`phases`), length(`phases`) == 1)
        self$`phases` <- `phases`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`messages`)) {
        stopifnot(is.character(`messages`), length(`messages`) == 1)
        self$`messages` <- `messages`
      }
      if (!is.null(`taskId`)) {
        stopifnot(is.character(`taskId`), length(`taskId`) == 1)
        self$`taskId` <- `taskId`
      }
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`user`)) {
        stopifnot(R6::is.R6(`user`))
        self$`user` <- `user`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return JobInfo in JSON format
    #' @keywords internal
    toJSON = function() {
      JobInfoObject <- list()
      if (!is.null(self$`runningStatus`)) {
        JobInfoObject[["runningStatus"]] <-
          self$`runningStatus`
      }
      if (!is.null(self$`failedMessage`)) {
        JobInfoObject[["failedMessage"]] <-
          self$`failedMessage`
      }
      if (!is.null(self$`startTime`)) {
        JobInfoObject[["startTime"]] <-
          self$`startTime`
      }
      if (!is.null(self$`endTime`)) {
        JobInfoObject[["endTime"]] <-
          self$`endTime`
      }
      if (!is.null(self$`phases`)) {
        JobInfoObject[["phases"]] <-
          self$`phases`
      }
      if (!is.null(self$`description`)) {
        JobInfoObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`messages`)) {
        JobInfoObject[["messages"]] <-
          self$`messages`
      }
      if (!is.null(self$`taskId`)) {
        JobInfoObject[["taskId"]] <-
          self$`taskId`
      }
      if (!is.null(self$`id`)) {
        JobInfoObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`user`)) {
        JobInfoObject[["user"]] <-
          self$`user`$toJSON()
      }

      JobInfoObject
    },
    #' Deserialize JSON string into an instance of JobInfo
    #'
    #' @description
    #' Deserialize JSON string into an instance of JobInfo
    #'
    #' @param input_json the JSON input
    #' @return the instance of JobInfo
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`runningStatus`)) {
        self$`runningStatus` <- this_object$`runningStatus`
      }
      if (!is.null(this_object$`failedMessage`)) {
        self$`failedMessage` <- this_object$`failedMessage`
      }
      if (!is.null(this_object$`startTime`)) {
        self$`startTime` <- this_object$`startTime`
      }
      if (!is.null(this_object$`endTime`)) {
        self$`endTime` <- this_object$`endTime`
      }
      if (!is.null(this_object$`phases`)) {
        self$`phases` <- this_object$`phases`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`messages`)) {
        self$`messages` <- this_object$`messages`
      }
      if (!is.null(this_object$`taskId`)) {
        self$`taskId` <- this_object$`taskId`
      }
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`user`)) {
        user_object <- User$new()
        user_object$fromJSON(jsonlite::toJSON(this_object$user, auto_unbox = TRUE, digits = NA))
        self$`user` <- user_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return JobInfo in JSON format
    #' @keywords internal
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`runningStatus`)) {
          sprintf(
          '"runningStatus":
            %s
                    ',
          tolower(self$`runningStatus`)
          )
        },
        if (!is.null(self$`failedMessage`)) {
          sprintf(
          '"failedMessage":
            "%s"
                    ',
          self$`failedMessage`
          )
        },
        if (!is.null(self$`startTime`)) {
          sprintf(
          '"startTime":
            "%s"
                    ',
          self$`startTime`
          )
        },
        if (!is.null(self$`endTime`)) {
          sprintf(
          '"endTime":
            "%s"
                    ',
          self$`endTime`
          )
        },
        if (!is.null(self$`phases`)) {
          sprintf(
          '"phases":
            %d
                    ',
          self$`phases`
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
        if (!is.null(self$`messages`)) {
          sprintf(
          '"messages":
            "%s"
                    ',
          self$`messages`
          )
        },
        if (!is.null(self$`taskId`)) {
          sprintf(
          '"taskId":
            "%s"
                    ',
          self$`taskId`
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
        if (!is.null(self$`user`)) {
          sprintf(
          '"user":
          %s
          ',
          jsonlite::toJSON(self$`user`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of JobInfo
    #'
    #' @description
    #' Deserialize JSON string into an instance of JobInfo
    #'
    #' @param input_json the JSON input
    #' @return the instance of JobInfo
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`runningStatus` <- this_object$`runningStatus`
      self$`failedMessage` <- this_object$`failedMessage`
      self$`startTime` <- this_object$`startTime`
      self$`endTime` <- this_object$`endTime`
      self$`phases` <- this_object$`phases`
      self$`description` <- this_object$`description`
      self$`messages` <- this_object$`messages`
      self$`taskId` <- this_object$`taskId`
      self$`id` <- this_object$`id`
      self$`user` <- User$new()$fromJSON(jsonlite::toJSON(this_object$user, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to JobInfo
    #'
    #' @description
    #' Validate JSON input with respect to JobInfo and throw an exception if invalid
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
    #' @return String representation of JobInfo
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
JobInfo$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
JobInfo$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
JobInfo$lock()

