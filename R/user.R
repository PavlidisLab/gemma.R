#' Create a new User
#'
#' @description
#' User Class
#'
#' @docType class
#' @title User
#' @description User Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field description  character [optional]
#' @field id  integer [optional]
#' @field email  character [optional]
#' @field lastName  character [optional]
#' @field userName  character [optional]
#' @field password  character [optional]
#' @field passwordHint  character [optional]
#' @field enabled  character [optional]
#' @field signupToken  character [optional]
#' @field signupTokenDatestamp  character [optional]
#' @field jobs  list(\link{JobInfo}) [optional]
#' @field fullName  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
User <- R6::R6Class(
  "User",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `email` = NULL,
    `lastName` = NULL,
    `userName` = NULL,
    `password` = NULL,
    `passwordHint` = NULL,
    `enabled` = NULL,
    `signupToken` = NULL,
    `signupTokenDatestamp` = NULL,
    `jobs` = NULL,
    `fullName` = NULL,
    #' Initialize a new User class.
    #'
    #' @description
    #' Initialize a new User class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param email email
    #' @param lastName lastName
    #' @param userName userName
    #' @param password password
    #' @param passwordHint passwordHint
    #' @param enabled enabled
    #' @param signupToken signupToken
    #' @param signupTokenDatestamp signupTokenDatestamp
    #' @param jobs jobs
    #' @param fullName fullName
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `email` = NULL, `lastName` = NULL, `userName` = NULL, `password` = NULL, `passwordHint` = NULL, `enabled` = NULL, `signupToken` = NULL, `signupTokenDatestamp` = NULL, `jobs` = NULL, `fullName` = NULL, ...
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
      if (!is.null(`email`)) {
        stopifnot(is.character(`email`), length(`email`) == 1)
        self$`email` <- `email`
      }
      if (!is.null(`lastName`)) {
        stopifnot(is.character(`lastName`), length(`lastName`) == 1)
        self$`lastName` <- `lastName`
      }
      if (!is.null(`userName`)) {
        stopifnot(is.character(`userName`), length(`userName`) == 1)
        self$`userName` <- `userName`
      }
      if (!is.null(`password`)) {
        stopifnot(is.character(`password`), length(`password`) == 1)
        self$`password` <- `password`
      }
      if (!is.null(`passwordHint`)) {
        stopifnot(is.character(`passwordHint`), length(`passwordHint`) == 1)
        self$`passwordHint` <- `passwordHint`
      }
      if (!is.null(`enabled`)) {
        stopifnot(is.logical(`enabled`), length(`enabled`) == 1)
        self$`enabled` <- `enabled`
      }
      if (!is.null(`signupToken`)) {
        stopifnot(is.character(`signupToken`), length(`signupToken`) == 1)
        self$`signupToken` <- `signupToken`
      }
      if (!is.null(`signupTokenDatestamp`)) {
        stopifnot(is.character(`signupTokenDatestamp`), length(`signupTokenDatestamp`) == 1)
        self$`signupTokenDatestamp` <- `signupTokenDatestamp`
      }
      if (!is.null(`jobs`)) {
        stopifnot(is.vector(`jobs`), length(`jobs`) != 0)
        sapply(`jobs`, function(x) stopifnot(R6::is.R6(x)))
        self$`jobs` <- `jobs`
      }
      if (!is.null(`fullName`)) {
        stopifnot(is.character(`fullName`), length(`fullName`) == 1)
        self$`fullName` <- `fullName`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return User in JSON format
    #' @keywords internal
    toJSON = function() {
      UserObject <- list()
      if (!is.null(self$`name`)) {
        UserObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        UserObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        UserObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`email`)) {
        UserObject[["email"]] <-
          self$`email`
      }
      if (!is.null(self$`lastName`)) {
        UserObject[["lastName"]] <-
          self$`lastName`
      }
      if (!is.null(self$`userName`)) {
        UserObject[["userName"]] <-
          self$`userName`
      }
      if (!is.null(self$`password`)) {
        UserObject[["password"]] <-
          self$`password`
      }
      if (!is.null(self$`passwordHint`)) {
        UserObject[["passwordHint"]] <-
          self$`passwordHint`
      }
      if (!is.null(self$`enabled`)) {
        UserObject[["enabled"]] <-
          self$`enabled`
      }
      if (!is.null(self$`signupToken`)) {
        UserObject[["signupToken"]] <-
          self$`signupToken`
      }
      if (!is.null(self$`signupTokenDatestamp`)) {
        UserObject[["signupTokenDatestamp"]] <-
          self$`signupTokenDatestamp`
      }
      if (!is.null(self$`jobs`)) {
        UserObject[["jobs"]] <-
          lapply(self$`jobs`, function(x) x$toJSON())
      }
      if (!is.null(self$`fullName`)) {
        UserObject[["fullName"]] <-
          self$`fullName`
      }

      UserObject
    },
    #' Deserialize JSON string into an instance of User
    #'
    #' @description
    #' Deserialize JSON string into an instance of User
    #'
    #' @param input_json the JSON input
    #' @return the instance of User
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
      if (!is.null(this_object$`email`)) {
        self$`email` <- this_object$`email`
      }
      if (!is.null(this_object$`lastName`)) {
        self$`lastName` <- this_object$`lastName`
      }
      if (!is.null(this_object$`userName`)) {
        self$`userName` <- this_object$`userName`
      }
      if (!is.null(this_object$`password`)) {
        self$`password` <- this_object$`password`
      }
      if (!is.null(this_object$`passwordHint`)) {
        self$`passwordHint` <- this_object$`passwordHint`
      }
      if (!is.null(this_object$`enabled`)) {
        self$`enabled` <- this_object$`enabled`
      }
      if (!is.null(this_object$`signupToken`)) {
        self$`signupToken` <- this_object$`signupToken`
      }
      if (!is.null(this_object$`signupTokenDatestamp`)) {
        self$`signupTokenDatestamp` <- this_object$`signupTokenDatestamp`
      }
      if (!is.null(this_object$`jobs`)) {
        self$`jobs` <- ApiClient$new()$deserializeObj(this_object$`jobs`, "set[JobInfo]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`fullName`)) {
        self$`fullName` <- this_object$`fullName`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return User in JSON format
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
        if (!is.null(self$`email`)) {
          sprintf(
          '"email":
            "%s"
                    ',
          self$`email`
          )
        },
        if (!is.null(self$`lastName`)) {
          sprintf(
          '"lastName":
            "%s"
                    ',
          self$`lastName`
          )
        },
        if (!is.null(self$`userName`)) {
          sprintf(
          '"userName":
            "%s"
                    ',
          self$`userName`
          )
        },
        if (!is.null(self$`password`)) {
          sprintf(
          '"password":
            "%s"
                    ',
          self$`password`
          )
        },
        if (!is.null(self$`passwordHint`)) {
          sprintf(
          '"passwordHint":
            "%s"
                    ',
          self$`passwordHint`
          )
        },
        if (!is.null(self$`enabled`)) {
          sprintf(
          '"enabled":
            %s
                    ',
          tolower(self$`enabled`)
          )
        },
        if (!is.null(self$`signupToken`)) {
          sprintf(
          '"signupToken":
            "%s"
                    ',
          self$`signupToken`
          )
        },
        if (!is.null(self$`signupTokenDatestamp`)) {
          sprintf(
          '"signupTokenDatestamp":
            "%s"
                    ',
          self$`signupTokenDatestamp`
          )
        },
        if (!is.null(self$`jobs`)) {
          sprintf(
          '"jobs":
          [%s]
',
          paste(sapply(self$`jobs`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`fullName`)) {
          sprintf(
          '"fullName":
            "%s"
                    ',
          self$`fullName`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of User
    #'
    #' @description
    #' Deserialize JSON string into an instance of User
    #'
    #' @param input_json the JSON input
    #' @return the instance of User
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`email` <- this_object$`email`
      self$`lastName` <- this_object$`lastName`
      self$`userName` <- this_object$`userName`
      self$`password` <- this_object$`password`
      self$`passwordHint` <- this_object$`passwordHint`
      self$`enabled` <- this_object$`enabled`
      self$`signupToken` <- this_object$`signupToken`
      self$`signupTokenDatestamp` <- this_object$`signupTokenDatestamp`
      self$`jobs` <- ApiClient$new()$deserializeObj(this_object$`jobs`, "set[JobInfo]", loadNamespace("gemma.R"))
      self$`fullName` <- this_object$`fullName`
      self
    },
    #' Validate JSON input with respect to User
    #'
    #' @description
    #' Validate JSON input with respect to User and throw an exception if invalid
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
    #' @return String representation of User
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
User$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
User$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
User$lock()

