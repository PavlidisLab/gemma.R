#' Create a new ExternalDatabase
#'
#' @description
#' ExternalDatabase Class
#'
#' @docType class
#' @title ExternalDatabase
#' @description ExternalDatabase Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field description  character [optional]
#' @field id  integer [optional]
#' @field localInstallDbName  character [optional]
#' @field webUri  character [optional]
#' @field ftpUri  character [optional]
#' @field type  \link{DatabaseType} [optional]
#' @field databaseSupplier  \link{Contact} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ExternalDatabase <- R6::R6Class(
  "ExternalDatabase",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `localInstallDbName` = NULL,
    `webUri` = NULL,
    `ftpUri` = NULL,
    `type` = NULL,
    `databaseSupplier` = NULL,
    #' Initialize a new ExternalDatabase class.
    #'
    #' @description
    #' Initialize a new ExternalDatabase class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param localInstallDbName localInstallDbName
    #' @param webUri webUri
    #' @param ftpUri ftpUri
    #' @param type type
    #' @param databaseSupplier databaseSupplier
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `localInstallDbName` = NULL, `webUri` = NULL, `ftpUri` = NULL, `type` = NULL, `databaseSupplier` = NULL, ...
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
      if (!is.null(`localInstallDbName`)) {
        stopifnot(is.character(`localInstallDbName`), length(`localInstallDbName`) == 1)
        self$`localInstallDbName` <- `localInstallDbName`
      }
      if (!is.null(`webUri`)) {
        stopifnot(is.character(`webUri`), length(`webUri`) == 1)
        self$`webUri` <- `webUri`
      }
      if (!is.null(`ftpUri`)) {
        stopifnot(is.character(`ftpUri`), length(`ftpUri`) == 1)
        self$`ftpUri` <- `ftpUri`
      }
      if (!is.null(`type`)) {
        stopifnot(R6::is.R6(`type`))
        self$`type` <- `type`
      }
      if (!is.null(`databaseSupplier`)) {
        stopifnot(R6::is.R6(`databaseSupplier`))
        self$`databaseSupplier` <- `databaseSupplier`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExternalDatabase in JSON format
    #' @keywords internal
    toJSON = function() {
      ExternalDatabaseObject <- list()
      if (!is.null(self$`name`)) {
        ExternalDatabaseObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        ExternalDatabaseObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        ExternalDatabaseObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`localInstallDbName`)) {
        ExternalDatabaseObject[["localInstallDbName"]] <-
          self$`localInstallDbName`
      }
      if (!is.null(self$`webUri`)) {
        ExternalDatabaseObject[["webUri"]] <-
          self$`webUri`
      }
      if (!is.null(self$`ftpUri`)) {
        ExternalDatabaseObject[["ftpUri"]] <-
          self$`ftpUri`
      }
      if (!is.null(self$`type`)) {
        ExternalDatabaseObject[["type"]] <-
          self$`type`$toJSON()
      }
      if (!is.null(self$`databaseSupplier`)) {
        ExternalDatabaseObject[["databaseSupplier"]] <-
          self$`databaseSupplier`$toJSON()
      }

      ExternalDatabaseObject
    },
    #' Deserialize JSON string into an instance of ExternalDatabase
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExternalDatabase
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExternalDatabase
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
      if (!is.null(this_object$`localInstallDbName`)) {
        self$`localInstallDbName` <- this_object$`localInstallDbName`
      }
      if (!is.null(this_object$`webUri`)) {
        self$`webUri` <- this_object$`webUri`
      }
      if (!is.null(this_object$`ftpUri`)) {
        self$`ftpUri` <- this_object$`ftpUri`
      }
      if (!is.null(this_object$`type`)) {
        type_object <- DatabaseType$new()
        type_object$fromJSON(jsonlite::toJSON(this_object$type, auto_unbox = TRUE, digits = NA))
        self$`type` <- type_object
      }
      if (!is.null(this_object$`databaseSupplier`)) {
        databasesupplier_object <- Contact$new()
        databasesupplier_object$fromJSON(jsonlite::toJSON(this_object$databaseSupplier, auto_unbox = TRUE, digits = NA))
        self$`databaseSupplier` <- databasesupplier_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExternalDatabase in JSON format
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
        if (!is.null(self$`localInstallDbName`)) {
          sprintf(
          '"localInstallDbName":
            "%s"
                    ',
          self$`localInstallDbName`
          )
        },
        if (!is.null(self$`webUri`)) {
          sprintf(
          '"webUri":
            "%s"
                    ',
          self$`webUri`
          )
        },
        if (!is.null(self$`ftpUri`)) {
          sprintf(
          '"ftpUri":
            "%s"
                    ',
          self$`ftpUri`
          )
        },
        if (!is.null(self$`type`)) {
          sprintf(
          '"type":
          %s
          ',
          jsonlite::toJSON(self$`type`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`databaseSupplier`)) {
          sprintf(
          '"databaseSupplier":
          %s
          ',
          jsonlite::toJSON(self$`databaseSupplier`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ExternalDatabase
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExternalDatabase
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExternalDatabase
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`localInstallDbName` <- this_object$`localInstallDbName`
      self$`webUri` <- this_object$`webUri`
      self$`ftpUri` <- this_object$`ftpUri`
      self$`type` <- DatabaseType$new()$fromJSON(jsonlite::toJSON(this_object$type, auto_unbox = TRUE, digits = NA))
      self$`databaseSupplier` <- Contact$new()$fromJSON(jsonlite::toJSON(this_object$databaseSupplier, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to ExternalDatabase
    #'
    #' @description
    #' Validate JSON input with respect to ExternalDatabase and throw an exception if invalid
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
    #' @return String representation of ExternalDatabase
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
ExternalDatabase$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ExternalDatabase$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ExternalDatabase$lock()

