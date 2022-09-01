#' Create a new AnnotationValueObject
#'
#' @description
#' AnnotationValueObject Class
#'
#' @docType class
#' @title AnnotationValueObject
#' @description AnnotationValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field classUri  character [optional]
#' @field className  character [optional]
#' @field termUri  character [optional]
#' @field termName  character [optional]
#' @field parentName  character [optional]
#' @field parentDescription  character [optional]
#' @field parentLink  character [optional]
#' @field parentOfParentName  character [optional]
#' @field parentOfParentDescription  character [optional]
#' @field parentOfParentLink  character [optional]
#' @field description  character [optional]
#' @field evidenceCode  character [optional]
#' @field objectClass  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
AnnotationValueObject <- R6::R6Class(
  "AnnotationValueObject",
  public = list(
    `id` = NULL,
    `classUri` = NULL,
    `className` = NULL,
    `termUri` = NULL,
    `termName` = NULL,
    `parentName` = NULL,
    `parentDescription` = NULL,
    `parentLink` = NULL,
    `parentOfParentName` = NULL,
    `parentOfParentDescription` = NULL,
    `parentOfParentLink` = NULL,
    `description` = NULL,
    `evidenceCode` = NULL,
    `objectClass` = NULL,
    #' Initialize a new AnnotationValueObject class.
    #'
    #' @description
    #' Initialize a new AnnotationValueObject class.
    #'
    #' @param id id
    #' @param classUri classUri
    #' @param className className
    #' @param termUri termUri
    #' @param termName termName
    #' @param parentName parentName
    #' @param parentDescription parentDescription
    #' @param parentLink parentLink
    #' @param parentOfParentName parentOfParentName
    #' @param parentOfParentDescription parentOfParentDescription
    #' @param parentOfParentLink parentOfParentLink
    #' @param description description
    #' @param evidenceCode evidenceCode
    #' @param objectClass objectClass
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `classUri` = NULL, `className` = NULL, `termUri` = NULL, `termName` = NULL, `parentName` = NULL, `parentDescription` = NULL, `parentLink` = NULL, `parentOfParentName` = NULL, `parentOfParentDescription` = NULL, `parentOfParentLink` = NULL, `description` = NULL, `evidenceCode` = NULL, `objectClass` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`classUri`)) {
        stopifnot(is.character(`classUri`), length(`classUri`) == 1)
        self$`classUri` <- `classUri`
      }
      if (!is.null(`className`)) {
        stopifnot(is.character(`className`), length(`className`) == 1)
        self$`className` <- `className`
      }
      if (!is.null(`termUri`)) {
        stopifnot(is.character(`termUri`), length(`termUri`) == 1)
        self$`termUri` <- `termUri`
      }
      if (!is.null(`termName`)) {
        stopifnot(is.character(`termName`), length(`termName`) == 1)
        self$`termName` <- `termName`
      }
      if (!is.null(`parentName`)) {
        stopifnot(is.character(`parentName`), length(`parentName`) == 1)
        self$`parentName` <- `parentName`
      }
      if (!is.null(`parentDescription`)) {
        stopifnot(is.character(`parentDescription`), length(`parentDescription`) == 1)
        self$`parentDescription` <- `parentDescription`
      }
      if (!is.null(`parentLink`)) {
        stopifnot(is.character(`parentLink`), length(`parentLink`) == 1)
        self$`parentLink` <- `parentLink`
      }
      if (!is.null(`parentOfParentName`)) {
        stopifnot(is.character(`parentOfParentName`), length(`parentOfParentName`) == 1)
        self$`parentOfParentName` <- `parentOfParentName`
      }
      if (!is.null(`parentOfParentDescription`)) {
        stopifnot(is.character(`parentOfParentDescription`), length(`parentOfParentDescription`) == 1)
        self$`parentOfParentDescription` <- `parentOfParentDescription`
      }
      if (!is.null(`parentOfParentLink`)) {
        stopifnot(is.character(`parentOfParentLink`), length(`parentOfParentLink`) == 1)
        self$`parentOfParentLink` <- `parentOfParentLink`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`evidenceCode`)) {
        stopifnot(is.character(`evidenceCode`), length(`evidenceCode`) == 1)
        self$`evidenceCode` <- `evidenceCode`
      }
      if (!is.null(`objectClass`)) {
        stopifnot(is.character(`objectClass`), length(`objectClass`) == 1)
        self$`objectClass` <- `objectClass`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return AnnotationValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      AnnotationValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        AnnotationValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`classUri`)) {
        AnnotationValueObjectObject[["classUri"]] <-
          self$`classUri`
      }
      if (!is.null(self$`className`)) {
        AnnotationValueObjectObject[["className"]] <-
          self$`className`
      }
      if (!is.null(self$`termUri`)) {
        AnnotationValueObjectObject[["termUri"]] <-
          self$`termUri`
      }
      if (!is.null(self$`termName`)) {
        AnnotationValueObjectObject[["termName"]] <-
          self$`termName`
      }
      if (!is.null(self$`parentName`)) {
        AnnotationValueObjectObject[["parentName"]] <-
          self$`parentName`
      }
      if (!is.null(self$`parentDescription`)) {
        AnnotationValueObjectObject[["parentDescription"]] <-
          self$`parentDescription`
      }
      if (!is.null(self$`parentLink`)) {
        AnnotationValueObjectObject[["parentLink"]] <-
          self$`parentLink`
      }
      if (!is.null(self$`parentOfParentName`)) {
        AnnotationValueObjectObject[["parentOfParentName"]] <-
          self$`parentOfParentName`
      }
      if (!is.null(self$`parentOfParentDescription`)) {
        AnnotationValueObjectObject[["parentOfParentDescription"]] <-
          self$`parentOfParentDescription`
      }
      if (!is.null(self$`parentOfParentLink`)) {
        AnnotationValueObjectObject[["parentOfParentLink"]] <-
          self$`parentOfParentLink`
      }
      if (!is.null(self$`description`)) {
        AnnotationValueObjectObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`evidenceCode`)) {
        AnnotationValueObjectObject[["evidenceCode"]] <-
          self$`evidenceCode`
      }
      if (!is.null(self$`objectClass`)) {
        AnnotationValueObjectObject[["objectClass"]] <-
          self$`objectClass`
      }

      AnnotationValueObjectObject
    },
    #' Deserialize JSON string into an instance of AnnotationValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of AnnotationValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of AnnotationValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`classUri`)) {
        self$`classUri` <- this_object$`classUri`
      }
      if (!is.null(this_object$`className`)) {
        self$`className` <- this_object$`className`
      }
      if (!is.null(this_object$`termUri`)) {
        self$`termUri` <- this_object$`termUri`
      }
      if (!is.null(this_object$`termName`)) {
        self$`termName` <- this_object$`termName`
      }
      if (!is.null(this_object$`parentName`)) {
        self$`parentName` <- this_object$`parentName`
      }
      if (!is.null(this_object$`parentDescription`)) {
        self$`parentDescription` <- this_object$`parentDescription`
      }
      if (!is.null(this_object$`parentLink`)) {
        self$`parentLink` <- this_object$`parentLink`
      }
      if (!is.null(this_object$`parentOfParentName`)) {
        self$`parentOfParentName` <- this_object$`parentOfParentName`
      }
      if (!is.null(this_object$`parentOfParentDescription`)) {
        self$`parentOfParentDescription` <- this_object$`parentOfParentDescription`
      }
      if (!is.null(this_object$`parentOfParentLink`)) {
        self$`parentOfParentLink` <- this_object$`parentOfParentLink`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`evidenceCode`)) {
        self$`evidenceCode` <- this_object$`evidenceCode`
      }
      if (!is.null(this_object$`objectClass`)) {
        self$`objectClass` <- this_object$`objectClass`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return AnnotationValueObject in JSON format
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
        if (!is.null(self$`classUri`)) {
          sprintf(
          '"classUri":
            "%s"
                    ',
          self$`classUri`
          )
        },
        if (!is.null(self$`className`)) {
          sprintf(
          '"className":
            "%s"
                    ',
          self$`className`
          )
        },
        if (!is.null(self$`termUri`)) {
          sprintf(
          '"termUri":
            "%s"
                    ',
          self$`termUri`
          )
        },
        if (!is.null(self$`termName`)) {
          sprintf(
          '"termName":
            "%s"
                    ',
          self$`termName`
          )
        },
        if (!is.null(self$`parentName`)) {
          sprintf(
          '"parentName":
            "%s"
                    ',
          self$`parentName`
          )
        },
        if (!is.null(self$`parentDescription`)) {
          sprintf(
          '"parentDescription":
            "%s"
                    ',
          self$`parentDescription`
          )
        },
        if (!is.null(self$`parentLink`)) {
          sprintf(
          '"parentLink":
            "%s"
                    ',
          self$`parentLink`
          )
        },
        if (!is.null(self$`parentOfParentName`)) {
          sprintf(
          '"parentOfParentName":
            "%s"
                    ',
          self$`parentOfParentName`
          )
        },
        if (!is.null(self$`parentOfParentDescription`)) {
          sprintf(
          '"parentOfParentDescription":
            "%s"
                    ',
          self$`parentOfParentDescription`
          )
        },
        if (!is.null(self$`parentOfParentLink`)) {
          sprintf(
          '"parentOfParentLink":
            "%s"
                    ',
          self$`parentOfParentLink`
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
        if (!is.null(self$`evidenceCode`)) {
          sprintf(
          '"evidenceCode":
            "%s"
                    ',
          self$`evidenceCode`
          )
        },
        if (!is.null(self$`objectClass`)) {
          sprintf(
          '"objectClass":
            "%s"
                    ',
          self$`objectClass`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of AnnotationValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of AnnotationValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of AnnotationValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`classUri` <- this_object$`classUri`
      self$`className` <- this_object$`className`
      self$`termUri` <- this_object$`termUri`
      self$`termName` <- this_object$`termName`
      self$`parentName` <- this_object$`parentName`
      self$`parentDescription` <- this_object$`parentDescription`
      self$`parentLink` <- this_object$`parentLink`
      self$`parentOfParentName` <- this_object$`parentOfParentName`
      self$`parentOfParentDescription` <- this_object$`parentOfParentDescription`
      self$`parentOfParentLink` <- this_object$`parentOfParentLink`
      self$`description` <- this_object$`description`
      self$`evidenceCode` <- this_object$`evidenceCode`
      self$`objectClass` <- this_object$`objectClass`
      self
    },
    #' Validate JSON input with respect to AnnotationValueObject
    #'
    #' @description
    #' Validate JSON input with respect to AnnotationValueObject and throw an exception if invalid
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
    #' @return String representation of AnnotationValueObject
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
AnnotationValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
AnnotationValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
AnnotationValueObject$lock()

