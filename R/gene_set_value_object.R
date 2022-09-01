#' Create a new GeneSetValueObject
#'
#' @description
#' GeneSetValueObject Class
#'
#' @docType class
#' @title GeneSetValueObject
#' @description GeneSetValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field description  character [optional]
#' @field geneIds  list(integer) [optional]
#' @field isPublic  character [optional]
#' @field isShared  character [optional]
#' @field name  character [optional]
#' @field size  integer [optional]
#' @field taxonId  integer [optional]
#' @field taxonName  character [optional]
#' @field userOwned  character [optional]
#' @field userCanWrite  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
GeneSetValueObject <- R6::R6Class(
  "GeneSetValueObject",
  public = list(
    `id` = NULL,
    `description` = NULL,
    `geneIds` = NULL,
    `isPublic` = NULL,
    `isShared` = NULL,
    `name` = NULL,
    `size` = NULL,
    `taxonId` = NULL,
    `taxonName` = NULL,
    `userOwned` = NULL,
    `userCanWrite` = NULL,
    #' Initialize a new GeneSetValueObject class.
    #'
    #' @description
    #' Initialize a new GeneSetValueObject class.
    #'
    #' @param id id
    #' @param description description
    #' @param geneIds geneIds
    #' @param isPublic isPublic
    #' @param isShared isShared
    #' @param name name
    #' @param size size
    #' @param taxonId taxonId
    #' @param taxonName taxonName
    #' @param userOwned userOwned
    #' @param userCanWrite userCanWrite
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `description` = NULL, `geneIds` = NULL, `isPublic` = NULL, `isShared` = NULL, `name` = NULL, `size` = NULL, `taxonId` = NULL, `taxonName` = NULL, `userOwned` = NULL, `userCanWrite` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`geneIds`)) {
        stopifnot(is.vector(`geneIds`), length(`geneIds`) != 0)
        sapply(`geneIds`, function(x) stopifnot(is.character(x)))
        self$`geneIds` <- `geneIds`
      }
      if (!is.null(`isPublic`)) {
        stopifnot(is.logical(`isPublic`), length(`isPublic`) == 1)
        self$`isPublic` <- `isPublic`
      }
      if (!is.null(`isShared`)) {
        stopifnot(is.logical(`isShared`), length(`isShared`) == 1)
        self$`isShared` <- `isShared`
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`size`)) {
        stopifnot(is.numeric(`size`), length(`size`) == 1)
        self$`size` <- `size`
      }
      if (!is.null(`taxonId`)) {
        stopifnot(is.numeric(`taxonId`), length(`taxonId`) == 1)
        self$`taxonId` <- `taxonId`
      }
      if (!is.null(`taxonName`)) {
        stopifnot(is.character(`taxonName`), length(`taxonName`) == 1)
        self$`taxonName` <- `taxonName`
      }
      if (!is.null(`userOwned`)) {
        stopifnot(is.logical(`userOwned`), length(`userOwned`) == 1)
        self$`userOwned` <- `userOwned`
      }
      if (!is.null(`userCanWrite`)) {
        stopifnot(is.logical(`userCanWrite`), length(`userCanWrite`) == 1)
        self$`userCanWrite` <- `userCanWrite`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneSetValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      GeneSetValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        GeneSetValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`description`)) {
        GeneSetValueObjectObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`geneIds`)) {
        GeneSetValueObjectObject[["geneIds"]] <-
          self$`geneIds`
      }
      if (!is.null(self$`isPublic`)) {
        GeneSetValueObjectObject[["isPublic"]] <-
          self$`isPublic`
      }
      if (!is.null(self$`isShared`)) {
        GeneSetValueObjectObject[["isShared"]] <-
          self$`isShared`
      }
      if (!is.null(self$`name`)) {
        GeneSetValueObjectObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`size`)) {
        GeneSetValueObjectObject[["size"]] <-
          self$`size`
      }
      if (!is.null(self$`taxonId`)) {
        GeneSetValueObjectObject[["taxonId"]] <-
          self$`taxonId`
      }
      if (!is.null(self$`taxonName`)) {
        GeneSetValueObjectObject[["taxonName"]] <-
          self$`taxonName`
      }
      if (!is.null(self$`userOwned`)) {
        GeneSetValueObjectObject[["userOwned"]] <-
          self$`userOwned`
      }
      if (!is.null(self$`userCanWrite`)) {
        GeneSetValueObjectObject[["userCanWrite"]] <-
          self$`userCanWrite`
      }

      GeneSetValueObjectObject
    },
    #' Deserialize JSON string into an instance of GeneSetValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneSetValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneSetValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`geneIds`)) {
        self$`geneIds` <- ApiClient$new()$deserializeObj(this_object$`geneIds`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`isPublic`)) {
        self$`isPublic` <- this_object$`isPublic`
      }
      if (!is.null(this_object$`isShared`)) {
        self$`isShared` <- this_object$`isShared`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`size`)) {
        self$`size` <- this_object$`size`
      }
      if (!is.null(this_object$`taxonId`)) {
        self$`taxonId` <- this_object$`taxonId`
      }
      if (!is.null(this_object$`taxonName`)) {
        self$`taxonName` <- this_object$`taxonName`
      }
      if (!is.null(this_object$`userOwned`)) {
        self$`userOwned` <- this_object$`userOwned`
      }
      if (!is.null(this_object$`userCanWrite`)) {
        self$`userCanWrite` <- this_object$`userCanWrite`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneSetValueObject in JSON format
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
        if (!is.null(self$`description`)) {
          sprintf(
          '"description":
            "%s"
                    ',
          self$`description`
          )
        },
        if (!is.null(self$`geneIds`)) {
          sprintf(
          '"geneIds":
             [%s]
          ',
          paste(unlist(lapply(self$`geneIds`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`isPublic`)) {
          sprintf(
          '"isPublic":
            %s
                    ',
          tolower(self$`isPublic`)
          )
        },
        if (!is.null(self$`isShared`)) {
          sprintf(
          '"isShared":
            %s
                    ',
          tolower(self$`isShared`)
          )
        },
        if (!is.null(self$`name`)) {
          sprintf(
          '"name":
            "%s"
                    ',
          self$`name`
          )
        },
        if (!is.null(self$`size`)) {
          sprintf(
          '"size":
            %d
                    ',
          self$`size`
          )
        },
        if (!is.null(self$`taxonId`)) {
          sprintf(
          '"taxonId":
            %d
                    ',
          self$`taxonId`
          )
        },
        if (!is.null(self$`taxonName`)) {
          sprintf(
          '"taxonName":
            "%s"
                    ',
          self$`taxonName`
          )
        },
        if (!is.null(self$`userOwned`)) {
          sprintf(
          '"userOwned":
            %s
                    ',
          tolower(self$`userOwned`)
          )
        },
        if (!is.null(self$`userCanWrite`)) {
          sprintf(
          '"userCanWrite":
            %s
                    ',
          tolower(self$`userCanWrite`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of GeneSetValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneSetValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneSetValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`description` <- this_object$`description`
      self$`geneIds` <- ApiClient$new()$deserializeObj(this_object$`geneIds`, "array[integer]", loadNamespace("gemma.R"))
      self$`isPublic` <- this_object$`isPublic`
      self$`isShared` <- this_object$`isShared`
      self$`name` <- this_object$`name`
      self$`size` <- this_object$`size`
      self$`taxonId` <- this_object$`taxonId`
      self$`taxonName` <- this_object$`taxonName`
      self$`userOwned` <- this_object$`userOwned`
      self$`userCanWrite` <- this_object$`userCanWrite`
      self
    },
    #' Validate JSON input with respect to GeneSetValueObject
    #'
    #' @description
    #' Validate JSON input with respect to GeneSetValueObject and throw an exception if invalid
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
    #' @return String representation of GeneSetValueObject
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
GeneSetValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
GeneSetValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
GeneSetValueObject$lock()

