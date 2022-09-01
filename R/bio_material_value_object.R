#' Create a new BioMaterialValueObject
#'
#' @description
#' BioMaterialValueObject Class
#'
#' @docType class
#' @title BioMaterialValueObject
#' @description BioMaterialValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field assayDescription  character optional
#' @field assayName  character optional
#' @field bioAssays  list(integer) optional
#' @field characteristics  list(\link{CharacteristicValueObject}) optional
#' @field description  character optional
#' @field factorIdToFactorValueId  named list(character) optional
#' @field factors  named list(character) optional
#' @field factorValueObjects  list(\link{IdentifiableValueObject}) optional
#' @field factorValues  named list(character) optional
#' @field name  character optional
#' @field assayProcessingDate  character optional
#' @field characteristicValues  named list(character) optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
BioMaterialValueObject <- R6::R6Class(
  "BioMaterialValueObject",
  public = list(
    `id` = NULL,
    `assayDescription` = NULL,
    `assayName` = NULL,
    `bioAssays` = NULL,
    `characteristics` = NULL,
    `description` = NULL,
    `factorIdToFactorValueId` = NULL,
    `factors` = NULL,
    `factorValueObjects` = NULL,
    `factorValues` = NULL,
    `name` = NULL,
    `assayProcessingDate` = NULL,
    `characteristicValues` = NULL,
    #' Initialize a new BioMaterialValueObject class.
    #'
    #' @description
    #' Initialize a new BioMaterialValueObject class.
    #'
    #' @param id id
    #' @param assayDescription assayDescription
    #' @param assayName assayName
    #' @param bioAssays bioAssays
    #' @param characteristics characteristics
    #' @param description description
    #' @param factorIdToFactorValueId factorIdToFactorValueId
    #' @param factors factors
    #' @param factorValueObjects factorValueObjects
    #' @param factorValues factorValues
    #' @param name name
    #' @param assayProcessingDate assayProcessingDate
    #' @param characteristicValues characteristicValues
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `assayDescription` = NULL, `assayName` = NULL, `bioAssays` = NULL, `characteristics` = NULL, `description` = NULL, `factorIdToFactorValueId` = NULL, `factors` = NULL, `factorValueObjects` = NULL, `factorValues` = NULL, `name` = NULL, `assayProcessingDate` = NULL, `characteristicValues` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`assayDescription`)) {
        stopifnot(is.character(`assayDescription`), length(`assayDescription`) == 1)
        self$`assayDescription` <- `assayDescription`
      }
      if (!is.null(`assayName`)) {
        stopifnot(is.character(`assayName`), length(`assayName`) == 1)
        self$`assayName` <- `assayName`
      }
      if (!is.null(`bioAssays`)) {
        stopifnot(is.vector(`bioAssays`), length(`bioAssays`) != 0)
        sapply(`bioAssays`, function(x) stopifnot(is.character(x)))
        self$`bioAssays` <- `bioAssays`
      }
      if (!is.null(`characteristics`)) {
        stopifnot(is.vector(`characteristics`), length(`characteristics`) != 0)
        sapply(`characteristics`, function(x) stopifnot(R6::is.R6(x)))
        self$`characteristics` <- `characteristics`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`factorIdToFactorValueId`)) {
        stopifnot(is.vector(`factorIdToFactorValueId`), length(`factorIdToFactorValueId`) != 0)
        sapply(`factorIdToFactorValueId`, function(x) stopifnot(is.character(x)))
        self$`factorIdToFactorValueId` <- `factorIdToFactorValueId`
      }
      if (!is.null(`factors`)) {
        stopifnot(is.vector(`factors`), length(`factors`) != 0)
        sapply(`factors`, function(x) stopifnot(is.character(x)))
        self$`factors` <- `factors`
      }
      if (!is.null(`factorValueObjects`)) {
        stopifnot(is.vector(`factorValueObjects`), length(`factorValueObjects`) != 0)
        sapply(`factorValueObjects`, function(x) stopifnot(R6::is.R6(x)))
        self$`factorValueObjects` <- `factorValueObjects`
      }
      if (!is.null(`factorValues`)) {
        stopifnot(is.vector(`factorValues`), length(`factorValues`) != 0)
        sapply(`factorValues`, function(x) stopifnot(is.character(x)))
        self$`factorValues` <- `factorValues`
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`assayProcessingDate`)) {
        stopifnot(is.character(`assayProcessingDate`), length(`assayProcessingDate`) == 1)
        self$`assayProcessingDate` <- `assayProcessingDate`
      }
      if (!is.null(`characteristicValues`)) {
        stopifnot(is.vector(`characteristicValues`), length(`characteristicValues`) != 0)
        sapply(`characteristicValues`, function(x) stopifnot(is.character(x)))
        self$`characteristicValues` <- `characteristicValues`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BioMaterialValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      BioMaterialValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        BioMaterialValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`assayDescription`)) {
        BioMaterialValueObjectObject[["assayDescription"]] <-
          self$`assayDescription`
      }
      if (!is.null(self$`assayName`)) {
        BioMaterialValueObjectObject[["assayName"]] <-
          self$`assayName`
      }
      if (!is.null(self$`bioAssays`)) {
        BioMaterialValueObjectObject[["bioAssays"]] <-
          self$`bioAssays`
      }
      if (!is.null(self$`characteristics`)) {
        BioMaterialValueObjectObject[["characteristics"]] <-
          lapply(self$`characteristics`, function(x) x$toJSON())
      }
      if (!is.null(self$`description`)) {
        BioMaterialValueObjectObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`factorIdToFactorValueId`)) {
        BioMaterialValueObjectObject[["factorIdToFactorValueId"]] <-
          self$`factorIdToFactorValueId`
      }
      if (!is.null(self$`factors`)) {
        BioMaterialValueObjectObject[["factors"]] <-
          self$`factors`
      }
      if (!is.null(self$`factorValueObjects`)) {
        BioMaterialValueObjectObject[["factorValueObjects"]] <-
          lapply(self$`factorValueObjects`, function(x) x$toJSON())
      }
      if (!is.null(self$`factorValues`)) {
        BioMaterialValueObjectObject[["factorValues"]] <-
          self$`factorValues`
      }
      if (!is.null(self$`name`)) {
        BioMaterialValueObjectObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`assayProcessingDate`)) {
        BioMaterialValueObjectObject[["assayProcessingDate"]] <-
          self$`assayProcessingDate`
      }
      if (!is.null(self$`characteristicValues`)) {
        BioMaterialValueObjectObject[["characteristicValues"]] <-
          self$`characteristicValues`
      }

      BioMaterialValueObjectObject
    },
    #' Deserialize JSON string into an instance of BioMaterialValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of BioMaterialValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of BioMaterialValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`assayDescription`)) {
        self$`assayDescription` <- this_object$`assayDescription`
      }
      if (!is.null(this_object$`assayName`)) {
        self$`assayName` <- this_object$`assayName`
      }
      if (!is.null(this_object$`bioAssays`)) {
        self$`bioAssays` <- ApiClient$new()$deserializeObj(this_object$`bioAssays`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`characteristics`)) {
        self$`characteristics` <- ApiClient$new()$deserializeObj(this_object$`characteristics`, "array[CharacteristicValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`factorIdToFactorValueId`)) {
        self$`factorIdToFactorValueId` <- ApiClient$new()$deserializeObj(this_object$`factorIdToFactorValueId`, "map(character)", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`factors`)) {
        self$`factors` <- ApiClient$new()$deserializeObj(this_object$`factors`, "map(character)", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`factorValueObjects`)) {
        self$`factorValueObjects` <- ApiClient$new()$deserializeObj(this_object$`factorValueObjects`, "array[IdentifiableValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`factorValues`)) {
        self$`factorValues` <- ApiClient$new()$deserializeObj(this_object$`factorValues`, "map(character)", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`assayProcessingDate`)) {
        self$`assayProcessingDate` <- this_object$`assayProcessingDate`
      }
      if (!is.null(this_object$`characteristicValues`)) {
        self$`characteristicValues` <- ApiClient$new()$deserializeObj(this_object$`characteristicValues`, "map(character)", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BioMaterialValueObject in JSON format
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
        if (!is.null(self$`assayDescription`)) {
          sprintf(
          '"assayDescription":
            "%s"
                    ',
          self$`assayDescription`
          )
        },
        if (!is.null(self$`assayName`)) {
          sprintf(
          '"assayName":
            "%s"
                    ',
          self$`assayName`
          )
        },
        if (!is.null(self$`bioAssays`)) {
          sprintf(
          '"bioAssays":
             [%s]
          ',
          paste(unlist(lapply(self$`bioAssays`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`characteristics`)) {
          sprintf(
          '"characteristics":
          [%s]
',
          paste(sapply(self$`characteristics`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
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
        if (!is.null(self$`factorIdToFactorValueId`)) {
          sprintf(
          '"factorIdToFactorValueId":
            "%s"
          ',
          jsonlite::toJSON(lapply(self$`factorIdToFactorValueId`, function(x){ x }), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`factors`)) {
          sprintf(
          '"factors":
            "%s"
          ',
          jsonlite::toJSON(lapply(self$`factors`, function(x){ x }), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`factorValueObjects`)) {
          sprintf(
          '"factorValueObjects":
          [%s]
',
          paste(sapply(self$`factorValueObjects`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`factorValues`)) {
          sprintf(
          '"factorValues":
            "%s"
          ',
          jsonlite::toJSON(lapply(self$`factorValues`, function(x){ x }), auto_unbox = TRUE, digits = NA)
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
        if (!is.null(self$`assayProcessingDate`)) {
          sprintf(
          '"assayProcessingDate":
            "%s"
                    ',
          self$`assayProcessingDate`
          )
        },
        if (!is.null(self$`characteristicValues`)) {
          sprintf(
          '"characteristicValues":
            "%s"
          ',
          jsonlite::toJSON(lapply(self$`characteristicValues`, function(x){ x }), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of BioMaterialValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of BioMaterialValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of BioMaterialValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`assayDescription` <- this_object$`assayDescription`
      self$`assayName` <- this_object$`assayName`
      self$`bioAssays` <- ApiClient$new()$deserializeObj(this_object$`bioAssays`, "array[integer]", loadNamespace("gemma.R"))
      self$`characteristics` <- ApiClient$new()$deserializeObj(this_object$`characteristics`, "array[CharacteristicValueObject]", loadNamespace("gemma.R"))
      self$`description` <- this_object$`description`
      self$`factorIdToFactorValueId` <- ApiClient$new()$deserializeObj(this_object$`factorIdToFactorValueId`, "map(character)", loadNamespace("gemma.R"))
      self$`factors` <- ApiClient$new()$deserializeObj(this_object$`factors`, "map(character)", loadNamespace("gemma.R"))
      self$`factorValueObjects` <- ApiClient$new()$deserializeObj(this_object$`factorValueObjects`, "array[IdentifiableValueObject]", loadNamespace("gemma.R"))
      self$`factorValues` <- ApiClient$new()$deserializeObj(this_object$`factorValues`, "map(character)", loadNamespace("gemma.R"))
      self$`name` <- this_object$`name`
      self$`assayProcessingDate` <- this_object$`assayProcessingDate`
      self$`characteristicValues` <- ApiClient$new()$deserializeObj(this_object$`characteristicValues`, "map(character)", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to BioMaterialValueObject
    #'
    #' @description
    #' Validate JSON input with respect to BioMaterialValueObject and throw an exception if invalid
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
    #' @return String representation of BioMaterialValueObject
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
BioMaterialValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
BioMaterialValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
BioMaterialValueObject$lock()

