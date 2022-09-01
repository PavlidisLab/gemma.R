#' Create a new FactorValueBasicValueObject
#'
#' @description
#' FactorValueBasicValueObject Class
#'
#' @docType class
#' @title FactorValueBasicValueObject
#' @description FactorValueBasicValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field characteristics  list(\link{CharacteristicBasicValueObject}) optional
#' @field experimentalFactorCategory  \link{CharacteristicBasicValueObject} optional
#' @field measurement  \link{MeasurementValueObject} optional
#' @field fvValue  character optional
#' @field fvSummary  character optional
#' @field experimentalFactorId  integer optional
#' @field baseline  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
FactorValueBasicValueObject <- R6::R6Class(
  "FactorValueBasicValueObject",
  public = list(
    `id` = NULL,
    `characteristics` = NULL,
    `experimentalFactorCategory` = NULL,
    `measurement` = NULL,
    `fvValue` = NULL,
    `fvSummary` = NULL,
    `experimentalFactorId` = NULL,
    `baseline` = NULL,
    #' Initialize a new FactorValueBasicValueObject class.
    #'
    #' @description
    #' Initialize a new FactorValueBasicValueObject class.
    #'
    #' @param id id
    #' @param characteristics characteristics
    #' @param experimentalFactorCategory experimentalFactorCategory
    #' @param measurement measurement
    #' @param fvValue fvValue
    #' @param fvSummary fvSummary
    #' @param experimentalFactorId experimentalFactorId
    #' @param baseline baseline
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `characteristics` = NULL, `experimentalFactorCategory` = NULL, `measurement` = NULL, `fvValue` = NULL, `fvSummary` = NULL, `experimentalFactorId` = NULL, `baseline` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`characteristics`)) {
        stopifnot(is.vector(`characteristics`), length(`characteristics`) != 0)
        sapply(`characteristics`, function(x) stopifnot(R6::is.R6(x)))
        self$`characteristics` <- `characteristics`
      }
      if (!is.null(`experimentalFactorCategory`)) {
        stopifnot(R6::is.R6(`experimentalFactorCategory`))
        self$`experimentalFactorCategory` <- `experimentalFactorCategory`
      }
      if (!is.null(`measurement`)) {
        stopifnot(R6::is.R6(`measurement`))
        self$`measurement` <- `measurement`
      }
      if (!is.null(`fvValue`)) {
        stopifnot(is.character(`fvValue`), length(`fvValue`) == 1)
        self$`fvValue` <- `fvValue`
      }
      if (!is.null(`fvSummary`)) {
        stopifnot(is.character(`fvSummary`), length(`fvSummary`) == 1)
        self$`fvSummary` <- `fvSummary`
      }
      if (!is.null(`experimentalFactorId`)) {
        stopifnot(is.numeric(`experimentalFactorId`), length(`experimentalFactorId`) == 1)
        self$`experimentalFactorId` <- `experimentalFactorId`
      }
      if (!is.null(`baseline`)) {
        stopifnot(is.logical(`baseline`), length(`baseline`) == 1)
        self$`baseline` <- `baseline`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return FactorValueBasicValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      FactorValueBasicValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        FactorValueBasicValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`characteristics`)) {
        FactorValueBasicValueObjectObject[["characteristics"]] <-
          lapply(self$`characteristics`, function(x) x$toJSON())
      }
      if (!is.null(self$`experimentalFactorCategory`)) {
        FactorValueBasicValueObjectObject[["experimentalFactorCategory"]] <-
          self$`experimentalFactorCategory`$toJSON()
      }
      if (!is.null(self$`measurement`)) {
        FactorValueBasicValueObjectObject[["measurement"]] <-
          self$`measurement`$toJSON()
      }
      if (!is.null(self$`fvValue`)) {
        FactorValueBasicValueObjectObject[["fvValue"]] <-
          self$`fvValue`
      }
      if (!is.null(self$`fvSummary`)) {
        FactorValueBasicValueObjectObject[["fvSummary"]] <-
          self$`fvSummary`
      }
      if (!is.null(self$`experimentalFactorId`)) {
        FactorValueBasicValueObjectObject[["experimentalFactorId"]] <-
          self$`experimentalFactorId`
      }
      if (!is.null(self$`baseline`)) {
        FactorValueBasicValueObjectObject[["baseline"]] <-
          self$`baseline`
      }

      FactorValueBasicValueObjectObject
    },
    #' Deserialize JSON string into an instance of FactorValueBasicValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of FactorValueBasicValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of FactorValueBasicValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`characteristics`)) {
        self$`characteristics` <- ApiClient$new()$deserializeObj(this_object$`characteristics`, "array[CharacteristicBasicValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`experimentalFactorCategory`)) {
        experimentalfactorcategory_object <- CharacteristicBasicValueObject$new()
        experimentalfactorcategory_object$fromJSON(jsonlite::toJSON(this_object$experimentalFactorCategory, auto_unbox = TRUE, digits = NA))
        self$`experimentalFactorCategory` <- experimentalfactorcategory_object
      }
      if (!is.null(this_object$`measurement`)) {
        measurement_object <- MeasurementValueObject$new()
        measurement_object$fromJSON(jsonlite::toJSON(this_object$measurement, auto_unbox = TRUE, digits = NA))
        self$`measurement` <- measurement_object
      }
      if (!is.null(this_object$`fvValue`)) {
        self$`fvValue` <- this_object$`fvValue`
      }
      if (!is.null(this_object$`fvSummary`)) {
        self$`fvSummary` <- this_object$`fvSummary`
      }
      if (!is.null(this_object$`experimentalFactorId`)) {
        self$`experimentalFactorId` <- this_object$`experimentalFactorId`
      }
      if (!is.null(this_object$`baseline`)) {
        self$`baseline` <- this_object$`baseline`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return FactorValueBasicValueObject in JSON format
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
        if (!is.null(self$`characteristics`)) {
          sprintf(
          '"characteristics":
          [%s]
',
          paste(sapply(self$`characteristics`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`experimentalFactorCategory`)) {
          sprintf(
          '"experimentalFactorCategory":
          %s
          ',
          jsonlite::toJSON(self$`experimentalFactorCategory`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`measurement`)) {
          sprintf(
          '"measurement":
          %s
          ',
          jsonlite::toJSON(self$`measurement`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`fvValue`)) {
          sprintf(
          '"fvValue":
            "%s"
                    ',
          self$`fvValue`
          )
        },
        if (!is.null(self$`fvSummary`)) {
          sprintf(
          '"fvSummary":
            "%s"
                    ',
          self$`fvSummary`
          )
        },
        if (!is.null(self$`experimentalFactorId`)) {
          sprintf(
          '"experimentalFactorId":
            %d
                    ',
          self$`experimentalFactorId`
          )
        },
        if (!is.null(self$`baseline`)) {
          sprintf(
          '"baseline":
            %s
                    ',
          tolower(self$`baseline`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of FactorValueBasicValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of FactorValueBasicValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of FactorValueBasicValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`characteristics` <- ApiClient$new()$deserializeObj(this_object$`characteristics`, "array[CharacteristicBasicValueObject]", loadNamespace("gemma.R"))
      self$`experimentalFactorCategory` <- CharacteristicBasicValueObject$new()$fromJSON(jsonlite::toJSON(this_object$experimentalFactorCategory, auto_unbox = TRUE, digits = NA))
      self$`measurement` <- MeasurementValueObject$new()$fromJSON(jsonlite::toJSON(this_object$measurement, auto_unbox = TRUE, digits = NA))
      self$`fvValue` <- this_object$`fvValue`
      self$`fvSummary` <- this_object$`fvSummary`
      self$`experimentalFactorId` <- this_object$`experimentalFactorId`
      self$`baseline` <- this_object$`baseline`
      self
    },
    #' Validate JSON input with respect to FactorValueBasicValueObject
    #'
    #' @description
    #' Validate JSON input with respect to FactorValueBasicValueObject and throw an exception if invalid
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
    #' @return String representation of FactorValueBasicValueObject
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
FactorValueBasicValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
FactorValueBasicValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
FactorValueBasicValueObject$lock()

