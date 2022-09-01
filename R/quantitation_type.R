#' Create a new QuantitationType
#'
#' @description
#' QuantitationType Class
#'
#' @docType class
#' @title QuantitationType
#' @description QuantitationType Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field description  character [optional]
#' @field id  integer [optional]
#' @field isBackground  character [optional]
#' @field isBackgroundSubtracted  character [optional]
#' @field isBatchCorrected  character [optional]
#' @field isMaskedPreferred  character [optional]
#' @field isNormalized  character [optional]
#' @field isPreferred  character [optional]
#' @field isRecomputedFromRawData  character [optional]
#' @field isRatio  character [optional]
#' @field generalType  \link{GeneralType} [optional]
#' @field representation  \link{PrimitiveType} [optional]
#' @field scale  \link{ScaleType} [optional]
#' @field type  \link{StandardQuantitationType} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
QuantitationType <- R6::R6Class(
  "QuantitationType",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `isBackground` = NULL,
    `isBackgroundSubtracted` = NULL,
    `isBatchCorrected` = NULL,
    `isMaskedPreferred` = NULL,
    `isNormalized` = NULL,
    `isPreferred` = NULL,
    `isRecomputedFromRawData` = NULL,
    `isRatio` = NULL,
    `generalType` = NULL,
    `representation` = NULL,
    `scale` = NULL,
    `type` = NULL,
    #' Initialize a new QuantitationType class.
    #'
    #' @description
    #' Initialize a new QuantitationType class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param isBackground isBackground
    #' @param isBackgroundSubtracted isBackgroundSubtracted
    #' @param isBatchCorrected isBatchCorrected
    #' @param isMaskedPreferred isMaskedPreferred
    #' @param isNormalized isNormalized
    #' @param isPreferred isPreferred
    #' @param isRecomputedFromRawData isRecomputedFromRawData
    #' @param isRatio isRatio
    #' @param generalType generalType
    #' @param representation representation
    #' @param scale scale
    #' @param type type
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `isBackground` = NULL, `isBackgroundSubtracted` = NULL, `isBatchCorrected` = NULL, `isMaskedPreferred` = NULL, `isNormalized` = NULL, `isPreferred` = NULL, `isRecomputedFromRawData` = NULL, `isRatio` = NULL, `generalType` = NULL, `representation` = NULL, `scale` = NULL, `type` = NULL, ...
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
      if (!is.null(`isBackground`)) {
        stopifnot(is.logical(`isBackground`), length(`isBackground`) == 1)
        self$`isBackground` <- `isBackground`
      }
      if (!is.null(`isBackgroundSubtracted`)) {
        stopifnot(is.logical(`isBackgroundSubtracted`), length(`isBackgroundSubtracted`) == 1)
        self$`isBackgroundSubtracted` <- `isBackgroundSubtracted`
      }
      if (!is.null(`isBatchCorrected`)) {
        stopifnot(is.logical(`isBatchCorrected`), length(`isBatchCorrected`) == 1)
        self$`isBatchCorrected` <- `isBatchCorrected`
      }
      if (!is.null(`isMaskedPreferred`)) {
        stopifnot(is.logical(`isMaskedPreferred`), length(`isMaskedPreferred`) == 1)
        self$`isMaskedPreferred` <- `isMaskedPreferred`
      }
      if (!is.null(`isNormalized`)) {
        stopifnot(is.logical(`isNormalized`), length(`isNormalized`) == 1)
        self$`isNormalized` <- `isNormalized`
      }
      if (!is.null(`isPreferred`)) {
        stopifnot(is.logical(`isPreferred`), length(`isPreferred`) == 1)
        self$`isPreferred` <- `isPreferred`
      }
      if (!is.null(`isRecomputedFromRawData`)) {
        stopifnot(is.logical(`isRecomputedFromRawData`), length(`isRecomputedFromRawData`) == 1)
        self$`isRecomputedFromRawData` <- `isRecomputedFromRawData`
      }
      if (!is.null(`isRatio`)) {
        stopifnot(is.logical(`isRatio`), length(`isRatio`) == 1)
        self$`isRatio` <- `isRatio`
      }
      if (!is.null(`generalType`)) {
        stopifnot(R6::is.R6(`generalType`))
        self$`generalType` <- `generalType`
      }
      if (!is.null(`representation`)) {
        stopifnot(R6::is.R6(`representation`))
        self$`representation` <- `representation`
      }
      if (!is.null(`scale`)) {
        stopifnot(R6::is.R6(`scale`))
        self$`scale` <- `scale`
      }
      if (!is.null(`type`)) {
        stopifnot(R6::is.R6(`type`))
        self$`type` <- `type`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return QuantitationType in JSON format
    #' @keywords internal
    toJSON = function() {
      QuantitationTypeObject <- list()
      if (!is.null(self$`name`)) {
        QuantitationTypeObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        QuantitationTypeObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        QuantitationTypeObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`isBackground`)) {
        QuantitationTypeObject[["isBackground"]] <-
          self$`isBackground`
      }
      if (!is.null(self$`isBackgroundSubtracted`)) {
        QuantitationTypeObject[["isBackgroundSubtracted"]] <-
          self$`isBackgroundSubtracted`
      }
      if (!is.null(self$`isBatchCorrected`)) {
        QuantitationTypeObject[["isBatchCorrected"]] <-
          self$`isBatchCorrected`
      }
      if (!is.null(self$`isMaskedPreferred`)) {
        QuantitationTypeObject[["isMaskedPreferred"]] <-
          self$`isMaskedPreferred`
      }
      if (!is.null(self$`isNormalized`)) {
        QuantitationTypeObject[["isNormalized"]] <-
          self$`isNormalized`
      }
      if (!is.null(self$`isPreferred`)) {
        QuantitationTypeObject[["isPreferred"]] <-
          self$`isPreferred`
      }
      if (!is.null(self$`isRecomputedFromRawData`)) {
        QuantitationTypeObject[["isRecomputedFromRawData"]] <-
          self$`isRecomputedFromRawData`
      }
      if (!is.null(self$`isRatio`)) {
        QuantitationTypeObject[["isRatio"]] <-
          self$`isRatio`
      }
      if (!is.null(self$`generalType`)) {
        QuantitationTypeObject[["generalType"]] <-
          self$`generalType`$toJSON()
      }
      if (!is.null(self$`representation`)) {
        QuantitationTypeObject[["representation"]] <-
          self$`representation`$toJSON()
      }
      if (!is.null(self$`scale`)) {
        QuantitationTypeObject[["scale"]] <-
          self$`scale`$toJSON()
      }
      if (!is.null(self$`type`)) {
        QuantitationTypeObject[["type"]] <-
          self$`type`$toJSON()
      }

      QuantitationTypeObject
    },
    #' Deserialize JSON string into an instance of QuantitationType
    #'
    #' @description
    #' Deserialize JSON string into an instance of QuantitationType
    #'
    #' @param input_json the JSON input
    #' @return the instance of QuantitationType
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
      if (!is.null(this_object$`isBackground`)) {
        self$`isBackground` <- this_object$`isBackground`
      }
      if (!is.null(this_object$`isBackgroundSubtracted`)) {
        self$`isBackgroundSubtracted` <- this_object$`isBackgroundSubtracted`
      }
      if (!is.null(this_object$`isBatchCorrected`)) {
        self$`isBatchCorrected` <- this_object$`isBatchCorrected`
      }
      if (!is.null(this_object$`isMaskedPreferred`)) {
        self$`isMaskedPreferred` <- this_object$`isMaskedPreferred`
      }
      if (!is.null(this_object$`isNormalized`)) {
        self$`isNormalized` <- this_object$`isNormalized`
      }
      if (!is.null(this_object$`isPreferred`)) {
        self$`isPreferred` <- this_object$`isPreferred`
      }
      if (!is.null(this_object$`isRecomputedFromRawData`)) {
        self$`isRecomputedFromRawData` <- this_object$`isRecomputedFromRawData`
      }
      if (!is.null(this_object$`isRatio`)) {
        self$`isRatio` <- this_object$`isRatio`
      }
      if (!is.null(this_object$`generalType`)) {
        generaltype_object <- GeneralType$new()
        generaltype_object$fromJSON(jsonlite::toJSON(this_object$generalType, auto_unbox = TRUE, digits = NA))
        self$`generalType` <- generaltype_object
      }
      if (!is.null(this_object$`representation`)) {
        representation_object <- PrimitiveType$new()
        representation_object$fromJSON(jsonlite::toJSON(this_object$representation, auto_unbox = TRUE, digits = NA))
        self$`representation` <- representation_object
      }
      if (!is.null(this_object$`scale`)) {
        scale_object <- ScaleType$new()
        scale_object$fromJSON(jsonlite::toJSON(this_object$scale, auto_unbox = TRUE, digits = NA))
        self$`scale` <- scale_object
      }
      if (!is.null(this_object$`type`)) {
        type_object <- StandardQuantitationType$new()
        type_object$fromJSON(jsonlite::toJSON(this_object$type, auto_unbox = TRUE, digits = NA))
        self$`type` <- type_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return QuantitationType in JSON format
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
        if (!is.null(self$`isBackground`)) {
          sprintf(
          '"isBackground":
            %s
                    ',
          tolower(self$`isBackground`)
          )
        },
        if (!is.null(self$`isBackgroundSubtracted`)) {
          sprintf(
          '"isBackgroundSubtracted":
            %s
                    ',
          tolower(self$`isBackgroundSubtracted`)
          )
        },
        if (!is.null(self$`isBatchCorrected`)) {
          sprintf(
          '"isBatchCorrected":
            %s
                    ',
          tolower(self$`isBatchCorrected`)
          )
        },
        if (!is.null(self$`isMaskedPreferred`)) {
          sprintf(
          '"isMaskedPreferred":
            %s
                    ',
          tolower(self$`isMaskedPreferred`)
          )
        },
        if (!is.null(self$`isNormalized`)) {
          sprintf(
          '"isNormalized":
            %s
                    ',
          tolower(self$`isNormalized`)
          )
        },
        if (!is.null(self$`isPreferred`)) {
          sprintf(
          '"isPreferred":
            %s
                    ',
          tolower(self$`isPreferred`)
          )
        },
        if (!is.null(self$`isRecomputedFromRawData`)) {
          sprintf(
          '"isRecomputedFromRawData":
            %s
                    ',
          tolower(self$`isRecomputedFromRawData`)
          )
        },
        if (!is.null(self$`isRatio`)) {
          sprintf(
          '"isRatio":
            %s
                    ',
          tolower(self$`isRatio`)
          )
        },
        if (!is.null(self$`generalType`)) {
          sprintf(
          '"generalType":
          %s
          ',
          jsonlite::toJSON(self$`generalType`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`representation`)) {
          sprintf(
          '"representation":
          %s
          ',
          jsonlite::toJSON(self$`representation`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`scale`)) {
          sprintf(
          '"scale":
          %s
          ',
          jsonlite::toJSON(self$`scale`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`type`)) {
          sprintf(
          '"type":
          %s
          ',
          jsonlite::toJSON(self$`type`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of QuantitationType
    #'
    #' @description
    #' Deserialize JSON string into an instance of QuantitationType
    #'
    #' @param input_json the JSON input
    #' @return the instance of QuantitationType
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`isBackground` <- this_object$`isBackground`
      self$`isBackgroundSubtracted` <- this_object$`isBackgroundSubtracted`
      self$`isBatchCorrected` <- this_object$`isBatchCorrected`
      self$`isMaskedPreferred` <- this_object$`isMaskedPreferred`
      self$`isNormalized` <- this_object$`isNormalized`
      self$`isPreferred` <- this_object$`isPreferred`
      self$`isRecomputedFromRawData` <- this_object$`isRecomputedFromRawData`
      self$`isRatio` <- this_object$`isRatio`
      self$`generalType` <- GeneralType$new()$fromJSON(jsonlite::toJSON(this_object$generalType, auto_unbox = TRUE, digits = NA))
      self$`representation` <- PrimitiveType$new()$fromJSON(jsonlite::toJSON(this_object$representation, auto_unbox = TRUE, digits = NA))
      self$`scale` <- ScaleType$new()$fromJSON(jsonlite::toJSON(this_object$scale, auto_unbox = TRUE, digits = NA))
      self$`type` <- StandardQuantitationType$new()$fromJSON(jsonlite::toJSON(this_object$type, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to QuantitationType
    #'
    #' @description
    #' Validate JSON input with respect to QuantitationType and throw an exception if invalid
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
    #' @return String representation of QuantitationType
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
QuantitationType$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
QuantitationType$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
QuantitationType$lock()

