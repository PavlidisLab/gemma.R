#' Create a new BioAssayValueObject
#'
#' @description
#' BioAssayValueObject Class
#'
#' @docType class
#' @title BioAssayValueObject
#' @description BioAssayValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field accession  \link{DatabaseEntryValueObject} [optional]
#' @field arrayDesign  \link{ArrayDesignValueObject} [optional]
#' @field description  character [optional]
#' @field metadata  character [optional]
#' @field name  character [optional]
#' @field originalPlatform  \link{ArrayDesignValueObject} [optional]
#' @field outlier  character [optional]
#' @field predictedOutlier  character [optional]
#' @field processingDate  character [optional]
#' @field sample  \link{BioMaterialValueObject} [optional]
#' @field sequencePairedReads  character [optional]
#' @field sequenceReadCount  integer [optional]
#' @field sequenceReadLength  integer [optional]
#' @field userFlaggedOutlier  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
BioAssayValueObject <- R6::R6Class(
  "BioAssayValueObject",
  public = list(
    `id` = NULL,
    `accession` = NULL,
    `arrayDesign` = NULL,
    `description` = NULL,
    `metadata` = NULL,
    `name` = NULL,
    `originalPlatform` = NULL,
    `outlier` = NULL,
    `predictedOutlier` = NULL,
    `processingDate` = NULL,
    `sample` = NULL,
    `sequencePairedReads` = NULL,
    `sequenceReadCount` = NULL,
    `sequenceReadLength` = NULL,
    `userFlaggedOutlier` = NULL,
    #' Initialize a new BioAssayValueObject class.
    #'
    #' @description
    #' Initialize a new BioAssayValueObject class.
    #'
    #' @param id id
    #' @param accession accession
    #' @param arrayDesign arrayDesign
    #' @param description description
    #' @param metadata metadata
    #' @param name name
    #' @param originalPlatform originalPlatform
    #' @param outlier outlier
    #' @param predictedOutlier predictedOutlier
    #' @param processingDate processingDate
    #' @param sample sample
    #' @param sequencePairedReads sequencePairedReads
    #' @param sequenceReadCount sequenceReadCount
    #' @param sequenceReadLength sequenceReadLength
    #' @param userFlaggedOutlier userFlaggedOutlier
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `accession` = NULL, `arrayDesign` = NULL, `description` = NULL, `metadata` = NULL, `name` = NULL, `originalPlatform` = NULL, `outlier` = NULL, `predictedOutlier` = NULL, `processingDate` = NULL, `sample` = NULL, `sequencePairedReads` = NULL, `sequenceReadCount` = NULL, `sequenceReadLength` = NULL, `userFlaggedOutlier` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`accession`)) {
        stopifnot(R6::is.R6(`accession`))
        self$`accession` <- `accession`
      }
      if (!is.null(`arrayDesign`)) {
        stopifnot(R6::is.R6(`arrayDesign`))
        self$`arrayDesign` <- `arrayDesign`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`metadata`)) {
        stopifnot(is.character(`metadata`), length(`metadata`) == 1)
        self$`metadata` <- `metadata`
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`originalPlatform`)) {
        stopifnot(R6::is.R6(`originalPlatform`))
        self$`originalPlatform` <- `originalPlatform`
      }
      if (!is.null(`outlier`)) {
        stopifnot(is.logical(`outlier`), length(`outlier`) == 1)
        self$`outlier` <- `outlier`
      }
      if (!is.null(`predictedOutlier`)) {
        stopifnot(is.logical(`predictedOutlier`), length(`predictedOutlier`) == 1)
        self$`predictedOutlier` <- `predictedOutlier`
      }
      if (!is.null(`processingDate`)) {
        stopifnot(is.character(`processingDate`), length(`processingDate`) == 1)
        self$`processingDate` <- `processingDate`
      }
      if (!is.null(`sample`)) {
        stopifnot(R6::is.R6(`sample`))
        self$`sample` <- `sample`
      }
      if (!is.null(`sequencePairedReads`)) {
        stopifnot(is.logical(`sequencePairedReads`), length(`sequencePairedReads`) == 1)
        self$`sequencePairedReads` <- `sequencePairedReads`
      }
      if (!is.null(`sequenceReadCount`)) {
        stopifnot(is.numeric(`sequenceReadCount`), length(`sequenceReadCount`) == 1)
        self$`sequenceReadCount` <- `sequenceReadCount`
      }
      if (!is.null(`sequenceReadLength`)) {
        stopifnot(is.numeric(`sequenceReadLength`), length(`sequenceReadLength`) == 1)
        self$`sequenceReadLength` <- `sequenceReadLength`
      }
      if (!is.null(`userFlaggedOutlier`)) {
        stopifnot(is.logical(`userFlaggedOutlier`), length(`userFlaggedOutlier`) == 1)
        self$`userFlaggedOutlier` <- `userFlaggedOutlier`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BioAssayValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      BioAssayValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        BioAssayValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`accession`)) {
        BioAssayValueObjectObject[["accession"]] <-
          self$`accession`$toJSON()
      }
      if (!is.null(self$`arrayDesign`)) {
        BioAssayValueObjectObject[["arrayDesign"]] <-
          self$`arrayDesign`$toJSON()
      }
      if (!is.null(self$`description`)) {
        BioAssayValueObjectObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`metadata`)) {
        BioAssayValueObjectObject[["metadata"]] <-
          self$`metadata`
      }
      if (!is.null(self$`name`)) {
        BioAssayValueObjectObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`originalPlatform`)) {
        BioAssayValueObjectObject[["originalPlatform"]] <-
          self$`originalPlatform`$toJSON()
      }
      if (!is.null(self$`outlier`)) {
        BioAssayValueObjectObject[["outlier"]] <-
          self$`outlier`
      }
      if (!is.null(self$`predictedOutlier`)) {
        BioAssayValueObjectObject[["predictedOutlier"]] <-
          self$`predictedOutlier`
      }
      if (!is.null(self$`processingDate`)) {
        BioAssayValueObjectObject[["processingDate"]] <-
          self$`processingDate`
      }
      if (!is.null(self$`sample`)) {
        BioAssayValueObjectObject[["sample"]] <-
          self$`sample`$toJSON()
      }
      if (!is.null(self$`sequencePairedReads`)) {
        BioAssayValueObjectObject[["sequencePairedReads"]] <-
          self$`sequencePairedReads`
      }
      if (!is.null(self$`sequenceReadCount`)) {
        BioAssayValueObjectObject[["sequenceReadCount"]] <-
          self$`sequenceReadCount`
      }
      if (!is.null(self$`sequenceReadLength`)) {
        BioAssayValueObjectObject[["sequenceReadLength"]] <-
          self$`sequenceReadLength`
      }
      if (!is.null(self$`userFlaggedOutlier`)) {
        BioAssayValueObjectObject[["userFlaggedOutlier"]] <-
          self$`userFlaggedOutlier`
      }

      BioAssayValueObjectObject
    },
    #' Deserialize JSON string into an instance of BioAssayValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of BioAssayValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of BioAssayValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`accession`)) {
        accession_object <- DatabaseEntryValueObject$new()
        accession_object$fromJSON(jsonlite::toJSON(this_object$accession, auto_unbox = TRUE, digits = NA))
        self$`accession` <- accession_object
      }
      if (!is.null(this_object$`arrayDesign`)) {
        arraydesign_object <- ArrayDesignValueObject$new()
        arraydesign_object$fromJSON(jsonlite::toJSON(this_object$arrayDesign, auto_unbox = TRUE, digits = NA))
        self$`arrayDesign` <- arraydesign_object
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`metadata`)) {
        self$`metadata` <- this_object$`metadata`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`originalPlatform`)) {
        originalplatform_object <- ArrayDesignValueObject$new()
        originalplatform_object$fromJSON(jsonlite::toJSON(this_object$originalPlatform, auto_unbox = TRUE, digits = NA))
        self$`originalPlatform` <- originalplatform_object
      }
      if (!is.null(this_object$`outlier`)) {
        self$`outlier` <- this_object$`outlier`
      }
      if (!is.null(this_object$`predictedOutlier`)) {
        self$`predictedOutlier` <- this_object$`predictedOutlier`
      }
      if (!is.null(this_object$`processingDate`)) {
        self$`processingDate` <- this_object$`processingDate`
      }
      if (!is.null(this_object$`sample`)) {
        sample_object <- BioMaterialValueObject$new()
        sample_object$fromJSON(jsonlite::toJSON(this_object$sample, auto_unbox = TRUE, digits = NA))
        self$`sample` <- sample_object
      }
      if (!is.null(this_object$`sequencePairedReads`)) {
        self$`sequencePairedReads` <- this_object$`sequencePairedReads`
      }
      if (!is.null(this_object$`sequenceReadCount`)) {
        self$`sequenceReadCount` <- this_object$`sequenceReadCount`
      }
      if (!is.null(this_object$`sequenceReadLength`)) {
        self$`sequenceReadLength` <- this_object$`sequenceReadLength`
      }
      if (!is.null(this_object$`userFlaggedOutlier`)) {
        self$`userFlaggedOutlier` <- this_object$`userFlaggedOutlier`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BioAssayValueObject in JSON format
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
        if (!is.null(self$`accession`)) {
          sprintf(
          '"accession":
          %s
          ',
          jsonlite::toJSON(self$`accession`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`arrayDesign`)) {
          sprintf(
          '"arrayDesign":
          %s
          ',
          jsonlite::toJSON(self$`arrayDesign`$toJSON(), auto_unbox = TRUE, digits = NA)
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
        if (!is.null(self$`metadata`)) {
          sprintf(
          '"metadata":
            "%s"
                    ',
          self$`metadata`
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
        if (!is.null(self$`originalPlatform`)) {
          sprintf(
          '"originalPlatform":
          %s
          ',
          jsonlite::toJSON(self$`originalPlatform`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`outlier`)) {
          sprintf(
          '"outlier":
            %s
                    ',
          tolower(self$`outlier`)
          )
        },
        if (!is.null(self$`predictedOutlier`)) {
          sprintf(
          '"predictedOutlier":
            %s
                    ',
          tolower(self$`predictedOutlier`)
          )
        },
        if (!is.null(self$`processingDate`)) {
          sprintf(
          '"processingDate":
            "%s"
                    ',
          self$`processingDate`
          )
        },
        if (!is.null(self$`sample`)) {
          sprintf(
          '"sample":
          %s
          ',
          jsonlite::toJSON(self$`sample`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`sequencePairedReads`)) {
          sprintf(
          '"sequencePairedReads":
            %s
                    ',
          tolower(self$`sequencePairedReads`)
          )
        },
        if (!is.null(self$`sequenceReadCount`)) {
          sprintf(
          '"sequenceReadCount":
            %d
                    ',
          self$`sequenceReadCount`
          )
        },
        if (!is.null(self$`sequenceReadLength`)) {
          sprintf(
          '"sequenceReadLength":
            %d
                    ',
          self$`sequenceReadLength`
          )
        },
        if (!is.null(self$`userFlaggedOutlier`)) {
          sprintf(
          '"userFlaggedOutlier":
            %s
                    ',
          tolower(self$`userFlaggedOutlier`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of BioAssayValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of BioAssayValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of BioAssayValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`accession` <- DatabaseEntryValueObject$new()$fromJSON(jsonlite::toJSON(this_object$accession, auto_unbox = TRUE, digits = NA))
      self$`arrayDesign` <- ArrayDesignValueObject$new()$fromJSON(jsonlite::toJSON(this_object$arrayDesign, auto_unbox = TRUE, digits = NA))
      self$`description` <- this_object$`description`
      self$`metadata` <- this_object$`metadata`
      self$`name` <- this_object$`name`
      self$`originalPlatform` <- ArrayDesignValueObject$new()$fromJSON(jsonlite::toJSON(this_object$originalPlatform, auto_unbox = TRUE, digits = NA))
      self$`outlier` <- this_object$`outlier`
      self$`predictedOutlier` <- this_object$`predictedOutlier`
      self$`processingDate` <- this_object$`processingDate`
      self$`sample` <- BioMaterialValueObject$new()$fromJSON(jsonlite::toJSON(this_object$sample, auto_unbox = TRUE, digits = NA))
      self$`sequencePairedReads` <- this_object$`sequencePairedReads`
      self$`sequenceReadCount` <- this_object$`sequenceReadCount`
      self$`sequenceReadLength` <- this_object$`sequenceReadLength`
      self$`userFlaggedOutlier` <- this_object$`userFlaggedOutlier`
      self
    },
    #' Validate JSON input with respect to BioAssayValueObject
    #'
    #' @description
    #' Validate JSON input with respect to BioAssayValueObject and throw an exception if invalid
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
    #' @return String representation of BioAssayValueObject
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
BioAssayValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
BioAssayValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
BioAssayValueObject$lock()

