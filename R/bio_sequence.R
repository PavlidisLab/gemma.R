#' Create a new BioSequence
#'
#' @description
#' BioSequence Class
#'
#' @docType class
#' @title BioSequence
#' @description BioSequence Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field description  character [optional]
#' @field id  integer [optional]
#' @field length  integer [optional]
#' @field sequence  character [optional]
#' @field isApproximateLength  character [optional]
#' @field isCircular  character [optional]
#' @field polymerType  \link{PolymerType} [optional]
#' @field type  \link{SequenceType} [optional]
#' @field fractionRepeats  numeric [optional]
#' @field sequenceDatabaseEntry  \link{DatabaseEntry} [optional]
#' @field taxon  \link{Taxon} [optional]
#' @field bioSequence2GeneProduct  list(\link{BioSequence2GeneProduct}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
BioSequence <- R6::R6Class(
  "BioSequence",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `length` = NULL,
    `sequence` = NULL,
    `isApproximateLength` = NULL,
    `isCircular` = NULL,
    `polymerType` = NULL,
    `type` = NULL,
    `fractionRepeats` = NULL,
    `sequenceDatabaseEntry` = NULL,
    `taxon` = NULL,
    `bioSequence2GeneProduct` = NULL,
    #' Initialize a new BioSequence class.
    #'
    #' @description
    #' Initialize a new BioSequence class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param length length
    #' @param sequence sequence
    #' @param isApproximateLength isApproximateLength
    #' @param isCircular isCircular
    #' @param polymerType polymerType
    #' @param type type
    #' @param fractionRepeats fractionRepeats
    #' @param sequenceDatabaseEntry sequenceDatabaseEntry
    #' @param taxon taxon
    #' @param bioSequence2GeneProduct bioSequence2GeneProduct
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `length` = NULL, `sequence` = NULL, `isApproximateLength` = NULL, `isCircular` = NULL, `polymerType` = NULL, `type` = NULL, `fractionRepeats` = NULL, `sequenceDatabaseEntry` = NULL, `taxon` = NULL, `bioSequence2GeneProduct` = NULL, ...
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
      if (!is.null(`length`)) {
        stopifnot(is.numeric(`length`), length(`length`) == 1)
        self$`length` <- `length`
      }
      if (!is.null(`sequence`)) {
        stopifnot(is.character(`sequence`), length(`sequence`) == 1)
        self$`sequence` <- `sequence`
      }
      if (!is.null(`isApproximateLength`)) {
        stopifnot(is.logical(`isApproximateLength`), length(`isApproximateLength`) == 1)
        self$`isApproximateLength` <- `isApproximateLength`
      }
      if (!is.null(`isCircular`)) {
        stopifnot(is.logical(`isCircular`), length(`isCircular`) == 1)
        self$`isCircular` <- `isCircular`
      }
      if (!is.null(`polymerType`)) {
        stopifnot(R6::is.R6(`polymerType`))
        self$`polymerType` <- `polymerType`
      }
      if (!is.null(`type`)) {
        stopifnot(R6::is.R6(`type`))
        self$`type` <- `type`
      }
      if (!is.null(`fractionRepeats`)) {
        stopifnot(is.numeric(`fractionRepeats`), length(`fractionRepeats`) == 1)
        self$`fractionRepeats` <- `fractionRepeats`
      }
      if (!is.null(`sequenceDatabaseEntry`)) {
        stopifnot(R6::is.R6(`sequenceDatabaseEntry`))
        self$`sequenceDatabaseEntry` <- `sequenceDatabaseEntry`
      }
      if (!is.null(`taxon`)) {
        stopifnot(R6::is.R6(`taxon`))
        self$`taxon` <- `taxon`
      }
      if (!is.null(`bioSequence2GeneProduct`)) {
        stopifnot(is.vector(`bioSequence2GeneProduct`), length(`bioSequence2GeneProduct`) != 0)
        sapply(`bioSequence2GeneProduct`, function(x) stopifnot(R6::is.R6(x)))
        self$`bioSequence2GeneProduct` <- `bioSequence2GeneProduct`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BioSequence in JSON format
    #' @keywords internal
    toJSON = function() {
      BioSequenceObject <- list()
      if (!is.null(self$`name`)) {
        BioSequenceObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        BioSequenceObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        BioSequenceObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`length`)) {
        BioSequenceObject[["length"]] <-
          self$`length`
      }
      if (!is.null(self$`sequence`)) {
        BioSequenceObject[["sequence"]] <-
          self$`sequence`
      }
      if (!is.null(self$`isApproximateLength`)) {
        BioSequenceObject[["isApproximateLength"]] <-
          self$`isApproximateLength`
      }
      if (!is.null(self$`isCircular`)) {
        BioSequenceObject[["isCircular"]] <-
          self$`isCircular`
      }
      if (!is.null(self$`polymerType`)) {
        BioSequenceObject[["polymerType"]] <-
          self$`polymerType`$toJSON()
      }
      if (!is.null(self$`type`)) {
        BioSequenceObject[["type"]] <-
          self$`type`$toJSON()
      }
      if (!is.null(self$`fractionRepeats`)) {
        BioSequenceObject[["fractionRepeats"]] <-
          self$`fractionRepeats`
      }
      if (!is.null(self$`sequenceDatabaseEntry`)) {
        BioSequenceObject[["sequenceDatabaseEntry"]] <-
          self$`sequenceDatabaseEntry`$toJSON()
      }
      if (!is.null(self$`taxon`)) {
        BioSequenceObject[["taxon"]] <-
          self$`taxon`$toJSON()
      }
      if (!is.null(self$`bioSequence2GeneProduct`)) {
        BioSequenceObject[["bioSequence2GeneProduct"]] <-
          lapply(self$`bioSequence2GeneProduct`, function(x) x$toJSON())
      }

      BioSequenceObject
    },
    #' Deserialize JSON string into an instance of BioSequence
    #'
    #' @description
    #' Deserialize JSON string into an instance of BioSequence
    #'
    #' @param input_json the JSON input
    #' @return the instance of BioSequence
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
      if (!is.null(this_object$`length`)) {
        self$`length` <- this_object$`length`
      }
      if (!is.null(this_object$`sequence`)) {
        self$`sequence` <- this_object$`sequence`
      }
      if (!is.null(this_object$`isApproximateLength`)) {
        self$`isApproximateLength` <- this_object$`isApproximateLength`
      }
      if (!is.null(this_object$`isCircular`)) {
        self$`isCircular` <- this_object$`isCircular`
      }
      if (!is.null(this_object$`polymerType`)) {
        polymertype_object <- PolymerType$new()
        polymertype_object$fromJSON(jsonlite::toJSON(this_object$polymerType, auto_unbox = TRUE, digits = NA))
        self$`polymerType` <- polymertype_object
      }
      if (!is.null(this_object$`type`)) {
        type_object <- SequenceType$new()
        type_object$fromJSON(jsonlite::toJSON(this_object$type, auto_unbox = TRUE, digits = NA))
        self$`type` <- type_object
      }
      if (!is.null(this_object$`fractionRepeats`)) {
        self$`fractionRepeats` <- this_object$`fractionRepeats`
      }
      if (!is.null(this_object$`sequenceDatabaseEntry`)) {
        sequencedatabaseentry_object <- DatabaseEntry$new()
        sequencedatabaseentry_object$fromJSON(jsonlite::toJSON(this_object$sequenceDatabaseEntry, auto_unbox = TRUE, digits = NA))
        self$`sequenceDatabaseEntry` <- sequencedatabaseentry_object
      }
      if (!is.null(this_object$`taxon`)) {
        taxon_object <- Taxon$new()
        taxon_object$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
        self$`taxon` <- taxon_object
      }
      if (!is.null(this_object$`bioSequence2GeneProduct`)) {
        self$`bioSequence2GeneProduct` <- ApiClient$new()$deserializeObj(this_object$`bioSequence2GeneProduct`, "set[BioSequence2GeneProduct]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BioSequence in JSON format
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
        if (!is.null(self$`length`)) {
          sprintf(
          '"length":
            %d
                    ',
          self$`length`
          )
        },
        if (!is.null(self$`sequence`)) {
          sprintf(
          '"sequence":
            "%s"
                    ',
          self$`sequence`
          )
        },
        if (!is.null(self$`isApproximateLength`)) {
          sprintf(
          '"isApproximateLength":
            %s
                    ',
          tolower(self$`isApproximateLength`)
          )
        },
        if (!is.null(self$`isCircular`)) {
          sprintf(
          '"isCircular":
            %s
                    ',
          tolower(self$`isCircular`)
          )
        },
        if (!is.null(self$`polymerType`)) {
          sprintf(
          '"polymerType":
          %s
          ',
          jsonlite::toJSON(self$`polymerType`$toJSON(), auto_unbox = TRUE, digits = NA)
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
        if (!is.null(self$`fractionRepeats`)) {
          sprintf(
          '"fractionRepeats":
            %d
                    ',
          self$`fractionRepeats`
          )
        },
        if (!is.null(self$`sequenceDatabaseEntry`)) {
          sprintf(
          '"sequenceDatabaseEntry":
          %s
          ',
          jsonlite::toJSON(self$`sequenceDatabaseEntry`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`taxon`)) {
          sprintf(
          '"taxon":
          %s
          ',
          jsonlite::toJSON(self$`taxon`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`bioSequence2GeneProduct`)) {
          sprintf(
          '"bioSequence2GeneProduct":
          [%s]
',
          paste(sapply(self$`bioSequence2GeneProduct`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of BioSequence
    #'
    #' @description
    #' Deserialize JSON string into an instance of BioSequence
    #'
    #' @param input_json the JSON input
    #' @return the instance of BioSequence
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`length` <- this_object$`length`
      self$`sequence` <- this_object$`sequence`
      self$`isApproximateLength` <- this_object$`isApproximateLength`
      self$`isCircular` <- this_object$`isCircular`
      self$`polymerType` <- PolymerType$new()$fromJSON(jsonlite::toJSON(this_object$polymerType, auto_unbox = TRUE, digits = NA))
      self$`type` <- SequenceType$new()$fromJSON(jsonlite::toJSON(this_object$type, auto_unbox = TRUE, digits = NA))
      self$`fractionRepeats` <- this_object$`fractionRepeats`
      self$`sequenceDatabaseEntry` <- DatabaseEntry$new()$fromJSON(jsonlite::toJSON(this_object$sequenceDatabaseEntry, auto_unbox = TRUE, digits = NA))
      self$`taxon` <- Taxon$new()$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
      self$`bioSequence2GeneProduct` <- ApiClient$new()$deserializeObj(this_object$`bioSequence2GeneProduct`, "set[BioSequence2GeneProduct]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to BioSequence
    #'
    #' @description
    #' Validate JSON input with respect to BioSequence and throw an exception if invalid
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
    #' @return String representation of BioSequence
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
BioSequence$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
BioSequence$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
BioSequence$lock()

