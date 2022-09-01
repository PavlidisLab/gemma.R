#' Create a new ArrayDesign
#'
#' @description
#' ArrayDesign Class
#'
#' @docType class
#' @title ArrayDesign
#' @description ArrayDesign Class
#' @format An \code{R6Class} generator object
#' @field name  character optional
#' @field description  character optional
#' @field id  integer optional
#' @field auditTrail  \link{AuditTrail} optional
#' @field advertisedNumberOfDesignElements  integer optional
#' @field alternateNames  list(\link{AlternateName}) optional
#' @field alternativeTo  \link{ArrayDesign} optional
#' @field compositeSequences  list(\link{CompositeSequence}) optional
#' @field curationDetails  \link{CurationDetails} optional
#' @field designProvider  \link{Contact} optional
#' @field externalReferences  list(\link{DatabaseEntry}) optional
#' @field mergedInto  \link{ArrayDesign} optional
#' @field mergees  list(\link{ArrayDesign}) optional
#' @field primaryTaxon  \link{Taxon} optional
#' @field shortName  character optional
#' @field subsumedArrayDesigns  list(\link{ArrayDesign}) optional
#' @field subsumingArrayDesign  \link{ArrayDesign} optional
#' @field technologyType  \link{TechnologyType} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ArrayDesign <- R6::R6Class(
  "ArrayDesign",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `auditTrail` = NULL,
    `advertisedNumberOfDesignElements` = NULL,
    `alternateNames` = NULL,
    `alternativeTo` = NULL,
    `compositeSequences` = NULL,
    `curationDetails` = NULL,
    `designProvider` = NULL,
    `externalReferences` = NULL,
    `mergedInto` = NULL,
    `mergees` = NULL,
    `primaryTaxon` = NULL,
    `shortName` = NULL,
    `subsumedArrayDesigns` = NULL,
    `subsumingArrayDesign` = NULL,
    `technologyType` = NULL,
    #' Initialize a new ArrayDesign class.
    #'
    #' @description
    #' Initialize a new ArrayDesign class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param auditTrail auditTrail
    #' @param advertisedNumberOfDesignElements advertisedNumberOfDesignElements
    #' @param alternateNames alternateNames
    #' @param alternativeTo alternativeTo
    #' @param compositeSequences compositeSequences
    #' @param curationDetails curationDetails
    #' @param designProvider designProvider
    #' @param externalReferences externalReferences
    #' @param mergedInto mergedInto
    #' @param mergees mergees
    #' @param primaryTaxon primaryTaxon
    #' @param shortName shortName
    #' @param subsumedArrayDesigns subsumedArrayDesigns
    #' @param subsumingArrayDesign subsumingArrayDesign
    #' @param technologyType technologyType
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `auditTrail` = NULL, `advertisedNumberOfDesignElements` = NULL, `alternateNames` = NULL, `alternativeTo` = NULL, `compositeSequences` = NULL, `curationDetails` = NULL, `designProvider` = NULL, `externalReferences` = NULL, `mergedInto` = NULL, `mergees` = NULL, `primaryTaxon` = NULL, `shortName` = NULL, `subsumedArrayDesigns` = NULL, `subsumingArrayDesign` = NULL, `technologyType` = NULL, ...
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
      if (!is.null(`auditTrail`)) {
        stopifnot(R6::is.R6(`auditTrail`))
        self$`auditTrail` <- `auditTrail`
      }
      if (!is.null(`advertisedNumberOfDesignElements`)) {
        stopifnot(is.numeric(`advertisedNumberOfDesignElements`), length(`advertisedNumberOfDesignElements`) == 1)
        self$`advertisedNumberOfDesignElements` <- `advertisedNumberOfDesignElements`
      }
      if (!is.null(`alternateNames`)) {
        stopifnot(is.vector(`alternateNames`), length(`alternateNames`) != 0)
        sapply(`alternateNames`, function(x) stopifnot(R6::is.R6(x)))
        self$`alternateNames` <- `alternateNames`
      }
      if (!is.null(`alternativeTo`)) {
        stopifnot(R6::is.R6(`alternativeTo`))
        self$`alternativeTo` <- `alternativeTo`
      }
      if (!is.null(`compositeSequences`)) {
        stopifnot(is.vector(`compositeSequences`), length(`compositeSequences`) != 0)
        sapply(`compositeSequences`, function(x) stopifnot(R6::is.R6(x)))
        self$`compositeSequences` <- `compositeSequences`
      }
      if (!is.null(`curationDetails`)) {
        stopifnot(R6::is.R6(`curationDetails`))
        self$`curationDetails` <- `curationDetails`
      }
      if (!is.null(`designProvider`)) {
        stopifnot(R6::is.R6(`designProvider`))
        self$`designProvider` <- `designProvider`
      }
      if (!is.null(`externalReferences`)) {
        stopifnot(is.vector(`externalReferences`), length(`externalReferences`) != 0)
        sapply(`externalReferences`, function(x) stopifnot(R6::is.R6(x)))
        self$`externalReferences` <- `externalReferences`
      }
      if (!is.null(`mergedInto`)) {
        stopifnot(R6::is.R6(`mergedInto`))
        self$`mergedInto` <- `mergedInto`
      }
      if (!is.null(`mergees`)) {
        stopifnot(is.vector(`mergees`), length(`mergees`) != 0)
        sapply(`mergees`, function(x) stopifnot(R6::is.R6(x)))
        self$`mergees` <- `mergees`
      }
      if (!is.null(`primaryTaxon`)) {
        stopifnot(R6::is.R6(`primaryTaxon`))
        self$`primaryTaxon` <- `primaryTaxon`
      }
      if (!is.null(`shortName`)) {
        stopifnot(is.character(`shortName`), length(`shortName`) == 1)
        self$`shortName` <- `shortName`
      }
      if (!is.null(`subsumedArrayDesigns`)) {
        stopifnot(is.vector(`subsumedArrayDesigns`), length(`subsumedArrayDesigns`) != 0)
        sapply(`subsumedArrayDesigns`, function(x) stopifnot(R6::is.R6(x)))
        self$`subsumedArrayDesigns` <- `subsumedArrayDesigns`
      }
      if (!is.null(`subsumingArrayDesign`)) {
        stopifnot(R6::is.R6(`subsumingArrayDesign`))
        self$`subsumingArrayDesign` <- `subsumingArrayDesign`
      }
      if (!is.null(`technologyType`)) {
        stopifnot(R6::is.R6(`technologyType`))
        self$`technologyType` <- `technologyType`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ArrayDesign in JSON format
    #' @keywords internal
    toJSON = function() {
      ArrayDesignObject <- list()
      if (!is.null(self$`name`)) {
        ArrayDesignObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        ArrayDesignObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        ArrayDesignObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`auditTrail`)) {
        ArrayDesignObject[["auditTrail"]] <-
          self$`auditTrail`$toJSON()
      }
      if (!is.null(self$`advertisedNumberOfDesignElements`)) {
        ArrayDesignObject[["advertisedNumberOfDesignElements"]] <-
          self$`advertisedNumberOfDesignElements`
      }
      if (!is.null(self$`alternateNames`)) {
        ArrayDesignObject[["alternateNames"]] <-
          lapply(self$`alternateNames`, function(x) x$toJSON())
      }
      if (!is.null(self$`alternativeTo`)) {
        ArrayDesignObject[["alternativeTo"]] <-
          self$`alternativeTo`$toJSON()
      }
      if (!is.null(self$`compositeSequences`)) {
        ArrayDesignObject[["compositeSequences"]] <-
          lapply(self$`compositeSequences`, function(x) x$toJSON())
      }
      if (!is.null(self$`curationDetails`)) {
        ArrayDesignObject[["curationDetails"]] <-
          self$`curationDetails`$toJSON()
      }
      if (!is.null(self$`designProvider`)) {
        ArrayDesignObject[["designProvider"]] <-
          self$`designProvider`$toJSON()
      }
      if (!is.null(self$`externalReferences`)) {
        ArrayDesignObject[["externalReferences"]] <-
          lapply(self$`externalReferences`, function(x) x$toJSON())
      }
      if (!is.null(self$`mergedInto`)) {
        ArrayDesignObject[["mergedInto"]] <-
          self$`mergedInto`$toJSON()
      }
      if (!is.null(self$`mergees`)) {
        ArrayDesignObject[["mergees"]] <-
          lapply(self$`mergees`, function(x) x$toJSON())
      }
      if (!is.null(self$`primaryTaxon`)) {
        ArrayDesignObject[["primaryTaxon"]] <-
          self$`primaryTaxon`$toJSON()
      }
      if (!is.null(self$`shortName`)) {
        ArrayDesignObject[["shortName"]] <-
          self$`shortName`
      }
      if (!is.null(self$`subsumedArrayDesigns`)) {
        ArrayDesignObject[["subsumedArrayDesigns"]] <-
          lapply(self$`subsumedArrayDesigns`, function(x) x$toJSON())
      }
      if (!is.null(self$`subsumingArrayDesign`)) {
        ArrayDesignObject[["subsumingArrayDesign"]] <-
          self$`subsumingArrayDesign`$toJSON()
      }
      if (!is.null(self$`technologyType`)) {
        ArrayDesignObject[["technologyType"]] <-
          self$`technologyType`$toJSON()
      }

      ArrayDesignObject
    },
    #' Deserialize JSON string into an instance of ArrayDesign
    #'
    #' @description
    #' Deserialize JSON string into an instance of ArrayDesign
    #'
    #' @param input_json the JSON input
    #' @return the instance of ArrayDesign
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
      if (!is.null(this_object$`auditTrail`)) {
        audittrail_object <- AuditTrail$new()
        audittrail_object$fromJSON(jsonlite::toJSON(this_object$auditTrail, auto_unbox = TRUE, digits = NA))
        self$`auditTrail` <- audittrail_object
      }
      if (!is.null(this_object$`advertisedNumberOfDesignElements`)) {
        self$`advertisedNumberOfDesignElements` <- this_object$`advertisedNumberOfDesignElements`
      }
      if (!is.null(this_object$`alternateNames`)) {
        self$`alternateNames` <- ApiClient$new()$deserializeObj(this_object$`alternateNames`, "set[AlternateName]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`alternativeTo`)) {
        alternativeto_object <- ArrayDesign$new()
        alternativeto_object$fromJSON(jsonlite::toJSON(this_object$alternativeTo, auto_unbox = TRUE, digits = NA))
        self$`alternativeTo` <- alternativeto_object
      }
      if (!is.null(this_object$`compositeSequences`)) {
        self$`compositeSequences` <- ApiClient$new()$deserializeObj(this_object$`compositeSequences`, "set[CompositeSequence]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`curationDetails`)) {
        curationdetails_object <- CurationDetails$new()
        curationdetails_object$fromJSON(jsonlite::toJSON(this_object$curationDetails, auto_unbox = TRUE, digits = NA))
        self$`curationDetails` <- curationdetails_object
      }
      if (!is.null(this_object$`designProvider`)) {
        designprovider_object <- Contact$new()
        designprovider_object$fromJSON(jsonlite::toJSON(this_object$designProvider, auto_unbox = TRUE, digits = NA))
        self$`designProvider` <- designprovider_object
      }
      if (!is.null(this_object$`externalReferences`)) {
        self$`externalReferences` <- ApiClient$new()$deserializeObj(this_object$`externalReferences`, "set[DatabaseEntry]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`mergedInto`)) {
        mergedinto_object <- ArrayDesign$new()
        mergedinto_object$fromJSON(jsonlite::toJSON(this_object$mergedInto, auto_unbox = TRUE, digits = NA))
        self$`mergedInto` <- mergedinto_object
      }
      if (!is.null(this_object$`mergees`)) {
        self$`mergees` <- ApiClient$new()$deserializeObj(this_object$`mergees`, "set[ArrayDesign]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`primaryTaxon`)) {
        primarytaxon_object <- Taxon$new()
        primarytaxon_object$fromJSON(jsonlite::toJSON(this_object$primaryTaxon, auto_unbox = TRUE, digits = NA))
        self$`primaryTaxon` <- primarytaxon_object
      }
      if (!is.null(this_object$`shortName`)) {
        self$`shortName` <- this_object$`shortName`
      }
      if (!is.null(this_object$`subsumedArrayDesigns`)) {
        self$`subsumedArrayDesigns` <- ApiClient$new()$deserializeObj(this_object$`subsumedArrayDesigns`, "set[ArrayDesign]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`subsumingArrayDesign`)) {
        subsumingarraydesign_object <- ArrayDesign$new()
        subsumingarraydesign_object$fromJSON(jsonlite::toJSON(this_object$subsumingArrayDesign, auto_unbox = TRUE, digits = NA))
        self$`subsumingArrayDesign` <- subsumingarraydesign_object
      }
      if (!is.null(this_object$`technologyType`)) {
        technologytype_object <- TechnologyType$new()
        technologytype_object$fromJSON(jsonlite::toJSON(this_object$technologyType, auto_unbox = TRUE, digits = NA))
        self$`technologyType` <- technologytype_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ArrayDesign in JSON format
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
        if (!is.null(self$`auditTrail`)) {
          sprintf(
          '"auditTrail":
          %s
          ',
          jsonlite::toJSON(self$`auditTrail`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`advertisedNumberOfDesignElements`)) {
          sprintf(
          '"advertisedNumberOfDesignElements":
            %d
                    ',
          self$`advertisedNumberOfDesignElements`
          )
        },
        if (!is.null(self$`alternateNames`)) {
          sprintf(
          '"alternateNames":
          [%s]
',
          paste(sapply(self$`alternateNames`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`alternativeTo`)) {
          sprintf(
          '"alternativeTo":
          %s
          ',
          jsonlite::toJSON(self$`alternativeTo`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`compositeSequences`)) {
          sprintf(
          '"compositeSequences":
          [%s]
',
          paste(sapply(self$`compositeSequences`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`curationDetails`)) {
          sprintf(
          '"curationDetails":
          %s
          ',
          jsonlite::toJSON(self$`curationDetails`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`designProvider`)) {
          sprintf(
          '"designProvider":
          %s
          ',
          jsonlite::toJSON(self$`designProvider`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`externalReferences`)) {
          sprintf(
          '"externalReferences":
          [%s]
',
          paste(sapply(self$`externalReferences`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`mergedInto`)) {
          sprintf(
          '"mergedInto":
          %s
          ',
          jsonlite::toJSON(self$`mergedInto`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`mergees`)) {
          sprintf(
          '"mergees":
          [%s]
',
          paste(sapply(self$`mergees`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`primaryTaxon`)) {
          sprintf(
          '"primaryTaxon":
          %s
          ',
          jsonlite::toJSON(self$`primaryTaxon`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`shortName`)) {
          sprintf(
          '"shortName":
            "%s"
                    ',
          self$`shortName`
          )
        },
        if (!is.null(self$`subsumedArrayDesigns`)) {
          sprintf(
          '"subsumedArrayDesigns":
          [%s]
',
          paste(sapply(self$`subsumedArrayDesigns`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`subsumingArrayDesign`)) {
          sprintf(
          '"subsumingArrayDesign":
          %s
          ',
          jsonlite::toJSON(self$`subsumingArrayDesign`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`technologyType`)) {
          sprintf(
          '"technologyType":
          %s
          ',
          jsonlite::toJSON(self$`technologyType`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ArrayDesign
    #'
    #' @description
    #' Deserialize JSON string into an instance of ArrayDesign
    #'
    #' @param input_json the JSON input
    #' @return the instance of ArrayDesign
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`auditTrail` <- AuditTrail$new()$fromJSON(jsonlite::toJSON(this_object$auditTrail, auto_unbox = TRUE, digits = NA))
      self$`advertisedNumberOfDesignElements` <- this_object$`advertisedNumberOfDesignElements`
      self$`alternateNames` <- ApiClient$new()$deserializeObj(this_object$`alternateNames`, "set[AlternateName]", loadNamespace("gemma.R"))
      self$`alternativeTo` <- ArrayDesign$new()$fromJSON(jsonlite::toJSON(this_object$alternativeTo, auto_unbox = TRUE, digits = NA))
      self$`compositeSequences` <- ApiClient$new()$deserializeObj(this_object$`compositeSequences`, "set[CompositeSequence]", loadNamespace("gemma.R"))
      self$`curationDetails` <- CurationDetails$new()$fromJSON(jsonlite::toJSON(this_object$curationDetails, auto_unbox = TRUE, digits = NA))
      self$`designProvider` <- Contact$new()$fromJSON(jsonlite::toJSON(this_object$designProvider, auto_unbox = TRUE, digits = NA))
      self$`externalReferences` <- ApiClient$new()$deserializeObj(this_object$`externalReferences`, "set[DatabaseEntry]", loadNamespace("gemma.R"))
      self$`mergedInto` <- ArrayDesign$new()$fromJSON(jsonlite::toJSON(this_object$mergedInto, auto_unbox = TRUE, digits = NA))
      self$`mergees` <- ApiClient$new()$deserializeObj(this_object$`mergees`, "set[ArrayDesign]", loadNamespace("gemma.R"))
      self$`primaryTaxon` <- Taxon$new()$fromJSON(jsonlite::toJSON(this_object$primaryTaxon, auto_unbox = TRUE, digits = NA))
      self$`shortName` <- this_object$`shortName`
      self$`subsumedArrayDesigns` <- ApiClient$new()$deserializeObj(this_object$`subsumedArrayDesigns`, "set[ArrayDesign]", loadNamespace("gemma.R"))
      self$`subsumingArrayDesign` <- ArrayDesign$new()$fromJSON(jsonlite::toJSON(this_object$subsumingArrayDesign, auto_unbox = TRUE, digits = NA))
      self$`technologyType` <- TechnologyType$new()$fromJSON(jsonlite::toJSON(this_object$technologyType, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to ArrayDesign
    #'
    #' @description
    #' Validate JSON input with respect to ArrayDesign and throw an exception if invalid
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
    #' @return String representation of ArrayDesign
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
ArrayDesign$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ArrayDesign$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ArrayDesign$lock()

