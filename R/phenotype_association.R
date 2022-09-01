#' Create a new PhenotypeAssociation
#'
#' @description
#' PhenotypeAssociation Class
#'
#' @docType class
#' @title PhenotypeAssociation
#' @description PhenotypeAssociation Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field description  character [optional]
#' @field id  integer [optional]
#' @field auditTrail  \link{AuditTrail} [optional]
#' @field evidenceCode  \link{GOEvidenceCode} [optional]
#' @field isNegativeEvidence  character [optional]
#' @field score  character [optional]
#' @field strength  numeric [optional]
#' @field gene  \link{Gene} [optional]
#' @field phenotypes  list(\link{Characteristic}) [optional]
#' @field associationType  \link{Characteristic} [optional]
#' @field evidenceSource  \link{DatabaseEntry} [optional]
#' @field scoreType  \link{QuantitationType} [optional]
#' @field phenotypeAssociationPublications  list(\link{PhenotypeAssociationPublication}) [optional]
#' @field mappingType  \link{PhenotypeMappingType} [optional]
#' @field originalPhenotype  character [optional]
#' @field relationship  character [optional]
#' @field lastUpdated  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
PhenotypeAssociation <- R6::R6Class(
  "PhenotypeAssociation",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `auditTrail` = NULL,
    `evidenceCode` = NULL,
    `isNegativeEvidence` = NULL,
    `score` = NULL,
    `strength` = NULL,
    `gene` = NULL,
    `phenotypes` = NULL,
    `associationType` = NULL,
    `evidenceSource` = NULL,
    `scoreType` = NULL,
    `phenotypeAssociationPublications` = NULL,
    `mappingType` = NULL,
    `originalPhenotype` = NULL,
    `relationship` = NULL,
    `lastUpdated` = NULL,
    #' Initialize a new PhenotypeAssociation class.
    #'
    #' @description
    #' Initialize a new PhenotypeAssociation class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param auditTrail auditTrail
    #' @param evidenceCode evidenceCode
    #' @param isNegativeEvidence isNegativeEvidence
    #' @param score score
    #' @param strength strength
    #' @param gene gene
    #' @param phenotypes phenotypes
    #' @param associationType associationType
    #' @param evidenceSource evidenceSource
    #' @param scoreType scoreType
    #' @param phenotypeAssociationPublications phenotypeAssociationPublications
    #' @param mappingType mappingType
    #' @param originalPhenotype originalPhenotype
    #' @param relationship relationship
    #' @param lastUpdated lastUpdated
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `auditTrail` = NULL, `evidenceCode` = NULL, `isNegativeEvidence` = NULL, `score` = NULL, `strength` = NULL, `gene` = NULL, `phenotypes` = NULL, `associationType` = NULL, `evidenceSource` = NULL, `scoreType` = NULL, `phenotypeAssociationPublications` = NULL, `mappingType` = NULL, `originalPhenotype` = NULL, `relationship` = NULL, `lastUpdated` = NULL, ...
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
      if (!is.null(`evidenceCode`)) {
        stopifnot(R6::is.R6(`evidenceCode`))
        self$`evidenceCode` <- `evidenceCode`
      }
      if (!is.null(`isNegativeEvidence`)) {
        stopifnot(is.logical(`isNegativeEvidence`), length(`isNegativeEvidence`) == 1)
        self$`isNegativeEvidence` <- `isNegativeEvidence`
      }
      if (!is.null(`score`)) {
        stopifnot(is.character(`score`), length(`score`) == 1)
        self$`score` <- `score`
      }
      if (!is.null(`strength`)) {
        stopifnot(is.numeric(`strength`), length(`strength`) == 1)
        self$`strength` <- `strength`
      }
      if (!is.null(`gene`)) {
        stopifnot(R6::is.R6(`gene`))
        self$`gene` <- `gene`
      }
      if (!is.null(`phenotypes`)) {
        stopifnot(is.vector(`phenotypes`), length(`phenotypes`) != 0)
        sapply(`phenotypes`, function(x) stopifnot(R6::is.R6(x)))
        self$`phenotypes` <- `phenotypes`
      }
      if (!is.null(`associationType`)) {
        stopifnot(R6::is.R6(`associationType`))
        self$`associationType` <- `associationType`
      }
      if (!is.null(`evidenceSource`)) {
        stopifnot(R6::is.R6(`evidenceSource`))
        self$`evidenceSource` <- `evidenceSource`
      }
      if (!is.null(`scoreType`)) {
        stopifnot(R6::is.R6(`scoreType`))
        self$`scoreType` <- `scoreType`
      }
      if (!is.null(`phenotypeAssociationPublications`)) {
        stopifnot(is.vector(`phenotypeAssociationPublications`), length(`phenotypeAssociationPublications`) != 0)
        sapply(`phenotypeAssociationPublications`, function(x) stopifnot(R6::is.R6(x)))
        self$`phenotypeAssociationPublications` <- `phenotypeAssociationPublications`
      }
      if (!is.null(`mappingType`)) {
        stopifnot(R6::is.R6(`mappingType`))
        self$`mappingType` <- `mappingType`
      }
      if (!is.null(`originalPhenotype`)) {
        stopifnot(is.character(`originalPhenotype`), length(`originalPhenotype`) == 1)
        self$`originalPhenotype` <- `originalPhenotype`
      }
      if (!is.null(`relationship`)) {
        stopifnot(is.character(`relationship`), length(`relationship`) == 1)
        self$`relationship` <- `relationship`
      }
      if (!is.null(`lastUpdated`)) {
        stopifnot(is.character(`lastUpdated`), length(`lastUpdated`) == 1)
        self$`lastUpdated` <- `lastUpdated`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PhenotypeAssociation in JSON format
    #' @keywords internal
    toJSON = function() {
      PhenotypeAssociationObject <- list()
      if (!is.null(self$`name`)) {
        PhenotypeAssociationObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        PhenotypeAssociationObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        PhenotypeAssociationObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`auditTrail`)) {
        PhenotypeAssociationObject[["auditTrail"]] <-
          self$`auditTrail`$toJSON()
      }
      if (!is.null(self$`evidenceCode`)) {
        PhenotypeAssociationObject[["evidenceCode"]] <-
          self$`evidenceCode`$toJSON()
      }
      if (!is.null(self$`isNegativeEvidence`)) {
        PhenotypeAssociationObject[["isNegativeEvidence"]] <-
          self$`isNegativeEvidence`
      }
      if (!is.null(self$`score`)) {
        PhenotypeAssociationObject[["score"]] <-
          self$`score`
      }
      if (!is.null(self$`strength`)) {
        PhenotypeAssociationObject[["strength"]] <-
          self$`strength`
      }
      if (!is.null(self$`gene`)) {
        PhenotypeAssociationObject[["gene"]] <-
          self$`gene`$toJSON()
      }
      if (!is.null(self$`phenotypes`)) {
        PhenotypeAssociationObject[["phenotypes"]] <-
          lapply(self$`phenotypes`, function(x) x$toJSON())
      }
      if (!is.null(self$`associationType`)) {
        PhenotypeAssociationObject[["associationType"]] <-
          self$`associationType`$toJSON()
      }
      if (!is.null(self$`evidenceSource`)) {
        PhenotypeAssociationObject[["evidenceSource"]] <-
          self$`evidenceSource`$toJSON()
      }
      if (!is.null(self$`scoreType`)) {
        PhenotypeAssociationObject[["scoreType"]] <-
          self$`scoreType`$toJSON()
      }
      if (!is.null(self$`phenotypeAssociationPublications`)) {
        PhenotypeAssociationObject[["phenotypeAssociationPublications"]] <-
          lapply(self$`phenotypeAssociationPublications`, function(x) x$toJSON())
      }
      if (!is.null(self$`mappingType`)) {
        PhenotypeAssociationObject[["mappingType"]] <-
          self$`mappingType`$toJSON()
      }
      if (!is.null(self$`originalPhenotype`)) {
        PhenotypeAssociationObject[["originalPhenotype"]] <-
          self$`originalPhenotype`
      }
      if (!is.null(self$`relationship`)) {
        PhenotypeAssociationObject[["relationship"]] <-
          self$`relationship`
      }
      if (!is.null(self$`lastUpdated`)) {
        PhenotypeAssociationObject[["lastUpdated"]] <-
          self$`lastUpdated`
      }

      PhenotypeAssociationObject
    },
    #' Deserialize JSON string into an instance of PhenotypeAssociation
    #'
    #' @description
    #' Deserialize JSON string into an instance of PhenotypeAssociation
    #'
    #' @param input_json the JSON input
    #' @return the instance of PhenotypeAssociation
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
      if (!is.null(this_object$`evidenceCode`)) {
        evidencecode_object <- GOEvidenceCode$new()
        evidencecode_object$fromJSON(jsonlite::toJSON(this_object$evidenceCode, auto_unbox = TRUE, digits = NA))
        self$`evidenceCode` <- evidencecode_object
      }
      if (!is.null(this_object$`isNegativeEvidence`)) {
        self$`isNegativeEvidence` <- this_object$`isNegativeEvidence`
      }
      if (!is.null(this_object$`score`)) {
        self$`score` <- this_object$`score`
      }
      if (!is.null(this_object$`strength`)) {
        self$`strength` <- this_object$`strength`
      }
      if (!is.null(this_object$`gene`)) {
        gene_object <- Gene$new()
        gene_object$fromJSON(jsonlite::toJSON(this_object$gene, auto_unbox = TRUE, digits = NA))
        self$`gene` <- gene_object
      }
      if (!is.null(this_object$`phenotypes`)) {
        self$`phenotypes` <- ApiClient$new()$deserializeObj(this_object$`phenotypes`, "set[Characteristic]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`associationType`)) {
        associationtype_object <- Characteristic$new()
        associationtype_object$fromJSON(jsonlite::toJSON(this_object$associationType, auto_unbox = TRUE, digits = NA))
        self$`associationType` <- associationtype_object
      }
      if (!is.null(this_object$`evidenceSource`)) {
        evidencesource_object <- DatabaseEntry$new()
        evidencesource_object$fromJSON(jsonlite::toJSON(this_object$evidenceSource, auto_unbox = TRUE, digits = NA))
        self$`evidenceSource` <- evidencesource_object
      }
      if (!is.null(this_object$`scoreType`)) {
        scoretype_object <- QuantitationType$new()
        scoretype_object$fromJSON(jsonlite::toJSON(this_object$scoreType, auto_unbox = TRUE, digits = NA))
        self$`scoreType` <- scoretype_object
      }
      if (!is.null(this_object$`phenotypeAssociationPublications`)) {
        self$`phenotypeAssociationPublications` <- ApiClient$new()$deserializeObj(this_object$`phenotypeAssociationPublications`, "set[PhenotypeAssociationPublication]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`mappingType`)) {
        mappingtype_object <- PhenotypeMappingType$new()
        mappingtype_object$fromJSON(jsonlite::toJSON(this_object$mappingType, auto_unbox = TRUE, digits = NA))
        self$`mappingType` <- mappingtype_object
      }
      if (!is.null(this_object$`originalPhenotype`)) {
        self$`originalPhenotype` <- this_object$`originalPhenotype`
      }
      if (!is.null(this_object$`relationship`)) {
        self$`relationship` <- this_object$`relationship`
      }
      if (!is.null(this_object$`lastUpdated`)) {
        self$`lastUpdated` <- this_object$`lastUpdated`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PhenotypeAssociation in JSON format
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
        if (!is.null(self$`evidenceCode`)) {
          sprintf(
          '"evidenceCode":
          %s
          ',
          jsonlite::toJSON(self$`evidenceCode`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`isNegativeEvidence`)) {
          sprintf(
          '"isNegativeEvidence":
            %s
                    ',
          tolower(self$`isNegativeEvidence`)
          )
        },
        if (!is.null(self$`score`)) {
          sprintf(
          '"score":
            "%s"
                    ',
          self$`score`
          )
        },
        if (!is.null(self$`strength`)) {
          sprintf(
          '"strength":
            %d
                    ',
          self$`strength`
          )
        },
        if (!is.null(self$`gene`)) {
          sprintf(
          '"gene":
          %s
          ',
          jsonlite::toJSON(self$`gene`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`phenotypes`)) {
          sprintf(
          '"phenotypes":
          [%s]
',
          paste(sapply(self$`phenotypes`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`associationType`)) {
          sprintf(
          '"associationType":
          %s
          ',
          jsonlite::toJSON(self$`associationType`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`evidenceSource`)) {
          sprintf(
          '"evidenceSource":
          %s
          ',
          jsonlite::toJSON(self$`evidenceSource`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`scoreType`)) {
          sprintf(
          '"scoreType":
          %s
          ',
          jsonlite::toJSON(self$`scoreType`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`phenotypeAssociationPublications`)) {
          sprintf(
          '"phenotypeAssociationPublications":
          [%s]
',
          paste(sapply(self$`phenotypeAssociationPublications`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`mappingType`)) {
          sprintf(
          '"mappingType":
          %s
          ',
          jsonlite::toJSON(self$`mappingType`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`originalPhenotype`)) {
          sprintf(
          '"originalPhenotype":
            "%s"
                    ',
          self$`originalPhenotype`
          )
        },
        if (!is.null(self$`relationship`)) {
          sprintf(
          '"relationship":
            "%s"
                    ',
          self$`relationship`
          )
        },
        if (!is.null(self$`lastUpdated`)) {
          sprintf(
          '"lastUpdated":
            "%s"
                    ',
          self$`lastUpdated`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of PhenotypeAssociation
    #'
    #' @description
    #' Deserialize JSON string into an instance of PhenotypeAssociation
    #'
    #' @param input_json the JSON input
    #' @return the instance of PhenotypeAssociation
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`auditTrail` <- AuditTrail$new()$fromJSON(jsonlite::toJSON(this_object$auditTrail, auto_unbox = TRUE, digits = NA))
      self$`evidenceCode` <- GOEvidenceCode$new()$fromJSON(jsonlite::toJSON(this_object$evidenceCode, auto_unbox = TRUE, digits = NA))
      self$`isNegativeEvidence` <- this_object$`isNegativeEvidence`
      self$`score` <- this_object$`score`
      self$`strength` <- this_object$`strength`
      self$`gene` <- Gene$new()$fromJSON(jsonlite::toJSON(this_object$gene, auto_unbox = TRUE, digits = NA))
      self$`phenotypes` <- ApiClient$new()$deserializeObj(this_object$`phenotypes`, "set[Characteristic]", loadNamespace("gemma.R"))
      self$`associationType` <- Characteristic$new()$fromJSON(jsonlite::toJSON(this_object$associationType, auto_unbox = TRUE, digits = NA))
      self$`evidenceSource` <- DatabaseEntry$new()$fromJSON(jsonlite::toJSON(this_object$evidenceSource, auto_unbox = TRUE, digits = NA))
      self$`scoreType` <- QuantitationType$new()$fromJSON(jsonlite::toJSON(this_object$scoreType, auto_unbox = TRUE, digits = NA))
      self$`phenotypeAssociationPublications` <- ApiClient$new()$deserializeObj(this_object$`phenotypeAssociationPublications`, "set[PhenotypeAssociationPublication]", loadNamespace("gemma.R"))
      self$`mappingType` <- PhenotypeMappingType$new()$fromJSON(jsonlite::toJSON(this_object$mappingType, auto_unbox = TRUE, digits = NA))
      self$`originalPhenotype` <- this_object$`originalPhenotype`
      self$`relationship` <- this_object$`relationship`
      self$`lastUpdated` <- this_object$`lastUpdated`
      self
    },
    #' Validate JSON input with respect to PhenotypeAssociation
    #'
    #' @description
    #' Validate JSON input with respect to PhenotypeAssociation and throw an exception if invalid
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
    #' @return String representation of PhenotypeAssociation
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
PhenotypeAssociation$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
PhenotypeAssociation$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
PhenotypeAssociation$lock()

