#' Create a new ExpressionExperimentValueObject
#'
#' @description
#' ExpressionExperimentValueObject Class
#'
#' @docType class
#' @title ExpressionExperimentValueObject
#' @description ExpressionExperimentValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field lastUpdated  character [optional]
#' @field troubled  character [optional]
#' @field lastTroubledEvent  \link{AuditEventValueObject} [optional]
#' @field needsAttention  character [optional]
#' @field lastNeedsAttentionEvent  \link{AuditEventValueObject} [optional]
#' @field curationNote  character [optional]
#' @field lastNoteUpdateEvent  \link{AuditEventValueObject} [optional]
#' @field bioAssayCount  integer [optional]
#' @field description  character [optional]
#' @field name  character [optional]
#' @field accession  character [optional]
#' @field arrayDesignCount  integer [optional]
#' @field batchConfound  character [optional]
#' @field batchEffect  character [optional]
#' @field bioMaterialCount  integer [optional]
#' @field currentUserHasWritePermission  character [optional]
#' @field currentUserIsOwner  character [optional]
#' @field experimentalDesign  integer [optional]
#' @field externalDatabase  character [optional]
#' @field externalUri  character [optional]
#' @field geeq  \link{GeeqValueObject} [optional]
#' @field isPublic  character [optional]
#' @field isShared  character [optional]
#' @field metadata  character [optional]
#' @field processedExpressionVectorCount  integer [optional]
#' @field shortName  character [optional]
#' @field source  character [optional]
#' @field suitableForDEA  character [optional]
#' @field taxon  character [optional]
#' @field taxonId  integer [optional]
#' @field technologyType  character [optional]
#' @field userOwned  character [optional]
#' @field userCanWrite  character [optional]
#' @field troubleDetails  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ExpressionExperimentValueObject <- R6::R6Class(
  "ExpressionExperimentValueObject",
  public = list(
    `id` = NULL,
    `lastUpdated` = NULL,
    `troubled` = NULL,
    `lastTroubledEvent` = NULL,
    `needsAttention` = NULL,
    `lastNeedsAttentionEvent` = NULL,
    `curationNote` = NULL,
    `lastNoteUpdateEvent` = NULL,
    `bioAssayCount` = NULL,
    `description` = NULL,
    `name` = NULL,
    `accession` = NULL,
    `arrayDesignCount` = NULL,
    `batchConfound` = NULL,
    `batchEffect` = NULL,
    `bioMaterialCount` = NULL,
    `currentUserHasWritePermission` = NULL,
    `currentUserIsOwner` = NULL,
    `experimentalDesign` = NULL,
    `externalDatabase` = NULL,
    `externalUri` = NULL,
    `geeq` = NULL,
    `isPublic` = NULL,
    `isShared` = NULL,
    `metadata` = NULL,
    `processedExpressionVectorCount` = NULL,
    `shortName` = NULL,
    `source` = NULL,
    `suitableForDEA` = NULL,
    `taxon` = NULL,
    `taxonId` = NULL,
    `technologyType` = NULL,
    `userOwned` = NULL,
    `userCanWrite` = NULL,
    `troubleDetails` = NULL,
    #' Initialize a new ExpressionExperimentValueObject class.
    #'
    #' @description
    #' Initialize a new ExpressionExperimentValueObject class.
    #'
    #' @param id id
    #' @param lastUpdated lastUpdated
    #' @param troubled troubled
    #' @param lastTroubledEvent lastTroubledEvent
    #' @param needsAttention needsAttention
    #' @param lastNeedsAttentionEvent lastNeedsAttentionEvent
    #' @param curationNote curationNote
    #' @param lastNoteUpdateEvent lastNoteUpdateEvent
    #' @param bioAssayCount bioAssayCount
    #' @param description description
    #' @param name name
    #' @param accession accession
    #' @param arrayDesignCount arrayDesignCount
    #' @param batchConfound batchConfound
    #' @param batchEffect batchEffect
    #' @param bioMaterialCount bioMaterialCount
    #' @param currentUserHasWritePermission currentUserHasWritePermission
    #' @param currentUserIsOwner currentUserIsOwner
    #' @param experimentalDesign experimentalDesign
    #' @param externalDatabase externalDatabase
    #' @param externalUri externalUri
    #' @param geeq geeq
    #' @param isPublic isPublic
    #' @param isShared isShared
    #' @param metadata metadata
    #' @param processedExpressionVectorCount processedExpressionVectorCount
    #' @param shortName shortName
    #' @param source source
    #' @param suitableForDEA suitableForDEA
    #' @param taxon taxon
    #' @param taxonId taxonId
    #' @param technologyType technologyType
    #' @param userOwned userOwned
    #' @param userCanWrite userCanWrite
    #' @param troubleDetails troubleDetails
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `lastUpdated` = NULL, `troubled` = NULL, `lastTroubledEvent` = NULL, `needsAttention` = NULL, `lastNeedsAttentionEvent` = NULL, `curationNote` = NULL, `lastNoteUpdateEvent` = NULL, `bioAssayCount` = NULL, `description` = NULL, `name` = NULL, `accession` = NULL, `arrayDesignCount` = NULL, `batchConfound` = NULL, `batchEffect` = NULL, `bioMaterialCount` = NULL, `currentUserHasWritePermission` = NULL, `currentUserIsOwner` = NULL, `experimentalDesign` = NULL, `externalDatabase` = NULL, `externalUri` = NULL, `geeq` = NULL, `isPublic` = NULL, `isShared` = NULL, `metadata` = NULL, `processedExpressionVectorCount` = NULL, `shortName` = NULL, `source` = NULL, `suitableForDEA` = NULL, `taxon` = NULL, `taxonId` = NULL, `technologyType` = NULL, `userOwned` = NULL, `userCanWrite` = NULL, `troubleDetails` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`lastUpdated`)) {
        stopifnot(is.character(`lastUpdated`), length(`lastUpdated`) == 1)
        self$`lastUpdated` <- `lastUpdated`
      }
      if (!is.null(`troubled`)) {
        stopifnot(is.logical(`troubled`), length(`troubled`) == 1)
        self$`troubled` <- `troubled`
      }
      if (!is.null(`lastTroubledEvent`)) {
        stopifnot(R6::is.R6(`lastTroubledEvent`))
        self$`lastTroubledEvent` <- `lastTroubledEvent`
      }
      if (!is.null(`needsAttention`)) {
        stopifnot(is.logical(`needsAttention`), length(`needsAttention`) == 1)
        self$`needsAttention` <- `needsAttention`
      }
      if (!is.null(`lastNeedsAttentionEvent`)) {
        stopifnot(R6::is.R6(`lastNeedsAttentionEvent`))
        self$`lastNeedsAttentionEvent` <- `lastNeedsAttentionEvent`
      }
      if (!is.null(`curationNote`)) {
        stopifnot(is.character(`curationNote`), length(`curationNote`) == 1)
        self$`curationNote` <- `curationNote`
      }
      if (!is.null(`lastNoteUpdateEvent`)) {
        stopifnot(R6::is.R6(`lastNoteUpdateEvent`))
        self$`lastNoteUpdateEvent` <- `lastNoteUpdateEvent`
      }
      if (!is.null(`bioAssayCount`)) {
        stopifnot(is.numeric(`bioAssayCount`), length(`bioAssayCount`) == 1)
        self$`bioAssayCount` <- `bioAssayCount`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`accession`)) {
        stopifnot(is.character(`accession`), length(`accession`) == 1)
        self$`accession` <- `accession`
      }
      if (!is.null(`arrayDesignCount`)) {
        stopifnot(is.numeric(`arrayDesignCount`), length(`arrayDesignCount`) == 1)
        self$`arrayDesignCount` <- `arrayDesignCount`
      }
      if (!is.null(`batchConfound`)) {
        stopifnot(is.character(`batchConfound`), length(`batchConfound`) == 1)
        self$`batchConfound` <- `batchConfound`
      }
      if (!is.null(`batchEffect`)) {
        stopifnot(is.character(`batchEffect`), length(`batchEffect`) == 1)
        self$`batchEffect` <- `batchEffect`
      }
      if (!is.null(`bioMaterialCount`)) {
        stopifnot(is.numeric(`bioMaterialCount`), length(`bioMaterialCount`) == 1)
        self$`bioMaterialCount` <- `bioMaterialCount`
      }
      if (!is.null(`currentUserHasWritePermission`)) {
        stopifnot(is.logical(`currentUserHasWritePermission`), length(`currentUserHasWritePermission`) == 1)
        self$`currentUserHasWritePermission` <- `currentUserHasWritePermission`
      }
      if (!is.null(`currentUserIsOwner`)) {
        stopifnot(is.logical(`currentUserIsOwner`), length(`currentUserIsOwner`) == 1)
        self$`currentUserIsOwner` <- `currentUserIsOwner`
      }
      if (!is.null(`experimentalDesign`)) {
        stopifnot(is.numeric(`experimentalDesign`), length(`experimentalDesign`) == 1)
        self$`experimentalDesign` <- `experimentalDesign`
      }
      if (!is.null(`externalDatabase`)) {
        stopifnot(is.character(`externalDatabase`), length(`externalDatabase`) == 1)
        self$`externalDatabase` <- `externalDatabase`
      }
      if (!is.null(`externalUri`)) {
        stopifnot(is.character(`externalUri`), length(`externalUri`) == 1)
        self$`externalUri` <- `externalUri`
      }
      if (!is.null(`geeq`)) {
        stopifnot(R6::is.R6(`geeq`))
        self$`geeq` <- `geeq`
      }
      if (!is.null(`isPublic`)) {
        stopifnot(is.logical(`isPublic`), length(`isPublic`) == 1)
        self$`isPublic` <- `isPublic`
      }
      if (!is.null(`isShared`)) {
        stopifnot(is.logical(`isShared`), length(`isShared`) == 1)
        self$`isShared` <- `isShared`
      }
      if (!is.null(`metadata`)) {
        stopifnot(is.character(`metadata`), length(`metadata`) == 1)
        self$`metadata` <- `metadata`
      }
      if (!is.null(`processedExpressionVectorCount`)) {
        stopifnot(is.numeric(`processedExpressionVectorCount`), length(`processedExpressionVectorCount`) == 1)
        self$`processedExpressionVectorCount` <- `processedExpressionVectorCount`
      }
      if (!is.null(`shortName`)) {
        stopifnot(is.character(`shortName`), length(`shortName`) == 1)
        self$`shortName` <- `shortName`
      }
      if (!is.null(`source`)) {
        stopifnot(is.character(`source`), length(`source`) == 1)
        self$`source` <- `source`
      }
      if (!is.null(`suitableForDEA`)) {
        stopifnot(is.logical(`suitableForDEA`), length(`suitableForDEA`) == 1)
        self$`suitableForDEA` <- `suitableForDEA`
      }
      if (!is.null(`taxon`)) {
        stopifnot(is.character(`taxon`), length(`taxon`) == 1)
        self$`taxon` <- `taxon`
      }
      if (!is.null(`taxonId`)) {
        stopifnot(is.numeric(`taxonId`), length(`taxonId`) == 1)
        self$`taxonId` <- `taxonId`
      }
      if (!is.null(`technologyType`)) {
        stopifnot(is.character(`technologyType`), length(`technologyType`) == 1)
        self$`technologyType` <- `technologyType`
      }
      if (!is.null(`userOwned`)) {
        stopifnot(is.logical(`userOwned`), length(`userOwned`) == 1)
        self$`userOwned` <- `userOwned`
      }
      if (!is.null(`userCanWrite`)) {
        stopifnot(is.logical(`userCanWrite`), length(`userCanWrite`) == 1)
        self$`userCanWrite` <- `userCanWrite`
      }
      if (!is.null(`troubleDetails`)) {
        stopifnot(is.character(`troubleDetails`), length(`troubleDetails`) == 1)
        self$`troubleDetails` <- `troubleDetails`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExpressionExperimentValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ExpressionExperimentValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        ExpressionExperimentValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`lastUpdated`)) {
        ExpressionExperimentValueObjectObject[["lastUpdated"]] <-
          self$`lastUpdated`
      }
      if (!is.null(self$`troubled`)) {
        ExpressionExperimentValueObjectObject[["troubled"]] <-
          self$`troubled`
      }
      if (!is.null(self$`lastTroubledEvent`)) {
        ExpressionExperimentValueObjectObject[["lastTroubledEvent"]] <-
          self$`lastTroubledEvent`$toJSON()
      }
      if (!is.null(self$`needsAttention`)) {
        ExpressionExperimentValueObjectObject[["needsAttention"]] <-
          self$`needsAttention`
      }
      if (!is.null(self$`lastNeedsAttentionEvent`)) {
        ExpressionExperimentValueObjectObject[["lastNeedsAttentionEvent"]] <-
          self$`lastNeedsAttentionEvent`$toJSON()
      }
      if (!is.null(self$`curationNote`)) {
        ExpressionExperimentValueObjectObject[["curationNote"]] <-
          self$`curationNote`
      }
      if (!is.null(self$`lastNoteUpdateEvent`)) {
        ExpressionExperimentValueObjectObject[["lastNoteUpdateEvent"]] <-
          self$`lastNoteUpdateEvent`$toJSON()
      }
      if (!is.null(self$`bioAssayCount`)) {
        ExpressionExperimentValueObjectObject[["bioAssayCount"]] <-
          self$`bioAssayCount`
      }
      if (!is.null(self$`description`)) {
        ExpressionExperimentValueObjectObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`name`)) {
        ExpressionExperimentValueObjectObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`accession`)) {
        ExpressionExperimentValueObjectObject[["accession"]] <-
          self$`accession`
      }
      if (!is.null(self$`arrayDesignCount`)) {
        ExpressionExperimentValueObjectObject[["arrayDesignCount"]] <-
          self$`arrayDesignCount`
      }
      if (!is.null(self$`batchConfound`)) {
        ExpressionExperimentValueObjectObject[["batchConfound"]] <-
          self$`batchConfound`
      }
      if (!is.null(self$`batchEffect`)) {
        ExpressionExperimentValueObjectObject[["batchEffect"]] <-
          self$`batchEffect`
      }
      if (!is.null(self$`bioMaterialCount`)) {
        ExpressionExperimentValueObjectObject[["bioMaterialCount"]] <-
          self$`bioMaterialCount`
      }
      if (!is.null(self$`currentUserHasWritePermission`)) {
        ExpressionExperimentValueObjectObject[["currentUserHasWritePermission"]] <-
          self$`currentUserHasWritePermission`
      }
      if (!is.null(self$`currentUserIsOwner`)) {
        ExpressionExperimentValueObjectObject[["currentUserIsOwner"]] <-
          self$`currentUserIsOwner`
      }
      if (!is.null(self$`experimentalDesign`)) {
        ExpressionExperimentValueObjectObject[["experimentalDesign"]] <-
          self$`experimentalDesign`
      }
      if (!is.null(self$`externalDatabase`)) {
        ExpressionExperimentValueObjectObject[["externalDatabase"]] <-
          self$`externalDatabase`
      }
      if (!is.null(self$`externalUri`)) {
        ExpressionExperimentValueObjectObject[["externalUri"]] <-
          self$`externalUri`
      }
      if (!is.null(self$`geeq`)) {
        ExpressionExperimentValueObjectObject[["geeq"]] <-
          self$`geeq`$toJSON()
      }
      if (!is.null(self$`isPublic`)) {
        ExpressionExperimentValueObjectObject[["isPublic"]] <-
          self$`isPublic`
      }
      if (!is.null(self$`isShared`)) {
        ExpressionExperimentValueObjectObject[["isShared"]] <-
          self$`isShared`
      }
      if (!is.null(self$`metadata`)) {
        ExpressionExperimentValueObjectObject[["metadata"]] <-
          self$`metadata`
      }
      if (!is.null(self$`processedExpressionVectorCount`)) {
        ExpressionExperimentValueObjectObject[["processedExpressionVectorCount"]] <-
          self$`processedExpressionVectorCount`
      }
      if (!is.null(self$`shortName`)) {
        ExpressionExperimentValueObjectObject[["shortName"]] <-
          self$`shortName`
      }
      if (!is.null(self$`source`)) {
        ExpressionExperimentValueObjectObject[["source"]] <-
          self$`source`
      }
      if (!is.null(self$`suitableForDEA`)) {
        ExpressionExperimentValueObjectObject[["suitableForDEA"]] <-
          self$`suitableForDEA`
      }
      if (!is.null(self$`taxon`)) {
        ExpressionExperimentValueObjectObject[["taxon"]] <-
          self$`taxon`
      }
      if (!is.null(self$`taxonId`)) {
        ExpressionExperimentValueObjectObject[["taxonId"]] <-
          self$`taxonId`
      }
      if (!is.null(self$`technologyType`)) {
        ExpressionExperimentValueObjectObject[["technologyType"]] <-
          self$`technologyType`
      }
      if (!is.null(self$`userOwned`)) {
        ExpressionExperimentValueObjectObject[["userOwned"]] <-
          self$`userOwned`
      }
      if (!is.null(self$`userCanWrite`)) {
        ExpressionExperimentValueObjectObject[["userCanWrite"]] <-
          self$`userCanWrite`
      }
      if (!is.null(self$`troubleDetails`)) {
        ExpressionExperimentValueObjectObject[["troubleDetails"]] <-
          self$`troubleDetails`
      }

      ExpressionExperimentValueObjectObject
    },
    #' Deserialize JSON string into an instance of ExpressionExperimentValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExpressionExperimentValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExpressionExperimentValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`lastUpdated`)) {
        self$`lastUpdated` <- this_object$`lastUpdated`
      }
      if (!is.null(this_object$`troubled`)) {
        self$`troubled` <- this_object$`troubled`
      }
      if (!is.null(this_object$`lastTroubledEvent`)) {
        lasttroubledevent_object <- AuditEventValueObject$new()
        lasttroubledevent_object$fromJSON(jsonlite::toJSON(this_object$lastTroubledEvent, auto_unbox = TRUE, digits = NA))
        self$`lastTroubledEvent` <- lasttroubledevent_object
      }
      if (!is.null(this_object$`needsAttention`)) {
        self$`needsAttention` <- this_object$`needsAttention`
      }
      if (!is.null(this_object$`lastNeedsAttentionEvent`)) {
        lastneedsattentionevent_object <- AuditEventValueObject$new()
        lastneedsattentionevent_object$fromJSON(jsonlite::toJSON(this_object$lastNeedsAttentionEvent, auto_unbox = TRUE, digits = NA))
        self$`lastNeedsAttentionEvent` <- lastneedsattentionevent_object
      }
      if (!is.null(this_object$`curationNote`)) {
        self$`curationNote` <- this_object$`curationNote`
      }
      if (!is.null(this_object$`lastNoteUpdateEvent`)) {
        lastnoteupdateevent_object <- AuditEventValueObject$new()
        lastnoteupdateevent_object$fromJSON(jsonlite::toJSON(this_object$lastNoteUpdateEvent, auto_unbox = TRUE, digits = NA))
        self$`lastNoteUpdateEvent` <- lastnoteupdateevent_object
      }
      if (!is.null(this_object$`bioAssayCount`)) {
        self$`bioAssayCount` <- this_object$`bioAssayCount`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`accession`)) {
        self$`accession` <- this_object$`accession`
      }
      if (!is.null(this_object$`arrayDesignCount`)) {
        self$`arrayDesignCount` <- this_object$`arrayDesignCount`
      }
      if (!is.null(this_object$`batchConfound`)) {
        self$`batchConfound` <- this_object$`batchConfound`
      }
      if (!is.null(this_object$`batchEffect`)) {
        self$`batchEffect` <- this_object$`batchEffect`
      }
      if (!is.null(this_object$`bioMaterialCount`)) {
        self$`bioMaterialCount` <- this_object$`bioMaterialCount`
      }
      if (!is.null(this_object$`currentUserHasWritePermission`)) {
        self$`currentUserHasWritePermission` <- this_object$`currentUserHasWritePermission`
      }
      if (!is.null(this_object$`currentUserIsOwner`)) {
        self$`currentUserIsOwner` <- this_object$`currentUserIsOwner`
      }
      if (!is.null(this_object$`experimentalDesign`)) {
        self$`experimentalDesign` <- this_object$`experimentalDesign`
      }
      if (!is.null(this_object$`externalDatabase`)) {
        self$`externalDatabase` <- this_object$`externalDatabase`
      }
      if (!is.null(this_object$`externalUri`)) {
        self$`externalUri` <- this_object$`externalUri`
      }
      if (!is.null(this_object$`geeq`)) {
        geeq_object <- GeeqValueObject$new()
        geeq_object$fromJSON(jsonlite::toJSON(this_object$geeq, auto_unbox = TRUE, digits = NA))
        self$`geeq` <- geeq_object
      }
      if (!is.null(this_object$`isPublic`)) {
        self$`isPublic` <- this_object$`isPublic`
      }
      if (!is.null(this_object$`isShared`)) {
        self$`isShared` <- this_object$`isShared`
      }
      if (!is.null(this_object$`metadata`)) {
        self$`metadata` <- this_object$`metadata`
      }
      if (!is.null(this_object$`processedExpressionVectorCount`)) {
        self$`processedExpressionVectorCount` <- this_object$`processedExpressionVectorCount`
      }
      if (!is.null(this_object$`shortName`)) {
        self$`shortName` <- this_object$`shortName`
      }
      if (!is.null(this_object$`source`)) {
        self$`source` <- this_object$`source`
      }
      if (!is.null(this_object$`suitableForDEA`)) {
        self$`suitableForDEA` <- this_object$`suitableForDEA`
      }
      if (!is.null(this_object$`taxon`)) {
        self$`taxon` <- this_object$`taxon`
      }
      if (!is.null(this_object$`taxonId`)) {
        self$`taxonId` <- this_object$`taxonId`
      }
      if (!is.null(this_object$`technologyType`)) {
        self$`technologyType` <- this_object$`technologyType`
      }
      if (!is.null(this_object$`userOwned`)) {
        self$`userOwned` <- this_object$`userOwned`
      }
      if (!is.null(this_object$`userCanWrite`)) {
        self$`userCanWrite` <- this_object$`userCanWrite`
      }
      if (!is.null(this_object$`troubleDetails`)) {
        self$`troubleDetails` <- this_object$`troubleDetails`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return ExpressionExperimentValueObject in JSON format
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
        if (!is.null(self$`lastUpdated`)) {
          sprintf(
          '"lastUpdated":
            "%s"
                    ',
          self$`lastUpdated`
          )
        },
        if (!is.null(self$`troubled`)) {
          sprintf(
          '"troubled":
            %s
                    ',
          tolower(self$`troubled`)
          )
        },
        if (!is.null(self$`lastTroubledEvent`)) {
          sprintf(
          '"lastTroubledEvent":
          %s
          ',
          jsonlite::toJSON(self$`lastTroubledEvent`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`needsAttention`)) {
          sprintf(
          '"needsAttention":
            %s
                    ',
          tolower(self$`needsAttention`)
          )
        },
        if (!is.null(self$`lastNeedsAttentionEvent`)) {
          sprintf(
          '"lastNeedsAttentionEvent":
          %s
          ',
          jsonlite::toJSON(self$`lastNeedsAttentionEvent`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`curationNote`)) {
          sprintf(
          '"curationNote":
            "%s"
                    ',
          self$`curationNote`
          )
        },
        if (!is.null(self$`lastNoteUpdateEvent`)) {
          sprintf(
          '"lastNoteUpdateEvent":
          %s
          ',
          jsonlite::toJSON(self$`lastNoteUpdateEvent`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`bioAssayCount`)) {
          sprintf(
          '"bioAssayCount":
            %d
                    ',
          self$`bioAssayCount`
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
        if (!is.null(self$`name`)) {
          sprintf(
          '"name":
            "%s"
                    ',
          self$`name`
          )
        },
        if (!is.null(self$`accession`)) {
          sprintf(
          '"accession":
            "%s"
                    ',
          self$`accession`
          )
        },
        if (!is.null(self$`arrayDesignCount`)) {
          sprintf(
          '"arrayDesignCount":
            %d
                    ',
          self$`arrayDesignCount`
          )
        },
        if (!is.null(self$`batchConfound`)) {
          sprintf(
          '"batchConfound":
            "%s"
                    ',
          self$`batchConfound`
          )
        },
        if (!is.null(self$`batchEffect`)) {
          sprintf(
          '"batchEffect":
            "%s"
                    ',
          self$`batchEffect`
          )
        },
        if (!is.null(self$`bioMaterialCount`)) {
          sprintf(
          '"bioMaterialCount":
            %d
                    ',
          self$`bioMaterialCount`
          )
        },
        if (!is.null(self$`currentUserHasWritePermission`)) {
          sprintf(
          '"currentUserHasWritePermission":
            %s
                    ',
          tolower(self$`currentUserHasWritePermission`)
          )
        },
        if (!is.null(self$`currentUserIsOwner`)) {
          sprintf(
          '"currentUserIsOwner":
            %s
                    ',
          tolower(self$`currentUserIsOwner`)
          )
        },
        if (!is.null(self$`experimentalDesign`)) {
          sprintf(
          '"experimentalDesign":
            %d
                    ',
          self$`experimentalDesign`
          )
        },
        if (!is.null(self$`externalDatabase`)) {
          sprintf(
          '"externalDatabase":
            "%s"
                    ',
          self$`externalDatabase`
          )
        },
        if (!is.null(self$`externalUri`)) {
          sprintf(
          '"externalUri":
            "%s"
                    ',
          self$`externalUri`
          )
        },
        if (!is.null(self$`geeq`)) {
          sprintf(
          '"geeq":
          %s
          ',
          jsonlite::toJSON(self$`geeq`$toJSON(), auto_unbox = TRUE, digits = NA)
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
        if (!is.null(self$`metadata`)) {
          sprintf(
          '"metadata":
            "%s"
                    ',
          self$`metadata`
          )
        },
        if (!is.null(self$`processedExpressionVectorCount`)) {
          sprintf(
          '"processedExpressionVectorCount":
            %d
                    ',
          self$`processedExpressionVectorCount`
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
        if (!is.null(self$`source`)) {
          sprintf(
          '"source":
            "%s"
                    ',
          self$`source`
          )
        },
        if (!is.null(self$`suitableForDEA`)) {
          sprintf(
          '"suitableForDEA":
            %s
                    ',
          tolower(self$`suitableForDEA`)
          )
        },
        if (!is.null(self$`taxon`)) {
          sprintf(
          '"taxon":
            "%s"
                    ',
          self$`taxon`
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
        if (!is.null(self$`technologyType`)) {
          sprintf(
          '"technologyType":
            "%s"
                    ',
          self$`technologyType`
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
        },
        if (!is.null(self$`troubleDetails`)) {
          sprintf(
          '"troubleDetails":
            "%s"
                    ',
          self$`troubleDetails`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of ExpressionExperimentValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ExpressionExperimentValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ExpressionExperimentValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`lastUpdated` <- this_object$`lastUpdated`
      self$`troubled` <- this_object$`troubled`
      self$`lastTroubledEvent` <- AuditEventValueObject$new()$fromJSON(jsonlite::toJSON(this_object$lastTroubledEvent, auto_unbox = TRUE, digits = NA))
      self$`needsAttention` <- this_object$`needsAttention`
      self$`lastNeedsAttentionEvent` <- AuditEventValueObject$new()$fromJSON(jsonlite::toJSON(this_object$lastNeedsAttentionEvent, auto_unbox = TRUE, digits = NA))
      self$`curationNote` <- this_object$`curationNote`
      self$`lastNoteUpdateEvent` <- AuditEventValueObject$new()$fromJSON(jsonlite::toJSON(this_object$lastNoteUpdateEvent, auto_unbox = TRUE, digits = NA))
      self$`bioAssayCount` <- this_object$`bioAssayCount`
      self$`description` <- this_object$`description`
      self$`name` <- this_object$`name`
      self$`accession` <- this_object$`accession`
      self$`arrayDesignCount` <- this_object$`arrayDesignCount`
      self$`batchConfound` <- this_object$`batchConfound`
      self$`batchEffect` <- this_object$`batchEffect`
      self$`bioMaterialCount` <- this_object$`bioMaterialCount`
      self$`currentUserHasWritePermission` <- this_object$`currentUserHasWritePermission`
      self$`currentUserIsOwner` <- this_object$`currentUserIsOwner`
      self$`experimentalDesign` <- this_object$`experimentalDesign`
      self$`externalDatabase` <- this_object$`externalDatabase`
      self$`externalUri` <- this_object$`externalUri`
      self$`geeq` <- GeeqValueObject$new()$fromJSON(jsonlite::toJSON(this_object$geeq, auto_unbox = TRUE, digits = NA))
      self$`isPublic` <- this_object$`isPublic`
      self$`isShared` <- this_object$`isShared`
      self$`metadata` <- this_object$`metadata`
      self$`processedExpressionVectorCount` <- this_object$`processedExpressionVectorCount`
      self$`shortName` <- this_object$`shortName`
      self$`source` <- this_object$`source`
      self$`suitableForDEA` <- this_object$`suitableForDEA`
      self$`taxon` <- this_object$`taxon`
      self$`taxonId` <- this_object$`taxonId`
      self$`technologyType` <- this_object$`technologyType`
      self$`userOwned` <- this_object$`userOwned`
      self$`userCanWrite` <- this_object$`userCanWrite`
      self$`troubleDetails` <- this_object$`troubleDetails`
      self
    },
    #' Validate JSON input with respect to ExpressionExperimentValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ExpressionExperimentValueObject and throw an exception if invalid
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
    #' @return String representation of ExpressionExperimentValueObject
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
ExpressionExperimentValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ExpressionExperimentValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ExpressionExperimentValueObject$lock()

