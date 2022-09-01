#' Create a new ArrayDesignValueObject
#'
#' @description
#' ArrayDesignValueObject Class
#'
#' @docType class
#' @title ArrayDesignValueObject
#' @description ArrayDesignValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field lastUpdated  character optional
#' @field troubled  character optional
#' @field lastTroubledEvent  \link{AuditEventValueObject} optional
#' @field needsAttention  character optional
#' @field lastNeedsAttentionEvent  \link{AuditEventValueObject} optional
#' @field curationNote  character optional
#' @field lastNoteUpdateEvent  \link{AuditEventValueObject} optional
#' @field blackListed  character optional
#' @field color  character optional
#' @field dateCached  character optional
#' @field description  character optional
#' @field designElementCount  integer optional
#' @field expressionExperimentCount  integer optional
#' @field hasBlatAssociations  character optional
#' @field hasGeneAssociations  character optional
#' @field hasSequenceAssociations  character optional
#' @field isAffymetrixAltCdf  character optional
#' @field isMerged  character optional
#' @field isMergee  character optional
#' @field isSubsumed  character optional
#' @field isSubsumer  character optional
#' @field lastGeneMapping  character optional
#' @field lastRepeatMask  character optional
#' @field lastSequenceAnalysis  character optional
#' @field lastSequenceUpdate  character optional
#' @field name  character optional
#' @field numGenes  character optional
#' @field numProbeAlignments  character optional
#' @field numProbeSequences  character optional
#' @field numProbesToGenes  character optional
#' @field shortName  character optional
#' @field switchedExpressionExperimentCount  integer optional
#' @field taxon  character optional
#' @field taxonID  integer optional
#' @field technologyType  character optional
#' @field troubleDetails  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ArrayDesignValueObject <- R6::R6Class(
  "ArrayDesignValueObject",
  public = list(
    `id` = NULL,
    `lastUpdated` = NULL,
    `troubled` = NULL,
    `lastTroubledEvent` = NULL,
    `needsAttention` = NULL,
    `lastNeedsAttentionEvent` = NULL,
    `curationNote` = NULL,
    `lastNoteUpdateEvent` = NULL,
    `blackListed` = NULL,
    `color` = NULL,
    `dateCached` = NULL,
    `description` = NULL,
    `designElementCount` = NULL,
    `expressionExperimentCount` = NULL,
    `hasBlatAssociations` = NULL,
    `hasGeneAssociations` = NULL,
    `hasSequenceAssociations` = NULL,
    `isAffymetrixAltCdf` = NULL,
    `isMerged` = NULL,
    `isMergee` = NULL,
    `isSubsumed` = NULL,
    `isSubsumer` = NULL,
    `lastGeneMapping` = NULL,
    `lastRepeatMask` = NULL,
    `lastSequenceAnalysis` = NULL,
    `lastSequenceUpdate` = NULL,
    `name` = NULL,
    `numGenes` = NULL,
    `numProbeAlignments` = NULL,
    `numProbeSequences` = NULL,
    `numProbesToGenes` = NULL,
    `shortName` = NULL,
    `switchedExpressionExperimentCount` = NULL,
    `taxon` = NULL,
    `taxonID` = NULL,
    `technologyType` = NULL,
    `troubleDetails` = NULL,
    #' Initialize a new ArrayDesignValueObject class.
    #'
    #' @description
    #' Initialize a new ArrayDesignValueObject class.
    #'
    #' @param id id
    #' @param lastUpdated lastUpdated
    #' @param troubled troubled
    #' @param lastTroubledEvent lastTroubledEvent
    #' @param needsAttention needsAttention
    #' @param lastNeedsAttentionEvent lastNeedsAttentionEvent
    #' @param curationNote curationNote
    #' @param lastNoteUpdateEvent lastNoteUpdateEvent
    #' @param blackListed blackListed
    #' @param color color
    #' @param dateCached dateCached
    #' @param description description
    #' @param designElementCount designElementCount
    #' @param expressionExperimentCount expressionExperimentCount
    #' @param hasBlatAssociations hasBlatAssociations
    #' @param hasGeneAssociations hasGeneAssociations
    #' @param hasSequenceAssociations hasSequenceAssociations
    #' @param isAffymetrixAltCdf isAffymetrixAltCdf
    #' @param isMerged isMerged
    #' @param isMergee isMergee
    #' @param isSubsumed isSubsumed
    #' @param isSubsumer isSubsumer
    #' @param lastGeneMapping lastGeneMapping
    #' @param lastRepeatMask lastRepeatMask
    #' @param lastSequenceAnalysis lastSequenceAnalysis
    #' @param lastSequenceUpdate lastSequenceUpdate
    #' @param name name
    #' @param numGenes numGenes
    #' @param numProbeAlignments numProbeAlignments
    #' @param numProbeSequences numProbeSequences
    #' @param numProbesToGenes numProbesToGenes
    #' @param shortName shortName
    #' @param switchedExpressionExperimentCount switchedExpressionExperimentCount
    #' @param taxon taxon
    #' @param taxonID taxonID
    #' @param technologyType technologyType
    #' @param troubleDetails troubleDetails
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `lastUpdated` = NULL, `troubled` = NULL, `lastTroubledEvent` = NULL, `needsAttention` = NULL, `lastNeedsAttentionEvent` = NULL, `curationNote` = NULL, `lastNoteUpdateEvent` = NULL, `blackListed` = NULL, `color` = NULL, `dateCached` = NULL, `description` = NULL, `designElementCount` = NULL, `expressionExperimentCount` = NULL, `hasBlatAssociations` = NULL, `hasGeneAssociations` = NULL, `hasSequenceAssociations` = NULL, `isAffymetrixAltCdf` = NULL, `isMerged` = NULL, `isMergee` = NULL, `isSubsumed` = NULL, `isSubsumer` = NULL, `lastGeneMapping` = NULL, `lastRepeatMask` = NULL, `lastSequenceAnalysis` = NULL, `lastSequenceUpdate` = NULL, `name` = NULL, `numGenes` = NULL, `numProbeAlignments` = NULL, `numProbeSequences` = NULL, `numProbesToGenes` = NULL, `shortName` = NULL, `switchedExpressionExperimentCount` = NULL, `taxon` = NULL, `taxonID` = NULL, `technologyType` = NULL, `troubleDetails` = NULL, ...
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
      if (!is.null(`blackListed`)) {
        stopifnot(is.logical(`blackListed`), length(`blackListed`) == 1)
        self$`blackListed` <- `blackListed`
      }
      if (!is.null(`color`)) {
        stopifnot(is.character(`color`), length(`color`) == 1)
        self$`color` <- `color`
      }
      if (!is.null(`dateCached`)) {
        stopifnot(is.character(`dateCached`), length(`dateCached`) == 1)
        self$`dateCached` <- `dateCached`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`designElementCount`)) {
        stopifnot(is.numeric(`designElementCount`), length(`designElementCount`) == 1)
        self$`designElementCount` <- `designElementCount`
      }
      if (!is.null(`expressionExperimentCount`)) {
        stopifnot(is.numeric(`expressionExperimentCount`), length(`expressionExperimentCount`) == 1)
        self$`expressionExperimentCount` <- `expressionExperimentCount`
      }
      if (!is.null(`hasBlatAssociations`)) {
        stopifnot(is.logical(`hasBlatAssociations`), length(`hasBlatAssociations`) == 1)
        self$`hasBlatAssociations` <- `hasBlatAssociations`
      }
      if (!is.null(`hasGeneAssociations`)) {
        stopifnot(is.logical(`hasGeneAssociations`), length(`hasGeneAssociations`) == 1)
        self$`hasGeneAssociations` <- `hasGeneAssociations`
      }
      if (!is.null(`hasSequenceAssociations`)) {
        stopifnot(is.logical(`hasSequenceAssociations`), length(`hasSequenceAssociations`) == 1)
        self$`hasSequenceAssociations` <- `hasSequenceAssociations`
      }
      if (!is.null(`isAffymetrixAltCdf`)) {
        stopifnot(is.logical(`isAffymetrixAltCdf`), length(`isAffymetrixAltCdf`) == 1)
        self$`isAffymetrixAltCdf` <- `isAffymetrixAltCdf`
      }
      if (!is.null(`isMerged`)) {
        stopifnot(is.logical(`isMerged`), length(`isMerged`) == 1)
        self$`isMerged` <- `isMerged`
      }
      if (!is.null(`isMergee`)) {
        stopifnot(is.logical(`isMergee`), length(`isMergee`) == 1)
        self$`isMergee` <- `isMergee`
      }
      if (!is.null(`isSubsumed`)) {
        stopifnot(is.logical(`isSubsumed`), length(`isSubsumed`) == 1)
        self$`isSubsumed` <- `isSubsumed`
      }
      if (!is.null(`isSubsumer`)) {
        stopifnot(is.logical(`isSubsumer`), length(`isSubsumer`) == 1)
        self$`isSubsumer` <- `isSubsumer`
      }
      if (!is.null(`lastGeneMapping`)) {
        stopifnot(is.character(`lastGeneMapping`), length(`lastGeneMapping`) == 1)
        self$`lastGeneMapping` <- `lastGeneMapping`
      }
      if (!is.null(`lastRepeatMask`)) {
        stopifnot(is.character(`lastRepeatMask`), length(`lastRepeatMask`) == 1)
        self$`lastRepeatMask` <- `lastRepeatMask`
      }
      if (!is.null(`lastSequenceAnalysis`)) {
        stopifnot(is.character(`lastSequenceAnalysis`), length(`lastSequenceAnalysis`) == 1)
        self$`lastSequenceAnalysis` <- `lastSequenceAnalysis`
      }
      if (!is.null(`lastSequenceUpdate`)) {
        stopifnot(is.character(`lastSequenceUpdate`), length(`lastSequenceUpdate`) == 1)
        self$`lastSequenceUpdate` <- `lastSequenceUpdate`
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`numGenes`)) {
        stopifnot(is.character(`numGenes`), length(`numGenes`) == 1)
        self$`numGenes` <- `numGenes`
      }
      if (!is.null(`numProbeAlignments`)) {
        stopifnot(is.character(`numProbeAlignments`), length(`numProbeAlignments`) == 1)
        self$`numProbeAlignments` <- `numProbeAlignments`
      }
      if (!is.null(`numProbeSequences`)) {
        stopifnot(is.character(`numProbeSequences`), length(`numProbeSequences`) == 1)
        self$`numProbeSequences` <- `numProbeSequences`
      }
      if (!is.null(`numProbesToGenes`)) {
        stopifnot(is.character(`numProbesToGenes`), length(`numProbesToGenes`) == 1)
        self$`numProbesToGenes` <- `numProbesToGenes`
      }
      if (!is.null(`shortName`)) {
        stopifnot(is.character(`shortName`), length(`shortName`) == 1)
        self$`shortName` <- `shortName`
      }
      if (!is.null(`switchedExpressionExperimentCount`)) {
        stopifnot(is.numeric(`switchedExpressionExperimentCount`), length(`switchedExpressionExperimentCount`) == 1)
        self$`switchedExpressionExperimentCount` <- `switchedExpressionExperimentCount`
      }
      if (!is.null(`taxon`)) {
        stopifnot(is.character(`taxon`), length(`taxon`) == 1)
        self$`taxon` <- `taxon`
      }
      if (!is.null(`taxonID`)) {
        stopifnot(is.numeric(`taxonID`), length(`taxonID`) == 1)
        self$`taxonID` <- `taxonID`
      }
      if (!is.null(`technologyType`)) {
        stopifnot(is.character(`technologyType`), length(`technologyType`) == 1)
        self$`technologyType` <- `technologyType`
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
    #' @return ArrayDesignValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      ArrayDesignValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        ArrayDesignValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`lastUpdated`)) {
        ArrayDesignValueObjectObject[["lastUpdated"]] <-
          self$`lastUpdated`
      }
      if (!is.null(self$`troubled`)) {
        ArrayDesignValueObjectObject[["troubled"]] <-
          self$`troubled`
      }
      if (!is.null(self$`lastTroubledEvent`)) {
        ArrayDesignValueObjectObject[["lastTroubledEvent"]] <-
          self$`lastTroubledEvent`$toJSON()
      }
      if (!is.null(self$`needsAttention`)) {
        ArrayDesignValueObjectObject[["needsAttention"]] <-
          self$`needsAttention`
      }
      if (!is.null(self$`lastNeedsAttentionEvent`)) {
        ArrayDesignValueObjectObject[["lastNeedsAttentionEvent"]] <-
          self$`lastNeedsAttentionEvent`$toJSON()
      }
      if (!is.null(self$`curationNote`)) {
        ArrayDesignValueObjectObject[["curationNote"]] <-
          self$`curationNote`
      }
      if (!is.null(self$`lastNoteUpdateEvent`)) {
        ArrayDesignValueObjectObject[["lastNoteUpdateEvent"]] <-
          self$`lastNoteUpdateEvent`$toJSON()
      }
      if (!is.null(self$`blackListed`)) {
        ArrayDesignValueObjectObject[["blackListed"]] <-
          self$`blackListed`
      }
      if (!is.null(self$`color`)) {
        ArrayDesignValueObjectObject[["color"]] <-
          self$`color`
      }
      if (!is.null(self$`dateCached`)) {
        ArrayDesignValueObjectObject[["dateCached"]] <-
          self$`dateCached`
      }
      if (!is.null(self$`description`)) {
        ArrayDesignValueObjectObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`designElementCount`)) {
        ArrayDesignValueObjectObject[["designElementCount"]] <-
          self$`designElementCount`
      }
      if (!is.null(self$`expressionExperimentCount`)) {
        ArrayDesignValueObjectObject[["expressionExperimentCount"]] <-
          self$`expressionExperimentCount`
      }
      if (!is.null(self$`hasBlatAssociations`)) {
        ArrayDesignValueObjectObject[["hasBlatAssociations"]] <-
          self$`hasBlatAssociations`
      }
      if (!is.null(self$`hasGeneAssociations`)) {
        ArrayDesignValueObjectObject[["hasGeneAssociations"]] <-
          self$`hasGeneAssociations`
      }
      if (!is.null(self$`hasSequenceAssociations`)) {
        ArrayDesignValueObjectObject[["hasSequenceAssociations"]] <-
          self$`hasSequenceAssociations`
      }
      if (!is.null(self$`isAffymetrixAltCdf`)) {
        ArrayDesignValueObjectObject[["isAffymetrixAltCdf"]] <-
          self$`isAffymetrixAltCdf`
      }
      if (!is.null(self$`isMerged`)) {
        ArrayDesignValueObjectObject[["isMerged"]] <-
          self$`isMerged`
      }
      if (!is.null(self$`isMergee`)) {
        ArrayDesignValueObjectObject[["isMergee"]] <-
          self$`isMergee`
      }
      if (!is.null(self$`isSubsumed`)) {
        ArrayDesignValueObjectObject[["isSubsumed"]] <-
          self$`isSubsumed`
      }
      if (!is.null(self$`isSubsumer`)) {
        ArrayDesignValueObjectObject[["isSubsumer"]] <-
          self$`isSubsumer`
      }
      if (!is.null(self$`lastGeneMapping`)) {
        ArrayDesignValueObjectObject[["lastGeneMapping"]] <-
          self$`lastGeneMapping`
      }
      if (!is.null(self$`lastRepeatMask`)) {
        ArrayDesignValueObjectObject[["lastRepeatMask"]] <-
          self$`lastRepeatMask`
      }
      if (!is.null(self$`lastSequenceAnalysis`)) {
        ArrayDesignValueObjectObject[["lastSequenceAnalysis"]] <-
          self$`lastSequenceAnalysis`
      }
      if (!is.null(self$`lastSequenceUpdate`)) {
        ArrayDesignValueObjectObject[["lastSequenceUpdate"]] <-
          self$`lastSequenceUpdate`
      }
      if (!is.null(self$`name`)) {
        ArrayDesignValueObjectObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`numGenes`)) {
        ArrayDesignValueObjectObject[["numGenes"]] <-
          self$`numGenes`
      }
      if (!is.null(self$`numProbeAlignments`)) {
        ArrayDesignValueObjectObject[["numProbeAlignments"]] <-
          self$`numProbeAlignments`
      }
      if (!is.null(self$`numProbeSequences`)) {
        ArrayDesignValueObjectObject[["numProbeSequences"]] <-
          self$`numProbeSequences`
      }
      if (!is.null(self$`numProbesToGenes`)) {
        ArrayDesignValueObjectObject[["numProbesToGenes"]] <-
          self$`numProbesToGenes`
      }
      if (!is.null(self$`shortName`)) {
        ArrayDesignValueObjectObject[["shortName"]] <-
          self$`shortName`
      }
      if (!is.null(self$`switchedExpressionExperimentCount`)) {
        ArrayDesignValueObjectObject[["switchedExpressionExperimentCount"]] <-
          self$`switchedExpressionExperimentCount`
      }
      if (!is.null(self$`taxon`)) {
        ArrayDesignValueObjectObject[["taxon"]] <-
          self$`taxon`
      }
      if (!is.null(self$`taxonID`)) {
        ArrayDesignValueObjectObject[["taxonID"]] <-
          self$`taxonID`
      }
      if (!is.null(self$`technologyType`)) {
        ArrayDesignValueObjectObject[["technologyType"]] <-
          self$`technologyType`
      }
      if (!is.null(self$`troubleDetails`)) {
        ArrayDesignValueObjectObject[["troubleDetails"]] <-
          self$`troubleDetails`
      }

      ArrayDesignValueObjectObject
    },
    #' Deserialize JSON string into an instance of ArrayDesignValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ArrayDesignValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ArrayDesignValueObject
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
      if (!is.null(this_object$`blackListed`)) {
        self$`blackListed` <- this_object$`blackListed`
      }
      if (!is.null(this_object$`color`)) {
        self$`color` <- this_object$`color`
      }
      if (!is.null(this_object$`dateCached`)) {
        self$`dateCached` <- this_object$`dateCached`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`designElementCount`)) {
        self$`designElementCount` <- this_object$`designElementCount`
      }
      if (!is.null(this_object$`expressionExperimentCount`)) {
        self$`expressionExperimentCount` <- this_object$`expressionExperimentCount`
      }
      if (!is.null(this_object$`hasBlatAssociations`)) {
        self$`hasBlatAssociations` <- this_object$`hasBlatAssociations`
      }
      if (!is.null(this_object$`hasGeneAssociations`)) {
        self$`hasGeneAssociations` <- this_object$`hasGeneAssociations`
      }
      if (!is.null(this_object$`hasSequenceAssociations`)) {
        self$`hasSequenceAssociations` <- this_object$`hasSequenceAssociations`
      }
      if (!is.null(this_object$`isAffymetrixAltCdf`)) {
        self$`isAffymetrixAltCdf` <- this_object$`isAffymetrixAltCdf`
      }
      if (!is.null(this_object$`isMerged`)) {
        self$`isMerged` <- this_object$`isMerged`
      }
      if (!is.null(this_object$`isMergee`)) {
        self$`isMergee` <- this_object$`isMergee`
      }
      if (!is.null(this_object$`isSubsumed`)) {
        self$`isSubsumed` <- this_object$`isSubsumed`
      }
      if (!is.null(this_object$`isSubsumer`)) {
        self$`isSubsumer` <- this_object$`isSubsumer`
      }
      if (!is.null(this_object$`lastGeneMapping`)) {
        self$`lastGeneMapping` <- this_object$`lastGeneMapping`
      }
      if (!is.null(this_object$`lastRepeatMask`)) {
        self$`lastRepeatMask` <- this_object$`lastRepeatMask`
      }
      if (!is.null(this_object$`lastSequenceAnalysis`)) {
        self$`lastSequenceAnalysis` <- this_object$`lastSequenceAnalysis`
      }
      if (!is.null(this_object$`lastSequenceUpdate`)) {
        self$`lastSequenceUpdate` <- this_object$`lastSequenceUpdate`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`numGenes`)) {
        self$`numGenes` <- this_object$`numGenes`
      }
      if (!is.null(this_object$`numProbeAlignments`)) {
        self$`numProbeAlignments` <- this_object$`numProbeAlignments`
      }
      if (!is.null(this_object$`numProbeSequences`)) {
        self$`numProbeSequences` <- this_object$`numProbeSequences`
      }
      if (!is.null(this_object$`numProbesToGenes`)) {
        self$`numProbesToGenes` <- this_object$`numProbesToGenes`
      }
      if (!is.null(this_object$`shortName`)) {
        self$`shortName` <- this_object$`shortName`
      }
      if (!is.null(this_object$`switchedExpressionExperimentCount`)) {
        self$`switchedExpressionExperimentCount` <- this_object$`switchedExpressionExperimentCount`
      }
      if (!is.null(this_object$`taxon`)) {
        self$`taxon` <- this_object$`taxon`
      }
      if (!is.null(this_object$`taxonID`)) {
        self$`taxonID` <- this_object$`taxonID`
      }
      if (!is.null(this_object$`technologyType`)) {
        self$`technologyType` <- this_object$`technologyType`
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
    #' @return ArrayDesignValueObject in JSON format
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
        if (!is.null(self$`blackListed`)) {
          sprintf(
          '"blackListed":
            %s
                    ',
          tolower(self$`blackListed`)
          )
        },
        if (!is.null(self$`color`)) {
          sprintf(
          '"color":
            "%s"
                    ',
          self$`color`
          )
        },
        if (!is.null(self$`dateCached`)) {
          sprintf(
          '"dateCached":
            "%s"
                    ',
          self$`dateCached`
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
        if (!is.null(self$`designElementCount`)) {
          sprintf(
          '"designElementCount":
            %d
                    ',
          self$`designElementCount`
          )
        },
        if (!is.null(self$`expressionExperimentCount`)) {
          sprintf(
          '"expressionExperimentCount":
            %d
                    ',
          self$`expressionExperimentCount`
          )
        },
        if (!is.null(self$`hasBlatAssociations`)) {
          sprintf(
          '"hasBlatAssociations":
            %s
                    ',
          tolower(self$`hasBlatAssociations`)
          )
        },
        if (!is.null(self$`hasGeneAssociations`)) {
          sprintf(
          '"hasGeneAssociations":
            %s
                    ',
          tolower(self$`hasGeneAssociations`)
          )
        },
        if (!is.null(self$`hasSequenceAssociations`)) {
          sprintf(
          '"hasSequenceAssociations":
            %s
                    ',
          tolower(self$`hasSequenceAssociations`)
          )
        },
        if (!is.null(self$`isAffymetrixAltCdf`)) {
          sprintf(
          '"isAffymetrixAltCdf":
            %s
                    ',
          tolower(self$`isAffymetrixAltCdf`)
          )
        },
        if (!is.null(self$`isMerged`)) {
          sprintf(
          '"isMerged":
            %s
                    ',
          tolower(self$`isMerged`)
          )
        },
        if (!is.null(self$`isMergee`)) {
          sprintf(
          '"isMergee":
            %s
                    ',
          tolower(self$`isMergee`)
          )
        },
        if (!is.null(self$`isSubsumed`)) {
          sprintf(
          '"isSubsumed":
            %s
                    ',
          tolower(self$`isSubsumed`)
          )
        },
        if (!is.null(self$`isSubsumer`)) {
          sprintf(
          '"isSubsumer":
            %s
                    ',
          tolower(self$`isSubsumer`)
          )
        },
        if (!is.null(self$`lastGeneMapping`)) {
          sprintf(
          '"lastGeneMapping":
            "%s"
                    ',
          self$`lastGeneMapping`
          )
        },
        if (!is.null(self$`lastRepeatMask`)) {
          sprintf(
          '"lastRepeatMask":
            "%s"
                    ',
          self$`lastRepeatMask`
          )
        },
        if (!is.null(self$`lastSequenceAnalysis`)) {
          sprintf(
          '"lastSequenceAnalysis":
            "%s"
                    ',
          self$`lastSequenceAnalysis`
          )
        },
        if (!is.null(self$`lastSequenceUpdate`)) {
          sprintf(
          '"lastSequenceUpdate":
            "%s"
                    ',
          self$`lastSequenceUpdate`
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
        if (!is.null(self$`numGenes`)) {
          sprintf(
          '"numGenes":
            "%s"
                    ',
          self$`numGenes`
          )
        },
        if (!is.null(self$`numProbeAlignments`)) {
          sprintf(
          '"numProbeAlignments":
            "%s"
                    ',
          self$`numProbeAlignments`
          )
        },
        if (!is.null(self$`numProbeSequences`)) {
          sprintf(
          '"numProbeSequences":
            "%s"
                    ',
          self$`numProbeSequences`
          )
        },
        if (!is.null(self$`numProbesToGenes`)) {
          sprintf(
          '"numProbesToGenes":
            "%s"
                    ',
          self$`numProbesToGenes`
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
        if (!is.null(self$`switchedExpressionExperimentCount`)) {
          sprintf(
          '"switchedExpressionExperimentCount":
            %d
                    ',
          self$`switchedExpressionExperimentCount`
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
        if (!is.null(self$`taxonID`)) {
          sprintf(
          '"taxonID":
            %d
                    ',
          self$`taxonID`
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
    #' Deserialize JSON string into an instance of ArrayDesignValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of ArrayDesignValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of ArrayDesignValueObject
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
      self$`blackListed` <- this_object$`blackListed`
      self$`color` <- this_object$`color`
      self$`dateCached` <- this_object$`dateCached`
      self$`description` <- this_object$`description`
      self$`designElementCount` <- this_object$`designElementCount`
      self$`expressionExperimentCount` <- this_object$`expressionExperimentCount`
      self$`hasBlatAssociations` <- this_object$`hasBlatAssociations`
      self$`hasGeneAssociations` <- this_object$`hasGeneAssociations`
      self$`hasSequenceAssociations` <- this_object$`hasSequenceAssociations`
      self$`isAffymetrixAltCdf` <- this_object$`isAffymetrixAltCdf`
      self$`isMerged` <- this_object$`isMerged`
      self$`isMergee` <- this_object$`isMergee`
      self$`isSubsumed` <- this_object$`isSubsumed`
      self$`isSubsumer` <- this_object$`isSubsumer`
      self$`lastGeneMapping` <- this_object$`lastGeneMapping`
      self$`lastRepeatMask` <- this_object$`lastRepeatMask`
      self$`lastSequenceAnalysis` <- this_object$`lastSequenceAnalysis`
      self$`lastSequenceUpdate` <- this_object$`lastSequenceUpdate`
      self$`name` <- this_object$`name`
      self$`numGenes` <- this_object$`numGenes`
      self$`numProbeAlignments` <- this_object$`numProbeAlignments`
      self$`numProbeSequences` <- this_object$`numProbeSequences`
      self$`numProbesToGenes` <- this_object$`numProbesToGenes`
      self$`shortName` <- this_object$`shortName`
      self$`switchedExpressionExperimentCount` <- this_object$`switchedExpressionExperimentCount`
      self$`taxon` <- this_object$`taxon`
      self$`taxonID` <- this_object$`taxonID`
      self$`technologyType` <- this_object$`technologyType`
      self$`troubleDetails` <- this_object$`troubleDetails`
      self
    },
    #' Validate JSON input with respect to ArrayDesignValueObject
    #'
    #' @description
    #' Validate JSON input with respect to ArrayDesignValueObject and throw an exception if invalid
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
    #' @return String representation of ArrayDesignValueObject
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
ArrayDesignValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
ArrayDesignValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
ArrayDesignValueObject$lock()

