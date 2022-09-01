#' Create a new GeeqValueObject
#'
#' @description
#' GeeqValueObject Class
#'
#' @docType class
#' @title GeeqValueObject
#' @description GeeqValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field publicQualityScore  numeric optional
#' @field publicSuitabilityScore  numeric optional
#' @field getsScorePublication  numeric optional
#' @field getsScorePlatformAmount  numeric optional
#' @field getsScorePlatformsTechMulti  numeric optional
#' @field getsScoreAvgPlatformPopularity  numeric optional
#' @field getsScoreAvgPlatformSize  numeric optional
#' @field getsScoreSampleSize  numeric optional
#' @field getsScoreRawData  numeric optional
#' @field getsScoreMissingValues  numeric optional
#' @field getqScoreOutliers  numeric optional
#' @field getqScoreSampleMeanCorrelation  numeric optional
#' @field getqScoreSampleMedianCorrelation  numeric optional
#' @field getqScoreSampleCorrelationVariance  numeric optional
#' @field getqScorePlatformsTech  numeric optional
#' @field getqScoreReplicates  numeric optional
#' @field getqScoreBatchInfo  numeric optional
#' @field getqScorePublicBatchEffect  numeric optional
#' @field getqScorePublicBatchConfound  numeric optional
#' @field noVectors  character optional
#' @field corrMatIssues  character optional
#' @field replicatesIssues  character optional
#' @field batchCorrected  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
GeeqValueObject <- R6::R6Class(
  "GeeqValueObject",
  public = list(
    `id` = NULL,
    `publicQualityScore` = NULL,
    `publicSuitabilityScore` = NULL,
    `getsScorePublication` = NULL,
    `getsScorePlatformAmount` = NULL,
    `getsScorePlatformsTechMulti` = NULL,
    `getsScoreAvgPlatformPopularity` = NULL,
    `getsScoreAvgPlatformSize` = NULL,
    `getsScoreSampleSize` = NULL,
    `getsScoreRawData` = NULL,
    `getsScoreMissingValues` = NULL,
    `getqScoreOutliers` = NULL,
    `getqScoreSampleMeanCorrelation` = NULL,
    `getqScoreSampleMedianCorrelation` = NULL,
    `getqScoreSampleCorrelationVariance` = NULL,
    `getqScorePlatformsTech` = NULL,
    `getqScoreReplicates` = NULL,
    `getqScoreBatchInfo` = NULL,
    `getqScorePublicBatchEffect` = NULL,
    `getqScorePublicBatchConfound` = NULL,
    `noVectors` = NULL,
    `corrMatIssues` = NULL,
    `replicatesIssues` = NULL,
    `batchCorrected` = NULL,
    #' Initialize a new GeeqValueObject class.
    #'
    #' @description
    #' Initialize a new GeeqValueObject class.
    #'
    #' @param id id
    #' @param publicQualityScore publicQualityScore
    #' @param publicSuitabilityScore publicSuitabilityScore
    #' @param getsScorePublication getsScorePublication
    #' @param getsScorePlatformAmount getsScorePlatformAmount
    #' @param getsScorePlatformsTechMulti getsScorePlatformsTechMulti
    #' @param getsScoreAvgPlatformPopularity getsScoreAvgPlatformPopularity
    #' @param getsScoreAvgPlatformSize getsScoreAvgPlatformSize
    #' @param getsScoreSampleSize getsScoreSampleSize
    #' @param getsScoreRawData getsScoreRawData
    #' @param getsScoreMissingValues getsScoreMissingValues
    #' @param getqScoreOutliers getqScoreOutliers
    #' @param getqScoreSampleMeanCorrelation getqScoreSampleMeanCorrelation
    #' @param getqScoreSampleMedianCorrelation getqScoreSampleMedianCorrelation
    #' @param getqScoreSampleCorrelationVariance getqScoreSampleCorrelationVariance
    #' @param getqScorePlatformsTech getqScorePlatformsTech
    #' @param getqScoreReplicates getqScoreReplicates
    #' @param getqScoreBatchInfo getqScoreBatchInfo
    #' @param getqScorePublicBatchEffect getqScorePublicBatchEffect
    #' @param getqScorePublicBatchConfound getqScorePublicBatchConfound
    #' @param noVectors noVectors
    #' @param corrMatIssues corrMatIssues
    #' @param replicatesIssues replicatesIssues
    #' @param batchCorrected batchCorrected
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `publicQualityScore` = NULL, `publicSuitabilityScore` = NULL, `getsScorePublication` = NULL, `getsScorePlatformAmount` = NULL, `getsScorePlatformsTechMulti` = NULL, `getsScoreAvgPlatformPopularity` = NULL, `getsScoreAvgPlatformSize` = NULL, `getsScoreSampleSize` = NULL, `getsScoreRawData` = NULL, `getsScoreMissingValues` = NULL, `getqScoreOutliers` = NULL, `getqScoreSampleMeanCorrelation` = NULL, `getqScoreSampleMedianCorrelation` = NULL, `getqScoreSampleCorrelationVariance` = NULL, `getqScorePlatformsTech` = NULL, `getqScoreReplicates` = NULL, `getqScoreBatchInfo` = NULL, `getqScorePublicBatchEffect` = NULL, `getqScorePublicBatchConfound` = NULL, `noVectors` = NULL, `corrMatIssues` = NULL, `replicatesIssues` = NULL, `batchCorrected` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`publicQualityScore`)) {
        stopifnot(is.numeric(`publicQualityScore`), length(`publicQualityScore`) == 1)
        self$`publicQualityScore` <- `publicQualityScore`
      }
      if (!is.null(`publicSuitabilityScore`)) {
        stopifnot(is.numeric(`publicSuitabilityScore`), length(`publicSuitabilityScore`) == 1)
        self$`publicSuitabilityScore` <- `publicSuitabilityScore`
      }
      if (!is.null(`getsScorePublication`)) {
        stopifnot(is.numeric(`getsScorePublication`), length(`getsScorePublication`) == 1)
        self$`getsScorePublication` <- `getsScorePublication`
      }
      if (!is.null(`getsScorePlatformAmount`)) {
        stopifnot(is.numeric(`getsScorePlatformAmount`), length(`getsScorePlatformAmount`) == 1)
        self$`getsScorePlatformAmount` <- `getsScorePlatformAmount`
      }
      if (!is.null(`getsScorePlatformsTechMulti`)) {
        stopifnot(is.numeric(`getsScorePlatformsTechMulti`), length(`getsScorePlatformsTechMulti`) == 1)
        self$`getsScorePlatformsTechMulti` <- `getsScorePlatformsTechMulti`
      }
      if (!is.null(`getsScoreAvgPlatformPopularity`)) {
        stopifnot(is.numeric(`getsScoreAvgPlatformPopularity`), length(`getsScoreAvgPlatformPopularity`) == 1)
        self$`getsScoreAvgPlatformPopularity` <- `getsScoreAvgPlatformPopularity`
      }
      if (!is.null(`getsScoreAvgPlatformSize`)) {
        stopifnot(is.numeric(`getsScoreAvgPlatformSize`), length(`getsScoreAvgPlatformSize`) == 1)
        self$`getsScoreAvgPlatformSize` <- `getsScoreAvgPlatformSize`
      }
      if (!is.null(`getsScoreSampleSize`)) {
        stopifnot(is.numeric(`getsScoreSampleSize`), length(`getsScoreSampleSize`) == 1)
        self$`getsScoreSampleSize` <- `getsScoreSampleSize`
      }
      if (!is.null(`getsScoreRawData`)) {
        stopifnot(is.numeric(`getsScoreRawData`), length(`getsScoreRawData`) == 1)
        self$`getsScoreRawData` <- `getsScoreRawData`
      }
      if (!is.null(`getsScoreMissingValues`)) {
        stopifnot(is.numeric(`getsScoreMissingValues`), length(`getsScoreMissingValues`) == 1)
        self$`getsScoreMissingValues` <- `getsScoreMissingValues`
      }
      if (!is.null(`getqScoreOutliers`)) {
        stopifnot(is.numeric(`getqScoreOutliers`), length(`getqScoreOutliers`) == 1)
        self$`getqScoreOutliers` <- `getqScoreOutliers`
      }
      if (!is.null(`getqScoreSampleMeanCorrelation`)) {
        stopifnot(is.numeric(`getqScoreSampleMeanCorrelation`), length(`getqScoreSampleMeanCorrelation`) == 1)
        self$`getqScoreSampleMeanCorrelation` <- `getqScoreSampleMeanCorrelation`
      }
      if (!is.null(`getqScoreSampleMedianCorrelation`)) {
        stopifnot(is.numeric(`getqScoreSampleMedianCorrelation`), length(`getqScoreSampleMedianCorrelation`) == 1)
        self$`getqScoreSampleMedianCorrelation` <- `getqScoreSampleMedianCorrelation`
      }
      if (!is.null(`getqScoreSampleCorrelationVariance`)) {
        stopifnot(is.numeric(`getqScoreSampleCorrelationVariance`), length(`getqScoreSampleCorrelationVariance`) == 1)
        self$`getqScoreSampleCorrelationVariance` <- `getqScoreSampleCorrelationVariance`
      }
      if (!is.null(`getqScorePlatformsTech`)) {
        stopifnot(is.numeric(`getqScorePlatformsTech`), length(`getqScorePlatformsTech`) == 1)
        self$`getqScorePlatformsTech` <- `getqScorePlatformsTech`
      }
      if (!is.null(`getqScoreReplicates`)) {
        stopifnot(is.numeric(`getqScoreReplicates`), length(`getqScoreReplicates`) == 1)
        self$`getqScoreReplicates` <- `getqScoreReplicates`
      }
      if (!is.null(`getqScoreBatchInfo`)) {
        stopifnot(is.numeric(`getqScoreBatchInfo`), length(`getqScoreBatchInfo`) == 1)
        self$`getqScoreBatchInfo` <- `getqScoreBatchInfo`
      }
      if (!is.null(`getqScorePublicBatchEffect`)) {
        stopifnot(is.numeric(`getqScorePublicBatchEffect`), length(`getqScorePublicBatchEffect`) == 1)
        self$`getqScorePublicBatchEffect` <- `getqScorePublicBatchEffect`
      }
      if (!is.null(`getqScorePublicBatchConfound`)) {
        stopifnot(is.numeric(`getqScorePublicBatchConfound`), length(`getqScorePublicBatchConfound`) == 1)
        self$`getqScorePublicBatchConfound` <- `getqScorePublicBatchConfound`
      }
      if (!is.null(`noVectors`)) {
        stopifnot(is.logical(`noVectors`), length(`noVectors`) == 1)
        self$`noVectors` <- `noVectors`
      }
      if (!is.null(`corrMatIssues`)) {
        self$`corrMatIssues` <- `corrMatIssues`
      }
      if (!is.null(`replicatesIssues`)) {
        self$`replicatesIssues` <- `replicatesIssues`
      }
      if (!is.null(`batchCorrected`)) {
        stopifnot(is.logical(`batchCorrected`), length(`batchCorrected`) == 1)
        self$`batchCorrected` <- `batchCorrected`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeeqValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      GeeqValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        GeeqValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`publicQualityScore`)) {
        GeeqValueObjectObject[["publicQualityScore"]] <-
          self$`publicQualityScore`
      }
      if (!is.null(self$`publicSuitabilityScore`)) {
        GeeqValueObjectObject[["publicSuitabilityScore"]] <-
          self$`publicSuitabilityScore`
      }
      if (!is.null(self$`getsScorePublication`)) {
        GeeqValueObjectObject[["getsScorePublication"]] <-
          self$`getsScorePublication`
      }
      if (!is.null(self$`getsScorePlatformAmount`)) {
        GeeqValueObjectObject[["getsScorePlatformAmount"]] <-
          self$`getsScorePlatformAmount`
      }
      if (!is.null(self$`getsScorePlatformsTechMulti`)) {
        GeeqValueObjectObject[["getsScorePlatformsTechMulti"]] <-
          self$`getsScorePlatformsTechMulti`
      }
      if (!is.null(self$`getsScoreAvgPlatformPopularity`)) {
        GeeqValueObjectObject[["getsScoreAvgPlatformPopularity"]] <-
          self$`getsScoreAvgPlatformPopularity`
      }
      if (!is.null(self$`getsScoreAvgPlatformSize`)) {
        GeeqValueObjectObject[["getsScoreAvgPlatformSize"]] <-
          self$`getsScoreAvgPlatformSize`
      }
      if (!is.null(self$`getsScoreSampleSize`)) {
        GeeqValueObjectObject[["getsScoreSampleSize"]] <-
          self$`getsScoreSampleSize`
      }
      if (!is.null(self$`getsScoreRawData`)) {
        GeeqValueObjectObject[["getsScoreRawData"]] <-
          self$`getsScoreRawData`
      }
      if (!is.null(self$`getsScoreMissingValues`)) {
        GeeqValueObjectObject[["getsScoreMissingValues"]] <-
          self$`getsScoreMissingValues`
      }
      if (!is.null(self$`getqScoreOutliers`)) {
        GeeqValueObjectObject[["getqScoreOutliers"]] <-
          self$`getqScoreOutliers`
      }
      if (!is.null(self$`getqScoreSampleMeanCorrelation`)) {
        GeeqValueObjectObject[["getqScoreSampleMeanCorrelation"]] <-
          self$`getqScoreSampleMeanCorrelation`
      }
      if (!is.null(self$`getqScoreSampleMedianCorrelation`)) {
        GeeqValueObjectObject[["getqScoreSampleMedianCorrelation"]] <-
          self$`getqScoreSampleMedianCorrelation`
      }
      if (!is.null(self$`getqScoreSampleCorrelationVariance`)) {
        GeeqValueObjectObject[["getqScoreSampleCorrelationVariance"]] <-
          self$`getqScoreSampleCorrelationVariance`
      }
      if (!is.null(self$`getqScorePlatformsTech`)) {
        GeeqValueObjectObject[["getqScorePlatformsTech"]] <-
          self$`getqScorePlatformsTech`
      }
      if (!is.null(self$`getqScoreReplicates`)) {
        GeeqValueObjectObject[["getqScoreReplicates"]] <-
          self$`getqScoreReplicates`
      }
      if (!is.null(self$`getqScoreBatchInfo`)) {
        GeeqValueObjectObject[["getqScoreBatchInfo"]] <-
          self$`getqScoreBatchInfo`
      }
      if (!is.null(self$`getqScorePublicBatchEffect`)) {
        GeeqValueObjectObject[["getqScorePublicBatchEffect"]] <-
          self$`getqScorePublicBatchEffect`
      }
      if (!is.null(self$`getqScorePublicBatchConfound`)) {
        GeeqValueObjectObject[["getqScorePublicBatchConfound"]] <-
          self$`getqScorePublicBatchConfound`
      }
      if (!is.null(self$`noVectors`)) {
        GeeqValueObjectObject[["noVectors"]] <-
          self$`noVectors`
      }
      if (!is.null(self$`corrMatIssues`)) {
        GeeqValueObjectObject[["corrMatIssues"]] <-
          self$`corrMatIssues`
      }
      if (!is.null(self$`replicatesIssues`)) {
        GeeqValueObjectObject[["replicatesIssues"]] <-
          self$`replicatesIssues`
      }
      if (!is.null(self$`batchCorrected`)) {
        GeeqValueObjectObject[["batchCorrected"]] <-
          self$`batchCorrected`
      }

      GeeqValueObjectObject
    },
    #' Deserialize JSON string into an instance of GeeqValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeeqValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeeqValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`publicQualityScore`)) {
        self$`publicQualityScore` <- this_object$`publicQualityScore`
      }
      if (!is.null(this_object$`publicSuitabilityScore`)) {
        self$`publicSuitabilityScore` <- this_object$`publicSuitabilityScore`
      }
      if (!is.null(this_object$`getsScorePublication`)) {
        self$`getsScorePublication` <- this_object$`getsScorePublication`
      }
      if (!is.null(this_object$`getsScorePlatformAmount`)) {
        self$`getsScorePlatformAmount` <- this_object$`getsScorePlatformAmount`
      }
      if (!is.null(this_object$`getsScorePlatformsTechMulti`)) {
        self$`getsScorePlatformsTechMulti` <- this_object$`getsScorePlatformsTechMulti`
      }
      if (!is.null(this_object$`getsScoreAvgPlatformPopularity`)) {
        self$`getsScoreAvgPlatformPopularity` <- this_object$`getsScoreAvgPlatformPopularity`
      }
      if (!is.null(this_object$`getsScoreAvgPlatformSize`)) {
        self$`getsScoreAvgPlatformSize` <- this_object$`getsScoreAvgPlatformSize`
      }
      if (!is.null(this_object$`getsScoreSampleSize`)) {
        self$`getsScoreSampleSize` <- this_object$`getsScoreSampleSize`
      }
      if (!is.null(this_object$`getsScoreRawData`)) {
        self$`getsScoreRawData` <- this_object$`getsScoreRawData`
      }
      if (!is.null(this_object$`getsScoreMissingValues`)) {
        self$`getsScoreMissingValues` <- this_object$`getsScoreMissingValues`
      }
      if (!is.null(this_object$`getqScoreOutliers`)) {
        self$`getqScoreOutliers` <- this_object$`getqScoreOutliers`
      }
      if (!is.null(this_object$`getqScoreSampleMeanCorrelation`)) {
        self$`getqScoreSampleMeanCorrelation` <- this_object$`getqScoreSampleMeanCorrelation`
      }
      if (!is.null(this_object$`getqScoreSampleMedianCorrelation`)) {
        self$`getqScoreSampleMedianCorrelation` <- this_object$`getqScoreSampleMedianCorrelation`
      }
      if (!is.null(this_object$`getqScoreSampleCorrelationVariance`)) {
        self$`getqScoreSampleCorrelationVariance` <- this_object$`getqScoreSampleCorrelationVariance`
      }
      if (!is.null(this_object$`getqScorePlatformsTech`)) {
        self$`getqScorePlatformsTech` <- this_object$`getqScorePlatformsTech`
      }
      if (!is.null(this_object$`getqScoreReplicates`)) {
        self$`getqScoreReplicates` <- this_object$`getqScoreReplicates`
      }
      if (!is.null(this_object$`getqScoreBatchInfo`)) {
        self$`getqScoreBatchInfo` <- this_object$`getqScoreBatchInfo`
      }
      if (!is.null(this_object$`getqScorePublicBatchEffect`)) {
        self$`getqScorePublicBatchEffect` <- this_object$`getqScorePublicBatchEffect`
      }
      if (!is.null(this_object$`getqScorePublicBatchConfound`)) {
        self$`getqScorePublicBatchConfound` <- this_object$`getqScorePublicBatchConfound`
      }
      if (!is.null(this_object$`noVectors`)) {
        self$`noVectors` <- this_object$`noVectors`
      }
      if (!is.null(this_object$`corrMatIssues`)) {
        self$`corrMatIssues` <- this_object$`corrMatIssues`
      }
      if (!is.null(this_object$`replicatesIssues`)) {
        self$`replicatesIssues` <- this_object$`replicatesIssues`
      }
      if (!is.null(this_object$`batchCorrected`)) {
        self$`batchCorrected` <- this_object$`batchCorrected`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeeqValueObject in JSON format
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
        if (!is.null(self$`publicQualityScore`)) {
          sprintf(
          '"publicQualityScore":
            %d
                    ',
          self$`publicQualityScore`
          )
        },
        if (!is.null(self$`publicSuitabilityScore`)) {
          sprintf(
          '"publicSuitabilityScore":
            %d
                    ',
          self$`publicSuitabilityScore`
          )
        },
        if (!is.null(self$`getsScorePublication`)) {
          sprintf(
          '"getsScorePublication":
            %d
                    ',
          self$`getsScorePublication`
          )
        },
        if (!is.null(self$`getsScorePlatformAmount`)) {
          sprintf(
          '"getsScorePlatformAmount":
            %d
                    ',
          self$`getsScorePlatformAmount`
          )
        },
        if (!is.null(self$`getsScorePlatformsTechMulti`)) {
          sprintf(
          '"getsScorePlatformsTechMulti":
            %d
                    ',
          self$`getsScorePlatformsTechMulti`
          )
        },
        if (!is.null(self$`getsScoreAvgPlatformPopularity`)) {
          sprintf(
          '"getsScoreAvgPlatformPopularity":
            %d
                    ',
          self$`getsScoreAvgPlatformPopularity`
          )
        },
        if (!is.null(self$`getsScoreAvgPlatformSize`)) {
          sprintf(
          '"getsScoreAvgPlatformSize":
            %d
                    ',
          self$`getsScoreAvgPlatformSize`
          )
        },
        if (!is.null(self$`getsScoreSampleSize`)) {
          sprintf(
          '"getsScoreSampleSize":
            %d
                    ',
          self$`getsScoreSampleSize`
          )
        },
        if (!is.null(self$`getsScoreRawData`)) {
          sprintf(
          '"getsScoreRawData":
            %d
                    ',
          self$`getsScoreRawData`
          )
        },
        if (!is.null(self$`getsScoreMissingValues`)) {
          sprintf(
          '"getsScoreMissingValues":
            %d
                    ',
          self$`getsScoreMissingValues`
          )
        },
        if (!is.null(self$`getqScoreOutliers`)) {
          sprintf(
          '"getqScoreOutliers":
            %d
                    ',
          self$`getqScoreOutliers`
          )
        },
        if (!is.null(self$`getqScoreSampleMeanCorrelation`)) {
          sprintf(
          '"getqScoreSampleMeanCorrelation":
            %d
                    ',
          self$`getqScoreSampleMeanCorrelation`
          )
        },
        if (!is.null(self$`getqScoreSampleMedianCorrelation`)) {
          sprintf(
          '"getqScoreSampleMedianCorrelation":
            %d
                    ',
          self$`getqScoreSampleMedianCorrelation`
          )
        },
        if (!is.null(self$`getqScoreSampleCorrelationVariance`)) {
          sprintf(
          '"getqScoreSampleCorrelationVariance":
            %d
                    ',
          self$`getqScoreSampleCorrelationVariance`
          )
        },
        if (!is.null(self$`getqScorePlatformsTech`)) {
          sprintf(
          '"getqScorePlatformsTech":
            %d
                    ',
          self$`getqScorePlatformsTech`
          )
        },
        if (!is.null(self$`getqScoreReplicates`)) {
          sprintf(
          '"getqScoreReplicates":
            %d
                    ',
          self$`getqScoreReplicates`
          )
        },
        if (!is.null(self$`getqScoreBatchInfo`)) {
          sprintf(
          '"getqScoreBatchInfo":
            %d
                    ',
          self$`getqScoreBatchInfo`
          )
        },
        if (!is.null(self$`getqScorePublicBatchEffect`)) {
          sprintf(
          '"getqScorePublicBatchEffect":
            %d
                    ',
          self$`getqScorePublicBatchEffect`
          )
        },
        if (!is.null(self$`getqScorePublicBatchConfound`)) {
          sprintf(
          '"getqScorePublicBatchConfound":
            %d
                    ',
          self$`getqScorePublicBatchConfound`
          )
        },
        if (!is.null(self$`noVectors`)) {
          sprintf(
          '"noVectors":
            %s
                    ',
          tolower(self$`noVectors`)
          )
        },
        if (!is.null(self$`corrMatIssues`)) {
          sprintf(
          '"corrMatIssues":
            "%s"
                    ',
          self$`corrMatIssues`
          )
        },
        if (!is.null(self$`replicatesIssues`)) {
          sprintf(
          '"replicatesIssues":
            "%s"
                    ',
          self$`replicatesIssues`
          )
        },
        if (!is.null(self$`batchCorrected`)) {
          sprintf(
          '"batchCorrected":
            %s
                    ',
          tolower(self$`batchCorrected`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of GeeqValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeeqValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeeqValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`publicQualityScore` <- this_object$`publicQualityScore`
      self$`publicSuitabilityScore` <- this_object$`publicSuitabilityScore`
      self$`getsScorePublication` <- this_object$`getsScorePublication`
      self$`getsScorePlatformAmount` <- this_object$`getsScorePlatformAmount`
      self$`getsScorePlatformsTechMulti` <- this_object$`getsScorePlatformsTechMulti`
      self$`getsScoreAvgPlatformPopularity` <- this_object$`getsScoreAvgPlatformPopularity`
      self$`getsScoreAvgPlatformSize` <- this_object$`getsScoreAvgPlatformSize`
      self$`getsScoreSampleSize` <- this_object$`getsScoreSampleSize`
      self$`getsScoreRawData` <- this_object$`getsScoreRawData`
      self$`getsScoreMissingValues` <- this_object$`getsScoreMissingValues`
      self$`getqScoreOutliers` <- this_object$`getqScoreOutliers`
      self$`getqScoreSampleMeanCorrelation` <- this_object$`getqScoreSampleMeanCorrelation`
      self$`getqScoreSampleMedianCorrelation` <- this_object$`getqScoreSampleMedianCorrelation`
      self$`getqScoreSampleCorrelationVariance` <- this_object$`getqScoreSampleCorrelationVariance`
      self$`getqScorePlatformsTech` <- this_object$`getqScorePlatformsTech`
      self$`getqScoreReplicates` <- this_object$`getqScoreReplicates`
      self$`getqScoreBatchInfo` <- this_object$`getqScoreBatchInfo`
      self$`getqScorePublicBatchEffect` <- this_object$`getqScorePublicBatchEffect`
      self$`getqScorePublicBatchConfound` <- this_object$`getqScorePublicBatchConfound`
      self$`noVectors` <- this_object$`noVectors`
      self$`corrMatIssues` <- this_object$`corrMatIssues`
      self$`replicatesIssues` <- this_object$`replicatesIssues`
      self$`batchCorrected` <- this_object$`batchCorrected`
      self
    },
    #' Validate JSON input with respect to GeeqValueObject
    #'
    #' @description
    #' Validate JSON input with respect to GeeqValueObject and throw an exception if invalid
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
    #' @return String representation of GeeqValueObject
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
GeeqValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
GeeqValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
GeeqValueObject$lock()

