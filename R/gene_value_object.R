#' Create a new GeneValueObject
#'
#' @description
#' GeneValueObject Class
#'
#' @docType class
#' @title GeneValueObject
#' @description GeneValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field aliases  list(character) [optional]
#' @field associatedExperimentCount  integer [optional]
#' @field compositeSequenceCount  integer [optional]
#' @field geneSets  list(\link{GeneSetValueObject}) [optional]
#' @field homologues  list(\link{GeneValueObject}) [optional]
#' @field isQuery  character [optional]
#' @field multifunctionalityRank  numeric [optional]
#' @field ncbiId  integer [optional]
#' @field ensemblId  character [optional]
#' @field nodeDegreeNegRanks  list(numeric) [optional]
#' @field nodeDegreePosRanks  list(numeric) [optional]
#' @field nodeDegreesNeg  list(integer) [optional]
#' @field nodeDegreesPos  list(integer) [optional]
#' @field numGoTerms  integer [optional]
#' @field officialName  character [optional]
#' @field officialSymbol  character [optional]
#' @field phenotypes  list(\link{CharacteristicValueObject}) [optional]
#' @field platformCount  integer [optional]
#' @field score  numeric [optional]
#' @field taxonCommonName  character [optional]
#' @field taxonId  integer [optional]
#' @field taxonScientificName  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
GeneValueObject <- R6::R6Class(
  "GeneValueObject",
  public = list(
    `id` = NULL,
    `aliases` = NULL,
    `associatedExperimentCount` = NULL,
    `compositeSequenceCount` = NULL,
    `geneSets` = NULL,
    `homologues` = NULL,
    `isQuery` = NULL,
    `multifunctionalityRank` = NULL,
    `ncbiId` = NULL,
    `ensemblId` = NULL,
    `nodeDegreeNegRanks` = NULL,
    `nodeDegreePosRanks` = NULL,
    `nodeDegreesNeg` = NULL,
    `nodeDegreesPos` = NULL,
    `numGoTerms` = NULL,
    `officialName` = NULL,
    `officialSymbol` = NULL,
    `phenotypes` = NULL,
    `platformCount` = NULL,
    `score` = NULL,
    `taxonCommonName` = NULL,
    `taxonId` = NULL,
    `taxonScientificName` = NULL,
    #' Initialize a new GeneValueObject class.
    #'
    #' @description
    #' Initialize a new GeneValueObject class.
    #'
    #' @param id id
    #' @param aliases aliases
    #' @param associatedExperimentCount associatedExperimentCount
    #' @param compositeSequenceCount compositeSequenceCount
    #' @param geneSets geneSets
    #' @param homologues homologues
    #' @param isQuery isQuery
    #' @param multifunctionalityRank multifunctionalityRank
    #' @param ncbiId ncbiId
    #' @param ensemblId ensemblId
    #' @param nodeDegreeNegRanks nodeDegreeNegRanks
    #' @param nodeDegreePosRanks nodeDegreePosRanks
    #' @param nodeDegreesNeg nodeDegreesNeg
    #' @param nodeDegreesPos nodeDegreesPos
    #' @param numGoTerms numGoTerms
    #' @param officialName officialName
    #' @param officialSymbol officialSymbol
    #' @param phenotypes phenotypes
    #' @param platformCount platformCount
    #' @param score score
    #' @param taxonCommonName taxonCommonName
    #' @param taxonId taxonId
    #' @param taxonScientificName taxonScientificName
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `aliases` = NULL, `associatedExperimentCount` = NULL, `compositeSequenceCount` = NULL, `geneSets` = NULL, `homologues` = NULL, `isQuery` = NULL, `multifunctionalityRank` = NULL, `ncbiId` = NULL, `ensemblId` = NULL, `nodeDegreeNegRanks` = NULL, `nodeDegreePosRanks` = NULL, `nodeDegreesNeg` = NULL, `nodeDegreesPos` = NULL, `numGoTerms` = NULL, `officialName` = NULL, `officialSymbol` = NULL, `phenotypes` = NULL, `platformCount` = NULL, `score` = NULL, `taxonCommonName` = NULL, `taxonId` = NULL, `taxonScientificName` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`aliases`)) {
        stopifnot(is.vector(`aliases`), length(`aliases`) != 0)
        sapply(`aliases`, function(x) stopifnot(is.character(x)))
        self$`aliases` <- `aliases`
      }
      if (!is.null(`associatedExperimentCount`)) {
        stopifnot(is.numeric(`associatedExperimentCount`), length(`associatedExperimentCount`) == 1)
        self$`associatedExperimentCount` <- `associatedExperimentCount`
      }
      if (!is.null(`compositeSequenceCount`)) {
        stopifnot(is.numeric(`compositeSequenceCount`), length(`compositeSequenceCount`) == 1)
        self$`compositeSequenceCount` <- `compositeSequenceCount`
      }
      if (!is.null(`geneSets`)) {
        stopifnot(is.vector(`geneSets`), length(`geneSets`) != 0)
        sapply(`geneSets`, function(x) stopifnot(R6::is.R6(x)))
        self$`geneSets` <- `geneSets`
      }
      if (!is.null(`homologues`)) {
        stopifnot(is.vector(`homologues`), length(`homologues`) != 0)
        sapply(`homologues`, function(x) stopifnot(R6::is.R6(x)))
        self$`homologues` <- `homologues`
      }
      if (!is.null(`isQuery`)) {
        stopifnot(is.logical(`isQuery`), length(`isQuery`) == 1)
        self$`isQuery` <- `isQuery`
      }
      if (!is.null(`multifunctionalityRank`)) {
        stopifnot(is.numeric(`multifunctionalityRank`), length(`multifunctionalityRank`) == 1)
        self$`multifunctionalityRank` <- `multifunctionalityRank`
      }
      if (!is.null(`ncbiId`)) {
        stopifnot(is.numeric(`ncbiId`), length(`ncbiId`) == 1)
        self$`ncbiId` <- `ncbiId`
      }
      if (!is.null(`ensemblId`)) {
        stopifnot(is.character(`ensemblId`), length(`ensemblId`) == 1)
        self$`ensemblId` <- `ensemblId`
      }
      if (!is.null(`nodeDegreeNegRanks`)) {
        stopifnot(is.vector(`nodeDegreeNegRanks`), length(`nodeDegreeNegRanks`) != 0)
        sapply(`nodeDegreeNegRanks`, function(x) stopifnot(is.character(x)))
        self$`nodeDegreeNegRanks` <- `nodeDegreeNegRanks`
      }
      if (!is.null(`nodeDegreePosRanks`)) {
        stopifnot(is.vector(`nodeDegreePosRanks`), length(`nodeDegreePosRanks`) != 0)
        sapply(`nodeDegreePosRanks`, function(x) stopifnot(is.character(x)))
        self$`nodeDegreePosRanks` <- `nodeDegreePosRanks`
      }
      if (!is.null(`nodeDegreesNeg`)) {
        stopifnot(is.vector(`nodeDegreesNeg`), length(`nodeDegreesNeg`) != 0)
        sapply(`nodeDegreesNeg`, function(x) stopifnot(is.character(x)))
        self$`nodeDegreesNeg` <- `nodeDegreesNeg`
      }
      if (!is.null(`nodeDegreesPos`)) {
        stopifnot(is.vector(`nodeDegreesPos`), length(`nodeDegreesPos`) != 0)
        sapply(`nodeDegreesPos`, function(x) stopifnot(is.character(x)))
        self$`nodeDegreesPos` <- `nodeDegreesPos`
      }
      if (!is.null(`numGoTerms`)) {
        stopifnot(is.numeric(`numGoTerms`), length(`numGoTerms`) == 1)
        self$`numGoTerms` <- `numGoTerms`
      }
      if (!is.null(`officialName`)) {
        stopifnot(is.character(`officialName`), length(`officialName`) == 1)
        self$`officialName` <- `officialName`
      }
      if (!is.null(`officialSymbol`)) {
        stopifnot(is.character(`officialSymbol`), length(`officialSymbol`) == 1)
        self$`officialSymbol` <- `officialSymbol`
      }
      if (!is.null(`phenotypes`)) {
        stopifnot(is.vector(`phenotypes`), length(`phenotypes`) != 0)
        sapply(`phenotypes`, function(x) stopifnot(R6::is.R6(x)))
        self$`phenotypes` <- `phenotypes`
      }
      if (!is.null(`platformCount`)) {
        stopifnot(is.numeric(`platformCount`), length(`platformCount`) == 1)
        self$`platformCount` <- `platformCount`
      }
      if (!is.null(`score`)) {
        stopifnot(is.numeric(`score`), length(`score`) == 1)
        self$`score` <- `score`
      }
      if (!is.null(`taxonCommonName`)) {
        stopifnot(is.character(`taxonCommonName`), length(`taxonCommonName`) == 1)
        self$`taxonCommonName` <- `taxonCommonName`
      }
      if (!is.null(`taxonId`)) {
        stopifnot(is.numeric(`taxonId`), length(`taxonId`) == 1)
        self$`taxonId` <- `taxonId`
      }
      if (!is.null(`taxonScientificName`)) {
        stopifnot(is.character(`taxonScientificName`), length(`taxonScientificName`) == 1)
        self$`taxonScientificName` <- `taxonScientificName`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      GeneValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        GeneValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`aliases`)) {
        GeneValueObjectObject[["aliases"]] <-
          self$`aliases`
      }
      if (!is.null(self$`associatedExperimentCount`)) {
        GeneValueObjectObject[["associatedExperimentCount"]] <-
          self$`associatedExperimentCount`
      }
      if (!is.null(self$`compositeSequenceCount`)) {
        GeneValueObjectObject[["compositeSequenceCount"]] <-
          self$`compositeSequenceCount`
      }
      if (!is.null(self$`geneSets`)) {
        GeneValueObjectObject[["geneSets"]] <-
          lapply(self$`geneSets`, function(x) x$toJSON())
      }
      if (!is.null(self$`homologues`)) {
        GeneValueObjectObject[["homologues"]] <-
          lapply(self$`homologues`, function(x) x$toJSON())
      }
      if (!is.null(self$`isQuery`)) {
        GeneValueObjectObject[["isQuery"]] <-
          self$`isQuery`
      }
      if (!is.null(self$`multifunctionalityRank`)) {
        GeneValueObjectObject[["multifunctionalityRank"]] <-
          self$`multifunctionalityRank`
      }
      if (!is.null(self$`ncbiId`)) {
        GeneValueObjectObject[["ncbiId"]] <-
          self$`ncbiId`
      }
      if (!is.null(self$`ensemblId`)) {
        GeneValueObjectObject[["ensemblId"]] <-
          self$`ensemblId`
      }
      if (!is.null(self$`nodeDegreeNegRanks`)) {
        GeneValueObjectObject[["nodeDegreeNegRanks"]] <-
          self$`nodeDegreeNegRanks`
      }
      if (!is.null(self$`nodeDegreePosRanks`)) {
        GeneValueObjectObject[["nodeDegreePosRanks"]] <-
          self$`nodeDegreePosRanks`
      }
      if (!is.null(self$`nodeDegreesNeg`)) {
        GeneValueObjectObject[["nodeDegreesNeg"]] <-
          self$`nodeDegreesNeg`
      }
      if (!is.null(self$`nodeDegreesPos`)) {
        GeneValueObjectObject[["nodeDegreesPos"]] <-
          self$`nodeDegreesPos`
      }
      if (!is.null(self$`numGoTerms`)) {
        GeneValueObjectObject[["numGoTerms"]] <-
          self$`numGoTerms`
      }
      if (!is.null(self$`officialName`)) {
        GeneValueObjectObject[["officialName"]] <-
          self$`officialName`
      }
      if (!is.null(self$`officialSymbol`)) {
        GeneValueObjectObject[["officialSymbol"]] <-
          self$`officialSymbol`
      }
      if (!is.null(self$`phenotypes`)) {
        GeneValueObjectObject[["phenotypes"]] <-
          lapply(self$`phenotypes`, function(x) x$toJSON())
      }
      if (!is.null(self$`platformCount`)) {
        GeneValueObjectObject[["platformCount"]] <-
          self$`platformCount`
      }
      if (!is.null(self$`score`)) {
        GeneValueObjectObject[["score"]] <-
          self$`score`
      }
      if (!is.null(self$`taxonCommonName`)) {
        GeneValueObjectObject[["taxonCommonName"]] <-
          self$`taxonCommonName`
      }
      if (!is.null(self$`taxonId`)) {
        GeneValueObjectObject[["taxonId"]] <-
          self$`taxonId`
      }
      if (!is.null(self$`taxonScientificName`)) {
        GeneValueObjectObject[["taxonScientificName"]] <-
          self$`taxonScientificName`
      }

      GeneValueObjectObject
    },
    #' Deserialize JSON string into an instance of GeneValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`aliases`)) {
        self$`aliases` <- ApiClient$new()$deserializeObj(this_object$`aliases`, "array[character]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`associatedExperimentCount`)) {
        self$`associatedExperimentCount` <- this_object$`associatedExperimentCount`
      }
      if (!is.null(this_object$`compositeSequenceCount`)) {
        self$`compositeSequenceCount` <- this_object$`compositeSequenceCount`
      }
      if (!is.null(this_object$`geneSets`)) {
        self$`geneSets` <- ApiClient$new()$deserializeObj(this_object$`geneSets`, "array[GeneSetValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`homologues`)) {
        self$`homologues` <- ApiClient$new()$deserializeObj(this_object$`homologues`, "array[GeneValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`isQuery`)) {
        self$`isQuery` <- this_object$`isQuery`
      }
      if (!is.null(this_object$`multifunctionalityRank`)) {
        self$`multifunctionalityRank` <- this_object$`multifunctionalityRank`
      }
      if (!is.null(this_object$`ncbiId`)) {
        self$`ncbiId` <- this_object$`ncbiId`
      }
      if (!is.null(this_object$`ensemblId`)) {
        self$`ensemblId` <- this_object$`ensemblId`
      }
      if (!is.null(this_object$`nodeDegreeNegRanks`)) {
        self$`nodeDegreeNegRanks` <- ApiClient$new()$deserializeObj(this_object$`nodeDegreeNegRanks`, "array[numeric]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`nodeDegreePosRanks`)) {
        self$`nodeDegreePosRanks` <- ApiClient$new()$deserializeObj(this_object$`nodeDegreePosRanks`, "array[numeric]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`nodeDegreesNeg`)) {
        self$`nodeDegreesNeg` <- ApiClient$new()$deserializeObj(this_object$`nodeDegreesNeg`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`nodeDegreesPos`)) {
        self$`nodeDegreesPos` <- ApiClient$new()$deserializeObj(this_object$`nodeDegreesPos`, "array[integer]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`numGoTerms`)) {
        self$`numGoTerms` <- this_object$`numGoTerms`
      }
      if (!is.null(this_object$`officialName`)) {
        self$`officialName` <- this_object$`officialName`
      }
      if (!is.null(this_object$`officialSymbol`)) {
        self$`officialSymbol` <- this_object$`officialSymbol`
      }
      if (!is.null(this_object$`phenotypes`)) {
        self$`phenotypes` <- ApiClient$new()$deserializeObj(this_object$`phenotypes`, "array[CharacteristicValueObject]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`platformCount`)) {
        self$`platformCount` <- this_object$`platformCount`
      }
      if (!is.null(this_object$`score`)) {
        self$`score` <- this_object$`score`
      }
      if (!is.null(this_object$`taxonCommonName`)) {
        self$`taxonCommonName` <- this_object$`taxonCommonName`
      }
      if (!is.null(this_object$`taxonId`)) {
        self$`taxonId` <- this_object$`taxonId`
      }
      if (!is.null(this_object$`taxonScientificName`)) {
        self$`taxonScientificName` <- this_object$`taxonScientificName`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneValueObject in JSON format
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
        if (!is.null(self$`aliases`)) {
          sprintf(
          '"aliases":
             [%s]
          ',
          paste(unlist(lapply(self$`aliases`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`associatedExperimentCount`)) {
          sprintf(
          '"associatedExperimentCount":
            %d
                    ',
          self$`associatedExperimentCount`
          )
        },
        if (!is.null(self$`compositeSequenceCount`)) {
          sprintf(
          '"compositeSequenceCount":
            %d
                    ',
          self$`compositeSequenceCount`
          )
        },
        if (!is.null(self$`geneSets`)) {
          sprintf(
          '"geneSets":
          [%s]
',
          paste(sapply(self$`geneSets`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`homologues`)) {
          sprintf(
          '"homologues":
          [%s]
',
          paste(sapply(self$`homologues`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`isQuery`)) {
          sprintf(
          '"isQuery":
            %s
                    ',
          tolower(self$`isQuery`)
          )
        },
        if (!is.null(self$`multifunctionalityRank`)) {
          sprintf(
          '"multifunctionalityRank":
            %d
                    ',
          self$`multifunctionalityRank`
          )
        },
        if (!is.null(self$`ncbiId`)) {
          sprintf(
          '"ncbiId":
            %d
                    ',
          self$`ncbiId`
          )
        },
        if (!is.null(self$`ensemblId`)) {
          sprintf(
          '"ensemblId":
            "%s"
                    ',
          self$`ensemblId`
          )
        },
        if (!is.null(self$`nodeDegreeNegRanks`)) {
          sprintf(
          '"nodeDegreeNegRanks":
             [%s]
          ',
          paste(unlist(lapply(self$`nodeDegreeNegRanks`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`nodeDegreePosRanks`)) {
          sprintf(
          '"nodeDegreePosRanks":
             [%s]
          ',
          paste(unlist(lapply(self$`nodeDegreePosRanks`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`nodeDegreesNeg`)) {
          sprintf(
          '"nodeDegreesNeg":
             [%s]
          ',
          paste(unlist(lapply(self$`nodeDegreesNeg`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`nodeDegreesPos`)) {
          sprintf(
          '"nodeDegreesPos":
             [%s]
          ',
          paste(unlist(lapply(self$`nodeDegreesPos`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`numGoTerms`)) {
          sprintf(
          '"numGoTerms":
            %d
                    ',
          self$`numGoTerms`
          )
        },
        if (!is.null(self$`officialName`)) {
          sprintf(
          '"officialName":
            "%s"
                    ',
          self$`officialName`
          )
        },
        if (!is.null(self$`officialSymbol`)) {
          sprintf(
          '"officialSymbol":
            "%s"
                    ',
          self$`officialSymbol`
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
        if (!is.null(self$`platformCount`)) {
          sprintf(
          '"platformCount":
            %d
                    ',
          self$`platformCount`
          )
        },
        if (!is.null(self$`score`)) {
          sprintf(
          '"score":
            %d
                    ',
          self$`score`
          )
        },
        if (!is.null(self$`taxonCommonName`)) {
          sprintf(
          '"taxonCommonName":
            "%s"
                    ',
          self$`taxonCommonName`
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
        if (!is.null(self$`taxonScientificName`)) {
          sprintf(
          '"taxonScientificName":
            "%s"
                    ',
          self$`taxonScientificName`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of GeneValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`aliases` <- ApiClient$new()$deserializeObj(this_object$`aliases`, "array[character]", loadNamespace("gemma.R"))
      self$`associatedExperimentCount` <- this_object$`associatedExperimentCount`
      self$`compositeSequenceCount` <- this_object$`compositeSequenceCount`
      self$`geneSets` <- ApiClient$new()$deserializeObj(this_object$`geneSets`, "array[GeneSetValueObject]", loadNamespace("gemma.R"))
      self$`homologues` <- ApiClient$new()$deserializeObj(this_object$`homologues`, "array[GeneValueObject]", loadNamespace("gemma.R"))
      self$`isQuery` <- this_object$`isQuery`
      self$`multifunctionalityRank` <- this_object$`multifunctionalityRank`
      self$`ncbiId` <- this_object$`ncbiId`
      self$`ensemblId` <- this_object$`ensemblId`
      self$`nodeDegreeNegRanks` <- ApiClient$new()$deserializeObj(this_object$`nodeDegreeNegRanks`, "array[numeric]", loadNamespace("gemma.R"))
      self$`nodeDegreePosRanks` <- ApiClient$new()$deserializeObj(this_object$`nodeDegreePosRanks`, "array[numeric]", loadNamespace("gemma.R"))
      self$`nodeDegreesNeg` <- ApiClient$new()$deserializeObj(this_object$`nodeDegreesNeg`, "array[integer]", loadNamespace("gemma.R"))
      self$`nodeDegreesPos` <- ApiClient$new()$deserializeObj(this_object$`nodeDegreesPos`, "array[integer]", loadNamespace("gemma.R"))
      self$`numGoTerms` <- this_object$`numGoTerms`
      self$`officialName` <- this_object$`officialName`
      self$`officialSymbol` <- this_object$`officialSymbol`
      self$`phenotypes` <- ApiClient$new()$deserializeObj(this_object$`phenotypes`, "array[CharacteristicValueObject]", loadNamespace("gemma.R"))
      self$`platformCount` <- this_object$`platformCount`
      self$`score` <- this_object$`score`
      self$`taxonCommonName` <- this_object$`taxonCommonName`
      self$`taxonId` <- this_object$`taxonId`
      self$`taxonScientificName` <- this_object$`taxonScientificName`
      self
    },
    #' Validate JSON input with respect to GeneValueObject
    #'
    #' @description
    #' Validate JSON input with respect to GeneValueObject and throw an exception if invalid
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
    #' @return String representation of GeneValueObject
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
GeneValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
GeneValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
GeneValueObject$lock()

