#' Create a new BlatResultValueObject
#'
#' @description
#' BlatResultValueObject Class
#'
#' @docType class
#' @title BlatResultValueObject
#' @description BlatResultValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field blockCount  integer [optional]
#' @field blockSizes  character [optional]
#' @field identity  numeric [optional]
#' @field matches  integer [optional]
#' @field mismatches  integer [optional]
#' @field ns  integer [optional]
#' @field queryEnd  integer [optional]
#' @field queryGapBases  integer [optional]
#' @field queryGapCount  integer [optional]
#' @field querySequence  \link{BioSequenceValueObject} [optional]
#' @field queryStart  integer [optional]
#' @field queryStarts  character [optional]
#' @field repMatches  integer [optional]
#' @field score  numeric [optional]
#' @field strand  character [optional]
#' @field targetChromosomeName  character [optional]
#' @field targetDatabase  character [optional]
#' @field taxon  \link{TaxonValueObject} [optional]
#' @field targetEnd  integer [optional]
#' @field targetGapBases  integer [optional]
#' @field targetGapCount  integer [optional]
#' @field targetStart  integer [optional]
#' @field targetStarts  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
BlatResultValueObject <- R6::R6Class(
  "BlatResultValueObject",
  public = list(
    `id` = NULL,
    `blockCount` = NULL,
    `blockSizes` = NULL,
    `identity` = NULL,
    `matches` = NULL,
    `mismatches` = NULL,
    `ns` = NULL,
    `queryEnd` = NULL,
    `queryGapBases` = NULL,
    `queryGapCount` = NULL,
    `querySequence` = NULL,
    `queryStart` = NULL,
    `queryStarts` = NULL,
    `repMatches` = NULL,
    `score` = NULL,
    `strand` = NULL,
    `targetChromosomeName` = NULL,
    `targetDatabase` = NULL,
    `taxon` = NULL,
    `targetEnd` = NULL,
    `targetGapBases` = NULL,
    `targetGapCount` = NULL,
    `targetStart` = NULL,
    `targetStarts` = NULL,
    #' Initialize a new BlatResultValueObject class.
    #'
    #' @description
    #' Initialize a new BlatResultValueObject class.
    #'
    #' @param id id
    #' @param blockCount blockCount
    #' @param blockSizes blockSizes
    #' @param identity identity
    #' @param matches matches
    #' @param mismatches mismatches
    #' @param ns ns
    #' @param queryEnd queryEnd
    #' @param queryGapBases queryGapBases
    #' @param queryGapCount queryGapCount
    #' @param querySequence querySequence
    #' @param queryStart queryStart
    #' @param queryStarts queryStarts
    #' @param repMatches repMatches
    #' @param score score
    #' @param strand strand
    #' @param targetChromosomeName targetChromosomeName
    #' @param targetDatabase targetDatabase
    #' @param taxon taxon
    #' @param targetEnd targetEnd
    #' @param targetGapBases targetGapBases
    #' @param targetGapCount targetGapCount
    #' @param targetStart targetStart
    #' @param targetStarts targetStarts
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `blockCount` = NULL, `blockSizes` = NULL, `identity` = NULL, `matches` = NULL, `mismatches` = NULL, `ns` = NULL, `queryEnd` = NULL, `queryGapBases` = NULL, `queryGapCount` = NULL, `querySequence` = NULL, `queryStart` = NULL, `queryStarts` = NULL, `repMatches` = NULL, `score` = NULL, `strand` = NULL, `targetChromosomeName` = NULL, `targetDatabase` = NULL, `taxon` = NULL, `targetEnd` = NULL, `targetGapBases` = NULL, `targetGapCount` = NULL, `targetStart` = NULL, `targetStarts` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`blockCount`)) {
        stopifnot(is.numeric(`blockCount`), length(`blockCount`) == 1)
        self$`blockCount` <- `blockCount`
      }
      if (!is.null(`blockSizes`)) {
        stopifnot(is.character(`blockSizes`), length(`blockSizes`) == 1)
        self$`blockSizes` <- `blockSizes`
      }
      if (!is.null(`identity`)) {
        stopifnot(is.numeric(`identity`), length(`identity`) == 1)
        self$`identity` <- `identity`
      }
      if (!is.null(`matches`)) {
        stopifnot(is.numeric(`matches`), length(`matches`) == 1)
        self$`matches` <- `matches`
      }
      if (!is.null(`mismatches`)) {
        stopifnot(is.numeric(`mismatches`), length(`mismatches`) == 1)
        self$`mismatches` <- `mismatches`
      }
      if (!is.null(`ns`)) {
        stopifnot(is.numeric(`ns`), length(`ns`) == 1)
        self$`ns` <- `ns`
      }
      if (!is.null(`queryEnd`)) {
        stopifnot(is.numeric(`queryEnd`), length(`queryEnd`) == 1)
        self$`queryEnd` <- `queryEnd`
      }
      if (!is.null(`queryGapBases`)) {
        stopifnot(is.numeric(`queryGapBases`), length(`queryGapBases`) == 1)
        self$`queryGapBases` <- `queryGapBases`
      }
      if (!is.null(`queryGapCount`)) {
        stopifnot(is.numeric(`queryGapCount`), length(`queryGapCount`) == 1)
        self$`queryGapCount` <- `queryGapCount`
      }
      if (!is.null(`querySequence`)) {
        stopifnot(R6::is.R6(`querySequence`))
        self$`querySequence` <- `querySequence`
      }
      if (!is.null(`queryStart`)) {
        stopifnot(is.numeric(`queryStart`), length(`queryStart`) == 1)
        self$`queryStart` <- `queryStart`
      }
      if (!is.null(`queryStarts`)) {
        stopifnot(is.character(`queryStarts`), length(`queryStarts`) == 1)
        self$`queryStarts` <- `queryStarts`
      }
      if (!is.null(`repMatches`)) {
        stopifnot(is.numeric(`repMatches`), length(`repMatches`) == 1)
        self$`repMatches` <- `repMatches`
      }
      if (!is.null(`score`)) {
        stopifnot(is.numeric(`score`), length(`score`) == 1)
        self$`score` <- `score`
      }
      if (!is.null(`strand`)) {
        stopifnot(is.character(`strand`), length(`strand`) == 1)
        self$`strand` <- `strand`
      }
      if (!is.null(`targetChromosomeName`)) {
        stopifnot(is.character(`targetChromosomeName`), length(`targetChromosomeName`) == 1)
        self$`targetChromosomeName` <- `targetChromosomeName`
      }
      if (!is.null(`targetDatabase`)) {
        stopifnot(is.character(`targetDatabase`), length(`targetDatabase`) == 1)
        self$`targetDatabase` <- `targetDatabase`
      }
      if (!is.null(`taxon`)) {
        stopifnot(R6::is.R6(`taxon`))
        self$`taxon` <- `taxon`
      }
      if (!is.null(`targetEnd`)) {
        stopifnot(is.numeric(`targetEnd`), length(`targetEnd`) == 1)
        self$`targetEnd` <- `targetEnd`
      }
      if (!is.null(`targetGapBases`)) {
        stopifnot(is.numeric(`targetGapBases`), length(`targetGapBases`) == 1)
        self$`targetGapBases` <- `targetGapBases`
      }
      if (!is.null(`targetGapCount`)) {
        stopifnot(is.numeric(`targetGapCount`), length(`targetGapCount`) == 1)
        self$`targetGapCount` <- `targetGapCount`
      }
      if (!is.null(`targetStart`)) {
        stopifnot(is.numeric(`targetStart`), length(`targetStart`) == 1)
        self$`targetStart` <- `targetStart`
      }
      if (!is.null(`targetStarts`)) {
        stopifnot(is.character(`targetStarts`), length(`targetStarts`) == 1)
        self$`targetStarts` <- `targetStarts`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BlatResultValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      BlatResultValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        BlatResultValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`blockCount`)) {
        BlatResultValueObjectObject[["blockCount"]] <-
          self$`blockCount`
      }
      if (!is.null(self$`blockSizes`)) {
        BlatResultValueObjectObject[["blockSizes"]] <-
          self$`blockSizes`
      }
      if (!is.null(self$`identity`)) {
        BlatResultValueObjectObject[["identity"]] <-
          self$`identity`
      }
      if (!is.null(self$`matches`)) {
        BlatResultValueObjectObject[["matches"]] <-
          self$`matches`
      }
      if (!is.null(self$`mismatches`)) {
        BlatResultValueObjectObject[["mismatches"]] <-
          self$`mismatches`
      }
      if (!is.null(self$`ns`)) {
        BlatResultValueObjectObject[["ns"]] <-
          self$`ns`
      }
      if (!is.null(self$`queryEnd`)) {
        BlatResultValueObjectObject[["queryEnd"]] <-
          self$`queryEnd`
      }
      if (!is.null(self$`queryGapBases`)) {
        BlatResultValueObjectObject[["queryGapBases"]] <-
          self$`queryGapBases`
      }
      if (!is.null(self$`queryGapCount`)) {
        BlatResultValueObjectObject[["queryGapCount"]] <-
          self$`queryGapCount`
      }
      if (!is.null(self$`querySequence`)) {
        BlatResultValueObjectObject[["querySequence"]] <-
          self$`querySequence`$toJSON()
      }
      if (!is.null(self$`queryStart`)) {
        BlatResultValueObjectObject[["queryStart"]] <-
          self$`queryStart`
      }
      if (!is.null(self$`queryStarts`)) {
        BlatResultValueObjectObject[["queryStarts"]] <-
          self$`queryStarts`
      }
      if (!is.null(self$`repMatches`)) {
        BlatResultValueObjectObject[["repMatches"]] <-
          self$`repMatches`
      }
      if (!is.null(self$`score`)) {
        BlatResultValueObjectObject[["score"]] <-
          self$`score`
      }
      if (!is.null(self$`strand`)) {
        BlatResultValueObjectObject[["strand"]] <-
          self$`strand`
      }
      if (!is.null(self$`targetChromosomeName`)) {
        BlatResultValueObjectObject[["targetChromosomeName"]] <-
          self$`targetChromosomeName`
      }
      if (!is.null(self$`targetDatabase`)) {
        BlatResultValueObjectObject[["targetDatabase"]] <-
          self$`targetDatabase`
      }
      if (!is.null(self$`taxon`)) {
        BlatResultValueObjectObject[["taxon"]] <-
          self$`taxon`$toJSON()
      }
      if (!is.null(self$`targetEnd`)) {
        BlatResultValueObjectObject[["targetEnd"]] <-
          self$`targetEnd`
      }
      if (!is.null(self$`targetGapBases`)) {
        BlatResultValueObjectObject[["targetGapBases"]] <-
          self$`targetGapBases`
      }
      if (!is.null(self$`targetGapCount`)) {
        BlatResultValueObjectObject[["targetGapCount"]] <-
          self$`targetGapCount`
      }
      if (!is.null(self$`targetStart`)) {
        BlatResultValueObjectObject[["targetStart"]] <-
          self$`targetStart`
      }
      if (!is.null(self$`targetStarts`)) {
        BlatResultValueObjectObject[["targetStarts"]] <-
          self$`targetStarts`
      }

      BlatResultValueObjectObject
    },
    #' Deserialize JSON string into an instance of BlatResultValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of BlatResultValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of BlatResultValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`blockCount`)) {
        self$`blockCount` <- this_object$`blockCount`
      }
      if (!is.null(this_object$`blockSizes`)) {
        self$`blockSizes` <- this_object$`blockSizes`
      }
      if (!is.null(this_object$`identity`)) {
        self$`identity` <- this_object$`identity`
      }
      if (!is.null(this_object$`matches`)) {
        self$`matches` <- this_object$`matches`
      }
      if (!is.null(this_object$`mismatches`)) {
        self$`mismatches` <- this_object$`mismatches`
      }
      if (!is.null(this_object$`ns`)) {
        self$`ns` <- this_object$`ns`
      }
      if (!is.null(this_object$`queryEnd`)) {
        self$`queryEnd` <- this_object$`queryEnd`
      }
      if (!is.null(this_object$`queryGapBases`)) {
        self$`queryGapBases` <- this_object$`queryGapBases`
      }
      if (!is.null(this_object$`queryGapCount`)) {
        self$`queryGapCount` <- this_object$`queryGapCount`
      }
      if (!is.null(this_object$`querySequence`)) {
        querysequence_object <- BioSequenceValueObject$new()
        querysequence_object$fromJSON(jsonlite::toJSON(this_object$querySequence, auto_unbox = TRUE, digits = NA))
        self$`querySequence` <- querysequence_object
      }
      if (!is.null(this_object$`queryStart`)) {
        self$`queryStart` <- this_object$`queryStart`
      }
      if (!is.null(this_object$`queryStarts`)) {
        self$`queryStarts` <- this_object$`queryStarts`
      }
      if (!is.null(this_object$`repMatches`)) {
        self$`repMatches` <- this_object$`repMatches`
      }
      if (!is.null(this_object$`score`)) {
        self$`score` <- this_object$`score`
      }
      if (!is.null(this_object$`strand`)) {
        self$`strand` <- this_object$`strand`
      }
      if (!is.null(this_object$`targetChromosomeName`)) {
        self$`targetChromosomeName` <- this_object$`targetChromosomeName`
      }
      if (!is.null(this_object$`targetDatabase`)) {
        self$`targetDatabase` <- this_object$`targetDatabase`
      }
      if (!is.null(this_object$`taxon`)) {
        taxon_object <- TaxonValueObject$new()
        taxon_object$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
        self$`taxon` <- taxon_object
      }
      if (!is.null(this_object$`targetEnd`)) {
        self$`targetEnd` <- this_object$`targetEnd`
      }
      if (!is.null(this_object$`targetGapBases`)) {
        self$`targetGapBases` <- this_object$`targetGapBases`
      }
      if (!is.null(this_object$`targetGapCount`)) {
        self$`targetGapCount` <- this_object$`targetGapCount`
      }
      if (!is.null(this_object$`targetStart`)) {
        self$`targetStart` <- this_object$`targetStart`
      }
      if (!is.null(this_object$`targetStarts`)) {
        self$`targetStarts` <- this_object$`targetStarts`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BlatResultValueObject in JSON format
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
        if (!is.null(self$`blockCount`)) {
          sprintf(
          '"blockCount":
            %d
                    ',
          self$`blockCount`
          )
        },
        if (!is.null(self$`blockSizes`)) {
          sprintf(
          '"blockSizes":
            "%s"
                    ',
          self$`blockSizes`
          )
        },
        if (!is.null(self$`identity`)) {
          sprintf(
          '"identity":
            %d
                    ',
          self$`identity`
          )
        },
        if (!is.null(self$`matches`)) {
          sprintf(
          '"matches":
            %d
                    ',
          self$`matches`
          )
        },
        if (!is.null(self$`mismatches`)) {
          sprintf(
          '"mismatches":
            %d
                    ',
          self$`mismatches`
          )
        },
        if (!is.null(self$`ns`)) {
          sprintf(
          '"ns":
            %d
                    ',
          self$`ns`
          )
        },
        if (!is.null(self$`queryEnd`)) {
          sprintf(
          '"queryEnd":
            %d
                    ',
          self$`queryEnd`
          )
        },
        if (!is.null(self$`queryGapBases`)) {
          sprintf(
          '"queryGapBases":
            %d
                    ',
          self$`queryGapBases`
          )
        },
        if (!is.null(self$`queryGapCount`)) {
          sprintf(
          '"queryGapCount":
            %d
                    ',
          self$`queryGapCount`
          )
        },
        if (!is.null(self$`querySequence`)) {
          sprintf(
          '"querySequence":
          %s
          ',
          jsonlite::toJSON(self$`querySequence`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`queryStart`)) {
          sprintf(
          '"queryStart":
            %d
                    ',
          self$`queryStart`
          )
        },
        if (!is.null(self$`queryStarts`)) {
          sprintf(
          '"queryStarts":
            "%s"
                    ',
          self$`queryStarts`
          )
        },
        if (!is.null(self$`repMatches`)) {
          sprintf(
          '"repMatches":
            %d
                    ',
          self$`repMatches`
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
        if (!is.null(self$`strand`)) {
          sprintf(
          '"strand":
            "%s"
                    ',
          self$`strand`
          )
        },
        if (!is.null(self$`targetChromosomeName`)) {
          sprintf(
          '"targetChromosomeName":
            "%s"
                    ',
          self$`targetChromosomeName`
          )
        },
        if (!is.null(self$`targetDatabase`)) {
          sprintf(
          '"targetDatabase":
            "%s"
                    ',
          self$`targetDatabase`
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
        if (!is.null(self$`targetEnd`)) {
          sprintf(
          '"targetEnd":
            %d
                    ',
          self$`targetEnd`
          )
        },
        if (!is.null(self$`targetGapBases`)) {
          sprintf(
          '"targetGapBases":
            %d
                    ',
          self$`targetGapBases`
          )
        },
        if (!is.null(self$`targetGapCount`)) {
          sprintf(
          '"targetGapCount":
            %d
                    ',
          self$`targetGapCount`
          )
        },
        if (!is.null(self$`targetStart`)) {
          sprintf(
          '"targetStart":
            %d
                    ',
          self$`targetStart`
          )
        },
        if (!is.null(self$`targetStarts`)) {
          sprintf(
          '"targetStarts":
            "%s"
                    ',
          self$`targetStarts`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of BlatResultValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of BlatResultValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of BlatResultValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`blockCount` <- this_object$`blockCount`
      self$`blockSizes` <- this_object$`blockSizes`
      self$`identity` <- this_object$`identity`
      self$`matches` <- this_object$`matches`
      self$`mismatches` <- this_object$`mismatches`
      self$`ns` <- this_object$`ns`
      self$`queryEnd` <- this_object$`queryEnd`
      self$`queryGapBases` <- this_object$`queryGapBases`
      self$`queryGapCount` <- this_object$`queryGapCount`
      self$`querySequence` <- BioSequenceValueObject$new()$fromJSON(jsonlite::toJSON(this_object$querySequence, auto_unbox = TRUE, digits = NA))
      self$`queryStart` <- this_object$`queryStart`
      self$`queryStarts` <- this_object$`queryStarts`
      self$`repMatches` <- this_object$`repMatches`
      self$`score` <- this_object$`score`
      self$`strand` <- this_object$`strand`
      self$`targetChromosomeName` <- this_object$`targetChromosomeName`
      self$`targetDatabase` <- this_object$`targetDatabase`
      self$`taxon` <- TaxonValueObject$new()$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
      self$`targetEnd` <- this_object$`targetEnd`
      self$`targetGapBases` <- this_object$`targetGapBases`
      self$`targetGapCount` <- this_object$`targetGapCount`
      self$`targetStart` <- this_object$`targetStart`
      self$`targetStarts` <- this_object$`targetStarts`
      self
    },
    #' Validate JSON input with respect to BlatResultValueObject
    #'
    #' @description
    #' Validate JSON input with respect to BlatResultValueObject and throw an exception if invalid
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
    #' @return String representation of BlatResultValueObject
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
BlatResultValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
BlatResultValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
BlatResultValueObject$lock()

