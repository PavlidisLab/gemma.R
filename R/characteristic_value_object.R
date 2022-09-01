#' Create a new CharacteristicValueObject
#'
#' @description
#' CharacteristicValueObject Class
#'
#' @docType class
#' @title CharacteristicValueObject
#' @description CharacteristicValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field urlId  character optional
#' @field alreadyPresentInDatabase  character optional
#' @field alreadyPresentOnGene  character optional
#' @field category  character optional
#' @field categoryUri  character optional
#' @field child  character optional
#' @field numTimesUsed  integer optional
#' @field ontologyUsed  character optional
#' @field privateGeneCount  integer optional
#' @field publicGeneCount  integer optional
#' @field root  character optional
#' @field taxon  character optional
#' @field value  character optional
#' @field valueUri  character optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
CharacteristicValueObject <- R6::R6Class(
  "CharacteristicValueObject",
  public = list(
    `id` = NULL,
    `urlId` = NULL,
    `alreadyPresentInDatabase` = NULL,
    `alreadyPresentOnGene` = NULL,
    `category` = NULL,
    `categoryUri` = NULL,
    `child` = NULL,
    `numTimesUsed` = NULL,
    `ontologyUsed` = NULL,
    `privateGeneCount` = NULL,
    `publicGeneCount` = NULL,
    `root` = NULL,
    `taxon` = NULL,
    `value` = NULL,
    `valueUri` = NULL,
    #' Initialize a new CharacteristicValueObject class.
    #'
    #' @description
    #' Initialize a new CharacteristicValueObject class.
    #'
    #' @param id id
    #' @param urlId urlId
    #' @param alreadyPresentInDatabase alreadyPresentInDatabase
    #' @param alreadyPresentOnGene alreadyPresentOnGene
    #' @param category category
    #' @param categoryUri categoryUri
    #' @param child child
    #' @param numTimesUsed numTimesUsed
    #' @param ontologyUsed ontologyUsed
    #' @param privateGeneCount privateGeneCount
    #' @param publicGeneCount publicGeneCount
    #' @param root root
    #' @param taxon taxon
    #' @param value value
    #' @param valueUri valueUri
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `urlId` = NULL, `alreadyPresentInDatabase` = NULL, `alreadyPresentOnGene` = NULL, `category` = NULL, `categoryUri` = NULL, `child` = NULL, `numTimesUsed` = NULL, `ontologyUsed` = NULL, `privateGeneCount` = NULL, `publicGeneCount` = NULL, `root` = NULL, `taxon` = NULL, `value` = NULL, `valueUri` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`urlId`)) {
        stopifnot(is.character(`urlId`), length(`urlId`) == 1)
        self$`urlId` <- `urlId`
      }
      if (!is.null(`alreadyPresentInDatabase`)) {
        stopifnot(is.logical(`alreadyPresentInDatabase`), length(`alreadyPresentInDatabase`) == 1)
        self$`alreadyPresentInDatabase` <- `alreadyPresentInDatabase`
      }
      if (!is.null(`alreadyPresentOnGene`)) {
        stopifnot(is.logical(`alreadyPresentOnGene`), length(`alreadyPresentOnGene`) == 1)
        self$`alreadyPresentOnGene` <- `alreadyPresentOnGene`
      }
      if (!is.null(`category`)) {
        stopifnot(is.character(`category`), length(`category`) == 1)
        self$`category` <- `category`
      }
      if (!is.null(`categoryUri`)) {
        stopifnot(is.character(`categoryUri`), length(`categoryUri`) == 1)
        self$`categoryUri` <- `categoryUri`
      }
      if (!is.null(`child`)) {
        stopifnot(is.logical(`child`), length(`child`) == 1)
        self$`child` <- `child`
      }
      if (!is.null(`numTimesUsed`)) {
        stopifnot(is.numeric(`numTimesUsed`), length(`numTimesUsed`) == 1)
        self$`numTimesUsed` <- `numTimesUsed`
      }
      if (!is.null(`ontologyUsed`)) {
        stopifnot(is.character(`ontologyUsed`), length(`ontologyUsed`) == 1)
        self$`ontologyUsed` <- `ontologyUsed`
      }
      if (!is.null(`privateGeneCount`)) {
        stopifnot(is.numeric(`privateGeneCount`), length(`privateGeneCount`) == 1)
        self$`privateGeneCount` <- `privateGeneCount`
      }
      if (!is.null(`publicGeneCount`)) {
        stopifnot(is.numeric(`publicGeneCount`), length(`publicGeneCount`) == 1)
        self$`publicGeneCount` <- `publicGeneCount`
      }
      if (!is.null(`root`)) {
        stopifnot(is.logical(`root`), length(`root`) == 1)
        self$`root` <- `root`
      }
      if (!is.null(`taxon`)) {
        stopifnot(is.character(`taxon`), length(`taxon`) == 1)
        self$`taxon` <- `taxon`
      }
      if (!is.null(`value`)) {
        stopifnot(is.character(`value`), length(`value`) == 1)
        self$`value` <- `value`
      }
      if (!is.null(`valueUri`)) {
        stopifnot(is.character(`valueUri`), length(`valueUri`) == 1)
        self$`valueUri` <- `valueUri`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CharacteristicValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      CharacteristicValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        CharacteristicValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`urlId`)) {
        CharacteristicValueObjectObject[["urlId"]] <-
          self$`urlId`
      }
      if (!is.null(self$`alreadyPresentInDatabase`)) {
        CharacteristicValueObjectObject[["alreadyPresentInDatabase"]] <-
          self$`alreadyPresentInDatabase`
      }
      if (!is.null(self$`alreadyPresentOnGene`)) {
        CharacteristicValueObjectObject[["alreadyPresentOnGene"]] <-
          self$`alreadyPresentOnGene`
      }
      if (!is.null(self$`category`)) {
        CharacteristicValueObjectObject[["category"]] <-
          self$`category`
      }
      if (!is.null(self$`categoryUri`)) {
        CharacteristicValueObjectObject[["categoryUri"]] <-
          self$`categoryUri`
      }
      if (!is.null(self$`child`)) {
        CharacteristicValueObjectObject[["child"]] <-
          self$`child`
      }
      if (!is.null(self$`numTimesUsed`)) {
        CharacteristicValueObjectObject[["numTimesUsed"]] <-
          self$`numTimesUsed`
      }
      if (!is.null(self$`ontologyUsed`)) {
        CharacteristicValueObjectObject[["ontologyUsed"]] <-
          self$`ontologyUsed`
      }
      if (!is.null(self$`privateGeneCount`)) {
        CharacteristicValueObjectObject[["privateGeneCount"]] <-
          self$`privateGeneCount`
      }
      if (!is.null(self$`publicGeneCount`)) {
        CharacteristicValueObjectObject[["publicGeneCount"]] <-
          self$`publicGeneCount`
      }
      if (!is.null(self$`root`)) {
        CharacteristicValueObjectObject[["root"]] <-
          self$`root`
      }
      if (!is.null(self$`taxon`)) {
        CharacteristicValueObjectObject[["taxon"]] <-
          self$`taxon`
      }
      if (!is.null(self$`value`)) {
        CharacteristicValueObjectObject[["value"]] <-
          self$`value`
      }
      if (!is.null(self$`valueUri`)) {
        CharacteristicValueObjectObject[["valueUri"]] <-
          self$`valueUri`
      }

      CharacteristicValueObjectObject
    },
    #' Deserialize JSON string into an instance of CharacteristicValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of CharacteristicValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of CharacteristicValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`urlId`)) {
        self$`urlId` <- this_object$`urlId`
      }
      if (!is.null(this_object$`alreadyPresentInDatabase`)) {
        self$`alreadyPresentInDatabase` <- this_object$`alreadyPresentInDatabase`
      }
      if (!is.null(this_object$`alreadyPresentOnGene`)) {
        self$`alreadyPresentOnGene` <- this_object$`alreadyPresentOnGene`
      }
      if (!is.null(this_object$`category`)) {
        self$`category` <- this_object$`category`
      }
      if (!is.null(this_object$`categoryUri`)) {
        self$`categoryUri` <- this_object$`categoryUri`
      }
      if (!is.null(this_object$`child`)) {
        self$`child` <- this_object$`child`
      }
      if (!is.null(this_object$`numTimesUsed`)) {
        self$`numTimesUsed` <- this_object$`numTimesUsed`
      }
      if (!is.null(this_object$`ontologyUsed`)) {
        self$`ontologyUsed` <- this_object$`ontologyUsed`
      }
      if (!is.null(this_object$`privateGeneCount`)) {
        self$`privateGeneCount` <- this_object$`privateGeneCount`
      }
      if (!is.null(this_object$`publicGeneCount`)) {
        self$`publicGeneCount` <- this_object$`publicGeneCount`
      }
      if (!is.null(this_object$`root`)) {
        self$`root` <- this_object$`root`
      }
      if (!is.null(this_object$`taxon`)) {
        self$`taxon` <- this_object$`taxon`
      }
      if (!is.null(this_object$`value`)) {
        self$`value` <- this_object$`value`
      }
      if (!is.null(this_object$`valueUri`)) {
        self$`valueUri` <- this_object$`valueUri`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CharacteristicValueObject in JSON format
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
        if (!is.null(self$`urlId`)) {
          sprintf(
          '"urlId":
            "%s"
                    ',
          self$`urlId`
          )
        },
        if (!is.null(self$`alreadyPresentInDatabase`)) {
          sprintf(
          '"alreadyPresentInDatabase":
            %s
                    ',
          tolower(self$`alreadyPresentInDatabase`)
          )
        },
        if (!is.null(self$`alreadyPresentOnGene`)) {
          sprintf(
          '"alreadyPresentOnGene":
            %s
                    ',
          tolower(self$`alreadyPresentOnGene`)
          )
        },
        if (!is.null(self$`category`)) {
          sprintf(
          '"category":
            "%s"
                    ',
          self$`category`
          )
        },
        if (!is.null(self$`categoryUri`)) {
          sprintf(
          '"categoryUri":
            "%s"
                    ',
          self$`categoryUri`
          )
        },
        if (!is.null(self$`child`)) {
          sprintf(
          '"child":
            %s
                    ',
          tolower(self$`child`)
          )
        },
        if (!is.null(self$`numTimesUsed`)) {
          sprintf(
          '"numTimesUsed":
            %d
                    ',
          self$`numTimesUsed`
          )
        },
        if (!is.null(self$`ontologyUsed`)) {
          sprintf(
          '"ontologyUsed":
            "%s"
                    ',
          self$`ontologyUsed`
          )
        },
        if (!is.null(self$`privateGeneCount`)) {
          sprintf(
          '"privateGeneCount":
            %d
                    ',
          self$`privateGeneCount`
          )
        },
        if (!is.null(self$`publicGeneCount`)) {
          sprintf(
          '"publicGeneCount":
            %d
                    ',
          self$`publicGeneCount`
          )
        },
        if (!is.null(self$`root`)) {
          sprintf(
          '"root":
            %s
                    ',
          tolower(self$`root`)
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
        if (!is.null(self$`value`)) {
          sprintf(
          '"value":
            "%s"
                    ',
          self$`value`
          )
        },
        if (!is.null(self$`valueUri`)) {
          sprintf(
          '"valueUri":
            "%s"
                    ',
          self$`valueUri`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of CharacteristicValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of CharacteristicValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of CharacteristicValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`urlId` <- this_object$`urlId`
      self$`alreadyPresentInDatabase` <- this_object$`alreadyPresentInDatabase`
      self$`alreadyPresentOnGene` <- this_object$`alreadyPresentOnGene`
      self$`category` <- this_object$`category`
      self$`categoryUri` <- this_object$`categoryUri`
      self$`child` <- this_object$`child`
      self$`numTimesUsed` <- this_object$`numTimesUsed`
      self$`ontologyUsed` <- this_object$`ontologyUsed`
      self$`privateGeneCount` <- this_object$`privateGeneCount`
      self$`publicGeneCount` <- this_object$`publicGeneCount`
      self$`root` <- this_object$`root`
      self$`taxon` <- this_object$`taxon`
      self$`value` <- this_object$`value`
      self$`valueUri` <- this_object$`valueUri`
      self
    },
    #' Validate JSON input with respect to CharacteristicValueObject
    #'
    #' @description
    #' Validate JSON input with respect to CharacteristicValueObject and throw an exception if invalid
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
    #' @return String representation of CharacteristicValueObject
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
CharacteristicValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
CharacteristicValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
CharacteristicValueObject$lock()

