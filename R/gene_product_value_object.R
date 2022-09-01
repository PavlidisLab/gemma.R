#' Create a new GeneProductValueObject
#'
#' @description
#' GeneProductValueObject Class
#'
#' @docType class
#' @title GeneProductValueObject
#' @description GeneProductValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field ncbiId  character optional
#' @field name  character optional
#' @field geneId  integer optional
#' @field chromosome  character optional
#' @field strand  character optional
#' @field nucleotideStart  integer optional
#' @field nucleotideEnd  integer optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
GeneProductValueObject <- R6::R6Class(
  "GeneProductValueObject",
  public = list(
    `id` = NULL,
    `ncbiId` = NULL,
    `name` = NULL,
    `geneId` = NULL,
    `chromosome` = NULL,
    `strand` = NULL,
    `nucleotideStart` = NULL,
    `nucleotideEnd` = NULL,
    #' Initialize a new GeneProductValueObject class.
    #'
    #' @description
    #' Initialize a new GeneProductValueObject class.
    #'
    #' @param id id
    #' @param ncbiId ncbiId
    #' @param name name
    #' @param geneId geneId
    #' @param chromosome chromosome
    #' @param strand strand
    #' @param nucleotideStart nucleotideStart
    #' @param nucleotideEnd nucleotideEnd
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `ncbiId` = NULL, `name` = NULL, `geneId` = NULL, `chromosome` = NULL, `strand` = NULL, `nucleotideStart` = NULL, `nucleotideEnd` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`ncbiId`)) {
        stopifnot(is.character(`ncbiId`), length(`ncbiId`) == 1)
        self$`ncbiId` <- `ncbiId`
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`geneId`)) {
        stopifnot(is.numeric(`geneId`), length(`geneId`) == 1)
        self$`geneId` <- `geneId`
      }
      if (!is.null(`chromosome`)) {
        stopifnot(is.character(`chromosome`), length(`chromosome`) == 1)
        self$`chromosome` <- `chromosome`
      }
      if (!is.null(`strand`)) {
        stopifnot(is.character(`strand`), length(`strand`) == 1)
        self$`strand` <- `strand`
      }
      if (!is.null(`nucleotideStart`)) {
        stopifnot(is.numeric(`nucleotideStart`), length(`nucleotideStart`) == 1)
        self$`nucleotideStart` <- `nucleotideStart`
      }
      if (!is.null(`nucleotideEnd`)) {
        stopifnot(is.numeric(`nucleotideEnd`), length(`nucleotideEnd`) == 1)
        self$`nucleotideEnd` <- `nucleotideEnd`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneProductValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      GeneProductValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        GeneProductValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`ncbiId`)) {
        GeneProductValueObjectObject[["ncbiId"]] <-
          self$`ncbiId`
      }
      if (!is.null(self$`name`)) {
        GeneProductValueObjectObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`geneId`)) {
        GeneProductValueObjectObject[["geneId"]] <-
          self$`geneId`
      }
      if (!is.null(self$`chromosome`)) {
        GeneProductValueObjectObject[["chromosome"]] <-
          self$`chromosome`
      }
      if (!is.null(self$`strand`)) {
        GeneProductValueObjectObject[["strand"]] <-
          self$`strand`
      }
      if (!is.null(self$`nucleotideStart`)) {
        GeneProductValueObjectObject[["nucleotideStart"]] <-
          self$`nucleotideStart`
      }
      if (!is.null(self$`nucleotideEnd`)) {
        GeneProductValueObjectObject[["nucleotideEnd"]] <-
          self$`nucleotideEnd`
      }

      GeneProductValueObjectObject
    },
    #' Deserialize JSON string into an instance of GeneProductValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneProductValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneProductValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`ncbiId`)) {
        self$`ncbiId` <- this_object$`ncbiId`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`geneId`)) {
        self$`geneId` <- this_object$`geneId`
      }
      if (!is.null(this_object$`chromosome`)) {
        self$`chromosome` <- this_object$`chromosome`
      }
      if (!is.null(this_object$`strand`)) {
        self$`strand` <- this_object$`strand`
      }
      if (!is.null(this_object$`nucleotideStart`)) {
        self$`nucleotideStart` <- this_object$`nucleotideStart`
      }
      if (!is.null(this_object$`nucleotideEnd`)) {
        self$`nucleotideEnd` <- this_object$`nucleotideEnd`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneProductValueObject in JSON format
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
        if (!is.null(self$`ncbiId`)) {
          sprintf(
          '"ncbiId":
            "%s"
                    ',
          self$`ncbiId`
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
        if (!is.null(self$`geneId`)) {
          sprintf(
          '"geneId":
            %d
                    ',
          self$`geneId`
          )
        },
        if (!is.null(self$`chromosome`)) {
          sprintf(
          '"chromosome":
            "%s"
                    ',
          self$`chromosome`
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
        if (!is.null(self$`nucleotideStart`)) {
          sprintf(
          '"nucleotideStart":
            %d
                    ',
          self$`nucleotideStart`
          )
        },
        if (!is.null(self$`nucleotideEnd`)) {
          sprintf(
          '"nucleotideEnd":
            %d
                    ',
          self$`nucleotideEnd`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of GeneProductValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneProductValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneProductValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`ncbiId` <- this_object$`ncbiId`
      self$`name` <- this_object$`name`
      self$`geneId` <- this_object$`geneId`
      self$`chromosome` <- this_object$`chromosome`
      self$`strand` <- this_object$`strand`
      self$`nucleotideStart` <- this_object$`nucleotideStart`
      self$`nucleotideEnd` <- this_object$`nucleotideEnd`
      self
    },
    #' Validate JSON input with respect to GeneProductValueObject
    #'
    #' @description
    #' Validate JSON input with respect to GeneProductValueObject and throw an exception if invalid
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
    #' @return String representation of GeneProductValueObject
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
GeneProductValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
GeneProductValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
GeneProductValueObject$lock()

