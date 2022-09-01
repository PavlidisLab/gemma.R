#' Create a new PhysicalLocationValueObject
#'
#' @description
#' PhysicalLocationValueObject Class
#'
#' @docType class
#' @title PhysicalLocationValueObject
#' @description PhysicalLocationValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field nucleotide  integer optional
#' @field nucleotideLength  integer optional
#' @field strand  character optional
#' @field bin  integer optional
#' @field chromosome  character optional
#' @field taxon  \link{TaxonValueObject} optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
PhysicalLocationValueObject <- R6::R6Class(
  "PhysicalLocationValueObject",
  public = list(
    `id` = NULL,
    `nucleotide` = NULL,
    `nucleotideLength` = NULL,
    `strand` = NULL,
    `bin` = NULL,
    `chromosome` = NULL,
    `taxon` = NULL,
    #' Initialize a new PhysicalLocationValueObject class.
    #'
    #' @description
    #' Initialize a new PhysicalLocationValueObject class.
    #'
    #' @param id id
    #' @param nucleotide nucleotide
    #' @param nucleotideLength nucleotideLength
    #' @param strand strand
    #' @param bin bin
    #' @param chromosome chromosome
    #' @param taxon taxon
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `nucleotide` = NULL, `nucleotideLength` = NULL, `strand` = NULL, `bin` = NULL, `chromosome` = NULL, `taxon` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`nucleotide`)) {
        stopifnot(is.numeric(`nucleotide`), length(`nucleotide`) == 1)
        self$`nucleotide` <- `nucleotide`
      }
      if (!is.null(`nucleotideLength`)) {
        stopifnot(is.numeric(`nucleotideLength`), length(`nucleotideLength`) == 1)
        self$`nucleotideLength` <- `nucleotideLength`
      }
      if (!is.null(`strand`)) {
        stopifnot(is.character(`strand`), length(`strand`) == 1)
        self$`strand` <- `strand`
      }
      if (!is.null(`bin`)) {
        stopifnot(is.numeric(`bin`), length(`bin`) == 1)
        self$`bin` <- `bin`
      }
      if (!is.null(`chromosome`)) {
        stopifnot(is.character(`chromosome`), length(`chromosome`) == 1)
        self$`chromosome` <- `chromosome`
      }
      if (!is.null(`taxon`)) {
        stopifnot(R6::is.R6(`taxon`))
        self$`taxon` <- `taxon`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PhysicalLocationValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      PhysicalLocationValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        PhysicalLocationValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`nucleotide`)) {
        PhysicalLocationValueObjectObject[["nucleotide"]] <-
          self$`nucleotide`
      }
      if (!is.null(self$`nucleotideLength`)) {
        PhysicalLocationValueObjectObject[["nucleotideLength"]] <-
          self$`nucleotideLength`
      }
      if (!is.null(self$`strand`)) {
        PhysicalLocationValueObjectObject[["strand"]] <-
          self$`strand`
      }
      if (!is.null(self$`bin`)) {
        PhysicalLocationValueObjectObject[["bin"]] <-
          self$`bin`
      }
      if (!is.null(self$`chromosome`)) {
        PhysicalLocationValueObjectObject[["chromosome"]] <-
          self$`chromosome`
      }
      if (!is.null(self$`taxon`)) {
        PhysicalLocationValueObjectObject[["taxon"]] <-
          self$`taxon`$toJSON()
      }

      PhysicalLocationValueObjectObject
    },
    #' Deserialize JSON string into an instance of PhysicalLocationValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of PhysicalLocationValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of PhysicalLocationValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`nucleotide`)) {
        self$`nucleotide` <- this_object$`nucleotide`
      }
      if (!is.null(this_object$`nucleotideLength`)) {
        self$`nucleotideLength` <- this_object$`nucleotideLength`
      }
      if (!is.null(this_object$`strand`)) {
        self$`strand` <- this_object$`strand`
      }
      if (!is.null(this_object$`bin`)) {
        self$`bin` <- this_object$`bin`
      }
      if (!is.null(this_object$`chromosome`)) {
        self$`chromosome` <- this_object$`chromosome`
      }
      if (!is.null(this_object$`taxon`)) {
        taxon_object <- TaxonValueObject$new()
        taxon_object$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
        self$`taxon` <- taxon_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PhysicalLocationValueObject in JSON format
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
        if (!is.null(self$`nucleotide`)) {
          sprintf(
          '"nucleotide":
            %d
                    ',
          self$`nucleotide`
          )
        },
        if (!is.null(self$`nucleotideLength`)) {
          sprintf(
          '"nucleotideLength":
            %d
                    ',
          self$`nucleotideLength`
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
        if (!is.null(self$`bin`)) {
          sprintf(
          '"bin":
            %d
                    ',
          self$`bin`
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
        if (!is.null(self$`taxon`)) {
          sprintf(
          '"taxon":
          %s
          ',
          jsonlite::toJSON(self$`taxon`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of PhysicalLocationValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of PhysicalLocationValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of PhysicalLocationValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`nucleotide` <- this_object$`nucleotide`
      self$`nucleotideLength` <- this_object$`nucleotideLength`
      self$`strand` <- this_object$`strand`
      self$`bin` <- this_object$`bin`
      self$`chromosome` <- this_object$`chromosome`
      self$`taxon` <- TaxonValueObject$new()$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to PhysicalLocationValueObject
    #'
    #' @description
    #' Validate JSON input with respect to PhysicalLocationValueObject and throw an exception if invalid
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
    #' @return String representation of PhysicalLocationValueObject
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
PhysicalLocationValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
PhysicalLocationValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
PhysicalLocationValueObject$lock()

