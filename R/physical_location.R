#' Create a new PhysicalLocation
#'
#' @description
#' PhysicalLocation Class
#'
#' @docType class
#' @title PhysicalLocation
#' @description PhysicalLocation Class
#' @format An \code{R6Class} generator object
#' @field id  integer optional
#' @field chromosome  \link{Chromosome} optional
#' @field nucleotide  integer optional
#' @field nucleotideLength  integer optional
#' @field strand  character optional
#' @field bin  integer optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
PhysicalLocation <- R6::R6Class(
  "PhysicalLocation",
  public = list(
    `id` = NULL,
    `chromosome` = NULL,
    `nucleotide` = NULL,
    `nucleotideLength` = NULL,
    `strand` = NULL,
    `bin` = NULL,
    #' Initialize a new PhysicalLocation class.
    #'
    #' @description
    #' Initialize a new PhysicalLocation class.
    #'
    #' @param id id
    #' @param chromosome chromosome
    #' @param nucleotide nucleotide
    #' @param nucleotideLength nucleotideLength
    #' @param strand strand
    #' @param bin bin
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `chromosome` = NULL, `nucleotide` = NULL, `nucleotideLength` = NULL, `strand` = NULL, `bin` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`chromosome`)) {
        stopifnot(R6::is.R6(`chromosome`))
        self$`chromosome` <- `chromosome`
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
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PhysicalLocation in JSON format
    #' @keywords internal
    toJSON = function() {
      PhysicalLocationObject <- list()
      if (!is.null(self$`id`)) {
        PhysicalLocationObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`chromosome`)) {
        PhysicalLocationObject[["chromosome"]] <-
          self$`chromosome`$toJSON()
      }
      if (!is.null(self$`nucleotide`)) {
        PhysicalLocationObject[["nucleotide"]] <-
          self$`nucleotide`
      }
      if (!is.null(self$`nucleotideLength`)) {
        PhysicalLocationObject[["nucleotideLength"]] <-
          self$`nucleotideLength`
      }
      if (!is.null(self$`strand`)) {
        PhysicalLocationObject[["strand"]] <-
          self$`strand`
      }
      if (!is.null(self$`bin`)) {
        PhysicalLocationObject[["bin"]] <-
          self$`bin`
      }

      PhysicalLocationObject
    },
    #' Deserialize JSON string into an instance of PhysicalLocation
    #'
    #' @description
    #' Deserialize JSON string into an instance of PhysicalLocation
    #'
    #' @param input_json the JSON input
    #' @return the instance of PhysicalLocation
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`chromosome`)) {
        chromosome_object <- Chromosome$new()
        chromosome_object$fromJSON(jsonlite::toJSON(this_object$chromosome, auto_unbox = TRUE, digits = NA))
        self$`chromosome` <- chromosome_object
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
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return PhysicalLocation in JSON format
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
        if (!is.null(self$`chromosome`)) {
          sprintf(
          '"chromosome":
          %s
          ',
          jsonlite::toJSON(self$`chromosome`$toJSON(), auto_unbox = TRUE, digits = NA)
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of PhysicalLocation
    #'
    #' @description
    #' Deserialize JSON string into an instance of PhysicalLocation
    #'
    #' @param input_json the JSON input
    #' @return the instance of PhysicalLocation
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`chromosome` <- Chromosome$new()$fromJSON(jsonlite::toJSON(this_object$chromosome, auto_unbox = TRUE, digits = NA))
      self$`nucleotide` <- this_object$`nucleotide`
      self$`nucleotideLength` <- this_object$`nucleotideLength`
      self$`strand` <- this_object$`strand`
      self$`bin` <- this_object$`bin`
      self
    },
    #' Validate JSON input with respect to PhysicalLocation
    #'
    #' @description
    #' Validate JSON input with respect to PhysicalLocation and throw an exception if invalid
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
    #' @return String representation of PhysicalLocation
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
PhysicalLocation$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
PhysicalLocation$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
PhysicalLocation$lock()

