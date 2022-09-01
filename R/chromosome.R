#' Create a new Chromosome
#'
#' @description
#' Chromosome Class
#'
#' @docType class
#' @title Chromosome
#' @description Chromosome Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field assemblyDatabase  \link{ExternalDatabase} [optional]
#' @field sequence  \link{BioSequence} [optional]
#' @field taxon  \link{Taxon} [optional]
#' @field id  integer [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Chromosome <- R6::R6Class(
  "Chromosome",
  public = list(
    `name` = NULL,
    `assemblyDatabase` = NULL,
    `sequence` = NULL,
    `taxon` = NULL,
    `id` = NULL,
    #' Initialize a new Chromosome class.
    #'
    #' @description
    #' Initialize a new Chromosome class.
    #'
    #' @param name name
    #' @param assemblyDatabase assemblyDatabase
    #' @param sequence sequence
    #' @param taxon taxon
    #' @param id id
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `assemblyDatabase` = NULL, `sequence` = NULL, `taxon` = NULL, `id` = NULL, ...
    ) {
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`assemblyDatabase`)) {
        stopifnot(R6::is.R6(`assemblyDatabase`))
        self$`assemblyDatabase` <- `assemblyDatabase`
      }
      if (!is.null(`sequence`)) {
        stopifnot(R6::is.R6(`sequence`))
        self$`sequence` <- `sequence`
      }
      if (!is.null(`taxon`)) {
        stopifnot(R6::is.R6(`taxon`))
        self$`taxon` <- `taxon`
      }
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Chromosome in JSON format
    #' @keywords internal
    toJSON = function() {
      ChromosomeObject <- list()
      if (!is.null(self$`name`)) {
        ChromosomeObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`assemblyDatabase`)) {
        ChromosomeObject[["assemblyDatabase"]] <-
          self$`assemblyDatabase`$toJSON()
      }
      if (!is.null(self$`sequence`)) {
        ChromosomeObject[["sequence"]] <-
          self$`sequence`$toJSON()
      }
      if (!is.null(self$`taxon`)) {
        ChromosomeObject[["taxon"]] <-
          self$`taxon`$toJSON()
      }
      if (!is.null(self$`id`)) {
        ChromosomeObject[["id"]] <-
          self$`id`
      }

      ChromosomeObject
    },
    #' Deserialize JSON string into an instance of Chromosome
    #'
    #' @description
    #' Deserialize JSON string into an instance of Chromosome
    #'
    #' @param input_json the JSON input
    #' @return the instance of Chromosome
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`assemblyDatabase`)) {
        assemblydatabase_object <- ExternalDatabase$new()
        assemblydatabase_object$fromJSON(jsonlite::toJSON(this_object$assemblyDatabase, auto_unbox = TRUE, digits = NA))
        self$`assemblyDatabase` <- assemblydatabase_object
      }
      if (!is.null(this_object$`sequence`)) {
        sequence_object <- BioSequence$new()
        sequence_object$fromJSON(jsonlite::toJSON(this_object$sequence, auto_unbox = TRUE, digits = NA))
        self$`sequence` <- sequence_object
      }
      if (!is.null(this_object$`taxon`)) {
        taxon_object <- Taxon$new()
        taxon_object$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
        self$`taxon` <- taxon_object
      }
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Chromosome in JSON format
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
        if (!is.null(self$`assemblyDatabase`)) {
          sprintf(
          '"assemblyDatabase":
          %s
          ',
          jsonlite::toJSON(self$`assemblyDatabase`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`sequence`)) {
          sprintf(
          '"sequence":
          %s
          ',
          jsonlite::toJSON(self$`sequence`$toJSON(), auto_unbox = TRUE, digits = NA)
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
        if (!is.null(self$`id`)) {
          sprintf(
          '"id":
            %d
                    ',
          self$`id`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Chromosome
    #'
    #' @description
    #' Deserialize JSON string into an instance of Chromosome
    #'
    #' @param input_json the JSON input
    #' @return the instance of Chromosome
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`assemblyDatabase` <- ExternalDatabase$new()$fromJSON(jsonlite::toJSON(this_object$assemblyDatabase, auto_unbox = TRUE, digits = NA))
      self$`sequence` <- BioSequence$new()$fromJSON(jsonlite::toJSON(this_object$sequence, auto_unbox = TRUE, digits = NA))
      self$`taxon` <- Taxon$new()$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
      self$`id` <- this_object$`id`
      self
    },
    #' Validate JSON input with respect to Chromosome
    #'
    #' @description
    #' Validate JSON input with respect to Chromosome and throw an exception if invalid
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
    #' @return String representation of Chromosome
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
Chromosome$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
Chromosome$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
Chromosome$lock()

