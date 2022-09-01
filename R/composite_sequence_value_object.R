#' Create a new CompositeSequenceValueObject
#'
#' @description
#' CompositeSequenceValueObject Class
#'
#' @docType class
#' @title CompositeSequenceValueObject
#' @description CompositeSequenceValueObject Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field name  character [optional]
#' @field description  character [optional]
#' @field arrayDesign  \link{ArrayDesignValueObject} [optional]
#' @field geneMappingSummaries  list(\link{GeneMappingSummary}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
CompositeSequenceValueObject <- R6::R6Class(
  "CompositeSequenceValueObject",
  public = list(
    `id` = NULL,
    `name` = NULL,
    `description` = NULL,
    `arrayDesign` = NULL,
    `geneMappingSummaries` = NULL,
    #' Initialize a new CompositeSequenceValueObject class.
    #'
    #' @description
    #' Initialize a new CompositeSequenceValueObject class.
    #'
    #' @param id id
    #' @param name name
    #' @param description description
    #' @param arrayDesign arrayDesign
    #' @param geneMappingSummaries geneMappingSummaries
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `id` = NULL, `name` = NULL, `description` = NULL, `arrayDesign` = NULL, `geneMappingSummaries` = NULL, ...
    ) {
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`arrayDesign`)) {
        stopifnot(R6::is.R6(`arrayDesign`))
        self$`arrayDesign` <- `arrayDesign`
      }
      if (!is.null(`geneMappingSummaries`)) {
        stopifnot(is.vector(`geneMappingSummaries`), length(`geneMappingSummaries`) != 0)
        sapply(`geneMappingSummaries`, function(x) stopifnot(R6::is.R6(x)))
        self$`geneMappingSummaries` <- `geneMappingSummaries`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CompositeSequenceValueObject in JSON format
    #' @keywords internal
    toJSON = function() {
      CompositeSequenceValueObjectObject <- list()
      if (!is.null(self$`id`)) {
        CompositeSequenceValueObjectObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`name`)) {
        CompositeSequenceValueObjectObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        CompositeSequenceValueObjectObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`arrayDesign`)) {
        CompositeSequenceValueObjectObject[["arrayDesign"]] <-
          self$`arrayDesign`$toJSON()
      }
      if (!is.null(self$`geneMappingSummaries`)) {
        CompositeSequenceValueObjectObject[["geneMappingSummaries"]] <-
          lapply(self$`geneMappingSummaries`, function(x) x$toJSON())
      }

      CompositeSequenceValueObjectObject
    },
    #' Deserialize JSON string into an instance of CompositeSequenceValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of CompositeSequenceValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of CompositeSequenceValueObject
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`arrayDesign`)) {
        arraydesign_object <- ArrayDesignValueObject$new()
        arraydesign_object$fromJSON(jsonlite::toJSON(this_object$arrayDesign, auto_unbox = TRUE, digits = NA))
        self$`arrayDesign` <- arraydesign_object
      }
      if (!is.null(this_object$`geneMappingSummaries`)) {
        self$`geneMappingSummaries` <- ApiClient$new()$deserializeObj(this_object$`geneMappingSummaries`, "array[GeneMappingSummary]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CompositeSequenceValueObject in JSON format
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
        if (!is.null(self$`name`)) {
          sprintf(
          '"name":
            "%s"
                    ',
          self$`name`
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
        if (!is.null(self$`arrayDesign`)) {
          sprintf(
          '"arrayDesign":
          %s
          ',
          jsonlite::toJSON(self$`arrayDesign`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`geneMappingSummaries`)) {
          sprintf(
          '"geneMappingSummaries":
          [%s]
',
          paste(sapply(self$`geneMappingSummaries`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of CompositeSequenceValueObject
    #'
    #' @description
    #' Deserialize JSON string into an instance of CompositeSequenceValueObject
    #'
    #' @param input_json the JSON input
    #' @return the instance of CompositeSequenceValueObject
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`arrayDesign` <- ArrayDesignValueObject$new()$fromJSON(jsonlite::toJSON(this_object$arrayDesign, auto_unbox = TRUE, digits = NA))
      self$`geneMappingSummaries` <- ApiClient$new()$deserializeObj(this_object$`geneMappingSummaries`, "array[GeneMappingSummary]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to CompositeSequenceValueObject
    #'
    #' @description
    #' Validate JSON input with respect to CompositeSequenceValueObject and throw an exception if invalid
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
    #' @return String representation of CompositeSequenceValueObject
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
CompositeSequenceValueObject$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
CompositeSequenceValueObject$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
CompositeSequenceValueObject$lock()

