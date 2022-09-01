#' Create a new CompositeSequence
#'
#' @description
#' CompositeSequence Class
#'
#' @docType class
#' @title CompositeSequence
#' @description CompositeSequence Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field description  character [optional]
#' @field id  integer [optional]
#' @field biologicalCharacteristic  \link{BioSequence} [optional]
#' @field arrayDesign  \link{ArrayDesign} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
CompositeSequence <- R6::R6Class(
  "CompositeSequence",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `biologicalCharacteristic` = NULL,
    `arrayDesign` = NULL,
    #' Initialize a new CompositeSequence class.
    #'
    #' @description
    #' Initialize a new CompositeSequence class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param biologicalCharacteristic biologicalCharacteristic
    #' @param arrayDesign arrayDesign
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `biologicalCharacteristic` = NULL, `arrayDesign` = NULL, ...
    ) {
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`biologicalCharacteristic`)) {
        stopifnot(R6::is.R6(`biologicalCharacteristic`))
        self$`biologicalCharacteristic` <- `biologicalCharacteristic`
      }
      if (!is.null(`arrayDesign`)) {
        stopifnot(R6::is.R6(`arrayDesign`))
        self$`arrayDesign` <- `arrayDesign`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CompositeSequence in JSON format
    #' @keywords internal
    toJSON = function() {
      CompositeSequenceObject <- list()
      if (!is.null(self$`name`)) {
        CompositeSequenceObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        CompositeSequenceObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        CompositeSequenceObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`biologicalCharacteristic`)) {
        CompositeSequenceObject[["biologicalCharacteristic"]] <-
          self$`biologicalCharacteristic`$toJSON()
      }
      if (!is.null(self$`arrayDesign`)) {
        CompositeSequenceObject[["arrayDesign"]] <-
          self$`arrayDesign`$toJSON()
      }

      CompositeSequenceObject
    },
    #' Deserialize JSON string into an instance of CompositeSequence
    #'
    #' @description
    #' Deserialize JSON string into an instance of CompositeSequence
    #'
    #' @param input_json the JSON input
    #' @return the instance of CompositeSequence
    #' @keywords internal
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`biologicalCharacteristic`)) {
        biologicalcharacteristic_object <- BioSequence$new()
        biologicalcharacteristic_object$fromJSON(jsonlite::toJSON(this_object$biologicalCharacteristic, auto_unbox = TRUE, digits = NA))
        self$`biologicalCharacteristic` <- biologicalcharacteristic_object
      }
      if (!is.null(this_object$`arrayDesign`)) {
        arraydesign_object <- ArrayDesign$new()
        arraydesign_object$fromJSON(jsonlite::toJSON(this_object$arrayDesign, auto_unbox = TRUE, digits = NA))
        self$`arrayDesign` <- arraydesign_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return CompositeSequence in JSON format
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
        if (!is.null(self$`description`)) {
          sprintf(
          '"description":
            "%s"
                    ',
          self$`description`
          )
        },
        if (!is.null(self$`id`)) {
          sprintf(
          '"id":
            %d
                    ',
          self$`id`
          )
        },
        if (!is.null(self$`biologicalCharacteristic`)) {
          sprintf(
          '"biologicalCharacteristic":
          %s
          ',
          jsonlite::toJSON(self$`biologicalCharacteristic`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`arrayDesign`)) {
          sprintf(
          '"arrayDesign":
          %s
          ',
          jsonlite::toJSON(self$`arrayDesign`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of CompositeSequence
    #'
    #' @description
    #' Deserialize JSON string into an instance of CompositeSequence
    #'
    #' @param input_json the JSON input
    #' @return the instance of CompositeSequence
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`biologicalCharacteristic` <- BioSequence$new()$fromJSON(jsonlite::toJSON(this_object$biologicalCharacteristic, auto_unbox = TRUE, digits = NA))
      self$`arrayDesign` <- ArrayDesign$new()$fromJSON(jsonlite::toJSON(this_object$arrayDesign, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to CompositeSequence
    #'
    #' @description
    #' Validate JSON input with respect to CompositeSequence and throw an exception if invalid
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
    #' @return String representation of CompositeSequence
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
CompositeSequence$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
CompositeSequence$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
CompositeSequence$lock()

