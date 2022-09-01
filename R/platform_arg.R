#' @docType class
#' @title PlatformArg
#'
#' @description PlatformArg Class
#'
#' @format An \code{R6Class} generator object
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
PlatformArg <- R6::R6Class(
  "PlatformArg",
  public = list(
    #' @field actual_instance the object stored in this instance.
    actual_instance = NULL,
    #' @field actual_type the type of the object stored in this instance.
    actual_type = NULL,
    #' @field one_of  a list of types defined in the oneOf schema.
    one_of = list("character", "integer"),
    #' Initialize a new PlatformArg.
    #'
    #' @description
    #' Initialize a new PlatformArg.
    #'
    #' @param instance an instance of the object defined in the oneOf schemas: "character", "integer"
    #' @keywords internal
    initialize = function(instance = NULL) {
      if (is.null(instance)) {
        # do nothing
      } else if (get(class(instance)[[1]], pos = -1)$classname ==  "character") {
        self$actual_instance <- instance
        self$actual_type <- "character"
      } else if (get(class(instance)[[1]], pos = -1)$classname ==  "integer") {
        self$actual_instance <- instance
        self$actual_type <- "integer"
      } else {
        stop(paste("Failed to initialize PlatformArg with oneOf schemas character, integer. Provided class name: ",
                   get(class(instance)[[1]], pos = -1)$classname))
      }
    },
    #' Deserialize JSON string into an instance of PlatformArg.
    #'
    #' @description
    #' Deserialize JSON string into an instance of PlatformArg.
    #' An alias to the method `fromJSON` .
    #'
    #' @param input The input JSON.
    #' @return An instance of PlatformArg.
    #' @keywords internal
    fromJSONString = function(input) {
      self$fromJSON(input)
    },
    #' Deserialize JSON string into an instance of PlatformArg.
    #'
    #' @description
    #' Deserialize JSON string into an instance of PlatformArg.
    #'
    #' @param input The input JSON.
    #' @return An instance of PlatformArg.
    #' @keywords internal
    fromJSON = function(input) {
      matched <- 0 # match counter
      matched_schemas <- list() #names of matched schemas
      error_messages <- list()
      instance <- NULL

      integer_result <- tryCatch({
          instance <- jsonlite::fromJSON(input, simplifyVector = FALSE)
          if (typeof(instance) != "integer") {
            stop(sprintf("Data type doesn't match. Expected: %s. Actual: %s.", "integer", typeof(instance)))
          }
          instance_type <- "integer"
          matched_schemas <- append(matched_schemas, "integer")
          matched <- matched + 1
        },
        error = function(err) err
      )

      if (!is.null(integer_result["error"])) {
        error_messages <- append(error_messages, integer_result["message"])
      }

      character_result <- tryCatch({
          instance <- jsonlite::fromJSON(input, simplifyVector = FALSE)
          if (typeof(instance) != "character") {
            stop(sprintf("Data type doesn't match. Expected: %s. Actual: %s.", "character", typeof(instance)))
          }
          instance_type <- "character"
          matched_schemas <- append(matched_schemas, "character")
          matched <- matched + 1
        },
        error = function(err) err
      )

      if (!is.null(character_result["error"])) {
        error_messages <- append(error_messages, character_result["message"])
      }

      if (matched == 1) {
        # successfully match exactly 1 schema specified in oneOf
        self$actual_instance <- instance
        self$actual_type <- instance_type
      } else if (matched > 1) {
        # more than 1 match
        stop("Multiple matches found when deserializing the payload into PlatformArg with oneOf schemas character, integer.")
      } else {
        # no match
        stop(paste("No match found when deserializing the payload into PlatformArg with oneOf schemas character, integer. Details: ",
                   paste(error_messages, collapse = ", ")))
      }

      self
    },
    #' Serialize PlatformArg to JSON string.
    #'
    #' @description
    #' Serialize PlatformArg to JSON string.
    #'
    #' @return JSON string representation of the PlatformArg.
    #' @keywords internal
    toJSONString = function() {
      if (!is.null(self$actual_instance)) {
        as.character(jsonlite::minify(self$actual_instance$toJSONString()))
      } else {
        NULL
      }
    },
    #' Serialize PlatformArg to JSON.
    #'
    #' @description
    #' Serialize PlatformArg to JSON.
    #'
    #' @return JSON representation of the PlatformArg.
    #' @keywords internal
    toJSON = function() {
      if (!is.null(self$actual_instance)) {
        self$actual_instance$toJSON()
      } else {
        NULL
      }
    },
    #' Validate the input JSON with respect to PlatformArg.
    #'
    #' @description
    #' Validate the input JSON with respect to PlatformArg and
    #' throw exception if invalid.
    #'
    #' @param input The input JSON.
    #' @keywords internal
    validateJSON = function(input) {
      # backup current values
      actual_instance_bak <- self$actual_instance
      actual_type_bak <- self$actual_type

      # if it's not valid, an error will be thrown
      self$fromJSON(input)

      # no error thrown, restore old values
      self$actual_instance <- actual_instance_bak
      self$actual_type <- actual_type_bak
    },
    #' Returns the string representation of the instance.
    #'
    #' @description
    #' Returns the string representation of the instance.
    #'
    #' @return The string representation of the instance.
    #' @keywords internal
    toString = function() {
      jsoncontent <- c(
        sprintf('"actual_instance": %s', if (is.null(self$actual_instance)) NULL else self$actual_instance$toJSONString()),
        sprintf('"actual_type": "%s"', self$actual_type),
        sprintf('"one_of": "%s"', paste(unlist(self$one_of), collapse = ", "))
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::prettify(paste("{", jsoncontent, "}", sep = "")))
    }
  ),
  # Lock the class to prevent modifications to the method or field
  lock_class = TRUE
)

# Unlock the class to allow modifications of the method or field
PlatformArg$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
PlatformArg$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
PlatformArg$lock()

