#' Create a new GeneProduct
#'
#' @description
#' GeneProduct Class
#'
#' @docType class
#' @title GeneProduct
#' @description GeneProduct Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @field description  character [optional]
#' @field id  integer [optional]
#' @field previousNcbiId  character [optional]
#' @field physicalLocation  \link{PhysicalLocation} [optional]
#' @field ncbiGi  character [optional]
#' @field accessions  list(\link{DatabaseEntry}) [optional]
#' @field exons  list(\link{PhysicalLocation}) [optional]
#' @field gene  \link{Gene} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
GeneProduct <- R6::R6Class(
  "GeneProduct",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `previousNcbiId` = NULL,
    `physicalLocation` = NULL,
    `ncbiGi` = NULL,
    `accessions` = NULL,
    `exons` = NULL,
    `gene` = NULL,
    #' Initialize a new GeneProduct class.
    #'
    #' @description
    #' Initialize a new GeneProduct class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param previousNcbiId previousNcbiId
    #' @param physicalLocation physicalLocation
    #' @param ncbiGi ncbiGi
    #' @param accessions accessions
    #' @param exons exons
    #' @param gene gene
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `previousNcbiId` = NULL, `physicalLocation` = NULL, `ncbiGi` = NULL, `accessions` = NULL, `exons` = NULL, `gene` = NULL, ...
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
      if (!is.null(`previousNcbiId`)) {
        stopifnot(is.character(`previousNcbiId`), length(`previousNcbiId`) == 1)
        self$`previousNcbiId` <- `previousNcbiId`
      }
      if (!is.null(`physicalLocation`)) {
        stopifnot(R6::is.R6(`physicalLocation`))
        self$`physicalLocation` <- `physicalLocation`
      }
      if (!is.null(`ncbiGi`)) {
        stopifnot(is.character(`ncbiGi`), length(`ncbiGi`) == 1)
        self$`ncbiGi` <- `ncbiGi`
      }
      if (!is.null(`accessions`)) {
        stopifnot(is.vector(`accessions`), length(`accessions`) != 0)
        sapply(`accessions`, function(x) stopifnot(R6::is.R6(x)))
        self$`accessions` <- `accessions`
      }
      if (!is.null(`exons`)) {
        stopifnot(is.vector(`exons`), length(`exons`) != 0)
        sapply(`exons`, function(x) stopifnot(R6::is.R6(x)))
        self$`exons` <- `exons`
      }
      if (!is.null(`gene`)) {
        stopifnot(R6::is.R6(`gene`))
        self$`gene` <- `gene`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneProduct in JSON format
    #' @keywords internal
    toJSON = function() {
      GeneProductObject <- list()
      if (!is.null(self$`name`)) {
        GeneProductObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        GeneProductObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        GeneProductObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`previousNcbiId`)) {
        GeneProductObject[["previousNcbiId"]] <-
          self$`previousNcbiId`
      }
      if (!is.null(self$`physicalLocation`)) {
        GeneProductObject[["physicalLocation"]] <-
          self$`physicalLocation`$toJSON()
      }
      if (!is.null(self$`ncbiGi`)) {
        GeneProductObject[["ncbiGi"]] <-
          self$`ncbiGi`
      }
      if (!is.null(self$`accessions`)) {
        GeneProductObject[["accessions"]] <-
          lapply(self$`accessions`, function(x) x$toJSON())
      }
      if (!is.null(self$`exons`)) {
        GeneProductObject[["exons"]] <-
          lapply(self$`exons`, function(x) x$toJSON())
      }
      if (!is.null(self$`gene`)) {
        GeneProductObject[["gene"]] <-
          self$`gene`$toJSON()
      }

      GeneProductObject
    },
    #' Deserialize JSON string into an instance of GeneProduct
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneProduct
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneProduct
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
      if (!is.null(this_object$`previousNcbiId`)) {
        self$`previousNcbiId` <- this_object$`previousNcbiId`
      }
      if (!is.null(this_object$`physicalLocation`)) {
        physicallocation_object <- PhysicalLocation$new()
        physicallocation_object$fromJSON(jsonlite::toJSON(this_object$physicalLocation, auto_unbox = TRUE, digits = NA))
        self$`physicalLocation` <- physicallocation_object
      }
      if (!is.null(this_object$`ncbiGi`)) {
        self$`ncbiGi` <- this_object$`ncbiGi`
      }
      if (!is.null(this_object$`accessions`)) {
        self$`accessions` <- ApiClient$new()$deserializeObj(this_object$`accessions`, "set[DatabaseEntry]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`exons`)) {
        self$`exons` <- ApiClient$new()$deserializeObj(this_object$`exons`, "set[PhysicalLocation]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`gene`)) {
        gene_object <- Gene$new()
        gene_object$fromJSON(jsonlite::toJSON(this_object$gene, auto_unbox = TRUE, digits = NA))
        self$`gene` <- gene_object
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return GeneProduct in JSON format
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
        if (!is.null(self$`previousNcbiId`)) {
          sprintf(
          '"previousNcbiId":
            "%s"
                    ',
          self$`previousNcbiId`
          )
        },
        if (!is.null(self$`physicalLocation`)) {
          sprintf(
          '"physicalLocation":
          %s
          ',
          jsonlite::toJSON(self$`physicalLocation`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`ncbiGi`)) {
          sprintf(
          '"ncbiGi":
            "%s"
                    ',
          self$`ncbiGi`
          )
        },
        if (!is.null(self$`accessions`)) {
          sprintf(
          '"accessions":
          [%s]
',
          paste(sapply(self$`accessions`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`exons`)) {
          sprintf(
          '"exons":
          [%s]
',
          paste(sapply(self$`exons`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`gene`)) {
          sprintf(
          '"gene":
          %s
          ',
          jsonlite::toJSON(self$`gene`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of GeneProduct
    #'
    #' @description
    #' Deserialize JSON string into an instance of GeneProduct
    #'
    #' @param input_json the JSON input
    #' @return the instance of GeneProduct
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`previousNcbiId` <- this_object$`previousNcbiId`
      self$`physicalLocation` <- PhysicalLocation$new()$fromJSON(jsonlite::toJSON(this_object$physicalLocation, auto_unbox = TRUE, digits = NA))
      self$`ncbiGi` <- this_object$`ncbiGi`
      self$`accessions` <- ApiClient$new()$deserializeObj(this_object$`accessions`, "set[DatabaseEntry]", loadNamespace("gemma.R"))
      self$`exons` <- ApiClient$new()$deserializeObj(this_object$`exons`, "set[PhysicalLocation]", loadNamespace("gemma.R"))
      self$`gene` <- Gene$new()$fromJSON(jsonlite::toJSON(this_object$gene, auto_unbox = TRUE, digits = NA))
      self
    },
    #' Validate JSON input with respect to GeneProduct
    #'
    #' @description
    #' Validate JSON input with respect to GeneProduct and throw an exception if invalid
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
    #' @return String representation of GeneProduct
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
GeneProduct$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
GeneProduct$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
GeneProduct$lock()

