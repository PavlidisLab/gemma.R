#' Create a new Gene
#'
#' @description
#' Gene Class
#'
#' @docType class
#' @title Gene
#' @description Gene Class
#' @format An \code{R6Class} generator object
#' @field name  character optional
#' @field description  character optional
#' @field id  integer optional
#' @field previousNcbiId  character optional
#' @field physicalLocation  \link{PhysicalLocation} optional
#' @field officialSymbol  character optional
#' @field officialName  character optional
#' @field ncbiGeneId  integer optional
#' @field ensemblId  character optional
#' @field products  list(\link{GeneProduct}) optional
#' @field aliases  list(\link{GeneAlias}) optional
#' @field taxon  \link{Taxon} optional
#' @field accessions  list(\link{DatabaseEntry}) optional
#' @field multifunctionality  \link{Multifunctionality} optional
#' @field phenotypeAssociations  list(\link{PhenotypeAssociation}) optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Gene <- R6::R6Class(
  "Gene",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `previousNcbiId` = NULL,
    `physicalLocation` = NULL,
    `officialSymbol` = NULL,
    `officialName` = NULL,
    `ncbiGeneId` = NULL,
    `ensemblId` = NULL,
    `products` = NULL,
    `aliases` = NULL,
    `taxon` = NULL,
    `accessions` = NULL,
    `multifunctionality` = NULL,
    `phenotypeAssociations` = NULL,
    #' Initialize a new Gene class.
    #'
    #' @description
    #' Initialize a new Gene class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param previousNcbiId previousNcbiId
    #' @param physicalLocation physicalLocation
    #' @param officialSymbol officialSymbol
    #' @param officialName officialName
    #' @param ncbiGeneId ncbiGeneId
    #' @param ensemblId ensemblId
    #' @param products products
    #' @param aliases aliases
    #' @param taxon taxon
    #' @param accessions accessions
    #' @param multifunctionality multifunctionality
    #' @param phenotypeAssociations phenotypeAssociations
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `previousNcbiId` = NULL, `physicalLocation` = NULL, `officialSymbol` = NULL, `officialName` = NULL, `ncbiGeneId` = NULL, `ensemblId` = NULL, `products` = NULL, `aliases` = NULL, `taxon` = NULL, `accessions` = NULL, `multifunctionality` = NULL, `phenotypeAssociations` = NULL, ...
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
      if (!is.null(`officialSymbol`)) {
        stopifnot(is.character(`officialSymbol`), length(`officialSymbol`) == 1)
        self$`officialSymbol` <- `officialSymbol`
      }
      if (!is.null(`officialName`)) {
        stopifnot(is.character(`officialName`), length(`officialName`) == 1)
        self$`officialName` <- `officialName`
      }
      if (!is.null(`ncbiGeneId`)) {
        stopifnot(is.numeric(`ncbiGeneId`), length(`ncbiGeneId`) == 1)
        self$`ncbiGeneId` <- `ncbiGeneId`
      }
      if (!is.null(`ensemblId`)) {
        stopifnot(is.character(`ensemblId`), length(`ensemblId`) == 1)
        self$`ensemblId` <- `ensemblId`
      }
      if (!is.null(`products`)) {
        stopifnot(is.vector(`products`), length(`products`) != 0)
        sapply(`products`, function(x) stopifnot(R6::is.R6(x)))
        self$`products` <- `products`
      }
      if (!is.null(`aliases`)) {
        stopifnot(is.vector(`aliases`), length(`aliases`) != 0)
        sapply(`aliases`, function(x) stopifnot(R6::is.R6(x)))
        self$`aliases` <- `aliases`
      }
      if (!is.null(`taxon`)) {
        stopifnot(R6::is.R6(`taxon`))
        self$`taxon` <- `taxon`
      }
      if (!is.null(`accessions`)) {
        stopifnot(is.vector(`accessions`), length(`accessions`) != 0)
        sapply(`accessions`, function(x) stopifnot(R6::is.R6(x)))
        self$`accessions` <- `accessions`
      }
      if (!is.null(`multifunctionality`)) {
        stopifnot(R6::is.R6(`multifunctionality`))
        self$`multifunctionality` <- `multifunctionality`
      }
      if (!is.null(`phenotypeAssociations`)) {
        stopifnot(is.vector(`phenotypeAssociations`), length(`phenotypeAssociations`) != 0)
        sapply(`phenotypeAssociations`, function(x) stopifnot(R6::is.R6(x)))
        self$`phenotypeAssociations` <- `phenotypeAssociations`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Gene in JSON format
    #' @keywords internal
    toJSON = function() {
      GeneObject <- list()
      if (!is.null(self$`name`)) {
        GeneObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        GeneObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        GeneObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`previousNcbiId`)) {
        GeneObject[["previousNcbiId"]] <-
          self$`previousNcbiId`
      }
      if (!is.null(self$`physicalLocation`)) {
        GeneObject[["physicalLocation"]] <-
          self$`physicalLocation`$toJSON()
      }
      if (!is.null(self$`officialSymbol`)) {
        GeneObject[["officialSymbol"]] <-
          self$`officialSymbol`
      }
      if (!is.null(self$`officialName`)) {
        GeneObject[["officialName"]] <-
          self$`officialName`
      }
      if (!is.null(self$`ncbiGeneId`)) {
        GeneObject[["ncbiGeneId"]] <-
          self$`ncbiGeneId`
      }
      if (!is.null(self$`ensemblId`)) {
        GeneObject[["ensemblId"]] <-
          self$`ensemblId`
      }
      if (!is.null(self$`products`)) {
        GeneObject[["products"]] <-
          lapply(self$`products`, function(x) x$toJSON())
      }
      if (!is.null(self$`aliases`)) {
        GeneObject[["aliases"]] <-
          lapply(self$`aliases`, function(x) x$toJSON())
      }
      if (!is.null(self$`taxon`)) {
        GeneObject[["taxon"]] <-
          self$`taxon`$toJSON()
      }
      if (!is.null(self$`accessions`)) {
        GeneObject[["accessions"]] <-
          lapply(self$`accessions`, function(x) x$toJSON())
      }
      if (!is.null(self$`multifunctionality`)) {
        GeneObject[["multifunctionality"]] <-
          self$`multifunctionality`$toJSON()
      }
      if (!is.null(self$`phenotypeAssociations`)) {
        GeneObject[["phenotypeAssociations"]] <-
          lapply(self$`phenotypeAssociations`, function(x) x$toJSON())
      }

      GeneObject
    },
    #' Deserialize JSON string into an instance of Gene
    #'
    #' @description
    #' Deserialize JSON string into an instance of Gene
    #'
    #' @param input_json the JSON input
    #' @return the instance of Gene
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
      if (!is.null(this_object$`officialSymbol`)) {
        self$`officialSymbol` <- this_object$`officialSymbol`
      }
      if (!is.null(this_object$`officialName`)) {
        self$`officialName` <- this_object$`officialName`
      }
      if (!is.null(this_object$`ncbiGeneId`)) {
        self$`ncbiGeneId` <- this_object$`ncbiGeneId`
      }
      if (!is.null(this_object$`ensemblId`)) {
        self$`ensemblId` <- this_object$`ensemblId`
      }
      if (!is.null(this_object$`products`)) {
        self$`products` <- ApiClient$new()$deserializeObj(this_object$`products`, "set[GeneProduct]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`aliases`)) {
        self$`aliases` <- ApiClient$new()$deserializeObj(this_object$`aliases`, "set[GeneAlias]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`taxon`)) {
        taxon_object <- Taxon$new()
        taxon_object$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
        self$`taxon` <- taxon_object
      }
      if (!is.null(this_object$`accessions`)) {
        self$`accessions` <- ApiClient$new()$deserializeObj(this_object$`accessions`, "set[DatabaseEntry]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`multifunctionality`)) {
        multifunctionality_object <- Multifunctionality$new()
        multifunctionality_object$fromJSON(jsonlite::toJSON(this_object$multifunctionality, auto_unbox = TRUE, digits = NA))
        self$`multifunctionality` <- multifunctionality_object
      }
      if (!is.null(this_object$`phenotypeAssociations`)) {
        self$`phenotypeAssociations` <- ApiClient$new()$deserializeObj(this_object$`phenotypeAssociations`, "set[PhenotypeAssociation]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Gene in JSON format
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
        if (!is.null(self$`officialSymbol`)) {
          sprintf(
          '"officialSymbol":
            "%s"
                    ',
          self$`officialSymbol`
          )
        },
        if (!is.null(self$`officialName`)) {
          sprintf(
          '"officialName":
            "%s"
                    ',
          self$`officialName`
          )
        },
        if (!is.null(self$`ncbiGeneId`)) {
          sprintf(
          '"ncbiGeneId":
            %d
                    ',
          self$`ncbiGeneId`
          )
        },
        if (!is.null(self$`ensemblId`)) {
          sprintf(
          '"ensemblId":
            "%s"
                    ',
          self$`ensemblId`
          )
        },
        if (!is.null(self$`products`)) {
          sprintf(
          '"products":
          [%s]
',
          paste(sapply(self$`products`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`aliases`)) {
          sprintf(
          '"aliases":
          [%s]
',
          paste(sapply(self$`aliases`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
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
        if (!is.null(self$`accessions`)) {
          sprintf(
          '"accessions":
          [%s]
',
          paste(sapply(self$`accessions`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`multifunctionality`)) {
          sprintf(
          '"multifunctionality":
          %s
          ',
          jsonlite::toJSON(self$`multifunctionality`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`phenotypeAssociations`)) {
          sprintf(
          '"phenotypeAssociations":
          [%s]
',
          paste(sapply(self$`phenotypeAssociations`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Gene
    #'
    #' @description
    #' Deserialize JSON string into an instance of Gene
    #'
    #' @param input_json the JSON input
    #' @return the instance of Gene
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`previousNcbiId` <- this_object$`previousNcbiId`
      self$`physicalLocation` <- PhysicalLocation$new()$fromJSON(jsonlite::toJSON(this_object$physicalLocation, auto_unbox = TRUE, digits = NA))
      self$`officialSymbol` <- this_object$`officialSymbol`
      self$`officialName` <- this_object$`officialName`
      self$`ncbiGeneId` <- this_object$`ncbiGeneId`
      self$`ensemblId` <- this_object$`ensemblId`
      self$`products` <- ApiClient$new()$deserializeObj(this_object$`products`, "set[GeneProduct]", loadNamespace("gemma.R"))
      self$`aliases` <- ApiClient$new()$deserializeObj(this_object$`aliases`, "set[GeneAlias]", loadNamespace("gemma.R"))
      self$`taxon` <- Taxon$new()$fromJSON(jsonlite::toJSON(this_object$taxon, auto_unbox = TRUE, digits = NA))
      self$`accessions` <- ApiClient$new()$deserializeObj(this_object$`accessions`, "set[DatabaseEntry]", loadNamespace("gemma.R"))
      self$`multifunctionality` <- Multifunctionality$new()$fromJSON(jsonlite::toJSON(this_object$multifunctionality, auto_unbox = TRUE, digits = NA))
      self$`phenotypeAssociations` <- ApiClient$new()$deserializeObj(this_object$`phenotypeAssociations`, "set[PhenotypeAssociation]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to Gene
    #'
    #' @description
    #' Validate JSON input with respect to Gene and throw an exception if invalid
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
    #' @return String representation of Gene
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
Gene$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
Gene$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
Gene$lock()

