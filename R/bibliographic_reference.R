#' Create a new BibliographicReference
#'
#' @description
#' BibliographicReference Class
#'
#' @docType class
#' @title BibliographicReference
#' @description BibliographicReference Class
#' @format An \code{R6Class} generator object
#' @field name  character optional
#' @field description  character optional
#' @field id  integer optional
#' @field authorList  character optional
#' @field title  character optional
#' @field publisher  character optional
#' @field editor  character optional
#' @field volume  character optional
#' @field issue  character optional
#' @field pages  character optional
#' @field publication  character optional
#' @field fullTextUri  character optional
#' @field abstractText  character optional
#' @field citation  character optional
#' @field publicationDate  character optional
#' @field annotatedAbstract  character optional
#' @field pubAccession  \link{DatabaseEntry} optional
#' @field retracted  character optional
#' @field annotations  list(\link{Characteristic}) optional
#' @field meshTerms  list(\link{MedicalSubjectHeading}) optional
#' @field keywords  list(\link{Keyword}) optional
#' @field chemicals  list(\link{Compound}) optional
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
BibliographicReference <- R6::R6Class(
  "BibliographicReference",
  public = list(
    `name` = NULL,
    `description` = NULL,
    `id` = NULL,
    `authorList` = NULL,
    `title` = NULL,
    `publisher` = NULL,
    `editor` = NULL,
    `volume` = NULL,
    `issue` = NULL,
    `pages` = NULL,
    `publication` = NULL,
    `fullTextUri` = NULL,
    `abstractText` = NULL,
    `citation` = NULL,
    `publicationDate` = NULL,
    `annotatedAbstract` = NULL,
    `pubAccession` = NULL,
    `retracted` = NULL,
    `annotations` = NULL,
    `meshTerms` = NULL,
    `keywords` = NULL,
    `chemicals` = NULL,
    #' Initialize a new BibliographicReference class.
    #'
    #' @description
    #' Initialize a new BibliographicReference class.
    #'
    #' @param name name
    #' @param description description
    #' @param id id
    #' @param authorList authorList
    #' @param title title
    #' @param publisher publisher
    #' @param editor editor
    #' @param volume volume
    #' @param issue issue
    #' @param pages pages
    #' @param publication publication
    #' @param fullTextUri fullTextUri
    #' @param abstractText abstractText
    #' @param citation citation
    #' @param publicationDate publicationDate
    #' @param annotatedAbstract annotatedAbstract
    #' @param pubAccession pubAccession
    #' @param retracted retracted
    #' @param annotations annotations
    #' @param meshTerms meshTerms
    #' @param keywords keywords
    #' @param chemicals chemicals
    #' @param ... Other optional arguments.
    #' @keywords internal
    initialize = function(
        `name` = NULL, `description` = NULL, `id` = NULL, `authorList` = NULL, `title` = NULL, `publisher` = NULL, `editor` = NULL, `volume` = NULL, `issue` = NULL, `pages` = NULL, `publication` = NULL, `fullTextUri` = NULL, `abstractText` = NULL, `citation` = NULL, `publicationDate` = NULL, `annotatedAbstract` = NULL, `pubAccession` = NULL, `retracted` = NULL, `annotations` = NULL, `meshTerms` = NULL, `keywords` = NULL, `chemicals` = NULL, ...
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
      if (!is.null(`authorList`)) {
        stopifnot(is.character(`authorList`), length(`authorList`) == 1)
        self$`authorList` <- `authorList`
      }
      if (!is.null(`title`)) {
        stopifnot(is.character(`title`), length(`title`) == 1)
        self$`title` <- `title`
      }
      if (!is.null(`publisher`)) {
        stopifnot(is.character(`publisher`), length(`publisher`) == 1)
        self$`publisher` <- `publisher`
      }
      if (!is.null(`editor`)) {
        stopifnot(is.character(`editor`), length(`editor`) == 1)
        self$`editor` <- `editor`
      }
      if (!is.null(`volume`)) {
        stopifnot(is.character(`volume`), length(`volume`) == 1)
        self$`volume` <- `volume`
      }
      if (!is.null(`issue`)) {
        stopifnot(is.character(`issue`), length(`issue`) == 1)
        self$`issue` <- `issue`
      }
      if (!is.null(`pages`)) {
        stopifnot(is.character(`pages`), length(`pages`) == 1)
        self$`pages` <- `pages`
      }
      if (!is.null(`publication`)) {
        stopifnot(is.character(`publication`), length(`publication`) == 1)
        self$`publication` <- `publication`
      }
      if (!is.null(`fullTextUri`)) {
        stopifnot(is.character(`fullTextUri`), length(`fullTextUri`) == 1)
        self$`fullTextUri` <- `fullTextUri`
      }
      if (!is.null(`abstractText`)) {
        stopifnot(is.character(`abstractText`), length(`abstractText`) == 1)
        self$`abstractText` <- `abstractText`
      }
      if (!is.null(`citation`)) {
        stopifnot(is.character(`citation`), length(`citation`) == 1)
        self$`citation` <- `citation`
      }
      if (!is.null(`publicationDate`)) {
        stopifnot(is.character(`publicationDate`), length(`publicationDate`) == 1)
        self$`publicationDate` <- `publicationDate`
      }
      if (!is.null(`annotatedAbstract`)) {
        stopifnot(is.character(`annotatedAbstract`), length(`annotatedAbstract`) == 1)
        self$`annotatedAbstract` <- `annotatedAbstract`
      }
      if (!is.null(`pubAccession`)) {
        stopifnot(R6::is.R6(`pubAccession`))
        self$`pubAccession` <- `pubAccession`
      }
      if (!is.null(`retracted`)) {
        stopifnot(is.logical(`retracted`), length(`retracted`) == 1)
        self$`retracted` <- `retracted`
      }
      if (!is.null(`annotations`)) {
        stopifnot(is.vector(`annotations`), length(`annotations`) != 0)
        sapply(`annotations`, function(x) stopifnot(R6::is.R6(x)))
        self$`annotations` <- `annotations`
      }
      if (!is.null(`meshTerms`)) {
        stopifnot(is.vector(`meshTerms`), length(`meshTerms`) != 0)
        sapply(`meshTerms`, function(x) stopifnot(R6::is.R6(x)))
        self$`meshTerms` <- `meshTerms`
      }
      if (!is.null(`keywords`)) {
        stopifnot(is.vector(`keywords`), length(`keywords`) != 0)
        sapply(`keywords`, function(x) stopifnot(R6::is.R6(x)))
        self$`keywords` <- `keywords`
      }
      if (!is.null(`chemicals`)) {
        stopifnot(is.vector(`chemicals`), length(`chemicals`) != 0)
        sapply(`chemicals`, function(x) stopifnot(R6::is.R6(x)))
        self$`chemicals` <- `chemicals`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BibliographicReference in JSON format
    #' @keywords internal
    toJSON = function() {
      BibliographicReferenceObject <- list()
      if (!is.null(self$`name`)) {
        BibliographicReferenceObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        BibliographicReferenceObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`id`)) {
        BibliographicReferenceObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`authorList`)) {
        BibliographicReferenceObject[["authorList"]] <-
          self$`authorList`
      }
      if (!is.null(self$`title`)) {
        BibliographicReferenceObject[["title"]] <-
          self$`title`
      }
      if (!is.null(self$`publisher`)) {
        BibliographicReferenceObject[["publisher"]] <-
          self$`publisher`
      }
      if (!is.null(self$`editor`)) {
        BibliographicReferenceObject[["editor"]] <-
          self$`editor`
      }
      if (!is.null(self$`volume`)) {
        BibliographicReferenceObject[["volume"]] <-
          self$`volume`
      }
      if (!is.null(self$`issue`)) {
        BibliographicReferenceObject[["issue"]] <-
          self$`issue`
      }
      if (!is.null(self$`pages`)) {
        BibliographicReferenceObject[["pages"]] <-
          self$`pages`
      }
      if (!is.null(self$`publication`)) {
        BibliographicReferenceObject[["publication"]] <-
          self$`publication`
      }
      if (!is.null(self$`fullTextUri`)) {
        BibliographicReferenceObject[["fullTextUri"]] <-
          self$`fullTextUri`
      }
      if (!is.null(self$`abstractText`)) {
        BibliographicReferenceObject[["abstractText"]] <-
          self$`abstractText`
      }
      if (!is.null(self$`citation`)) {
        BibliographicReferenceObject[["citation"]] <-
          self$`citation`
      }
      if (!is.null(self$`publicationDate`)) {
        BibliographicReferenceObject[["publicationDate"]] <-
          self$`publicationDate`
      }
      if (!is.null(self$`annotatedAbstract`)) {
        BibliographicReferenceObject[["annotatedAbstract"]] <-
          self$`annotatedAbstract`
      }
      if (!is.null(self$`pubAccession`)) {
        BibliographicReferenceObject[["pubAccession"]] <-
          self$`pubAccession`$toJSON()
      }
      if (!is.null(self$`retracted`)) {
        BibliographicReferenceObject[["retracted"]] <-
          self$`retracted`
      }
      if (!is.null(self$`annotations`)) {
        BibliographicReferenceObject[["annotations"]] <-
          lapply(self$`annotations`, function(x) x$toJSON())
      }
      if (!is.null(self$`meshTerms`)) {
        BibliographicReferenceObject[["meshTerms"]] <-
          lapply(self$`meshTerms`, function(x) x$toJSON())
      }
      if (!is.null(self$`keywords`)) {
        BibliographicReferenceObject[["keywords"]] <-
          lapply(self$`keywords`, function(x) x$toJSON())
      }
      if (!is.null(self$`chemicals`)) {
        BibliographicReferenceObject[["chemicals"]] <-
          lapply(self$`chemicals`, function(x) x$toJSON())
      }

      BibliographicReferenceObject
    },
    #' Deserialize JSON string into an instance of BibliographicReference
    #'
    #' @description
    #' Deserialize JSON string into an instance of BibliographicReference
    #'
    #' @param input_json the JSON input
    #' @return the instance of BibliographicReference
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
      if (!is.null(this_object$`authorList`)) {
        self$`authorList` <- this_object$`authorList`
      }
      if (!is.null(this_object$`title`)) {
        self$`title` <- this_object$`title`
      }
      if (!is.null(this_object$`publisher`)) {
        self$`publisher` <- this_object$`publisher`
      }
      if (!is.null(this_object$`editor`)) {
        self$`editor` <- this_object$`editor`
      }
      if (!is.null(this_object$`volume`)) {
        self$`volume` <- this_object$`volume`
      }
      if (!is.null(this_object$`issue`)) {
        self$`issue` <- this_object$`issue`
      }
      if (!is.null(this_object$`pages`)) {
        self$`pages` <- this_object$`pages`
      }
      if (!is.null(this_object$`publication`)) {
        self$`publication` <- this_object$`publication`
      }
      if (!is.null(this_object$`fullTextUri`)) {
        self$`fullTextUri` <- this_object$`fullTextUri`
      }
      if (!is.null(this_object$`abstractText`)) {
        self$`abstractText` <- this_object$`abstractText`
      }
      if (!is.null(this_object$`citation`)) {
        self$`citation` <- this_object$`citation`
      }
      if (!is.null(this_object$`publicationDate`)) {
        self$`publicationDate` <- this_object$`publicationDate`
      }
      if (!is.null(this_object$`annotatedAbstract`)) {
        self$`annotatedAbstract` <- this_object$`annotatedAbstract`
      }
      if (!is.null(this_object$`pubAccession`)) {
        pubaccession_object <- DatabaseEntry$new()
        pubaccession_object$fromJSON(jsonlite::toJSON(this_object$pubAccession, auto_unbox = TRUE, digits = NA))
        self$`pubAccession` <- pubaccession_object
      }
      if (!is.null(this_object$`retracted`)) {
        self$`retracted` <- this_object$`retracted`
      }
      if (!is.null(this_object$`annotations`)) {
        self$`annotations` <- ApiClient$new()$deserializeObj(this_object$`annotations`, "set[Characteristic]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`meshTerms`)) {
        self$`meshTerms` <- ApiClient$new()$deserializeObj(this_object$`meshTerms`, "set[MedicalSubjectHeading]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`keywords`)) {
        self$`keywords` <- ApiClient$new()$deserializeObj(this_object$`keywords`, "set[Keyword]", loadNamespace("gemma.R"))
      }
      if (!is.null(this_object$`chemicals`)) {
        self$`chemicals` <- ApiClient$new()$deserializeObj(this_object$`chemicals`, "set[Compound]", loadNamespace("gemma.R"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BibliographicReference in JSON format
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
        if (!is.null(self$`authorList`)) {
          sprintf(
          '"authorList":
            "%s"
                    ',
          self$`authorList`
          )
        },
        if (!is.null(self$`title`)) {
          sprintf(
          '"title":
            "%s"
                    ',
          self$`title`
          )
        },
        if (!is.null(self$`publisher`)) {
          sprintf(
          '"publisher":
            "%s"
                    ',
          self$`publisher`
          )
        },
        if (!is.null(self$`editor`)) {
          sprintf(
          '"editor":
            "%s"
                    ',
          self$`editor`
          )
        },
        if (!is.null(self$`volume`)) {
          sprintf(
          '"volume":
            "%s"
                    ',
          self$`volume`
          )
        },
        if (!is.null(self$`issue`)) {
          sprintf(
          '"issue":
            "%s"
                    ',
          self$`issue`
          )
        },
        if (!is.null(self$`pages`)) {
          sprintf(
          '"pages":
            "%s"
                    ',
          self$`pages`
          )
        },
        if (!is.null(self$`publication`)) {
          sprintf(
          '"publication":
            "%s"
                    ',
          self$`publication`
          )
        },
        if (!is.null(self$`fullTextUri`)) {
          sprintf(
          '"fullTextUri":
            "%s"
                    ',
          self$`fullTextUri`
          )
        },
        if (!is.null(self$`abstractText`)) {
          sprintf(
          '"abstractText":
            "%s"
                    ',
          self$`abstractText`
          )
        },
        if (!is.null(self$`citation`)) {
          sprintf(
          '"citation":
            "%s"
                    ',
          self$`citation`
          )
        },
        if (!is.null(self$`publicationDate`)) {
          sprintf(
          '"publicationDate":
            "%s"
                    ',
          self$`publicationDate`
          )
        },
        if (!is.null(self$`annotatedAbstract`)) {
          sprintf(
          '"annotatedAbstract":
            "%s"
                    ',
          self$`annotatedAbstract`
          )
        },
        if (!is.null(self$`pubAccession`)) {
          sprintf(
          '"pubAccession":
          %s
          ',
          jsonlite::toJSON(self$`pubAccession`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`retracted`)) {
          sprintf(
          '"retracted":
            %s
                    ',
          tolower(self$`retracted`)
          )
        },
        if (!is.null(self$`annotations`)) {
          sprintf(
          '"annotations":
          [%s]
',
          paste(sapply(self$`annotations`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`meshTerms`)) {
          sprintf(
          '"meshTerms":
          [%s]
',
          paste(sapply(self$`meshTerms`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`keywords`)) {
          sprintf(
          '"keywords":
          [%s]
',
          paste(sapply(self$`keywords`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`chemicals`)) {
          sprintf(
          '"chemicals":
          [%s]
',
          paste(sapply(self$`chemicals`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of BibliographicReference
    #'
    #' @description
    #' Deserialize JSON string into an instance of BibliographicReference
    #'
    #' @param input_json the JSON input
    #' @return the instance of BibliographicReference
    #' @keywords internal
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`id` <- this_object$`id`
      self$`authorList` <- this_object$`authorList`
      self$`title` <- this_object$`title`
      self$`publisher` <- this_object$`publisher`
      self$`editor` <- this_object$`editor`
      self$`volume` <- this_object$`volume`
      self$`issue` <- this_object$`issue`
      self$`pages` <- this_object$`pages`
      self$`publication` <- this_object$`publication`
      self$`fullTextUri` <- this_object$`fullTextUri`
      self$`abstractText` <- this_object$`abstractText`
      self$`citation` <- this_object$`citation`
      self$`publicationDate` <- this_object$`publicationDate`
      self$`annotatedAbstract` <- this_object$`annotatedAbstract`
      self$`pubAccession` <- DatabaseEntry$new()$fromJSON(jsonlite::toJSON(this_object$pubAccession, auto_unbox = TRUE, digits = NA))
      self$`retracted` <- this_object$`retracted`
      self$`annotations` <- ApiClient$new()$deserializeObj(this_object$`annotations`, "set[Characteristic]", loadNamespace("gemma.R"))
      self$`meshTerms` <- ApiClient$new()$deserializeObj(this_object$`meshTerms`, "set[MedicalSubjectHeading]", loadNamespace("gemma.R"))
      self$`keywords` <- ApiClient$new()$deserializeObj(this_object$`keywords`, "set[Keyword]", loadNamespace("gemma.R"))
      self$`chemicals` <- ApiClient$new()$deserializeObj(this_object$`chemicals`, "set[Compound]", loadNamespace("gemma.R"))
      self
    },
    #' Validate JSON input with respect to BibliographicReference
    #'
    #' @description
    #' Validate JSON input with respect to BibliographicReference and throw an exception if invalid
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
    #' @return String representation of BibliographicReference
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
BibliographicReference$unlock()

#' Print the object
#'
#' @description
#' Print the object
#'
#' @keywords internal
BibliographicReference$set("public", "print", function(...) {
  print(jsonlite::prettify(self$toJSONString()))
  invisible(self)
})

# Lock the class to prevent modifications to the method or field
BibliographicReference$lock()

