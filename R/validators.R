#' Validate identifiers (ie. gene ID, platform ID, etc.) that are homogeneous (either all numerics or all not)
#'
#' @param name The variable name
#' @param ... Any identifiers
#'
#' @return The validated identifiers, or stop with an error message
#'
#' @keywords internal
validateID <- function(name, ...) {
    ID <- unlist(list(...))
    isID <- grepl("^\\d+$", ID)

    if (any(is.na(ID)) || (any(isID) && !all(isID)) || any(ID == "")) {
        stop(glue::glue("Please specify valid identifiers for {name} and do not combine different types of identifiers."), call. = FALSE)
    }
    paste0(ID, collapse = ",")
}

#' Validate identifiers (ie. gene ID, platform ID, etc.) that are homogeneous (either all numerics or all not)
#'
#' @param name The variable name
#' @param ... Any identifiers
#'
#' @return The validated identifiers, or stop with an error message
#'
#' @keywords internal
validateOptionalID <- function(name, ...) {
    if (all(is.na(as.character(unlist(list(...)))))) {
        ""
    } else {
        validateID(name, ...)
    }
}

#' Validate a single identifier(ie. gene ID, platform ID, etc.)
#'
#' @param name The variable name
#' @param ... An identifier
#'
#' @return The validated identifier, or stop with an error message
#'
#' @keywords internal
validateSingleID <- function(name, ...) {
    ID <- unlist(list(...))

    if (length(ID) > 1) {
        stop(glue::glue("Please specify one valid identifier for {name}."), call. = FALSE)
    }
    validateID(name, ...)
}

#' Validate a taxon using the acceptable taxa entries
#'
#' @param name The variable name
#' @param ... Any taxa to validate
#'
#' @return The validated taxon, or stop with an error message
#'
#' @keywords internal
validateOptionalTaxon <- function(name, ...) {
    if (all(is.na(as.character(unlist(list(...)))))) {
        ""
    } else {
        validateTaxon(name, ...)
    }
}

#' Validate a taxon using the acceptable taxa entries
#'
#' @param name The variable name
#' @param ... Any taxa to validate
#'
#' @return The validated taxon, or stop with an error message
#'
#' @keywords internal
validateTaxon <- function(name, ...) {
    # TODO LOOKUP_TABLE is insufficient so
    return(validateID(name, ...))

    taxa <- as.character(unlist(list(...)))

    LOOKUP_TABLE <- data.table(
        id = c(1, 2, 3, 11, 12, 13, 14),
        name = c("human", "mouse", "rat", "yeast", "zebrafish", "fly", "worm"),
        scientific = c(
            "Homo sapiens", "Mus musculus", "Rattus norvegicus",
            "Saccharomyces cerevisiae", "Danio rerio", "Drosophila melanogaster",
            "Caenorhabditis elegans"
        ),
        ncbi = c(9606, 10090, 10116, 4932, 7955, 7227, 6239)
    )

    if (!all(taxa %in% c("", unlist(LOOKUP_TABLE)))) {
        print(LOOKUP_TABLE)
        stop(glue::glue("You must specify a valid taxon for {name}."), call. = FALSE)
    }
    paste0(taxa, collapse = ",")
}

#' Validate a taxon using the acceptable taxa entries
#'
#' @param name The variable name
#' @param ... Any taxa to validate
#'
#' @return The validated taxon, or stop with an error message
#'
#' @keywords internal
validateSingleTaxon <- function(name, ...) {
    taxon <- unlist(list(...))

    if (length(taxon) > 1) {
        stop(glue::glue("Please specify one taxon for {name}."), call. = FALSE)
    }
    validateTaxon(name, ...)
}

#' Validate a query
#'
#' @param name The variable name
#' @param ... Any queries
#'
#' @return The validated queries, or stop with an error message
#'
#' @keywords internal
validateQuery <- function(name, ...) {
    query <- unlist(list(...))

    if (all(is.na(query)) || length(query) == 0) {
        stop(glue::glue("Please specify a query for {name}."), call. = FALSE)
    }

    paste0(query, collapse = ",")
}

#' Validate a filter
#'
#' @param name The variable name
#' @param ... Any filters
#'
#' @return The validated filters, or stop with an error message
#'
#' @keywords internal
validateFilter <- function(name, ...) {
    filters <- unlist(list(...))
    # TODO stub
    paste0(filters, collapse = " AND ")
}

#' Validate a non-negative integer value
#'
#' @param name The variable name
#' @param ... Any possible integers
#'
#' @return The validated integers, or stop with an error message
#'
#' @keywords internal
validatePositiveInteger <- function(name, ...) {
    args <- list(...)
    if (length(unlist(args)) != 1 || any(is.na(unlist(args))) || !is.numeric(unlist(args)) || any(vapply(args, "%%", 1, FUN.VALUE = numeric(1)) != 0) || any(vapply(args, sign, FUN.VALUE = numeric(1)) < 0)) {
        stop(glue::glue("Please only specify positive integer values for {name}."), call. = FALSE)
    }
    unlist(args)
}

#' Validate a non-negative number
#'
#' @param name The variable name
#' @param ... Any possible numbers
#'
#' @return The validated numbers, or stop with an error message
#'
#' @keywords internal
validatePositiveReal <- function(name, ...) {
    args <- list(...)
    if (length(unlist(args)) != 1 || !is.numeric(unlist(args)) || any(vapply(args, sign, FUN.VALUE = numeric(1)) < 0)) {
        stop(glue::glue("Please only specify positive values for {name}."), call. = FALSE)
    }
    unlist(args)
}

#' Validate a boolean value
#'
#' @param name The variable name
#' @param ... Any boolean types
#'
#' @return The validated boolean as a character string (true or false), or stop with an error message
#'
#' @keywords internal
validateBoolean <- function(name, ...) {
    args <- unlist(list(...))
    if (length(args) != 1 || !is.logical(args)) {
        stop(glue::glue("Please only specify boolean values for {name}."), call. = FALSE)
    }
    tolower(as.character(args))
}

#' Validate a strand (ie. + or -)
#'
#' @param name The variable name
#' @param ... Any strands
#'
#' @return The validated strands, or stop with an error message
#'
#' @keywords internal
validateStrand <- function(name, ...) {
    strand <- unlist(list(...))
    if (length(strand) != 1 || !(strand %in% c("+", "-"))) {
        stop(glue::glue("Please specify + or - for {name}."), call. = FALSE)
    }
    strand
}

#' Validate a consolidate entry (one of pickmax, pickvar, average or missing)
#'
#' @param name The variable name
#' @param ... Any consolidate entries
#'
#' @return The validated consolidate entries, or stop with an error message
#'
#' @keywords internal
validateConsolidate <- function(name, ...) {
    consolidate <- unlist(list(...))
    if (length(consolidate) != 1 || !all(is.na(consolidate) | consolidate %in% c("", "pickmax", "pickvar", "average"))) {
        stop(glue::glue('{name} must be one of "pickmax", "pickvar", "average" or empty.'), call. = FALSE)
    }
    consolidate
}

#' Validate a sort argument
#'
#' @param name The variable name
#' @param ... Any sort arguments
#'
#' @return The validated sort arguments, or stop with an error message
#'
#' @keywords internal
validateSort <- function(name, ...) {
    sort <- unlist(list(...))
    if (length(sort) != 1 || !all(grepl("^[+-].+", sort))) {
        stop("Sort must match [+,-][property name].", call. = FALSE)
    }
    sort
}
