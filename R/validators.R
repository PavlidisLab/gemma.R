validateID <- function(name, ...) {
    ID <- unlist(list(...))
    isID <- grepl('^\\d+$', ID)
    
    if(any(is.na(ID)) || (any(isID) && !all(isID)) || any(ID == ''))
        stop(glue('Please specify valid identifiers for {name} and do not combine different types of identifiers.'), call. = F)
    ID
}

validateSingleID <- function(name, ...) {
    ID <- unlist(list(...))
    isID <- grepl('^\\d+$', ID)
    
    if(length(ID) > 1 || is.na(ID) || any(ID == ''))
        stop(glue('Please specify one valid identifier for {name}.'), call. = F)
    ID
}

validateTaxon <- function(name, ...) {
    taxa <- as.character(unlist(list(...)))
    
    LOOKUP_TABLE <- data.table(id = c(1, 2, 3, 11, 12, 13, 14),
                               name = c('human', 'mouse', 'rat', 'yeast', 'zebrafish', 'fly', 'worm'),
                               scientific = c('Homo sapiens', 'Mus musculus', 'Rattus norvegicus',
                                              'Saccharomyces cerevisiae', 'Danio rerio', 'Drosophila melanogaster',
                                              'Caenorhabditis elegans'),
                               ncbi = c(9606, 10090, 10116, 4932, 7955, 7227, 6239))
    
    if(!all(taxa %in% c('', unlist(LOOKUP_TABLE)))) {
        print(LOOKUP_TABLE)
        stop(glue('You must specify a valid taxon for {name}.'), call. = F)
    }
    taxa
}

validateQuery <- function(name, ...) {
    query <- unlist(list(...))
    # TODO Stub
    query
}

validateFilter <- function(name, ...) {
    filters <- unlist(list(...))
    # TODO stub
    filters
}

validatePositiveInteger <- function(name, ...) {
    args <- list(...)
    if(any(is.na(unlist(args))) || !is.numeric(unlist(args)) || any(sapply(args, '%%', 1) != 0) || any(sapply(args, sign) < 0))
        stop(glue('Please only specify positive integer values for {name}.'), call. = F)
    unlist(args)
}

validatePositiveReal <- function(name, ...) {
    args <- list(...)
    if(!is.numeric(unlist(args)) || any(sapply(args, sign) < 0))
        stop(glue('Please only specify positive values for {name}.'), call. = F)
    unlist(args)
}

validateBoolean <- function(name, ...) {
    args <- unlist(list(...))
    if(!is.logical(args))
        stop(glue('Please only specify boolean values for {name}.'), call. = F)
    tolower(as.character(args))
}

validateStrand <- function(name, ...) {
    strand <- unlist(list(...))
    if(length(strand) != 1 || !(strand %in% c('+', '-')))
        stop(glue('Please specify + or - for {name}.'), call. = F)
    strand
}

validateConsolidate <- function(name, ...) {
    consolidate <- unlist(list(...))
    if(!all(is.na(consolidate) | consolidate %in% c('', 'pickmax', 'pickvar', 'average')))
        stop(glue('{name} must be one of "pickmax", "pickvar", "average" or empty.'), call. = F)
    consolidate
}

validateSort <- function(name, ...) {
    sort <- unlist(list(...))
    if(!all(grepl('^[+-].+', sort)))
        stop('Sort must match [+,-][property name].', call. = F)
    sort
}
