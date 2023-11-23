
#' @keywords internal
processDate <- function(x){
    switch(as.character(is.character(x)),
           `TRUE` = lubridate::ymd_hms(x), # parse ISO 8601 format
           `FALSE` = as.POSIXct(x / 1e3, origin = "1970-01-01"))
}


#' Replace missing data with NAs
#' @param x Data
#' @param natype type of NA to replace the missing data with
#' @return Data or NA in case of an out of bounds error
#'
#' @keywords internal
checkBounds <- function(x,natype = NA){
    tryCatch(x, error = function(e){
        if(e$message == "subscript out of bounds"){
            return(natype)
        } else{
            stop(e$message)
        }
    })
}


#' Access the field in a list
#'
#' This function accesses named field within the elements of a list. If an element
#' lacks the field, it's filled in by natype.
#'
#' @param d Input data list
#' @param field Field name to access in each element
#' @param natype What to fill in when field is unavailable
#' @return A vector of elements
#' @keywords internal
accessField <- function(d, field, natype = NA){
    field <- lapply(d,function(e){
        out <- checkBounds(e[[field]], natype)
        if(is.null(out)){
            out <- natype
        }
        return(out)
    }) %>% unlist
    if(is.null(field)){
        field <- array(dim=0)
    }
    return(field)
}

#' Avoid NULLS as data.table columns
#'
#' @param x A value that might be null
#' @param natype What to fill in when data is unavailable
#' @return x as is or natypee
#' @keywords internal
nullCheck <- function(x,natype= NA){
    if(is.null(x)){
        return(natype)
    } else{
        return(x)
    }
}


#' Processes JSON as a factor
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processCharacteristicValueObject <- function(d){
    data.table(
        category = d %>% accessField('category',NA_character_),
        categoryURI = d %>% accessField('categoryUri',NA_character_),
        value = d %>% accessField('value',NA_character_),
        valueUri = d %>% accessField('valueUri',NA_character_),
        valueId = d %>% accessField('valueId',NA_character_)
    )
}


processStatementValueObject <- function(d){
    data.table(
        category = d %>% accessField('category',NA_character_),
        categoryURI = d %>% accessField('categoryUri',NA_character_),
        subject = d %>% accessField('subject', NA_character_),
        subjectUri = d %>% accessField('subjectUri',NA_character_),
        subjectId = d %>% accessField('subjectId',NA_character_),
        predicate = d %>% accessField('predicate',NA_character_),
        predicateUri = d %>% accessField('predicateUri',NA_character_),
        object = d %>% accessField('object',NA_character_),
        objectUri = d %>% accessField('objectUri',NA_character_)
    )
}


processFactorValueValueObject <- function(d){
    if(is.null(d)){
        return(data.table(
            category = array(dim=0),
            categoryURI = array(dim=0),
            value = array(dim=0),
            valueUri = array(dim=0),
            predicate = array(dim=0),
            predicateUri = array(dim=0),
            object = array(dim=0),
            objectUri = array(dim=0),
            summary = array(dim=0)
        ))
    } else if(!is.null(d$isMeasurement) && d$isMeasurement){
        data.table(
            category = nullCheck(d$category,natype = NA_character_),
            categoryURI = nullCheck(d$categoryUri,natype = NA_character_),
            value = nullCheck(d$measurement$value,NA_character_),
            valueUri = NA_character_,
            predicate = NA_character_,
            predicateUri = NA_character_,
            object = NA_character_,
            objectUri = NA_character_,
            summary = NA_character_
        )
        
    } else{
        characteristics <- d$characteristics %>% processCharacteristicValueObject()
        statements <- d$statements %>% processStatementValueObject()
        # remove characteristics already covered by statements
        characteristics <- characteristics[!characteristics$valueId %in% statements$subjectId,]
        # edit characteristics fields to match statements
        statements %>% data.table::setnames(old = c("subject",'subjectUri','subjectId'),
                                new = c("value","valueUri","valueId"),skip_absent = TRUE)
        out <- rbind(characteristics,statements,fill= TRUE)
        out$summary <- d$summary %>% nullCheck(NA_character_)
        out <- out[,!"valueId"]
        return(out)
    }
}


#' A blank processor that returns data as is
#'
#' @param data any data
#' @return Data as is
#' @keywords internal
blank_processor <- function(data){
    return(data)
}
