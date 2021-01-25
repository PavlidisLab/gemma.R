encode <- function(url) {
    if(is.na(url) || !is.character(url)) url
    else {
        if(length(url) > 1)
            url <- paste0(url, collapse = ',')
        URLencode(url, T)
    }
}

registerEndpoint <- function(endpoint,
                             fname,
                             preprocessor,
                             defaults = NULL,
                             validators = NULL,
                             where = parent.env(environment())) {
    gemmaAPI <- 'https://gemma.msl.ubc.ca/rest/v2/'
    
    if(missing(endpoint) || missing(fname) || missing(preprocessor))
        stop('Please specify an endpoint, function name and preprocessor.')
    if(exists(fname, envir = where)) {
        warning(glue('{fname} already exists. Skipping.'))
        return(NULL)
    }
    
    # Make sure arguments are URL encoded
    endpoint <- gsub('\\{([^\\}]+)\\}', '\\{encode\\(\\1\\)\\}', endpoint)
    
    f <- function() {}
    
    fargs <- alist()
    for(d in names(defaults)) {
        fargs[[d]] <- defaults[[d]]
    }
    fargs$async <- getOption('gemma.async', F)
    fargs$memoise <- getOption('gemma.memoise', F)
    
    formals(f) <- fargs
    body(f) <- quote({
        # Call a memoised version if applicable
        if(memoise) {
            newArgs <- as.list(match.call())[-1]
            newArgs$memoise <- F
            return(do.call(glue('mem{fname}'), newArgs))
        }
        
        # Validate arguments
        if(!is.null(validators)) {
            for(v in names(validators)) {
                assign(v, eval(validators[[v]])(get(v), name = v))
            }
        }
        
        # Generate request
        endpoint <- paste0(gemmaAPI, gsub('/(NA|/)', '/', gsub('\\?[^=]+=NA', '\\?', gsub('&[^=]+=NA', '', glue(endpoint)))))
        request <- quote(http_get(endpoint)$then(function(response) {
            if(response$status_code == 200)
                eval(preprocessor)(fromJSON(rawToChar(response$content))$data)
            else
                response
        }))
        
        if(!async)
            synchronise(eval(request))
        else
            eval(request)
    })
    
    fenv <- new.env(parent = environment())
    fenv$endpoint <- endpoint
    fenv$validators <- validators
    fenv$preprocessor <- preprocessor
    fenv$fname <- fname
    environment(f) <- fenv
    
    # Make this function available in the parent environment...
    assign(fname, f, env = where)
    # And a memoised one in this function environment
    memF <- glue('mem', fname)
    assign(memF, memoise(f), fenv)
    
    # A disgusting way to hide away the memoised functions
    if(!exists('forgetGemmaMemoised', envir = where))
        assign('forgetGemmaMemoised', function() {}, envir = where)
    
    forgetMe <- get('forgetGemmaMemoised', envir = where)

    body(forgetMe) <- body(forgetMe) %>% as.list %>%
        append(str2expression(glue('forget(environment({fname})${memF})'))) %>% as.call
    
    assign('forgetGemmaMemoised', forgetMe, envir = where)
    
    # Cleanup a bit 
    rm(defaults, d, endpoint, preprocessor, validators, f, memF, fargs, fname, fenv, where, forgetMe)
}

registerSimpleEndpoint <- function(root, query, fname, preprocessor) {
    registerEndpoint(glue('{paste0(root, "s")}/{{{root}}}/{query}'),
                     fname,
                     defaults = setNames(NA_character_, root),
                     validators = validateSingleID,
                     preprocessor = preprocessor,
                     where = parent.env(environment()))
}