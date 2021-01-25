# -------------------------------
# You should define all endpoints in this file. This ensures everything is uniform
# and prevents you from rewriting boilerplate.
# To package the wrapper:
# 1. Source this file after you're done making changes. Functions will be written to allEndpoints.R
# 2. Copy the body of forgetGemmaMemoised to the end of allEndpoints.R
# 3. Build the package as normal
# -------------------------------

#' Register an API endpoint (internal use)
#'
#' @param endpoint The API endpoint URL with parameters in glue formatting
#' @param fname The name of the function to create
#' @param preprocessor The preprocessing function to run on the output
#' @param defaults Default values for the endpoint
#' @param validators Validators for the inputs
#' @param where The environment to add the new function to
#' @param document A file to print information for pasting generating the package
registerEndpoint <- function(endpoint,
                             fname,
                             preprocessor,
                             defaults = NULL,
                             validators = NULL,
                             where = parent.env(environment()),
                             document = NULL) {
  if(missing(endpoint) || missing(fname) || missing(preprocessor))
    stop('Please specify an endpoint, function name and preprocessor.')
  if(exists(fname, envir = where, inherits = F)) {
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
  fargs$raw <- getOption('gemma.raw', T)
  fargs$async <- getOption('gemma.async', F)
  fargs$memoised <- getOption('gemma.memoise', F)

  formals(f) <- fargs
  body(f) <- quote({
    # Call a memoised version if applicable
    if(memoised) {
      newArgs <- as.list(match.call())[-1]
      newArgs$memoised <- F
      return(do.call(glue('mem{fname}'), newArgs))
    }

    # Validate arguments
    if(!is.null(validators)) {
      for(v in names(validators)) {
        assign(v, eval(validators[[v]])(get(v), name = v))
      }
    }

    # Generate request
    endpoint <- paste0(getOption('gemma.API', 'https://gemma.msl.ubc.ca/rest/v2/'), gsub('/(NA|/)', '/', gsub('\\?[^=]+=NA', '\\?', gsub('&[^=]+=NA', '', glue(endpoint)))))
    request <- quote(http_get(endpoint)$then(function(response) {
      if(response$status_code == 200) {
        mData <- fromJSON(rawToChar(response$content))$data
        if(raw)
          mData
        else
          eval(preprocessor)(mData)
      } else
        response
    }))

    if(!async)
      synchronise(eval(request))
    else
      eval(request)
  })

  # Add our variables
  for(i in c('endpoint', 'validators', 'preprocessor', 'fname')) {
    if(is.character(get(i)))
      v <- glue('"{get(i)}"')
    else if(is.list(get(i)))
      v <- get(i) %>% { paste0('list(', paste0(names(.), ' = ', ., collapse = ', '), ')') }
    else
      v <- get(i)

    body(f) <- body(f) %>% as.list %>%
      append(str2expression(glue('{i} <- {v}')), 1) %>%
      as.call
  }

  # And our memoised function
  environment(f) <- where

  # Make this function available in the parent environment...
  assign(fname, f, env = where)
  memF <- glue('mem', fname)
  assign(memF, memoise(f), where)

  # A disgusting way to hide away the memoised functions
  if(!exists('forgetGemmaMemoised', envir = where))
    assign('forgetGemmaMemoised', function() {}, envir = where)

  forgetMe <- get('forgetGemmaMemoised', envir = where)

  body(forgetMe) <- body(forgetMe) %>% as.list %>%
    append(str2expression(glue('forget({memF})'))) %>% as.call

  assign('forgetGemmaMemoised', forgetMe, envir = where)

  if(!is.null(document)) {
    cat(glue("#' {fname}\n#' @export\n\n"), file = document, append = T)
    cat(glue('{fname} <- '), file = document, append = T)
    cat(deparse(f) %>% paste0(collapse = '\n'), file = document, append = T)
    cat('\n\n', file = document, append = T)
    cat(glue("#' Memoise {fname}\n#'\n\n"), file = document, append = T)
    cat(glue('mem{fname} <- memoise::memoise({fname})'), file = document, append = T)
    cat('\n\n', file = document, append = T)
  }

  # Cleanup a bit
  rm(defaults, d, endpoint, preprocessor, validators, f, memF, fargs, fname, where, forgetMe)
}

#' Register a simple endpoint (ie. one that accepts no parameters) (internal use)
#'
#' @param root The type of endpoint (ie. dataset, platform, gene)
#' @param query The last part of the endpoint URL
#' @param fname The name of the function to create
#' @param preprocessor The preprocessing function to run on the output
#' @param document A file to print information for pasting generating the package
registerSimpleEndpoint <- function(root, query, fname, preprocessor, document) {
  registerEndpoint(glue('{paste0(root, "s")}/{{{root}}}/{query}'),
                   fname,
                   defaults = setNames(NA_character_, root),
                   validators = alist(validateSingleID) %>% `names<-`(root),
                   preprocessor = quote(preprocessor),
                   where = parent.env(environment()),
                   document = document)
}

# Dataset endpoints ----
registerEndpoint('datasets/{datasets}?filter={filter}&offset={offset}&limit={limit}&sort={sort}',
                 'getDatasets',
                 defaults = list(datasets = NA_character_,
                                 filter = NA_character_,
                                 offset = 0L,
                                 limit = 20L,
                                 sort = '+id'),
                 validators = alist(datasets = validateID,
                                    filter = validateFilter,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    sort = validateSort),
                 preprocessor = quote(processDatasets), document = 'R/allEndpoints.R')

registerEndpoint('annotations/{taxon}/search/{query}/datasets?filter={filter}&offset={offset}&limit={limit}&sort={sort}',
                 'searchDatasets',
                 defaults = list(taxon = '',
                                 query = NA_character_,
                                 filter = NA_character_,
                                 offset = 0L,
                                 limit = 0L,
                                 sort = '+id'),
                 validators = alist(taxon = validateTaxon,
                                    query = validateQuery,
                                    filter = validateFilter,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    sort = validateSort),
                 preprocessor = quote(processDatasets), document = 'R/allEndpoints.R')

registerEndpoint('datasets/{dataset}/analyses/differential?offset={offset}&limit={limit}',
                 'getDatasetDEA',
                 defaults = list(dataset = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(dataset = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processDEA), document = 'R/allEndpoints.R')

registerEndpoint('datasets/{datasets}/expressions/pca?component={component}&limit={limit}&keepNonSpecific={keepNonSpecific}&consolidate={consolidate}',
                 'getDatasetPCA',
                 defaults = list(datasets = NA_character_,
                                 component = 1L,
                                 limit = 100L,
                                 keepNonSpecific = F,
                                 consolidate = NA_character_),
                 validators = alist(datasets = validateID,
                                    component = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    keepNonSpecific = validateBoolean,
                                    consolidate = validateConsolidate),
                 preprocessor = quote(processExpression), document = 'R/allEndpoints.R')

registerEndpoint('datasets/{datasets}/expressions/differential?keepNonSpecific={keepNonSpecific}&diffExSet={diffExSet}&threshold={threshold}&limit={limit}&consolidate={consolidate}',
                 'getDatasetDE',
                 defaults = list(datasets = NA_character_,
                                 keepNonSpecific = F,
                                 diffExSet = NA_integer_,
                                 threshold = 100.0,
                                 limit = 100L,
                                 consolidate = NA_character_),
                 validators = alist(datasets = validateID,
                                    keepNonSpecific = validateBoolean,
                                    diffExSet = validatePositiveInteger,
                                    threshold = validatePositiveReal,
                                    limit = validatePositiveInteger,
                                    consolidate = validateConsolidate),
                 preprocessor = quote(processExpression), document = 'R/allEndpoints.R')

registerSimpleEndpoint('dataset', 'samples',
                       'getDatasetSamples',
                       preprocessor = quote(processSamples), document = 'R/allEndpoints.R')

registerSimpleEndpoint('dataset', 'svd',
                       'getDatasetSVD',
                       preprocessor = quote(processSVD), document = 'R/allEndpoints.R')

registerSimpleEndpoint('dataset', 'platforms',
                       'getDatasetPlatforms',
                       preprocessor = quote(processPlatforms), document = 'R/allEndpoints.R')

registerSimpleEndpoint('dataset', 'annotations',
                       'getDatasetAnnotations',
                       preprocessor = quote(processAnnotations), document = 'R/allEndpoints.R')

# Platform endpoints ----
registerEndpoint('platforms/{platforms}?filter={filter}&offset={offset}&limit={limit}&sort={sort}',
                 'getPlatforms',
                 defaults = list(platforms = NA_character_,
                                 filter = NA_character_,
                                 offset = 0L,
                                 limit = 20L,
                                 sort = '+id'),
                 validators = alist(filter = validateFilter,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    sort = validateSort),
                 preprocessor = quote(processPlatforms), document = 'R/allEndpoints.R')

registerEndpoint('platforms/{platform}/datasets?offset={offset}&limit={limit}',
                 'getPlatformDatasets',
                 defaults = list(platform = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(platform = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processDatasets), document = 'R/allEndpoints.R')

registerEndpoint('platforms/{platform}/elements?offset={offset}&limit={limit}',
                 'getPlatformElements',
                 defaults = list(platform = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(platform = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processElements), document = 'R/allEndpoints.R')

registerEndpoint('platforms/{platform}/elements/{element}/genes?offset={offset}&limit={limit}',
                 'getPlatformElementGenes',
                 defaults = list(platform = NA_character_,
                                 element = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(platform = validateSingleID,
                                    element = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processGenes), document = 'R/allEndpoints.R')

# Gene endpoints ----
registerSimpleEndpoint('gene', '',
                       'getGenes',
                       preprocessor = quote(processGenes), document = 'R/allEndpoints.R')

registerSimpleEndpoint('gene', 'evidence',
                       'getGeneEvidence',
                       preprocessor = quote(processGeneEvidence), document = 'R/allEndpoints.R')

registerSimpleEndpoint('gene', 'locations',
                       'getGeneLocation',
                       preprocessor = quote(processGeneLocation), document = 'R/allEndpoints.R')

registerEndpoint('genes/{gene}/probes?offset={offset}&limit={limit}',
                 'getGeneProbes',
                 defaults = list(gene = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(gene = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processElements), document = 'R/allEndpoints.R')

registerSimpleEndpoint('gene', 'goTerms',
                       'getGeneGO',
                       preprocessor = quote(processGO), document = 'R/allEndpoints.R')

registerEndpoint('genes/{gene}/coexpression?with={with}&limit={limit}&stringency={stringency}',
                 'getGeneCoexpression',
                 defaults = list(gene = NA_character_,
                                 with = NA_character_,
                                 limit = 20L,
                                 stringency = 1L),
                 validators = alist(gene = validateSingleID,
                                    with = validateSingleID,
                                    limit = validatePositiveInteger,
                                    stringency = validatePositiveInteger),
                 preprocessor = quote(processCoexpression), document = 'R/allEndpoints.R')

# Taxon endpoints ----
registerEndpoint('taxa/{taxon}/datasets?filter={filter}&offset={offset}&limit={limit}&sort={sort}',
                 'getTaxonDatasets',
                 defaults = list(taxon = NA_character_,
                                 filter = NA_character_,
                                 offset = 0L,
                                 limit = 20L,
                                 sort = '+id'),
                 validators = alist(taxon = validateSingleID,
                                    filter = validateFilter,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger,
                                    sort = validateSort),
                 preprocessor = quote(processDatasets), document = 'R/allEndpoints.R')

registerEndpoint('taxa/{taxon}/phenotypes?editableOnly={editableOnly}&tree={tree}',
                 'getTaxonPhenotypes',
                 defaults = list(taxon = NA_character_,
                                 editableOnly = F,
                                 tree = F),
                 validators = alist(taxon = validateSingleID,
                                    editableOnly = validateBoolean,
                                    tree = validateBoolean),
                 preprocessor = quote(processPhenotypes), document = 'R/allEndpoints.R')

registerEndpoint('taxa/{taxon}/phenotypes/candidates?editableOnly={editableOnly}&phenotypes={phenotypes}',
                 'getTaxonPhenotypeCandidates',
                 defaults = list(taxon = NA_character_,
                                 editableOnly = F,
                                 phenotypes = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    editableOnly = validateBoolean,
                                    phenotypes = validateSingleID),
                 preprocessor = quote(processGeneEvidence), document = 'R/allEndpoints.R')

registerEndpoint('taxa/{taxon}/genes/{gene}',
                 'getGeneOnTaxon',
                 defaults = list(taxon = NA_character_,
                                 gene = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    gene = validateSingleID),
                 preprocessor = quote(processGenes), document = 'R/allEndpoints.R')

registerEndpoint('taxa/{taxon}/genes/{gene}/evidence',
                 'getEvidenceOnTaxon',
                 defaults = list(taxon = NA_character_,
                                 gene = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    gene = validateSingleID),
                 preprocessor = quote(processGeneEvidence), document = 'R/allEndpoints.R')

registerEndpoint('taxa/{taxon}/genes/{gene}/locations',
                 'getGeneLocationOnTaxon',
                 defaults = list(taxon = NA_character_,
                                 gene = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    gene = validateSingleID),
                 preprocessor = quote(processGeneLocation), document = 'R/allEndpoints.R')

registerEndpoint('taxa/{taxon}/chromosomes/{chromosome}/genes?strand={strand}&start={start}&size={size}',
                 'getGenesAtLocation',
                 defaults = list(taxon = NA_character_,
                                 chromosome = NA_character_,
                                 strand = '+',
                                 start = NA_integer_,
                                 size = NA_integer_),
                 validators = alist(taxon = validateSingleID,
                                    chromosome = validateSingleID,
                                    strand = validateStrand,
                                    start = validatePositiveInteger,
                                    size = validatePositiveInteger),
                 preprocessor = quote(processGenes), document = 'R/allEndpoints.R')
