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
                 preprocessor = quote(processDatasets))

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
                 preprocessor = quote(processDatasets))

registerEndpoint('datasets/{dataset}/analyses/differential?offset={offset}&limit={limit}',
                 'getDatasetDEA',
                 defaults = list(dataset = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(dataset = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processDEA))

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
                 preprocessor = quote(processExpression))

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
                 preprocessor = quote(processExpression))

registerSimpleEndpoint('dataset', 'samples',
                       'getDatasetSamples',
                       preprocessor = quote(processSamples))

registerSimpleEndpoint('dataset', 'svd',
                       'getDatasetSVD',
                       preprocessor = quote(processSVD))

registerSimpleEndpoint('dataset', 'platforms',
                       'getDatasetPlatforms',
                       preprocessor = quote(processPlatforms))

registerSimpleEndpoint('dataset', 'annotations',
                       'getDatasetAnnotations',
                       preprocessor = quote(processAnnotations))

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
                 preprocessor = quote(processPlatforms))

registerEndpoint('platforms/{platform}/datasets?offset={offset}&limit={limit}',
                 'getPlatformDatasets',
                 defaults = list(platform = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(platform = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processDatasets))

registerEndpoint('platforms/{platform}/elements?offset={offset}&limit={limit}',
                 'getPlatformElements',
                 defaults = list(platform = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(platform = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processElements))

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
                 preprocessor = quote(processGenes))

# Gene endpoints ----
registerSimpleEndpoint('gene', '',
                       'getGenes',
                       preprocessor = quote(processGenes))

registerSimpleEndpoint('gene', 'evidence',
                       'getGeneEvidence',
                       preprocessor = quote(processGeneEvidence))

registerSimpleEndpoint('gene', 'locations',
                       'getGeneLocation',
                       preprocessor = quote(processGeneLocation))

registerEndpoint('genes/{gene}/probes?offset={offset}&limit={limit}',
                 'getGeneProbes',
                 defaults = list(gene = NA_character_,
                                 offset = 0L,
                                 limit = 20L),
                 validators = alist(gene = validateSingleID,
                                    offset = validatePositiveInteger,
                                    limit = validatePositiveInteger),
                 preprocessor = quote(processGeneProbes))

registerSimpleEndpoint('gene', 'goTerms',
                       'getGeneGO',
                       preprocessor = quote(processGO))

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
                 preprocessor = quote(processCoexpression))

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
                 preprocessor = quote(processDatasets))

registerEndpoint('taxa/{taxon}/phenotypes?editableOnly={editableOnly}&tree={tree}',
                 'getTaxonPhenotypes',
                 defaults = list(taxon = NA_character_,
                                 editableOnly = F,
                                 tree = F),
                 validators = alist(taxon = validateSingleID,
                                    editableOnly = validateBoolean,
                                    tree = validateBoolean),
                 preprocessor = quote(processPhenotypes))

registerEndpoint('taxa/{taxon}/phenotypes/candidates?editableOnly={editableOnly}&phenotypes={phenotypes}',
                 'getTaxonPhenotypeCandidates',
                 defaults = list(taxon = NA_character_,
                                 editableOnly = F,
                                 phenotypes = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    editableOnly = validateBoolean,
                                    phenotypes = validateSingleID),
                 preprocessor = quote(processGeneEvidence))

registerEndpoint('taxa/{taxon}/genes/{gene}',
                 'getGeneOnTaxon',
                 defaults = list(taxon = NA_character_,
                                 gene = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    gene = validateSingleID),
                 preprocessor = quote(processGenes))

registerEndpoint('taxa/{taxon}/genes/{gene}/evidence',
                 'getEvidenceOnTaxon',
                 defaults = list(taxon = NA_character_,
                                 gene = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    gene = validateSingleID),
                 preprocessor = processGeneEvidence)

registerEndpoint('taxa/{taxon}/genes/{gene}/locations',
                 'getGeneLocationOnTaxon',
                 defaults = list(taxon = NA_character_,
                                 gene = NA_character_),
                 validators = alist(taxon = validateSingleID,
                                    gene = validateSingleID),
                 preprocessor = quote(processGeneLocation))

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
                 preprocessor = quote(processGenes))