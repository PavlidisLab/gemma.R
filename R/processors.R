#' Prototype function body
#'
#' This should not be called directly, but is called from the API functions.
#'
#' @param memoised Whether or not to memoise results
#' @param fname The function name
#' @param validators Validators for parameters
#' @param endpoint Formatted endpoint URL
#' @param envWhere Environment to evaluate in
#' @param isFile Whether or not the endpoint is expect to return a file
#' @param raw Whether to return JSON (`TRUE`) or data.table (`FALSE`)
#' @param overwrite Whether or not to overwrite the file if @param file is specified
#' @param file A filename to save results to
#' @param async Whether or not to run asynchronously
#' @param .call The original function call
#'
#' @keywords internal
.body <- function(memoised, fname, validators, endpoint, envWhere, isFile, raw, overwrite, file, async, .call) {
  # Call a memoised version if applicable
  if(memoised) {
    newArgs <- as.list(.call)[-1]
    newArgs$memoised <- F
    return(do.call(glue::glue('mem{fname}'), newArgs))
  }

  # Validate arguments
  if(!is.null(validators)) {
    for(v in names(validators)) {
      assign(v, eval(validators[[v]])(get(v, envir = envWhere, inherits = F), name = v), envir = envWhere)
    }
  }

  # Generate request
  request <- quote(http_get(
    paste0(getOption('gemma.API', 'https://gemma.msl.ubc.ca/rest/v2/'), gsub('/((NA)?/)', '/', gsub('\\?[^=]+=NA', '\\?', gsub('&[^=]+=NA', '', glue::glue(endpoint))))),
    options = switch(is.null(getOption('gemma.password', NULL)) + 1, list(userpwd = paste0(getOption('gemma.username'), ':', getOption('gemma.password'))), list()))$then(function(response) {
      if(response$status_code == 200) {
        mData <- tryCatch({
          if(isFile) response
          else jsonlite::fromJSON(rawToChar(response$content))$data
        }, error = function(e) {
          message(paste0('Failed to parse ', response$type, ' from ', response$url))
          warning(e$message)
          NULL
        })

        if(raw || length(mData) == 0)
          mOut <- mData
        else
          mOut <- eval(preprocessor)(mData)

        if(!is.null(file) && !is.na(file)) {
          extension <- ifelse(raw, '.json', ifelse(any(sapply(mOut, typeof) == 'list'), '.rds', '.csv'))
          file <- paste0(tools::file_path_sans_ext(file), extension)

          if(file.exists(file) && !overwrite && !file.info(file)$isdir)
            warning(paste0(file, ' exists. Not overwriting.'))
          else {
            if(extension == '.json')
              write(jsonlite::toJSON(mOut, pretty = 2), file)
            else if(extension == '.rds')
              saveRDS(mOut, file)
            else
              write.csv2(mOut, file, row.names = F)
          }
        }

        mOut
      } else
        response
    }))

  if(!async)
    synchronise(eval(request, envir = envWhere))
  else
    eval(request, envir = envWhere)
}

#' URL encode a string safely
#'
#' @param url The string to URL encode. Vectors are delimited by a comma.
#'
#' @return A URL encoding of url
#'
#' @keywords internal
encode <- function(url) {
  if(is.na(url) || !is.character(url)) url
  else {
    if(length(url) > 1)
      url <- paste0(url, collapse = ',')
    utils::URLencode(url, T)
  }
}

#' Processes JSON as a factor
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processGemmaFactor <- function(d) {
  if(all(is.na(d)))
    data.table(name = NA_character_, URL = NA_character_,
               description = NA_character_, category.Name = NA_character_,
               category.URL = NA_character_, measurement = F, type = NA_character_)
  else
    data.table(name = switch(is.null(d[['factorValue']]) + 1, d[['factorValue']], d[['value']]),
               URL = d[['valueUri']],
               description = d[['description']],
               category.Name = d[['category']],
               category.URL = d[['categoryUri']],
               measurement = d[['measurement']],
               type = d[['type']])
}

#' Processes JSON as an array
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processGemmaArray <- function(d) {
  data.table(array.ShortName = d[['shortName']],
             array.Name = d[['name']],
             array.Taxon = d[['taxon']],
             array.TaxonID = d[['taxonID']],
             array.Type = d[['technologyType']],
             array.Description = d[['description']],
             array.Troubled = d[['troubled']])
}

#' Processes JSON as a vector of datasets
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processDatasets <- function(d) {
  data.table(ee.ShortName = d[['shortName']],
             ee.Name = d[['name']],
             ee.ID = d[['id']],
             ee.Description = d[['description']],
             ee.Public = d[['isPublic']],
             ee.Troubled = d[['troubled']],
             ee.Accession = d[['accession']],
             ee.Database = d[['externalDatabase']],
             ee.URL = d[['externalUri']],
             ee.Samples = d[['bioAssayCount']],
             ee.LastUpdated = as.POSIXct(d[['lastUpdated']] / 1e3, origin = '1970-01-01'),
             ee.Batch.P = suppressWarnings(as.numeric(gsub('.*p=(\\d+\\.?\\d+).*', '\\1', d[['batchEffect']]))),
             ee.Batch.PC = suppressWarnings(as.numeric(gsub('.*PC (\\d+).*', '\\1', d[['batchEffect']]))),
             geeq.batchCorrected = d[['geeq']][['batchCorrected']],
             geeq.qScore = d[['geeq']][['publicQualityScore']],
             geeq.sScore = d[['geeq']][['publicSuitabilityScore']],
             taxon.Name = d[['taxon']],
             taxon.ID = d[['taxonId']],
             technology.Type = d[['technologyType']])
}

#' Processes JSON as an annotation
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processAnnotations <- function(d) {
  data.table(category.Name = d[['category']],
             category.URL = d[['categoryUri']],
             value.Name = d[['value']],
             value.URL = d[['valueUri']])
}

#' Processes JSON as a differential expression analysis
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processDEA <- function(d) {
  divides <- data.table(analysis.ID = d[['id']],
                        ee.ID = d[['sourceExperiment']],
                        sf.Enabled = d[['subset']],
                        sf = processGemmaFactor(d[['subsetFactorValue']]),
                        resultIds = lapply(d[['resultSets']], '[[', 'resultSetId')) %>%
    .[, .(result.ID = unlist(resultIds)), setdiff(names(.), 'resultIds')]

  rs <- lapply(d[['resultSets']], function(r) {
    data.table(analysis.Threshold = r[['threshold']],
               analysis.ID = r[['analysisId']],
               result.ID = r[['resultSetId']],
               stats.DE = r[['numberOfDiffExpressedProbes']],
               stats.Down = r[['downregulatedCount']],
               stats.Up = r[['upregulatedCount']],
               probes.Analyzed = r[['numberOfProbesAnalyzed']],
               genes.Analyzed = r[['numberOfGenesAnalyzed']],
               factor.ID = r[['factorIds']],
               r[['baselineGroup']] %>% {
                 data.table(cf.Cat = .[['category']],
                            cf.CatLongUri = .[['categoryUri']],
                            cf.Baseline = .[['factorValue']],
                            cf.BaseLongUri = .[['valueUri']])
               }) %>%
      .[, .(factor.ID = unlist(factor.ID)), setdiff(names(.), 'factor.ID')]
  }) %>% rbindlist %>% .[!is.na(cf.Baseline)]

  rsd <- merge(rs, divides, by = c('result.ID', 'analysis.ID'), all = T)
  lapply(unique(rsd[, factor.ID]), function(fid) {
    lapply(d$factorValuesUsed[[as.character(fid)]], function(fv) {
      if(!is.null(fv) && nrow(fv) > 0) {
        as.data.table(fv)[!(factorValue %in% rsd[factor.ID == fid, unique(cf.Baseline)]),
                          .(cf.Val = factorValue,
                            cf.ValLongUri = valueUri,
                            factor.ID = factorId,
                            id)]
      }
    }) %>% .[lengths(.) > 0] %>% rbindlist
  }) %>% rbindlist %>% unique %>% merge(rsd, by = 'factor.ID', allow.cartesian = T, all = T) %>%
    merge(data.table(analysis.ID = d[['id']],
                     ad.ID = d[['arrayDesignsUsed']]), by = 'analysis.ID', all = T) %>%
    .[, .(rsc.ID = paste('RSCID', result.ID, id, sep = '.'),
          analysis.ID,
          ee.ID, cf.Cat, cf.CatLongUri, cf.Baseline, cf.BaseLongUri,
          cf.Val, cf.ValLongUri, sf.Subset = sf.Enabled,
          sf.Cat = sf.category.Name, sf.CatLongUri = sf.category.URL,
          sf.Val = sf.name, sf.ValLongUri = sf.URL,
          stats.DE, stats.Down, stats.Up, analysis.Threshold, probes.Analyzed,
          genes.Analyzed, ad.ID), .(result.ID, id)] %>% .[, !c('result.ID', 'id')]
}

#' Processes JSON as expression data
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processExpression <- function(d) {
  expr <- lapply(d[['geneExpressionLevels']], function(x) {
    n.reps <- unlist(lapply(x[['vectors']], nrow))

    data.table(gene.ID = rep(x[['geneNcbiId']], n.reps),
               gene.Symbol = rep(x[['geneOfficialSymbol']], n.reps),
               probe = unlist(lapply(x[['vectors']], '[[', 'designElementName')),
               rbindlist(lapply(x[['vectors']], '[[', 'bioAssayExpressionLevels')))
  })
  if(length(d[['datasetId']]) > 0)
    data.table(ee.ID = d[['datasetId']], expr)
  else
    expr
}

#' Processes JSON as an experiment's SVD
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processSVD <- function(d) {
  vM <- d$vMatrix$rawMatrix
  colnames(vM) <- d$vMatrix$colNames
  rownames(vM) <- d$vMatrix$rowNames
  list(variance = d$variances,
       VMatrix = vM)
}

#' Processes JSON as a result set
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
# TODO: Finish implementation
processResultSets <- function(d){
  data.table(rs.ID = d[['id']])
}

#' Processes JSON as annotations
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processAnnotations <- function(d) {
  data.table(class.Type = d[['objectClass']],
             class.Name = d[['className']],
             class.URL = d[['classUri']],
             evidence.Code = d[['evidenceCode']],
             term.Name = d[['termName']],
             term.URL = d[['termUri']])
}

#' Processes a response as a gzip file
#'
#' @param response The response from an `http_get` request
#'
#' @return A processed data.table
#'
#' @keywords internal
processFile <- function(response) {
  tmp <- tempfile() # Make a temp file
  writeBin(response$content, tmp) # Save to that file
  tmp2 <- gzfile(tmp)
  ret <- tmp2 %>% readLines %>%
    .[which(!startsWith(., '#'))[1]:length(.)] %>% # Strip comments
    paste0(collapse = '\n') %>% {
    fread(text = .)
    }
  close(tmp2)
  unlink(tmp) # Delete the temp file
  ret
}

#' Processes JSON as a vector of samples
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processSamples <- function(d) {
  data.table(bioMaterial.Name = d[['sample']][['name']],
             sample.Name = d[['name']],
             sample.ID = d[['sample']][['id']],
             sample.Correspondence = d[['sample']][['description']],
             sample.Description = d[['description']],
             sample.Outlier = d[['outlier']],
             sample.Accession = d[['accession']][['accession']],
             sample.Database = d[['accession']][['externalDatabase.name']],
             sample.Processed = d[['processingDate']],
             sample.Characteristics = lapply(d[['sample']][['characteristics']], processGemmaFactor),
             sample.FactorValues = lapply(lapply(d[['sample']][['factorValueObjects']], '[[', 'characteristics'), function(x) processGemmaFactor(rbindlist(x))),
             processGemmaArray(d[['arrayDesign']]))
}

#' Processes JSON as a vector of platforms
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processPlatforms <- function(d) {
  data.table(platform.ID = d[['id']],
             platform.ShortName = d[['shortName']],
             platform.Name = d[['name']],
             platform.Description = d[['description']],
             platform.Troubled = d[['troubled']],
             platform.ExperimentCount = d[['expressionExperimentCount']],
             platform.GeneCount = d[['numGenes']],
             platform.ProbeSequenceCount = d[['numProbeSequences']],
             platform.ProbeAlignmentCount = d[['numProbeAlignments']],
             platform.ProbeGeneCount = d[['numProbesToGenes']],
             platform.ElementCount = d[['designElementCount']],
             taxon.Name = d[['taxon']],
             taxon.ID = d[['taxonID']],
             technology.Type = d[['technologyType']],
             technology.Color = d[['color']])
}

#' Processes JSON as a vector of elements
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processElements <- function(d) {
  data.table(mapping.Name = d[['name']],
             mapping.Summary = ifelse(!is.na(d[['geneMappingSummaries']]),
                                      lapply(d[['geneMappingSummaries']], function(x) {
                                        data.table(score = x[['score']],
                                                   products = lapply(x[['geneProducts']], function(y) {
                                                     data.table(gene.Name = y[['name']],
                                                     gene.ID = y[['geneId']],
                                                     gene.NCBI = y[['ncbiId']],
                                                     chromosome = y[['chromosome']],
                                                     strand = y[['strand']],
                                                     nucleotide.Start = y[['nucleotideStart']],
                                                    nucleotide.End = y[['nucleotideEnd']])
                                                     }),
                                                   productMaps = lapply(x[['geneProductMap']], processGenes))
                                        }),
                                      NA),
             mapping.Description = d[['description']],
             processGemmaArray(d[['arrayDesign']]))
}

#' Processes JSON as a vector of genes
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processGenes <- function(d) {
  data.table(gene.Symbol = d[['officialSymbol']],
             gene.Ensembl = d[['ensemblId']],
             gene.NCBI = d[['ncbiId']],
             gene.Name = d[['officialName']],
             gene.Aliases = d[['aliases']],
             gene.Description = d[['description']],
             gene.GO = d[['numGoTerms']],
             gene.Homologues = d[['homologues']],
             gene.MFX.Rank = d[['multifunctionalityRank']],
             taxon.Name = d[['taxonCommonName']],
             taxon.ID = d[['taxonId']],
             taxon.Scientific = d[['taxonScientificName']],
             phenotypes = d[['phenotypes']])
}

#' Processes JSON as a vector of gene evidence (which also contains genes)
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processGeneEvidence <- function(d) {
  suppressWarnings(data.table(evidence = lapply(d[['evidence']], function(x) {
    data.table(gene.ID = x[['geneId']],
               gene.NCBI = x[['geneNCBI']],
               gene.Symbol = x[['geneOfficialSymbol']],
               gene.Name = x[['geneOfficialName']],
               relationship = x[['relationship']],
               phenotypes = lapply(x[['phenotypes']], processPhenotypes),
               negative = x[['isNegativeEvidence']],
               description = x[['description']],
               URL = x[['evidenceSource']][['externalUrl']],
               code = x[['evidenceCode']],
               accession = x[['evidenceSource']][['accession']],
               rbindlist(lapply(x[['phenotypeAssPubVO']], function(y) {
                 data.table(type = y[['type']],
                            pubmed.URL = y[['citationValueObject']][['pubmedURL']],
                            pubmed.Accession = y[['citationValueObject']][['pubmedAccession']],
                            citation = y[['citationValueObject']][['citation']],
                            retracted = y[['citationValueObject']][['retracted']])
               })))
  }), processGenes(d)))
}

#' Processes JSON as a vector of taxa
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processTaxon <- function(d) {
  data.table(taxon.Name = d[['commonName']],
             taxon.Scientific = d[['scientificName']],
             taxon.ID = d[['id']],
             taxon.NCBI = d[['ncbiId']],
             taxon.Database.Name = d[['externalDatabase']][['name']],
             taxon.Database.ID = d[['externalDatabase.ID']])
}

#' Processes JSON as a vector of gene locations
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processGeneLocation <- function(d) {
  data.table(chromosome = d[['chromosome']],
             strand = d[['strand']],
             bin = d[['bin']],
             nucleotide = d[['nucleotide']],
             length = d[['nucleotideLength']],
             processTaxon(d[['taxon']]))
}

#' Processes JSON as GO terms
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processGO <- function(d) {
  data.table(term.Name = d[['term']],
             term.ID = d[['goId']],
             term.URL = d[['uri']])
}

#' Processes JSON as coexpression data
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processCoexpression <- function(d) {
  data.table(query.Degree = d[['queryGeneNodeDegree']],
             query.Rank = d[['queryGeneNodeDegreeRank']],
             found.Degree = d[['foundGeneNodeDegree']],
             found.Rank = d[['foundGeneNodeDegreeRank']],
             tested = d[['numTestedIn']],
             support.Positive = d[['posSupp']],
             support.Negative = d[['negSupp']],
             support.N = d[['support']],
             support.Experiments = d[['supportingExperiments']],
             setnames(processGenes(d[['queryGene']]), function(x) paste0('query.', x)),
             setnames(processGenes(d[['foundGene']]), function(x) paste0('found.', x)))
}

#' Processes JSON as a vector of phenotypes
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processPhenotypes <- function(d) {
  data.table(value.Name = d[['factorValue']],
             value.URL = d[['valueUri']],
             value.GeneCount = d[['publicGeneCount']],
             parents = lapply(d[['_parent']], function(x) paste0('http://purl.obolibrary.org/obo/', unlist(strsplit(x, '<', T)))),
             children = lapply(d[['children']], function(x) paste0('http://purl.obolibrary.org/obo/', unlist(strsplit(x, '<', T)))))
}
