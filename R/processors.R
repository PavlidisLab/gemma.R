#' URL encode a string safely
#'
#' @param url The string to URL encode. Vectors are delimited by a comma.
#'
#' @return A URL encoding of url
encode <- function(url) {
    if(is.na(url) || !is.character(url)) url
    else {
        if(length(url) > 1)
            url <- paste0(url, collapse = ',')
        URLencode(url, T)
    }
}

#' Processes JSON as a factor
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
processGemmaFactor <- function(d) {
    data.table(name = d[['value']],
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
processDatasets <- function(d) {
    data.table(experiment.ShortName = d[['shortName']],
               experiment.Name = d[['name']],
               experiment.ID = d[['id']],
               experiment.Description = d[['description']],
               experiment.Public = d[['isPublic']],
               experiment.Troubled = d[['troubled']],
               experiment.Accession = d[['accession']],
               experiment.Database = d[['externalDatabase']],
               experiment.URL = d[['externalUri']],
               experiment.Samples = d[['bioAssayCount']],
               experiment.Batch.P = suppressWarnings(as.numeric(gsub('.*p=(\\d+\\.?\\d+).*', '\\1', d[['batchEffect']]))),
               experiment.Batch.PC = suppressWarnings(as.numeric(gsub('.*PC (\\d+).*', '\\1', d[['batchEffect']]))),
               geeq.batchCorrected = d[['geeq']][['batchCorrected']],
               geeq.qScore = d[['geeq']][['publicQualityScore']],
               geeq.sScore = d[['geeq']][['publicSuitabilityScore']],
               taxon.Name = d[['taxon']],
               taxon.ID = d[['taxonId']],
               technology.Type = d[['technologyType']])
}

#' Processes JSON as a differential expression analysis
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
processDEA <- function(d) {
    data.table(analysis.ID = d[['id']],
               subset.Enabled = d[['subset']],
               subset.Factor = d[['subsetFactor']],
               subset.FactorValue = d[['subsetFactorValue']],
               factors = list(lapply(d[['factorValuesUsed']], function(x) {
                   rbindlist(lapply(x, processGemmaFactor))
               })),
               results = lapply(d[['resultSets']], function(x) {
                   data.table(ID = x[['resultSetId']],
                              threshold = x[['threshold']],
                              qValue = x[['qvalue']],
                              upregulated = x[['upregulatedCount']],
                              downregulated = x[['downregulatedCount']],
                              probes.DE = x[['numberOfDiffExpressedProbes']],
                              probes.Analyzed = x[['numberOfProbesAnalyzed']],
                              genes.Analyzed = x[['numberOfGenesAnalyzed']],
                              factors = lapply(x[['experimentalFactors']], function(y) {
                                  data.table(processGemmaFactor(y),
                                             values = lapply(y[['values']], processGemmaFactor))
                              }),
                              baseline = list(
                                  processGemmaFactor(x[['baselineGroup']])
                              ))
               }))
}

#' Processes JSON as expression data
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
processExpression <- function(d) {
    expr <- lapply(d[['geneExpressionLevels']], function(x) {
        n.reps <- unlist(lapply(x[['vectors']], nrow))

        data.table(gene.ID = rep(x[['geneNcbiId']], n.reps),
                   gene.Symbol = rep(x[['geneOfficialSymbol']], n.reps),
                   probe = unlist(lapply(x[['vectors']], '[[', 'designElementName')),
                   rbindlist(lapply(x[['vectors']], '[[', 'bioAssayExpressionLevels')))
    })
    if(length(d[['datasetId']]) > 0)
        data.table(experiment.ID = d[['datasetId']], expr)
    else
        expr
}

#' Processes JSON as an experiment's SVD
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
processSVD <- function(d) {
    list(variance = d$variances,
         VMatrix = d$vMatrix$rawMatrix %>% `colnames<-`(d$vMatrix$colNames) %>% `rownames<-`(d$vMatrix$rowNames))
}

#' Processes JSON as annotations
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
processAnnotations <- function(d) {
    data.table(class.Type = d[['objectClass']],
               class.Name = d[['className']],
               class.URL = d[['classUri']],
               evidence.Code = d[['evidenceCode']],
               term.Name = d[['termName']],
               term.URL = d[['termUri']])
}

#' Processes JSON as a vector of samples
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
processSamples <- function(d) {
    data.table(sample.Name = d[['sample']][['name']],
               sample.ID = d[['sample']][['id']],
               sample.Description = d[['sample']][['description']],
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
processElements <- function(d) {
    data.table(mapping.Name = d[['name']],
               mapping.Summary = lapply(d[['geneMappingSummaries']], function(x) {
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
               mapping.Description = d[['description']],
               processGemmaArray(d[['arrayDesign']]))
}

#' Processes JSON as a vector of genes
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
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
processGeneEvidence <- function(d) {
    data.table(evidence = lapply(d[['evidence']], function(x) {
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
    }), processGenes(d))
}

#' Processes JSON as a vector of gene locations
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
processGeneLocation <- function(d) {
    data.table(chromosome = d[['chromosome']],
               strand = d[['strand']],
               bin = d[['bin']],
               nucleotide = d[['nucleotide']],
               length = d[['nucleotideLength']],
               taxon.Name = d[['taxon']][['commonName']],
               taxon.Scientific = d[['taxon']][['scientificName']],
               taxon.ID = d[['taxon']][['id']],
               taxon.NCBI = d[['taxon']][['ncbiId']],
               taxon.Database.Name = d[['taxon']][['externalDatabase']][['name']],
               taxon.Database.ID = d[['taxon']][['externalDatabase.ID']])
}

#' Processes JSON as GO terms
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
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
               processGenes(d[['queryGene']]) %>% setnames(paste0('query.', colnames(.))),
               processGenes(d[['foundGene']]) %>% setnames(paste0('found.', colnames(.))))
}

#' Processes JSON as a vector of phenotypes
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
processPhenotypes <- function(d) {
    data.table(value.Name = d[['value']],
               value.URL = d[['valueUri']],
               value.GeneCount = d[['publicGeneCount']],
               parents = lapply(d[['_parent']], function(x) paste0('http://purl.obolibrary.org/obo/', unlist(strsplit(x, '<', T)))),
               children = lapply(d[['children']], function(x) paste0('http://purl.obolibrary.org/obo/', unlist(strsplit(x, '<', T)))))
}
