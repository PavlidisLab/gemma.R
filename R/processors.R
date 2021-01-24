processDatasets <- function(d) {
    data.table(experiment.ShortName = d$shortName,
               experiment.Name = d$name,
               experiment.Description = d$description,
               experiment.Public = d$isPublic,
               experiment.Troubled = d$troubled,
               experiment.Accession = d$accession,
               experiment.Database = d$externalDatabase,
               experiment.URL = d$externalUri,
               experiment.Samples = d$bioAssayCount,
               experiment.Batch.P = suppressWarnings(as.numeric(gsub('.*p=(\\d+\\.?\\d+).*', '\\1', d$batchEffect))),
               experiment.Batch.PC = suppressWarnings(as.numeric(gsub('.*PC (\\d+).*', '\\1', d$batchEffect))),
               geeq.batchCorrected = d$geeq$batchCorrected,
               geeq.qScore = d$geeq$publicQualityScore,
               geeq.sScore = d$geeq$publicSuitabilityScore,
               taxon.Name = d$taxon,
               taxon.ID = d$taxonId,
               technology.Type = d$technologyType)
}

processDEA <- function(d) {
    data.table(subset.Enabled = d$subset,
               subset.Factor = d$subsetFactor,
               subset.FactorValue = d$subsetFactorValue,
               factors = list(lapply(d$factorValuesUsed, function(x) {
                   rbindlist(lapply(x, as.gemma.factor))
               })),
               results = lapply(d$resultSets, function(x) {
                   data.table(ID = x$resultSetId,
                              threshold = x$threshold,
                              qValue = x$qvalue,
                              upregulated = x$upregulatedCount,
                              downregulated = x$downregulatedCount,
                              probes.DE = x$numberOfDiffExpressedProbes,
                              probes.Analyzed = x$numberOfProbesAnalyzed,
                              genes.Analyzed = x$numberOfGenesAnalyzed,
                              factors = lapply(x$experimentalFactors, function(y) {
                                  data.table(as.gemma.factor(y),
                                             values = lapply(y$values, as.gemma.factor))
                              }),
                              baseline = list(
                                  as.gemma.factor(x$baselineGroup)
                              ))
               }))
}

processExpression <- function(d) {
    expr <- lapply(d$geneExpressionLevels, function(x) {
        n.reps <- unlist(lapply(x$vectors, nrow))
        
        data.table(gene.ID = rep(x$geneNcbiId, n.reps),
                   gene.Symbol = rep(x$geneOfficialSymbol, n.reps),
                   probe = unlist(lapply(x$vectors, '[[', 'designElementName')),
                   rbindlist(lapply(x$vectors, '[[', 'bioAssayExpressionLevels')))
    })
    if(length(d$datasetId) > 0)
        data.table(experiment.ID = d$datasetId, expr)
    else
        expr
}

processSVD <- function(d) {
    # TODO Stub
}

processAnnotations <- function(d) {
    data.table(class.Type = d$objectClass,
               class.Name = d$className,
               class.URL = d$classUri,
               evidence.Code = d$evidenceCode,
               term.Name = d$termName,
               term.URL = d$termUri)
}

processSamples <- function(d) {
    data.table(sample.Name = d$sample$name,
               sample.ID = d$sample$id,
               sample.Description = d$sample$description,
               sample.Outlier = d$outlier,
               sample.Accession = d$accession$accession,
               sample.Database = d$accession$externalDatabase.name,
               sample.Processed = d$processingDate,
               sample.Characteristics = lapply(d$sample$characteristics, as.gemma.factor),
               sample.FactorValues = lapply(lapply(d$sample$factorValueObjects, '[[', 'characteristics'), function(x) as.gemma.factor(rbindlist(x))),
               as.gemma.array(d$arrayDesign))
}

processPlatforms <- function(d) {
    data.table(platform.ShortName = d$shortName,
               platform.Name = d$name,
               platform.Description = d$description,
               platform.Troubled = d$troubled,
               platform.ExperimentCount = d$expressionExperimentCount,
               taxon.Name = d$taxon,
               taxon.ID = d$taxonID,
               technology.Type = d$technologyType,
               technology.Color = d$color)
}

processElements <- function(d) {
    # TODO Stub
    data.table(element.Name = d$name,
               element.Description = d$description,
               as.gemma.array(d$arrayDesign))
}

processGenes <- function(d) {
    # TODO Stub
    data.table(gene.Symbol = d$officialSymbol,
               gene.Ensembl = d$ensemblId,
               gene.ID = d$ncbiId,
               gene.Name = d$officialName,
               gene.Aliases = d$aliases,
               gene.Description = d$description,
               gene.GO = d$numGoTerms,
               gene.Homologues = d$homologues,
               gene.MFX.Rank = d$multifunctionalityRank,
               taxon.Name = d$taxonCommonName,
               taxon.ID = d$taxonId,
               taxon.Scientific = d$taxonScientificName,
               phenotypes = d$phenotypes)
}

processGeneEvidence <- function(d) {
    # TODO Stub
    data.table(TODO = 'Stub',
               processGenes(d))
}

processGeneLocation <- function(d) {
    data.table(chromosome = d$chromosome,
               strand = d$strand,
               bin = d$bin,
               nucleotide = d$nucleotide,
               length = d$nucleotideLength,
               taxon.Name = d$taxon$commonName,
               taxon.Scientific = d$taxon$scientificName,
               taxon.ID = d$taxon$id,
               taxon.NCBI = d$taxon$ncbiId,
               taxon.Database.Name = d$taxon$externalDatabase$name,
               taxon.Database.ID = d$taxon$externalDatabase.ID)
}

processGeneProbes <- function(d) {
    # TODO Stub
    data.table(TODO = 'Stub',
               as.gemma.array(d$arrayDesign))
}

processGO <- function(d) {
    data.table(term.Name = d$term,
               term.ID = d$goId,
               term.URL = d$uri)
}

processCoexpression <- function(d) {
    data.table(query.Degree = d$queryGeneNodeDegree,
               query.Rank = d$queryGeneNodeDegreeRank,
               found.Degree = d$foundGeneNodeDegree,
               found.Rank = d$foundGeneNodeDegreeRank,
               tested = d$numTestedIn,
               support.Positive = d$posSupp,
               support.Negative = d$negSupp,
               support.N = d$support,
               support.Experiments = d$supportingExperiments,
               processGenes(d$queryGene) %>% setnames(paste0('query.', colnames(.))),
               processGenes(d$foundGene) %>% setnames(paste0('found.', colnames(.))))
}

processPhenotypes <- function(d) {
    data.table(value.Name = d$value,
               value.URL = d$valueUri,
               value.GeneCount = d$publicGeneCount,
               parents = lapply(d$`_parent`, function(x) paste0('http://purl.obolibrary.org/obo/', unlist(strsplit(x, '<', T)))),
               children = lapply(d$children, function(x) paste0('http://purl.obolibrary.org/obo/', unlist(strsplit(x, '<', T)))))
}