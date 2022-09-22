
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

#' Processes JSON as a factor
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processGemmaFactor <- function(d) {
    d <- jsonlite:::simplify(d)
    if (all(is.na(d))) {
        data.table(
            factorValue = NA_character_, factorValueURI = NA_character_,
            description = NA_character_, category = NA_character_,
            categoryURI = NA_character_, measurement = FALSE, type = NA_character_
        )
    } else {
        data.table(
            factorValue = switch(is.null(d[["factorValue"]]) + 1,
                d[["factorValue"]],
                d[["value"]]
            ),
            factorValueURI = d[["valueUri"]],
            description = d[["description"]],
            category = d[["category"]],
            categoryURI = d[["categoryUri"]],
            measurement = d[["measurement"]],
            type = d[["type"]]
        )
    }
}

#' Processes JSON as an array
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the probes representing the gene
#' across different platforms. 
#' 
#' 
#' @keywords internal
processGemmaArray <- function(d) {
    d <- jsonlite:::simplify(d)
    data.table(
        platform.ShortName = d[["shortName"]],
        platform.Name = d[["name"]],
        platform.ID = d[['id']],
        platform.Taxon = d[["taxon"]],
        platform.TaxonID = d[["taxonID"]],
        platform.Type = d[["technologyType"]],
        platform.Description = d[["description"]],
        platform.Troubled = d[["troubled"]]
    )
}



#' Processes JSON as a vector of datasets
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the queried dataset(s). A list if
#' \code{raw = TRUE}. Returns an empty list if no datasets matched. A successful 
#' response may contain 'Geeq' information, which aims to provide a unified 
#' metric to measure experiments by the quality of their data, and their 
#' suitability for use in Gemma. You can
#' read more about the geeq properties [here](https://pavlidislab.github.io/Gemma/geeq.html).
#' 
#' The fields of the output data.table are:
#' 
#' \itemize{
#'     \item \code{experiment.ShortName}: Shortname given to the dataset within Gemma. Often corresponds to accession ID
#'     \item \code{experiment.Name}: Full title of the dataset
#'     \item \code{experiment.ID}: Internal ID of the dataset.
#'     \item \code{experiment.Description}: Description of the dataset
#'     \item \code{experiment.Public}: Is the dataset publicly available. Only useful for logged in users with access to non-public data
#'     \item \code{experiment.Troubled}: Did an automatic process within gemma or a curator mark the dataset as "troubled"
#'     \item \code{experiment.Accession}: Accession ID of the dataset in the external database it was taken from
#'     \item \code{experiment.Database}: The name of the database where the dataset was taken from
#'     \item \code{experiment.URI}: URI of the original database
#'     \item \code{experiment.SampleCount}: Number of samples in the dataset
#'     \item \code{experiment.batchEffect}: A text field describing whether the dataset has batch effects
#'     \item \code{experiment.batchCorrected}: Whether batch correction has been performed on the dataset. This is a text field with possible details about the batch correction. Use \code{geeq.batchEffect} if a more structured field is needed.
#'     \item \code{geeq.batchConfound}: 0 if batch info isn't available, -1 if batch counfoud is detected, 1 if batch information is available and no batch confound found 
#'     \item \code{geeq.batchEffect}: -1 if batch p value < 0.0001, 1 if batch p value > 0.1, 0 if otherwise and when there is no batch information is available or when the data is confounded with batches.
#'     \item \code{geeq.rawData}: -1 if no raw data available, 1 if raw data was available. When available, Gemma reprocesses raw data to get expression values and batches
#'     \item \code{geeq.qScore}: Data quality score given to the dataset by Gemma.
#'     \item \code{geeq.sScore}: Suitability score given to the dataset by Gemma. Refers to factors like batches, platforms and other aspects of experimental design
#'     \item \code{taxon.Name}: The taxa of the study. In Gemma each study will include a single species. If the original source has samples from multiple species, they will be split into different studies within Gemma
#'     \item \code{taxon.ID}: Internal ID given to the taxon by Gemma
#' }
#'
#' @keywords internal
processDatasets <- function(d) {
    d <- jsonlite:::simplify(d)
    data.table(
        experiment.ShortName = d[["shortName"]],
        experiment.Name = d[["name"]],
        experiment.ID = d[["id"]],
        experiment.Description = d[["description"]],
        experiment.Public = d[["isPublic"]],
        experiment.Troubled = d[["troubled"]],
        experiment.Accession = d[["accession"]],
        experiment.Database = d[["externalDatabase"]],
        experiment.URI = d[["externalUri"]],
        experiment.SampleCount = d[["bioAssayCount"]],
        experiment.LastUpdated = processDate(d[["lastUpdated"]]),
        experiment.batchEffect = d[["batchEffect"]],
        geeq.batchCorrected = checkBounds(d[["geeq"]][["batchCorrected"]]),
        geeq.batchConfound = checkBounds(d[["geeq"]][["qScorePublicBatchConfound"]]),
        geeq.batchEffect = checkBounds(d[["geeq"]][["qScorePublicBatchEffect"]]),
        geeq.rawData = checkBounds(d[["geeq"]][["sScoreRawData"]]),
        geeq.qScore = checkBounds(d[["geeq"]][["publicQualityScore"]]),
        geeq.sScore = checkBounds(d[["geeq"]][["publicSuitabilityScore"]]),
        taxon.Name = d[["taxon"]],
        taxon.ID = d[["taxonId"]]# ,
        # technology.Type = d[["technologyType"]]
    )
}

#' Processes JSON as an annotation
#'
#' @param d The JSON to process
#'
#' @return A data table with annotations (annotation search result value objects)
#' matching the given identifiers. A list if \code{raw = TRUE}. A \code{400 error} if required parameters are missing.
#' 
#' The fields of the output data.table are:
#' 
#' \itemize{
#'     \item \code{category.Name}: Category that the annotation belongs to
#'     \item \code{category.URI}: URI for the category.Name
#'     \item \code{value.Name}: Annotation term
#'     \item \code{value.URI}: URI for the value.Name
#' }
#' 
#' @keywords internal
processSearchAnnotations <- function(d) {
    d <- jsonlite:::simplify(d)

    data.table(
        category.Name = d[["category"]],
        category.URI = d[["categoryUri"]],
        value.Name = d[["value"]],
        value.URI = d[["valueUri"]]
    )
}

#' Processes JSON as a differential expression analysis
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the differential expression
#' analysis of the queried dataset. Note that this funciton does not return
#' differential expression values themselves. Use \code{\link{get_differential_expression_values}}
#' to get differential expression values (see examples).
#' 
#' The fields of the output data.table are:
#' 
#' \itemize{
#'     \item \code{result.ID}: Result set ID of the differential expression analysis.
#'     May represent multiple factors in a single model.
#'     \item \code{contrast.ID}: Id of the specific contrast factor. Together with the result.ID
#'     they uniquely represent a given contrast.
#'     \item \code{experiment.ID}: Id of the source experiment
#'     \item \code{baseline.category}: Category for the contrast
#'     \item \code{baseline.categoryURI}: URI for the baseline category
#'     \item \code{baseline.factorValue}: Factor value assigned as the baseline in the contrast. Typically represent control samples
#'     \item \code{baseline.factorValueURI}: URI for the baseline.factorValue
#'     \item \code{experimental.factorValue}: Factor value assigned to the experimental group.
#'     \item \code{experimental.factorValueURI}: URI for the experimental.factorValue
#'     \item \code{subsetFactor.subset}:
#'     \item \code{subsetFactor.category}:
#'     \item \code{subsetFactor.categoryURI}:
#'     \item \code{subsetFactor.factorValue}:
#'     \item \code{subsetFactor.factorValueURI}:
#'     \item \code{stats.DE}: Number of differentially expressed genes for the contrast
#'     \item \code{stats.Down}: Number of downregulated genes for the contrast
#'     \item \code{stats.Up}: Number of upregulated genes for the contrast
#'     \item \code{analysis.Threshold}: P value threshold used to determine the number
#'     of differentially expressed genes
#'     \item \code{probes.Analyzed}: Number of probesets represented in the contrast
#'     \item \code{genes.Analyzed}: Number of genes represented in the contrast
#'     \item \code{platform.ID}: Platform id for the contrast
#' }
#'
#' @keywords internal
processDEA <- function(d) {
    d <- jsonlite:::simplify(d)

    # Initialize internal variables to avoid R CMD check notes

    divides <- data.table(
        analysis.ID = d[["id"]],
        experiment.ID = ifelse(is.na(d[["sourceExperiment"]]), d[["bioAssaySetId"]], d[["sourceExperiment"]]),
        subsetFactor.Enabled = d[["subset"]],
        subsetFactor = processGemmaFactor(d[["subsetFactorValue"]]),
        resultIds = lapply(d[["resultSets"]], "[[", "resultSetId")
    ) %>%
        .[, .(result.ID = unlist(resultIds)), setdiff(names(.), "resultIds")]

    rs <- lapply(d[["resultSets"]], function(r) {
        data.table(
            analysis.Threshold = r[["threshold"]],
            analysis.ID = r[["analysisId"]],
            result.ID = r[["resultSetId"]],
            stats.DE = r[["numberOfDiffExpressedProbes"]],
            stats.Down = r[["downregulatedCount"]],
            stats.Up = r[["upregulatedCount"]],
            probes.Analyzed = r[["numberOfProbesAnalyzed"]],
            genes.Analyzed = r[["numberOfGenesAnalyzed"]],
            factor.ID = r[["factorIds"]],
            r[["baselineGroup"]] %>%
                {
                    data.table(
                        baseline.category= .[["category"]],
                        baseline.categoryURI = .[["categoryUri"]],
                        baseline.factorValue = .[["factorValue"]],
                        baseline.factorValueURI = .[["valueUri"]]
                    )
                }
        ) %>%
            .[, .(factor.ID = unlist(factor.ID)), setdiff(names(.), "factor.ID")]
    }) %>%
        rbindlist() %>%
        .[!is.na(baseline.category)]

    rsd <- merge(rs, divides, by = c("result.ID", "analysis.ID"), all = TRUE)
    lapply(unique(rsd[, factor.ID]), function(fid) {
        lapply(d$factorValuesUsed[[as.character(fid)]], function(fv) {
            if (!is.null(fv) && nrow(fv) > 0) {
                as.data.table(fv)[
                    !(factorValue %in% rsd[factor.ID == fid, unique(baseline.factorValue)]),
                    .(
                        cf.Val = factorValue,
                        cf.ValLongUri = valueUri,
                        factor.ID = factorId,
                        id
                    )
                ]
            }
        }) %>%
            .[lengths(.) > 0] %>%
            rbindlist()
    }) %>%
        rbindlist() %>%
        unique() %>%
        merge(rsd, by = "factor.ID", allow.cartesian = TRUE, all = TRUE) %>%
        merge(data.table(
            analysis.ID = d[["id"]],
            platform.ID = d[["arrayDesignsUsed"]]
        ), by = "analysis.ID", all = TRUE) %>%
        .[, .(
            # rsc.ID = paste("RSCID", result.ID, id, sep = "."),
            contrast.id = id,
            experiment.ID, baseline.category, baseline.categoryURI, baseline.factorValue, baseline.factorValueURI,
            experimental.factorValue = cf.Val, 
            experimental.factorValueURI = cf.ValLongUri, 
            subsetFactor.subset = subsetFactor.Enabled,
            subsetFactor.category = subsetFactor.category, 
            subsetFactor.categoryURI = subsetFactor.categoryURI,
            subsetFactor.factorValue = subsetFactor.factorValue,
            subsetFactor.factorValueURI = subsetFactor.factorValueURI,
            stats.DE, stats.Down, stats.Up, analysis.Threshold, probes.Analyzed,
            genes.Analyzed, platform.ID
        ), .(result.ID, id)] %>%
        .[, !"id"]
}

#' Processes JSON as a result set
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processResultSetFactors <- function(d) {
    d <- jsonlite:::simplify(d)

    lapply(
        d$experimentalFactors$values,
        dplyr::select, c("id", "factorValue", "category")
    ) %>%
        dplyr::bind_rows()
}

#' Processes JSON as a datasets result set
#'
#' @param d The JSON to process
#'
#' @return A data table with the queried datasets' resultSet ID(s). A list if
#' \code{raw = TRUE}. Use 
#' \code{\link{get_differential_expression_values}} to get differential expression
#' values (see examples). Use \code{\link{get_dataset_differential_expression_analyses}}
#' to get more detailed information about a result set.
#' 
#' The fields of the output data.table are:
#' 
#' \itemize{
#'     \item \code{resultSet.id}: Internal ID given to the result set. Can be used to access the results using \code{\link{get_differential_expression_values}}
#'     \item \code{factor.category}: What is the category splitting the experimental groups in the result set (e.g. disease )
#'     \item \code{factor.levels}: What are the conditions that are compared in the result set (e.g control, bipolar disorder)
#' 
#' }
#' 
#' @keywords internal
processDatasetResultSets <- function(d) {
    d <- jsonlite:::simplify(d)

    data.table(
        resultSet.id = d$id,
        # analysis.id = d$analysis$id,
        factors = lapply(
            d$experimentalFactors,
            dplyr::select, c("category", "description")
        )
    ) %>%
        tidyr::unnest(.data$factors) %>%
        dplyr::rename(
            "factor.category" = "category",
            "factor.levels" = "description"
        ) %>%
        dplyr::group_by(.data$resultSet.id) %>%
        dplyr::mutate(
            factor.category = paste0(.data$factor.category, collapse = " x "),
            factor.levels = paste0(.data$factor.levels, collapse = "; ")
            ) %>%
        unique() %>%
        as.data.table()
}

#' Processes JSON as annotations
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the annotations of the queried 
#' dataset. A list if \code{raw = TRUE}.A \code{404 error} if the given 
#' identifier does not map to any object.
#' 
#' The fields of the output data.table are:
#' 
#' \itemize{
#'     \item \code{class.Type}: Type of the annotation class
#'     \item \code{class.Name}: Name of the annotation class (e.g. organism part)
#'     \item \code{class.URI}: URI for the annotation class
#'     \item \code{evidence.Code}:
#'     \item \code{term.Name}: Name of the annotation term (e.g. lung)
#'     \item \code{term.URI}: URI for the annotation term
#' }
#' 
#' 
#'
#' @keywords internal
processAnnotations <- function(d) {
    d <- jsonlite:::simplify(d)

    data.table(
        class.Type = d[["objectClass"]],
        class.Name = d[["className"]],
        class.URI = d[["classUri"]],
        evidence.Code = d[["evidenceCode"]],
        term.Name = d[["termName"]],
        term.URI = d[["termUri"]]
    )
}

#' Processes a response as a gzip file
#'
#' @param content The content from an `http_get` request
#'
#' @return A processed data.table
#'
#' @keywords internal
processFile <- function(content) {
    tmp <- tempfile() # Make a temp file
    writeBin(content, tmp) # Save to that file
    tmp2 <- gzfile(tmp)
    ret <- tmp2 %>%
        readLines() %>%
        .[which(!startsWith(., "#"))[1]:length(.)] %>%
        # Strip comments
        paste0(collapse = "\n") %>%
        paste0('\n') %>% 
        {
            fread(text = .)
        }
    close(tmp2)
    unlink(tmp) # Delete the temp file
    # Process matrix according to data type
    if (colnames(ret)[1] == "Probe") {
        ret <- processExpressionMatrix(ret)
    } else if (colnames(ret)[1] == "id") {
        ret <- processDEMatrix(ret)
    } else {
        ret <- processDesignMatrix(ret)
    }
    ret
}

#' Processes JSON as a vector of samples
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the samples of the queried dataset. A list if
#' \code{raw = TRUE}. A \code{404 error} if the given identifier does not map to any object.
#' 
#' The fields of the output data.table are:
#' \itemize{
#'     \item \code{sample.Name}: Internal name given to the sample.
#'     \item \code{sample.ID}: Internal ID of the sample
#'     \item \code{sample.Description}: Free text description of the sample
#'     \item \code{sample.Outlier}: Whether or not the sample is marked as an outlier
#'     \item \code{sample.Accession}: Accession ID of the sample in it's original database
#'     \item \code{sample.Database}: Database of origin for the sample
#'     \item \code{sample.Characteristics}: Characteristics of the sample. This field is a data table
#'     \item \code{sample.FactorValues}: Experimental factor values of the sample. This field is a data table
#' }
#'
#' @keywords internal
processSamples <- function(d) {
    d <- jsonlite:::simplify(d)

    data.table(
        # bioMaterial.Name = checkBounds(d[["sample"]][["name"]]),
        sample.Name = checkBounds(d[["name"]],NA_character_),
        sample.ID = checkBounds(d[["sample"]][["id"]],NA_integer_),
        # sample.Correspondence = checkBounds(d[["sample"]][["description"]]),
        sample.Description = d[["description"]],
        sample.Outlier = d[["outlier"]],
        sample.Accession = checkBounds(d[["accession"]][["accession"]],NA_character_),
        sample.Database = checkBounds(d[["accession"]][["externalDatabase"]][["name"]]),
        # sample.Processed = processDate(d[["processingDate"]]),# not sure what this format is, the function fails
        sample.Characteristics = lapply(checkBounds(d[["sample"]][["characteristics"]]), processGemmaFactor),
        sample.FactorValues = lapply(lapply(checkBounds(d[["sample"]][["factorValueObjects"]]), "[[", "characteristics"), function(x) processGemmaFactor(rbindlist(x)))# ,
        # processGemmaArray(d[["arrayDesign"]]
        )
}

#' Processes JSON as a vector of platforms
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the platform(s). A list if \code{raw = TRUE}. A \code{404 error} if the given identifier
#'  does not map to any object
#'  
#'  The fields of the output data.table are:
#'  \itemize{
#'  \item \code{platform.ID}: Internal identifier of the platform
#'  \item \code{platform.ShortName}: Shortname of the platform.
#'  \item \code{platform.Name}: Full name of the platform.
#'  \item \code{platform.Description}: Free text description of the platform
#'  \item \code{platform.Troubled}: Whether or not the platform was marked "troubled" by a Gemma process or a curator
#'  \item \code{platform.ExperimentCount}: Number of experiments using the platform within Gemma
#'  \item \code{platform.GeneCount}
#'  \item \code{platform.ProbeSequenceCount}
#'  \item \code{platform.ProbeAlignmentCount}
#'  \item \code{platform.ProbeGeneCount}
#'  \item \code{platform.ElementCount}
#'  \item \code{taxon.Name}: Name of the species platform was made for
#'  \item \code{taxon.ID}: Internal identifier given to the species by Gemma
#'  \item \code{platform.Type}: Technology type for the platform.
#'  }
#'  
#' @keywords internal
processPlatforms <- function(d) {
    d <- jsonlite:::simplify(d)

    data.table(
        platform.ID = d[["id"]],
        platform.ShortName = d[["shortName"]],
        platform.Name = d[["name"]],
        platform.Description = d[["description"]],
        platform.Troubled = d[["troubled"]],
        platform.ExperimentCount = d[["expressionExperimentCount"]],
        platform.GeneCount = d[["numGenes"]],
        platform.ProbeSequenceCount = d[["numProbeSequences"]],
        platform.ProbeAlignmentCount = d[["numProbeAlignments"]],
        platform.ProbeGeneCount = d[["numProbesToGenes"]],
        platform.ElementCount = d[["designElementCount"]],
        taxon.Name = d[["taxon"]],
        taxon.ID = d[["taxonID"]],
        platform.Type = d[["technologyType"]]#,
        # technology.Color = d[["color"]]
    )
}

#' Processes JSON as a vector of elements
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the probes representing a gene across
#' all platrofms. A list if \code{raw = TRUE}.
#' A \code{404 error} if the given identifier does not map to any genes.
#' 
#'  The fields of the output data.table are:
#'  
#'  \itemize{
#'      \item \code{mapping.name}: Name of the mapping. Typically the probeset name
#'      \item \code{mappping.description}: A free text field providing optional information about the mapping
#'      \item \code{platform.ShortName}: Shortname of the platform given by Gemma. Typically the GPL identifier.
#'      \item \code{platform.Name}: Full name of the platform
#'      \item \code{platform.ID}: Id number of the platform given by Gemma
#'      \item \code{platform.Taxon}: Species the platform was designed for
#'      \item \code{platform.TaxonID}: Id number of the species given by Gemma
#'      \item \code{platform.Type}: Type of the platform.
#'      \item \code{platform.Description}: Free text field describing the platform.
#'      \item \code{platform.Troubled}: Whether the platform is marked as troubled by a Gemma curator.
#'  }
#' 
#' @keywords internal
processElements <- function(d) {
    d <- jsonlite:::simplify(d)

    data.table(
        mapping.Name = d[["name"]],
        mapping.Description = d[["description"]],
        processGemmaArray(d$arrayDesign)
    )
}

#' Processes JSON as a vector of genes
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the querried gene(s) 
#' A list if \code{raw = TRUE}.
#' 
#' The fields of the output data.table are:
#' 
#' \itemize{
#'     \item \code{gene.Symbol}: Symbol for the gene
#'     \item \code{gene.Ensembl}: Ensembl ID for the gene
#'     \item \code{gene.NCBI}: NCBI id for the gene
#'     \item \code{gene.Name}: Name of the gene
#'     \item \code{taxon.name}: Name of the taxon of origin
#'     \item \code{taxon.ID}: Gemma ID for the taxon
#'     \item \code{taxon.Scientific}: Scientific name for the taxon
#' }
#'
#' @keywords internal
processGenes <- function(d) {
    d <- jsonlite:::simplify(d)

    data.table(
        gene.Symbol = d[["officialSymbol"]],
        gene.Ensembl = d[["ensemblId"]],
        gene.NCBI = d[["ncbiId"]],
        gene.Name = d[["officialName"]],
        # gene.Aliases = d[["aliases"]],
        # gene.GO = d[["numGoTerms"]],
        # gene.Homologues = d[["homologues"]],
        # gene.MFX.Rank = d[["multifunctionalityRank"]],
        taxon.Name = d[["taxonCommonName"]],
        taxon.ID = d[["taxonId"]],
        taxon.Scientific = d[["taxonScientificName"]]
        # phenotypes = d[["phenotypes"]]
    )
}

#' Processes JSON as a vector of taxa
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processTaxon <- function(d) {
    d <- jsonlite:::simplify(d)

    data.table(
        taxon.Name = d[["commonName"]],
        taxon.Scientific = d[["scientificName"]],
        taxon.ID = d[["id"]],
        taxon.NCBI = d[["ncbiId"]],
        taxon.Database.Name = checkBounds(d[["externalDatabase"]][["name"]]),
        taxon.Database.ID = d[["externalDatabase.ID"]]
    )
}

#' Processes JSON as a vector of gene locations
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the physical location of the
#' queried gene. A list if \code{raw = TRUE}. A \code{404 error} if the given identifier does not map to any object.
#' 
#' The fields of the output data.table are:
#' 
#' \itemize{
#'     \item \code{chromosome}: Name of the chromosome the gene is located
#'     \item \code{strand}: Which strand the gene is located
#'     \item \code{bin}:
#'     \item \code{nucleotide}: Nucleotide number for the gene
#'     \item \code{length}: Gene length
#'     \item \code{taxon.name}: Name of the taxon
#'     \item \code{taxon.Scientific}: Scientific name for the taxon
#'     \item \code{taxon.ID}: Internal ID for the taxon given by Gemma
#'     \item \code{taxon.NCBI}: NCBI ID for the taxon
#'     \item \code{taxon.Database.Name}: Name of the database used in Gemma for the taxon
#' }
#'
#' @keywords internal
processGeneLocation <- function(d) {
    d <- jsonlite:::simplify(d)

    data.table(
        chromosome = d[["chromosome"]],
        strand = d[["strand"]],
        bin = d[["bin"]],
        nucleotide = d[["nucleotide"]],
        length = d[["nucleotideLength"]],
        processTaxon(d[["taxon"]])
    )
}

#' Processes JSON as GO terms
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the GO terms assigned to the
#' queried gene. A list if \code{raw = TRUE}. A \code{404 error} if the given identifier does not map to any
#' object. Go terms were updated on June 10 2022.
#' 
#' The fields of the output data.table are:
#' 
#' \itemize{
#'     \item \code{term.Name}: Name of the term
#'     \item \code{term.ID}: ID of the term
#'     \item \code{term.URI}: URI of the term
#' }
#' 
#'
#' @keywords internal
processGO <- function(d) {
    d <- jsonlite:::simplify(d)

    data.table(
        term.Name = d[["term"]],
        term.ID = d[["goId"]],
        term.URI = d[["uri"]]
    )
}

#' Processes design matrix
#'
#' @param m The design matrix to process
#'
#' @return A processed matrix
#'
#' @keywords internal
processDesignMatrix <- function(m) {
    # Remove redundant strings from sample names, unnecessary columns
    data.frame(m, row.names = stringr::str_extract(m$Bioassay, "(?<=Name=).*")) %>%
        dplyr::select(-c(.data$ExternalID, .data$Bioassay))
}

#' Processes expression matrix
#'
#' @param m The expression matrix to process
#'
#' @return A processed matrix
#'
#' @keywords internal
processExpressionMatrix <- function(m) {
    # this version is a bit more forgiving to missing data fields
    m <- m[,!colnames(m) %in% c('Sequence','GemmaId'),with = FALSE]
    # m <- dplyr::select(m, -.data$Sequence, -.data$GemmaId)
    # Remove redundant strings from sample names
    colnames(m) <- stringr::str_extract(colnames(m), "(?<=Name=).*")
    m
}

#' Processes differential expression matrix
#'
#' @param m The differential expression matrix to process
#'
#' @return A processed matrix
#'
#' @keywords internal
processDEMatrix <- function(m) {
    m <- m[,!colnames(m) %in% c('id','probe_id','gene_id','gene_name'),with = FALSE]
    m %>%
        dplyr::rename(
            Probe = .data$probe_name,
            GeneSymbol = .data$gene_official_symbol,
            GeneName = .data$gene_official_name,
            NCBIid = .data$gene_ncbi_id
        )
}

#' Replaces factor ids by the factors strings in DE table columns
#'
#' @param rs The resultSet matrix to process
#'
#' @return A processed matrix
#'
#' @keywords internal
processDEcontrasts <- function(rs, rsID) {
    factors <- .getResultSetFactors(rsID)
    colnames(rs) <- stringr::str_replace(colnames(rs), "log2fc", "logFoldChange")
    # Replace factor IDs by the factor names
    for (f in factors$id) {
        colnames(rs) <- stringr::str_replace(colnames(rs), as.character(f), factors[factors$id == f, 2])
    }
    rs
}

#' A blank processor that returns data as is
#' 
#' @param data any data
#' @return Data as is
#' @keywords internal
blank_processor <- function(data){
    return(data)
}

# processSVD <- function(d){
#     d$vMatrix$rawMatrix
#     browser()
# }
