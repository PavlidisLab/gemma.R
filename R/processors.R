
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
    data.table(
        platform.shortName = accessField(d,"elementName",NA_character_),
        platform.name = accessField(d,"name",NA_character_),
        platform.ID = accessField(d,'id',NA_integer_),
        platform.type = accessField(d, "technologyType", NA_character_),
        platform.description = accessField(d, "description", NA_character_),
        platform.troubled = accessField(d,"troubled",NA),
        d %>% purrr::map('taxon') %>% processTaxon()
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
#'     \item \code{experiment.shortName}: Shortname given to the dataset within Gemma. Often corresponds to accession ID
#'     \item \code{experiment.name}: Full title of the dataset
#'     \item \code{experiment.ID}: Internal ID of the dataset.
#'     \item \code{experiment.description}: Description of the dataset
#'     \item \code{experiment.troubled}: Did an automatic process within gemma or a curator mark the dataset as "troubled"
#'     \item \code{experiment.accession}: Accession ID of the dataset in the external database it was taken from
#'     \item \code{experiment.database}: The name of the database where the dataset was taken from
#'     \item \code{experiment.URI}: URI of the original database
#'     \item \code{experiment.sampleCount}: Number of samples in the dataset
#'     \item \code{experiment.batchEffect}: A text field describing whether the dataset has batch effects
#'     \item \code{geeq.batchCorrected}: Whether batch correction has been performed on the dataset.
#'     \item \code{geeq.batchConfound}: 0 if batch info isn't available, -1 if batch counfoud is detected, 1 if batch information is available and no batch confound found
#'     \item \code{geeq.batchEffect}: -1 if batch p value < 0.0001, 1 if batch p value > 0.1, 0 if otherwise and when there is no batch information is available or when the data is confounded with batches.
#'     \item \code{geeq.rawData}: -1 if no raw data available, 1 if raw data was available. When available, Gemma reprocesses raw data to get expression values and batches
#'     \item \code{geeq.qScore}: Data quality score given to the dataset by Gemma.
#'     \item \code{geeq.sScore}: Suitability score given to the dataset by Gemma. Refers to factors like batches, platforms and other aspects of experimental design
#'     \item \code{taxon.Name}: Name of the species
#'     \item \code{taxon.Scientific}: Scientific name for the taxon
#'     \item \code{taxon.ID}: Internal identifier given to the species by Gemma
#'     \item \code{taxon.NCBI}: NCBI ID of the taxon
#'     \item \code{taxon.Database.Name}: Underlying database used in Gemma for the taxon
#'     \item \code{taxon.Database.ID}: ID of the underyling database used in Gemma for the taxon
#' }
#'
#' @keywords internal
processDatasets <- function(d) {
    data.table(
        experiment.shortName = accessField(d,'shortName',NA_character_),
        experiment.name = accessField(d, "name",NA_character_),
        experiment.ID = accessField(d, "id",NA_integer_),
        experiment.description = accessField(d, "description",NA_character_),
        experiment.troubled = accessField(d, "troubled",NA),
        experiment.accession = accessField(d, "accession",NA_character_),
        experiment.database = accessField(d, "externalDatabase",NA_character_),
        experiment.URI = accessField(d, "externalUri",NA_character_),
        experiment.sampleCount = accessField(d, "bioAssayCount",NA_integer_),
        experiment.lastUpdated = processDate(accessField(d, "lastUpdated",NA_real_)),
        experiment.batchEffect = accessField(d, "batchEffect",NA_character_),
        geeq.batchCorrected = d %>% purrr::map('geeq') %>% accessField('batchCorrected',NA),
        geeq.batchConfound = d %>% purrr::map('geeq') %>% accessField("qScorePublicBatchConfound",NA_integer_),
        geeq.batchEffect = d %>% purrr::map('geeq') %>% accessField("qScorePublicBatchEffect",NA_integer_),
        geeq.rawData = d %>% purrr::map('geeq') %>% accessField("sScoreRawData",NA_integer_),
        geeq.qScore = d %>% purrr::map('geeq') %>% accessField("publicQualityScore",NA_real_),
        geeq.sScore = d %>% purrr::map('geeq') %>% accessField("publicSuitabilityScore",NA_real_),
        d %>% purrr::map('taxon') %>% processTaxon()# ,
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
    data.table(
        category.name = accessField(d,'category',NA_character_),
        category.URI = accessField(d,"categoryUri",NA_character_),
        value.name = accessField(d,"value",NA_character_),
        value.URI = accessField(d,"valueUri",NA_character_)
    )
}


# good test cases 442, 448, 200, 174
# 200 also has statements, 548 double statements
# for values 326
# GSE26366 has a gene fusion
# GSE106 has measurements
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
#'     \item \code{baseline.factors}: Characteristics of the baseline. This field is a data.table
#'     \item \code{experimental.factors}: Characteristics of the experimental group. This field is a data.table
#'     \item \code{subsetFactor.subset}: TRUE if the result set belong to a subset, FALSE if not. Subsets are created when performing differential expression to avoid unhelpful comparisons.
#'     \item \code{subsetFactor.category}: Category of the subset
#'     \item \code{subsetFactor}: Characteristics of the subset. This field is a data.table
#'     \item \code{probes.analyzed}: Number of probesets represented in the contrast
#'     \item \code{genes.analyzed}: Number of genes represented in the contrast
#' }
#'
#' @keywords internal
processDEA <- function(d) {
    # Initialize internal variables to avoid R CMD check notes

    result_ids <- d %>% purrr::map('resultSets') %>% purrr::map(function(x){x %>% accessField('id')})

    result_factors <- seq_along(result_ids) %>% lapply(function(i){
        results <- seq_along(result_ids[[i]]) %>% lapply(function(j){
            

            if(length(d[[i]]$resultSets[[j]]$experimentalFactors)==1){

                contrast.id <-  d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$values %>% accessField('id',NA_integer_)
                
                baseline_id <- d[[i]]$resultSets[[j]]$baselineGroup$id
                
                baseline_factor <-  d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$values[contrast.id == baseline_id]
                
                non_control_factors <- d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$values[!contrast.id %in% baseline_id]
                non_control_ids <- contrast.id[!contrast.id %in% baseline_id]
                size <- length(non_control_ids)
                
                exp.factors <- non_control_factors %>% 
                    purrr::map(processFactorValueValueObject)
                
                out <- data.table(
                    result.ID = d[[i]]$resultSets[[j]]$id,
                    contrast.ID = non_control_ids,
                    experiment.ID = ifelse(is.null(d[[i]]$sourceExperiment),
                                           d[[i]]$bioAssaySetId, 
                                           d[[i]]$sourceExperiment),
                    factor.category = d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$category,
                    factor.category.URI = d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$categoryUri %>%
                        nullCheck(NA_character_),
                    factor.ID =  d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$id %>% 
                        nullCheck(NA_integer_),
                    baseline.factors = d[[i]]$resultSets[[j]]$baselineGroup %>% 
                        processFactorValueValueObject %>% list() %>% rep(size),
                    experimental.factors = exp.factors,
                    subsetFactor.subset = d[[i]]$isSubset %>% nullCheck(),
                    subsetFactor = d[i] %>% purrr::map('subsetFactorValue') %>% 
                        purrr::map(processFactorValueValueObject) %>% 
                        do.call(rbind,.) %>% list() %>%
                        rep(size),
                    probes.analyzed = d[[i]]$resultSets[[j]]$numberOfProbesAnalyzed %>% nullCheck(NA_integer_),
                    genes.analyzed =  d[[i]]$resultSets[[j]]$numberOfGenesAnalyzed %>% nullCheck(NA_integer_)
                )

            }else{
                # if more than 2 factors are present take a look at the
                # differential expression results to isolate the relevant results
                # this adds quite a bit of overhead for studies like this but
                # they should be relatively rare. if coupled with memoisation
                # overall hit on performance should not be too much
                # this was needed because for multi-factor result-sets, the baseline
                # for each factor is not specified
                

                ids <- d[[i]]$resultSets[[j]]$experimentalFactors %>%
                    purrr::map('values')  %>%
                    purrr::map(function(x){x %>% accessField('id')}) %>%
                    expand.grid()

                factor_ids <- d[[i]]$resultSets[[j]]$experimentalFactors %>% purrr::map_int('id')
                

                dif_exp <- get_differential_expression_values(resultSet = d[[i]]$resultSets[[j]]$id)
                relevant_ids <- dif_exp[[1]] %>% colnames %>%
                    {.[grepl('[0-9]_pvalue',.)]} %>% strsplit('_') %>% lapply(function(x){
                        x[c(-1,-length(x))] %>% as.integer()
                    }) %>% as.data.frame %>% t

                if(ncol(relevant_ids)>0){
                    relevant_id_factor_id <- relevant_ids[1,] %>% purrr::map_int(function(x){
                        apply(ids,2,function(y){x %in% y}) %>% which %>% {factor_ids[[.]]}
                    })

                    colnames(relevant_ids) <- relevant_id_factor_id

                    experimental_factors <-
                        d[[i]]$resultSets[[j]]$experimentalFactors %>%
                        purrr::map('id') %>%
                        purrr::map(function(x){d[[i]]$factorValuesUsed[[as.character(x)]]})
                    names(experimental_factors) <- d[[i]]$resultSets[[j]]$experimentalFactors %>%
                        purrr::map_int('id')
                    exp.factors <- seq_len(nrow(relevant_ids)) %>%
                        purrr::map(function(k){
                            seq_along(relevant_ids[k,]) %>% purrr::map(function(l){
                                factors <- experimental_factors[[colnames(relevant_ids)[l]]]
                                ids <- factors %>% purrr::map_int('id')
                                out <- factors[[which(ids == relevant_ids[k,l])]]%>% processFactorValueValueObject
                                return(out)
                                    
                            }) %>% {do.call(rbind,.)}
                        })
                    
                    size <- length(exp.factors)

                    out <- data.table(
                        result.ID = d[[i]]$resultSets[[j]]$id,
                        contrast.ID = unname(apply(relevant_ids,1,paste,collapse = '_')),
                        experiment.ID = ifelse(is.null(d[[i]]$sourceExperiment), d[[i]]$bioAssaySetId, d[[i]]$sourceExperiment),
                        factor.category = d[[i]]$resultSets[[j]]$experimentalFactors %>% 
                            purrr::map_chr('category') %>% unlist %>% sort %>%
                            paste(collapse = ','),
                        factor.category.URI = d[[i]]$resultSets[[j]]$experimentalFactors %>% 
                            purrr::map_chr('categoryUri') %>% unlist %>% sort %>% paste(collapse = ','),
                        factor.ID = d[[i]]$resultSets[[j]]$experimentalFactors %>%  purrr::map_int('id') %>% unlist %>% sort %>% paste(collapse=','),
                        baseline.factors = d[[i]]$resultSets[[j]]$baselineGroup %>% 
                            processFactorValueValueObject %>% list() %>% rep(size),
                        experimental.factors = exp.factors,
                        subsetFactor.subset = d[[i]]$isSubset %>% nullCheck(),
                        subsetFactor = d[i] %>% purrr::map('subsetFactorValue') %>% 
                            purrr::map(processFactorValueValueObject) %>% 
                            do.call(rbind,.) %>% list() %>%
                            rep(size),
                        probes.analyzed = d[[i]]$resultSets[[j]]$numberOfProbesAnalyzed %>% nullCheck(NA_integer_),
                        genes.analyzed =  d[[i]]$resultSets[[j]]$numberOfGenesAnalyzed %>% nullCheck(NA_integer_)
                    )

                } else {
                    # if no ids were present in the expression_values matrix,
                    # there's nothing to return
                    return(NULL)
                }
            }
        }) %>% do.call(rbind,.)
        
        # process baseline factors for interaction effects
        contrast_factors <- results$factor.ID %>% as.character %>% strsplit(',')
        interactions <- contrast_factors %>%
            purrr::map_int(length) %>% 
            {.>1} %>%
            which
        for(j in interactions){
            factors <- contrast_factors[[j]]
            
            baselines <- factors %>% lapply(function(x){
                baseline <- results[results$factor.ID == x,] %$% 
                    baseline.factors %>% unique
                # baseline is accessed per result set. all should be the same
                # this should hold unless something upstream changes
                assertthat::assert_that(length(baseline)==1) 
                return(baseline[[1]])
            }) %>% do.call(rbind,.)
            
            results$baseline.factors[[j]] = baselines
        }
        
        return(results)
    }) %>% do.call(rbind,.)
}

#' Processes JSON as a result set
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processResultSetFactors <- function(d) {

    d$experimentalFactors %>%
        purrr::map('values') %>%
        purrr::map(function(x){data.frame(id = accessField(x,'id'),
                                           factorValue = accessField(x,'factorValue'),
                                           category = accessField(x,'category'))}) %>%
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

    data.table(
        resultSet.id = d %>% accessField('id',NA_integer_),
        # analysis.id = d$analysis$id,
        factors = d %>% purrr::map('experimentalFactors') %>% purrr::map(function(x){data.table(category = accessField(x,'category'),description = accessField(x,'description'))})
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
#'     \item \code{class.name}: Name of the annotation class (e.g. organism part)
#'     \item \code{class.URI}: URI for the annotation class
#'     \item \code{term.name}: Name of the annotation term (e.g. lung)
#'     \item \code{term.URI}: URI for the annotation term
#' }
#'
#'
#'
#' @keywords internal
processAnnotations <- function(d) {

    data.table(
        class.name = accessField(d,"className",NA_character_),
        class.URI = accessField(d,"classUri",NA_character_),
        term.name = accessField(d,"termName",NA_character_),
        term.URI = accessField(d,"termUri",NA_character_)
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
    attr <- attributes(content)
    attributes(content)<- NULL
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
    attributes(ret) <- c(attributes(ret),attr)
    return(ret)
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
#'     \item \code{sample.description}: Free text description of the sample
#'     \item \code{sample.outlier}: Whether or not the sample is marked as an outlier
#'     \item \code{sample.accession}: Accession ID of the sample in it's original database
#'     \item \code{sample.database}: Database of origin for the sample
#'     \item \code{sample.characteristics}: Characteristics of the sample. This field is a data table
#'     \item \code{sample.factorValues}: Experimental factor values of the sample. This field is a data table
#' }
#'
#' @keywords internal
processSamples <- function(d) {
        data.table(
        # bioMaterial.Name = checkBounds(d[["sample"]][["name"]]),
        sample.name = accessField(d,'name',NA_character_),
        sample.ID = d %>% purrr::map('sample') %>% accessField('id',NA_integer_),
        # sample.Correspondence = checkBounds(d[["sample"]][["description"]]),
        sample.description = accessField(d,"description",NA_character_),
        sample.outlier = accessField(d, "outlier",NA),
        sample.accession = d %>% purrr::map('accession') %>% 
            accessField('accession',NA_character_),
        sample.database = d %>% purrr::map('accession') %>% 
            purrr::map('externalDatabase') %>% accessField('name',NA_character_),
        # sample.Processed = processDate(d[["processingDate"]]),# not sure what this format is, the function fails
        sample.characteristics = lapply(d %>% purrr::map('sample') %>% 
                                            purrr::map('characteristics'), 
                                        processCharacteristicValueObject) %>% 
            purrr::map(function(x){
                x[,!"value.ID"]
            }),
        sample.factorValues = d %>% purrr::map('sample') %>% 
            purrr::map('factorValueObjects') %>% 
            purrr::map(function(x){x %>% purrr::map(processFactorValueBasicValueObject)}) %>% 
            purrr::map(data.table::rbindlist)# ,
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
#'  \item \code{platform.Type}: Technology type for the platform.
#'  \item \code{taxon.name}: Name of the species platform was made for
#'  \item \code{taxon.scientific}: Scientific name for the taxon
#'  \item \code{taxon.ID}: Internal identifier given to the species by Gemma
#'  \item \code{taxon.NCBI}: NCBI ID of the taxon
#'  \item \code{taxon.database.name}: Underlying database used in Gemma for the taxon
#'  \item \code{taxon.database.ID}: ID of the underyling database used in Gemma for the taxon
#'  }
#'
#' @keywords internal
processPlatforms <- function(d) {
    data.table(
        platform.ID = accessField(d,"id",NA_integer_),
        platform.shortName = accessField(d, "shortName",NA_character_),
        platform.name = accessField(d, "name",NA_character_),
        platform.description = accessField(d, "description",NA_character_),
        platform.troubled = accessField(d, "troubled",NA),
        platform.experimentCount = accessField(d, "expressionExperimentCount",NA_integer_),
        # platform.GeneCount = accessField(d, "numGenes"),
        # platform.ProbeSequenceCount = accessField(d, "numProbeSequences"),
        # platform.ProbeAlignmentCount = accessField(d, "numProbeAlignments"),
        # platform.ProbeGeneCount = accessField(d, "numProbesToGenes"),
        # platform.ElementCount = accessField(d, "designElementCount"),
        platform.type = accessField(d, "technologyType",NA_character_),
        d %>% purrr::map('taxon') %>% processTaxon()#,
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
#'      \item \code{platform.taxon}: Species the platform was designed for
#'      \item \code{platform.taxonID}: Id number of the species given by Gemma
#'      \item \code{platform.type}: Type of the platform.
#'      \item \code{platform.description}: Free text field describing the platform.
#'      \item \code{platform.troubled}: Whether the platform is marked as troubled by a Gemma curator.
#'  }
#'
#' @keywords internal
processElements <- function(d) {
    data.table(
        mapping.name = accessField(d,'name',NA_character_),
        mapping.description =accessField(d,'description',NA_character_),
        processGemmaArray(d %>% purrr::map('arrayDesign'))
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
#'     \item \code{gene.symbol}: Symbol for the gene
#'     \item \code{gene.ensembl}: Ensembl ID for the gene
#'     \item \code{gene.NCBI}: NCBI id for the gene
#'     \item \code{gene.name}: Name of the gene
#'     \item \code{gene.aliases}: Gene aliases. Each row includes a vector
#'     \item \code{gene.MFX.rank}: Multifunctionality rank for the gene
#'     \item \code{taxon.name}: Name of the species
#'     \item \code{taxon.scientific}: Scientific name for the taxon
#'     \item \code{taxon.ID}: Internal identifier given to the species by Gemma
#'     \item \code{taxon.NCBI}: NCBI ID of the taxon
#'     \item \code{taxon.database.name}: Underlying database used in Gemma for the taxon
#'     \item \code{taxon.database.ID}: ID of the underlying database used in Gemma for the taxon
#' }
#'
#' @keywords internal
processGenes <- function(d) {
    data.table(
        gene.symbol = accessField(d,'officialSymbol',NA_character_),
        gene.ensembl = accessField(d,'ensemblId',NA_character_),
        gene.NCBI = accessField(d,"ncbiId",NA_integer_),
        gene.name = accessField(d, "officialName",NA_character_),
        gene.aliases = d %>% purrr::map(function(x){
            x$aliases %>% unlist
        }),
        # gene.Aliases = d[["aliases"]],
        # gene.GO = d[["numGoTerms"]],
        # gene.Homologues = d[["homologues"]],
        gene.MFX.rank = accessField(d, "multifunctionalityRank",NA_real_),
        processTaxon(d %>% purrr::map('taxon'))
        # phenotypes = d[["phenotypes"]]
    )
}

#' Processes JSON as a vector of taxa
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' \itemize{
#'     \item \code{taxon.name}: Name of the species
#'     \item \code{taxon.scientific}: Scientific name for the taxon
#'     \item \code{taxon.ID}: Internal identifier given to the species by Gemma
#'     \item \code{taxon.NCBI}: NCBI ID of the taxon
#'     \item \code{taxon.database.name}: Underlying database used in Gemma for the taxon
#'     \item \code{taxon.database.ID}: ID of the underyling database used in Gemma for the taxon
#' }
#'
#' @keywords internal
processTaxon <- function(d) {
    data.table(
        taxon.name = accessField(d,'commonName',NA_character_),
        taxon.scientific = accessField(d, "scientificName",NA_character_),
        taxon.ID = accessField(d,'id',NA_integer_),
        taxon.NCBI = accessField(d,"ncbiId",NA_integer_),
        taxon.database.name = d %>% purrr::map('externalDatabase') %>% accessField('name',NA_character_),
        taxon.database.ID = d %>% purrr::map('externalDatabase') %>% accessField('id',NA_integer_)
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

    data.table(
        chromosome = accessField(d,'chromosome',NA_character_),
        strand = accessField(d,'strand',NA_character_),
        nucleotide = accessField(d,'nucleotide',NA_integer_) ,
        length = accessField(d,"nucleotideLength",NA_integer_),
        processTaxon(d %>% purrr::map('taxon'))
    )
}

#' Processes JSON as GO terms
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the GO terms assigned to the
#' queried gene. A list if \code{raw = TRUE}. A \code{404 error} if the given identifier does not map to any
#' object.
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

    data.table(
        term.name = accessField(d,'term',NA_character_),
        term.ID = accessField(d, "goId", NA_character_),
        term.URI = accessField(d, "uri", NA_character_)
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
        dplyr::select(-c("ExternalID", "Bioassay"))
}

#' Processes expression matrix
#'
#' @param m The expression matrix to process
#'
#' @return A processed matrix
#'
#' @keywords internal
processExpressionMatrix <- function(m) {


    m <- m[,!colnames(m) %in% c('Sequence','GemmaId'),with = FALSE]
    # here we standardize the output column names so that they fit output
    # from other endpoints
    m_cols <- make.names(colnames(m))

    dataset <- parent.frame(n=2)$dataset
    samples <- get_dataset_samples(dataset, raw = TRUE)
    sample_ids <- samples %>% purrr::map('sample') %>% purrr::map_chr('name')
    sample_names <- samples %>% purrr::map_chr('name')
    sample_matches <- sample_ids %>% gsub(' ','',.,fixed = TRUE) %>%
        make.names %>% purrr::map_int(function(x){
            o <- grep(paste0(x,'_'),m_cols, fixed = TRUE)
            if(length(o)==0){
                return(NA_integer_)
            } else{
                return(o)
            }
        })
    sample_names <- sample_names[!is.na(sample_matches)]
    sample_matches <- sample_matches[!is.na(sample_matches)]


    colnames(m)[sample_matches] <- sample_names
    assertthat::assert_that(all(sample_names %in% colnames(m)))

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
            Probe = "probe_name",
            GeneSymbol = "gene_official_symbol",
            GeneName = "gene_official_name",
            NCBIid = "gene_ncbi_id"
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



#' Returns the ids of the found results
#' @return A data.table or a list of resultObjects
#'
#' @keywords internal
process_search <- function(d){

    if (attributes(d)$searchSettings$resultTypes[[1]] == "ubic.gemma.model.expression.experiment.ExpressionExperiment"){
        d %>% purrr::map('resultObject') %>% processDatasets()
    } else if(attributes(d)$searchSettings$resultTypes[[1]] == 'ubic.gemma.model.genome.Gene'){
        d %>% purrr::map('resultObject') %>% processGenes()
    } else if(attributes(d)$searchSettings$resultTypes[[1]] == "ubic.gemma.model.expression.arrayDesign.ArrayDesign"){
        d %>% purrr::map('resultObject') %>% processPlatforms()
    } else{
        d %>% purrr::map('resultObject')
    }


}


#' @keywords internal
process_dataset_gene_expression <- function(d){
    datasets <- d %>% purrr::map('datasetId')
    out<- lapply(d,function(x){
        x$geneExpressionLevels %>% lapply(function(y){

            expression <- y$vectors %>% lapply(function(z){
                z$bioAssayExpressionLevels %>% purrr::map_dbl(function(t){
                    if(is.double(t)){
                        return(t)
                    } else{
                        return(NaN)
                    }
                })
            }) %>% do.call(rbind,.)

            gene_data <- data.table(
                Probe = y$vectors %>% accessField('designElementName',NA_character_),
                GeneSymbol = y$geneOfficialSymbol,
                NCBIid = y$geneNcbiId
            )

            cbind(gene_data,expression)
        }) %>% do.call(rbind,.)

    })

    names(out) <- datasets


    return(out)
}


#' processQuantitationTypeValueObject
#' 
#' @param d The JSON to process
#' 
#' @return A data.table containing the quantitation types
#' 
#' The fields of the output data.table are:
#'
#' \itemize{
#'     \item \code{id}: If of the quantitation type. Any raw quantitation type
#'     can be accessed by \code{\link{get_dataset_raw_expression}} function using
#'     this id.
#'     \item \code{name}: Name of the quantitation type
#'     \item \code{description}: Description of the quantitation type
#'     \item \code{type}: Type of the quantitation type. Either raw or processed.
#'     Each dataset will have one processed quantitation type which is the data
#'     returned using \code{\link{get_dataset_processed_expression}}
#'     \item \code{preferred}: The preferred raw quantitation type. This version
#'     is used in generation of the processed data within gemma.
#'     \item \code{recomputed}: If TRUE this quantitation type is generated by 
#'     recomputing raw data files Gemma had access to.
#' }
#' @keywords internal
processQuantitationTypeValueObject <- function(d){
   # data.table(
   #     id = d %>% accessField('id'),
   #     description = d %>% accessField('description'),
   #     generalType = d %>% accessField('generalType'),
   #     isBackground = d %>% accessField('isBackground'),
   #     isBackgroundSubtracted = d %>% accessField('isBackgroundSubtracted'),
   #     isBatchCorrected = d  %>% accessField('isBatchCorrected'),
   #     isMaskedPreferred = d %>% accessField('isMaskedPreferred'),
   #     isNormalized = d %>% accessField('isNormalized'),
   #     isPreferred = d %>% accessField('isPreferred'),
   #     isRatio = d %>% accessField('isRatio'),
   #     isRecomputedFromRawData = d %>% accessField('isRecomputedFromRawData'),
   #     name = d %>% accessField('name'),
   #     representation = d %>% accessField('representation'),
   #     scale = d %>% accessField('scale'),
   #     type = d %>% accessField('type'),
   #     vectorType = d %>% accessField('vectorType')
   # )
    
    data.table(
        id = d %>% accessField('id'),
        name = d %>% accessField('name'),
        description = d %>% accessField('description'),
        type = d %>% accessField('vectorType') %>% grepl('ProcessedExpressionDataVector',.) %>% ifelse('processed','raw'),
        scale = d %>% accessField('scale'),
        preferred = d %>% accessField('isPreferred'),
        recomputed =  d %>% accessField('isRecomputedFromRawData')
    )
}

# processSVD <- function(d){
#     d$vMatrix$rawMatrix
#     browser()
# }
