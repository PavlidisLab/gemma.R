
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
        platform.shortName = accessField(d,"shortName",NA_character_),
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
#' \code{raw = TRUE}. Returns an empty list if no datasets matched.
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
#'     \item \code{experiment.batchEffectText}: A text field describing whether the dataset has batch effects
#'     \item \code{experiment.batchCorrected}: Whether batch correction has been performed on the dataset.
#'     \item \code{experiment.batchConfound}: 0 if batch info isn't available, -1 if batch counfoud is detected, 1 if batch information is available and no batch confound found
#'     \item \code{experiment.batchEffect}: -1 if batch p value < 0.0001, 1 if batch p value > 0.1, 0 if otherwise and when there is no batch information is available or when the data is confounded with batches.
#'     \item \code{experiment.rawData}: -1 if no raw data available, 1 if raw data was available. When available, Gemma reprocesses raw data to get expression values and batches
#'     \item \code{geeq.qScore}: Data quality score given to the dataset by Gemma.
#'     \item \code{geeq.sScore}: Suitability score given to the dataset by Gemma. Refers to factors like batches, platforms and other aspects of experimental design
#'     \item \code{taxon.name}: Name of the species
#'     \item \code{taxon.scientific}: Scientific name for the taxon
#'     \item \code{taxon.ID}: Internal identifier given to the species by Gemma
#'     \item \code{taxon.NCBI}: NCBI ID of the taxon
#'     \item \code{taxon.database.name}: Underlying database used in Gemma for the taxon
#'     \item \code{taxon.database.ID}: ID of the underyling database used in Gemma for the taxon
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
        experiment.batchEffectText = accessField(d, "batchEffect",NA_character_),
        experiment.batchCorrected = d %>% purrr::map('geeq') %>% accessField('batchCorrected',NA),
        experiment.batchConfound = d %>% purrr::map('geeq') %>% accessField("qScorePublicBatchConfound",NA_integer_),
        experiment.batchEffect = d %>% purrr::map('geeq') %>% accessField("qScorePublicBatchEffect",NA_integer_),
        experiment.rawData = d %>% purrr::map('geeq') %>% accessField("sScoreRawData",NA_integer_),
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
#'     \item \code{category.name}: Category that the annotation belongs to
#'     \item \code{category.URI}: URI for the category.name
#'     \item \code{value.name}: Annotation term
#'     \item \code{value.URI}: URI for the value.name
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


# good test cases 442 (subsets), 448, 200 (interaction), 174
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
#'     \item \code{factor.category}: Category for the contrast
#'     \item \code{factor.category.URI}: URI for the contrast category
#'     \item \code{factor.ID}: ID of the factor
#'     \item \code{baseline.factors}: Characteristics of the baseline. This field is a data.table
#'     \item \code{experimental.factors}: Characteristics of the experimental group. This field is a data.table
#'     \item \code{isSubset}: TRUE if the result set belong to a subset, FALSE if not. Subsets are created when performing differential expression to avoid unhelpful comparisons.
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
        
        experiment.ID<- ifelse(is.null(d[[i]]$sourceExperiment),
                               d[[i]]$bioAssaySetId, 
                               d[[i]]$sourceExperiment)
        
        results <- seq_along(result_ids[[i]]) %>% lapply(function(j){

            # re-order experimental factors based on their IDs to ensure compatibility
            # with dif exp tables
            factor_ids <- d[[i]]$resultSets[[j]]$experimentalFactors %>% purrr::map_int('id')
            d[[i]]$resultSets[[j]]$experimentalFactors <- 
                d[[i]]$resultSets[[j]]$experimentalFactors[order(factor_ids)]
            

            if(length(d[[i]]$resultSets[[j]]$experimentalFactors)==1){
                contrast_id <-  d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$values %>%
                    accessField('id',NA_integer_)
                baseline_id <- d[[i]]$resultSets[[j]]$baselineGroup$id
                
                non_control_factors <- d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$values[!contrast_id %in% baseline_id]
                contrast.ID <- contrast_id[!contrast_id %in% baseline_id]
                size <- length(contrast.ID)
                
                experimental.factors <- non_control_factors %>% 
                    purrr::map(processFactorValueValueObject)
                
                baseline.factors <- d[[i]]$resultSets[[j]]$baselineGroup %>% 
                    processFactorValueValueObject %>% list() %>% rep(size)
                

            }else{
                # if more than 2 factors are present take a look at the the other
                # factor values to idenfity the baseline values

                # order the factors based on their ids
                factor_ids <- d[[i]]$resultSets[[j]]$experimentalFactors %>% purrr::map_int('id')
                factor_order <- order(factor_ids)
                d[[i]]$resultSets[[j]]$experimentalFactors <- d[[i]]$resultSets[[j]]$experimentalFactors[factor_order]
                factor_ids <- factor_ids[factor_order]
                
                
                ids <- d[[i]]$resultSets[[j]]$experimentalFactors %>%
                    purrr::map('values')  %>%
                    purrr::map(function(x){x %>% accessField('id')}) %>%
                    expand.grid()

                names(ids) <- factor_ids
                
                baseline_ids <- d[[i]]$resultSets %>% lapply(function(x){
                    x$baselineGroup$id
                }) %>% unlist


                relevant_ids <- ids[
                    apply(ids,1, function(x){
                        !any(x %in% baseline_ids)})
                    ,]
                colnames(relevant_ids) <-factor_ids
                
                all_factors <- d[[i]]$resultSets[[j]]$experimentalFactors %>% lapply(function(y){
                    y$values %>% purrr::map(processFactorValueValueObject) %>% do.call(rbind,.)
                }) %>% do.call(rbind,.)
                
                baseline_factors <- seq_along(baseline_ids) %>% lapply(function(i){
                    all_factors %>%
                        dplyr::filter(ID == baseline_ids[i] & factor.ID == colnames(relevant_ids)[i])
                }) %>% do.call(rbind,.)
                
                experimental.factors <- seq_len(nrow(relevant_ids)) %>% purrr::map(function(i){
                    seq_along(relevant_ids[i,]) %>% purrr::map(function(j){
                        all_factors %>% dplyr::filter(ID == relevant_ids[i,j] & factor.ID == colnames(relevant_ids)[j]) 
                    }) %>% do.call(rbind,.)
                })
                
                size <- length(experimental.factors)
                
                contrast.ID <- unname(apply(relevant_ids,1,paste,collapse = '_'))
                baseline.factors <- list(baseline_factors) %>% rep(size)
            }
            
            out <- data.table(
                result.ID = d[[i]]$resultSets[[j]]$id,
                contrast.ID = contrast.ID,
                experiment.ID = experiment.ID,
                factor.category = d[[i]]$resultSets[[j]]$experimentalFactors %>% 
                    purrr::map_chr('category') %>% unlist %>% sort %>%
                    paste(collapse = ','),
                factor.category.URI = d[[i]]$resultSets[[j]]$experimentalFactors %>% 
                    purrr::map_chr('categoryUri') %>% unlist %>% sort %>% paste(collapse = ','),
                factor.ID = d[[i]]$resultSets[[j]]$experimentalFactors %>% 
                    purrr::map_int('id') %>% unlist %>% sort %>% paste(collapse=','),
                baseline.factors = baseline.factors,
                experimental.factors = experimental.factors,
                isSubset = !is.null(d[[i]]$subsetFactorValue),
                subsetFactor = d[[i]]$subsetFactorValue %>% processFactorValueValueObject %>% list %>% rep(size),
                probes.analyzed = d[[i]]$resultSets[[j]]$numberOfProbesAnalyzed %>% nullCheck(NA_integer_),
                genes.analyzed =  d[[i]]$resultSets[[j]]$numberOfGenesAnalyzed %>% nullCheck(NA_integer_)
            )
            
            return(out)
            
        }) %>% do.call(rbind,.)
        
    }) %>% do.call(rbind,.)
    
    if(is.null(result_factors)){
        return(data.table(
            result.ID = integer(0),
            contrast.ID = integer(0),
            experiment.ID = integer(0),
            factor.category = character(0),
            factor.category.URI = character(0),
            factor.ID = character(0),
            baseline.factors = list(),
            experimental.factors = list(),
            isSubset = logical(0),
            subsetFactor = list(),
            probes.analyzed = integer(0),
            genes.analyzed = integer(0)
        ))
    } else{
        return(result_factors)
    }
}


#' Process JSON of a result set
#' 
#' @return A data table with information about the queried result sets. Note that this function does not return
#' differential expression values themselves. Use \code{\link{get_differential_expression_values}}
#' to get differential expression values
#' 
#' 
#' \itemize{
#'     \item \code{result.ID}: Result set ID of the differential expression analysis.
#'     May represent multiple factors in a single model.
#'     \item \code{contrast.ID}: Id of the specific contrast factor. Together with the result.ID
#'     they uniquely represent a given contrast.
#'     \item \code{experiment.ID}: Id of the source experiment
#'     \item \code{factor.category}: Category for the contrast
#'     \item \code{factor.category.URI}: URI for the contrast category
#'     \item \code{factor.ID}: ID of the factor
#'     \item \code{baseline.factors}: Characteristics of the baseline. This field is a data.table
#'     \item \code{experimental.factors}: Characteristics of the experimental group. This field is a data.table
#'     \item \code{isSubset}: TRUE if the result set belong to a subset, FALSE if not. Subsets are created when performing differential expression to avoid unhelpful comparisons.
#'     \item \code{subsetFactor}: Characteristics of the subset. This field is a data.table
#' }
#' 
#' @keywords internal
processDifferentialExpressionAnalysisResultSetValueObject = function(d){
    out <- d %>% lapply(function(x){
        experiment.ID <- ifelse(is.null(x$analysis$sourceExperiment),
                                x$analysis$bioAssaySetId,
                                x$analysis$sourceExperiment)
        
        # re-order experimental factors based on their IDs to ensure compatibility
        # with dif exp tables
        factor_ids <- x$experimentalFactors %>% purrr::map_int('id')
        x$experimentalFactors <- x$experimentalFactors[order(factor_ids)]
        

        if(length(x$experimentalFactors) == 1){
            contrast_id <- x$experimentalFactors[[1]]$values %>% accessField('id',NA_integer_)
            baseline_id <- x$baselineGroup$id
            
            non_control_factors <- x$experimentalFactors[[1]]$values[!contrast_id %in% baseline_id]
            contrast.ID <- contrast_id[!contrast_id %in% baseline_id]
            size <- length(contrast.ID)
            
            experimental.factors <- non_control_factors %>% 
                purrr::map(processFactorValueValueObject)
            baseline.factors <- x$baselineGroup %>% processFactorValueValueObject() %>% list() %>% rep(size)
            
        } else{
            # if more than 2 factors are present take a look at the the other
            # factor values to idenfity the baseline values
            
            # order the factors based on their ids
            factor_ids <- x$experimentalFactors %>% purrr::map_int('id')
            factor_order <- order(factor_ids)
            x$experimentalFactors <- x$experimentalFactors[factor_order]
            factor_ids <- factor_ids[factor_order]
            
            
            ids <- x$experimentalFactors %>% purrr::map('values') %>% 
                purrr::map(function(x){x %>% accessField('id')}) %>% 
                expand.grid()
            
            names(ids) <- factor_ids
            
            
            subsetFactor <-  x$analysis$subsetFactorValue %>%
                processFactorValueValueObject() 
            
            all_sets <- get_result_sets(datasets = experiment.ID,raw = TRUE)
            
            all_sets <- all_sets %>% sapply(function(y){
                subset <- y$analysis$subsetFactorValue %>%
                    processFactorValueValueObject()
                all(subsetFactor$ID %in% subset$ID)
            }) %>% {all_sets[.]}
            
            baseline_ids <- all_sets %>% lapply(function(y){
                y$baselineGroup$id
            }) %>% unlist
            
            relevant_ids <- ids[apply(ids,1, function(y){!any(y %in% baseline_ids)}),]
            
            
            
            colnames(relevant_ids) <-factor_ids
            
            
            all_factors <- x$experimentalFactors %>% lapply(function(y){
                y$values %>% purrr::map(processFactorValueValueObject) %>% do.call(rbind,.)
            }) %>% do.call(rbind,.)
            
            baseline_factors <- all_factors %>% dplyr::filter(ID %in% baseline_ids)
            
            
            experimental.factors <- seq_len(nrow(relevant_ids)) %>% purrr::map(function(i){
                all_factors %>% dplyr::filter(ID %in% relevant_ids[i,])
            })
            
            size <- length(experimental.factors)
            
            contrast.ID <- unname(apply(relevant_ids,1,paste,collapse = '_'))
            baseline.factors <- list(baseline_factors) %>% rep(size)
        }

        out <- data.table(
            result.ID = x$id,
            contrast.ID = contrast.ID,
            experiment.ID = experiment.ID,
            factor.category = x$experimentalFactors %>%
                purrr::map_chr('category') %>% unlist %>% sort %>%
                paste(collapse = ','),
            factor.category.URI = x$experimentalFactors %>%
                purrr::map_chr('categoryUri') %>% unlist %>% sort %>%
                paste(collapse = ','),
            factor.ID = x$experimentalFactors %>%  purrr::map_int('id') %>% unlist %>% sort %>% paste(collapse=','),
            baseline.factors = baseline.factors,
            experimental.factors = experimental.factors,
            isSubset = !is.null(x$analysis$subsetFactorValue),
            subsetFactor =  x$analysis$subsetFactorValue %>%
                processFactorValueValueObject() %>% list() %>% rep(size)
        )
        
        return(out)
        
    }) %>% do.call(rbind,.)
    
    if(is.null(out)){
        return(data.table(
            result.ID = integer(0),
            contrast.ID =  integer(0),
            experiment.ID = integer(0),
            factor.category = character(0),
            factor.category.URI = character(0),
            factor.ID = character(0),
            baseline.factors = list(),
            experimental.factors = list(),
            isSubset = logical(0),
            subsetFactor = list()
        ))
        
    } else{
        return(out)
    }
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
#'     \item \code{object.class}: Class of object that the term originated from.
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
        term.URI = accessField(d,"termUri",NA_character_),
        object.class = accessField(d,'objectClass',NA_character_)
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
    ret <- read_gzipped_tsv(content)
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
#'     \item \code{sample.name}: Internal name given to the sample.
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
    ) %>% dplyr::arrange(sample.ID)
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
#'  \item \code{platform.shortName}: Shortname of the platform.
#'  \item \code{platform.name}: Full name of the platform.
#'  \item \code{platform.description}: Free text description of the platform
#'  \item \code{platform.troubled}: Whether or not the platform was marked "troubled" by a Gemma process or a curator
#'  \item \code{platform.experimentCount}: Number of experiments using the platform within Gemma
#'  \item \code{platform.type}: Technology type for the platform.
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
#'      \item \code{element.name}: Name of the element. Typically the probeset name
#'      \item \code{element.description}: A free text field providing optional information about the element
#'      \item \code{platform.shortName}: Shortname of the platform given by Gemma. Typically the GPL identifier.
#'      \item \code{platform.name}: Full name of the platform
#'      \item \code{platform.ID}: Id number of the platform given by Gemma
#'      \item \code{platform.type}: Type of the platform.
#'      \item \code{platform.description}: Free text field describing the platform.
#'      \item \code{platform.troubled}: Whether the platform is marked as troubled by a Gemma curator.
#'      \item \code{taxon.name}: Name of the species platform was made for
#'      \item \code{taxon.scientific}: Scientific name for the taxon
#'      \item \code{taxon.ID}: Internal identifier given to the species by Gemma
#'      \item \code{taxon.NCBI}: NCBI ID of the taxon
#'      \item \code{taxon.database.name}: Underlying database used in Gemma for the taxon
#'      \item \code{taxon.database.ID}: ID of the underyling database used in Gemma for the taxon
#'  }
#'
#' @keywords internal
processElements <- function(d) {
    data.table(
        element.name = accessField(d,'name',NA_character_),
        element.description =accessField(d,'description',NA_character_),
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
#'     \item \code{taxon.scientific}: Scientific name for the taxon
#'     \item \code{taxon.ID}: Internal ID for the taxon given by Gemma
#'     \item \code{taxon.NCBI}: NCBI ID for the taxon
#'     \item \code{taxon.database.name}: Name of the database used in Gemma for the taxon
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
#'     \item \code{term.name}: Name of the term
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
    # we use the order returned by get_dataset_samples as authoritative which makes
    # it a bit awkward when we need to access a property left out of the processed output
    samples <- get_dataset_samples(dataset) 
    raw_ids <- attributes(samples)$env$response %>% purrr::map('sample') %>% 
        purrr::map_int('id')
    sample_internal_names <- attributes(samples)$env$response %>% 
        purrr::map('sample') %>% purrr::map_chr('name') %>%
        {.[match(samples$sample.ID,raw_ids)]}
    sample_names <- samples$sample.name
    sample_matches <- sample_internal_names %>% gsub(' ','',.,fixed = TRUE) %>%
        make.names %>% purrr::map_int(function(x){
            o <- grep(paste0(x,'_'),m_cols, fixed = TRUE)
            if(length(o)==0){
                return(NA_integer_)
            } else{
                return(o)
            }
        })
    sample_names_reord <- sample_names[!is.na(sample_matches)]
    sample_matches <- sample_matches[!is.na(sample_matches)]

    # reorder to match sample output
    colnames(m)[sample_matches] <- sample_names_reord
    assertthat::assert_that(all(sample_names %in% colnames(m)))
    
    non_samples <- colnames(m)[!colnames(m) %in% sample_names]
    m %>% data.table::setcolorder(c(non_samples,sample_names))
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
    

    m <- m %>%
        dplyr::rename(
            Probe = "probe_name",
            GeneSymbol = "gene_official_symbol",
            GeneName = "gene_official_name",
            NCBIid = "gene_ncbi_id"
        )
    
    # match the order of factors with the order returned from the result set endpoints
    if(any(grepl('contrast_[0-9]+?_[0-9]+?_',colnames(m)))){
        result_set <- get_result_sets(resultSets = parent.frame(n=2)$resultSet)
        colnames(m)[grepl('contrast_[0-9]+?_[0-9]+?_',colnames(m))] <- 
            colnames(m)[grepl('contrast_[0-9]+?_[0-9]+?_',colnames(m))] %>% strsplit('_') %>% sapply(function(x){
                ifelse(glue::glue("{x[2]}_{x[3]}") %in% result_set$contrast.ID,
                       paste(x,collapse = '_'),
                       {
                           # just to make sure something horribly wrong is happening
                           assertthat::assert_that(glue::glue("{x[3]}_{x[2]}") %in% result_set$contrast.ID)
                           o <- x[2]; x[2] <- x[3]; x[3] <- o;
                           paste(x,collapse = '_')
                       })
                
            }) 
    }

    
    return(m)
}

#' Replaces factor ids by the factors strings in DE table columns
#'
#' @param rs The resultSet matrix to process
#'
#' @return A processed matrix
#'
#' @keywords internal
processDEcontrasts <- function(rs, rsID) {
    factors <- get_result_sets(resultSets = rsID)$experimental.factors %>%
        do.call(rbind,.) %>% unique

    colnames(rs) <- stringr::str_replace(colnames(rs), "log2fc", "logFoldChange")
    # Replace factor IDs by the factor names
    for (f in factors$ID) {
        colnames(rs) <- stringr::str_replace(colnames(rs), as.character(f), factors[factors$ID == f,]$summary)
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
        dataset_exp <- x$geneExpressionLevels %>% lapply(function(y){

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
        
        samples <- get_dataset_samples(x$datasetId)
        
        if(is.null(dataset_exp)){
            gene_data <- data.table(Probe = character(0),
                       GeneSymbol = character(0),
                       NCBIid = integer(0))
            
            expression <- samples$sample.name %>% lapply(function(x){numeric(0)})
            names(expression) = samples$sample.name
            return(cbind(gene_data, do.call(data.table,expression)))
        } else{
            return(data.table::setcolorder(dataset_exp,
                                    c(colnames(dataset_exp)[!colnames(dataset_exp) %in% samples$sample.name],
                                      samples$sample.name)))
        }
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
#'     \item \code{ratio}: Whether or not the quanitation type is a ratio of multiple
#'     quantitation types. Typically TRUE for processed TWOCOLOR quantitation type.
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
        ratio = d %>% accessField('isRatio'),
        scale = d %>% accessField('scale'),
        preferred = d %>% accessField('isPreferred'),
        recomputed =  d %>% accessField('isRecomputedFromRawData')
    )
}

# processSVD <- function(d){
#     d$vMatrix$rawMatrix
#     browser()
# }
