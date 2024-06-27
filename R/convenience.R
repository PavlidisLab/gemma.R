#' Authentication by user name
#'
#' Allows the user to access information that requires logging in to Gemma. To log out, run `set_gemma_user` without specifying the username or password.
#'
#' @param username Your username (or empty, if logging out)
#' @param password Your password (or empty, if logging out)
#'
#' @keywords misc
#' @return TRUE if authentication is successful, FALSE if not
#' @export
set_gemma_user <- function(username = NULL, password = NULL) {
    options(gemma.username = username)
    options(gemma.password = password)
    response <- gemma_call('',json = FALSE)
    if(response$status_code==200){
        return(TRUE)
    } else{
        return(FALSE)
    }
}

#' Retrieve Platform Annotations by Gemma
#'
#' Gets Gemma's platform annotations including mappings of microarray probes to genes.
#'
#' @param platform A platform numerical identifiers or platform short name.
#' @param annotType Which GO terms should the output include
#' @param file Where to save the annotation file to, or empty to just load into memory
#' @param overwrite Whether or not to overwrite an existing file
#' @inheritParams memoise
#' @param unzip Whether or not to unzip the file (if @param file is not empty)
#'
#' @return A table of annotations
#' \itemize{
#'     \item \code{ProbeName}: Probeset names provided by the platform.
#'     Gene symbols for generic annotations
#'     \item \code{GeneSymbols}: Genes that were found to be aligned to
#'     the probe sequence. Note that it is possible for probes to be
#'     non-specific. Alignment to multiple genes are indicated with gene
#'     symbols separated by "|"s
#'     \item \code{GeneNames}: Name of the gene
#'     \item \code{GOTerms}: GO Terms associated with the genes. \code{annotType}
#'     argument can be used to choose which terms should be included.
#'     \item \code{GemmaIDs} and \code{NCBIids}: respective IDs for the genes.
#' }
#' @keywords platform
#' @export
#' @examples
#' head(get_platform_annotations("GPL96"))
#' head(get_platform_annotations('Generic_human_ncbiIds'))
get_platform_annotations <- function(platform,
    annotType = c("noParents","allParents","bioProcess"),
    file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE),
    memoised = getOption("gemma.memoise", FALSE),
    unzip = FALSE){

    if (memoised){
        if (!is.na(file)){
            warning("Saving to files is not supported with memoisation.")
        }
        if ("character" %in% class(gemmaCache()) && gemmaCache() ==
            "cache_in_memory") {
            return(mem_in_memory_cache("get_platform_annotations",
                                       platform = platform,
                                       annotType = annotType,
                                       file = NA,
                                       overwrite = overwrite,
                                       memoised = FALSE,
                                       unzip = unzip
            ))
        } else{
            out <- memget_platform_annotations(
                platform = platform,
                annotType = annotType,
                file = NA,
                overwrite = overwrite,
                memoised = FALSE,
                unzip = unzip
            )
            return(out)
        }

    }

    if (!is.numeric(platform)) {
        platforms <- get_platforms_by_ids(platform)
        if (!isTRUE(nrow(platforms) == 1)) {
            stop(platform, " is not a valid single platform.")
        }
        platform <- platforms[, "platform.ID"]
    }

    annotType <- match.arg(annotType)

    is.tmp <- is.na(file)

    if (is.na(file)) {
        file <- tempfile(fileext = ".gz")
    } else {
        file <- paste0(tools::file_path_sans_ext(file), ".gz")
    }

    doReadFile <- function(file) {
        if (file.exists(file)) {
            tmp <- gzfile(file)
            ret <- tmp %>%
                readLines() %>%
                .[which(!startsWith(., "#"))[1]:length(.)] %>%
                # Strip comments
                paste0(collapse = "\n") %>%
                {
                    fread(text = .)
                }
            close(tmp)

            if (!is.tmp && unzip) {
                utils::write.table(ret, tools::file_path_sans_ext(file),
                    sep = "\t", quote = FALSE, row.names = FALSE)
            }

            if (is.tmp || unzip) {
                unlink(file)
            }

            ret
        } else {
            fread(tools::file_path_sans_ext(file))
        }
    }

    if ((file.exists(file) || file.exists(tools::file_path_sans_ext(file))) && !overwrite) {
        warning(tools::file_path_sans_ext(file), " exists. Not overwriting.")
        doReadFile(file)
    } else {
        response <- httr::GET(glue::glue(
            paste0(dirname(dirname(gemmaPath())),
                "/arrays/downloadAnnotationFile.html?id={platform}&fileType={annotType}")),
            httr::write_disk(file))
        if (response$status_code==200){
            return(doReadFile(file))
        } else{
            warning(glue::glue("Unable to access annotation file for {platform}. Can get more information about the platform at https://gemma.msl.ubc.ca/arrays/showArrayDesign.html?id={platform}"))
            return(NULL)
        }
    }
}

#' Memoise get_platform_annotations
#'
#' @noRd
memget_platform_annotations <- function(platform,
                                     annotType = c("bioProcess", "noParents", "allParents"),
                                     file = getOption("gemma.file", NA_character_),
                                     overwrite = getOption("gemma.overwrite", FALSE),
                                     memoised = getOption("gemma.memoise", FALSE),
                                     unzip = FALSE) {
    mem_call <- memoise::memoise(get_platform_annotations, cache = gemmaCache())
    mem_call(
        platform = platform, annotType = annotType, memoised = FALSE, file = file,
        overwrite = overwrite, unzip=unzip
    )
}


#' Make simplified design frames
#' 
#' Using on the output of \code{\link{get_dataset_samples}}, this function creates
#' a simplified design table, granting one column to each experimental variable
#' 
#' @param samples An output from get_dataset_samples. The output should not be raw
#' @param metaType Type of metadata to include in the output. "text", "uri" or "both"
#' 
#' @return A data.frame including the design table for the dataset
#' 
#' @examples 
#' samples <- get_dataset_samples('GSE46416') 
#' make_design(samples)
#' 
#' @keywords misc
#' 
#' @export
make_design <- function(samples,metaType = "text"){
    
    metaType <- match.arg(metaType, c('text','uri','both'))
    
    categories <- samples$sample.factorValues %>% purrr::map(
        function(x){
            x %>% dplyr::select('factor.ID','factor.category','factor.category.URI')
        }) %>% do.call(rbind,.) %>% unique
    
    
    factorURIs <- categories$factor.ID %>% lapply(function(x){
        samples$sample.factorValues %>% purrr::map_chr(function(y){
            y$factor.ID[is.na(y$factor.ID)]<-'NA'
            
            y %>% dplyr::filter(factor.ID == x) %>% {.$value.URI} %>% sort %>% paste(collapse = ',')
        })
    })
    
    text <- categories$factor.ID %>% lapply(function(x){
        samples$sample.factorValues %>% purrr::map_chr(function(y){
            y$factor.ID[is.na(y$factor.ID)]<-'NA'
            
            y %>% dplyr::filter(factor.ID == x) %>% 
                dplyr::mutate(text = ifelse(is.na(summary),value,summary)) %>% 
                {.$text} %>% unique %>% sort %>% paste(collapse = ',')
        })
    })
    
    if(metaType == 'text'){
        design_frame <- text %>% as.data.frame()
        colnames(design_frame) <- categories$factor.category
    } else if (metaType == 'uri'){
        design_frame <- factorURIs %>% as.data.frame()
        colnames(design_frame) <- categories$factor.category.URI
    } else if (metaType == 'both'){
        design_frame <- seq_along(text) %>% lapply(function(i){
            paste(text[[i]],factorURIs[[i]],sep = '|')
        }) %>% as.data.frame()
        colnames(design_frame) <- paste( categories$factor.category,categories$factor.category.URI,sep = '|')
    }
    design_frame <- design_frame %>% dplyr::mutate(factorValues = samples$sample.factorValues, .before = 1)
    rownames(design_frame) <- samples$sample.name
    return(design_frame)
}


#' Get a subset of an array of factorValues
#' @param factorValue unimplemented
#' @param differential_expressions  
#' @keywords internal
#' @return a boolean vector, samples representing the resultSet and/or the contrast
#' are set to TRUE
subset_factorValues <- function(factorValues,
                                factorValue = NULL,
                                differential_expressions = NULL,
                                resultSet = NULL,
                                contrast = NULL){
    out <- rep(TRUE,length(factorValues))
    
    
    if(!is.null(factorValue)){
        # unimplemented
    }
    
    if(!is.null(differential_expressions)){
        
        # this should never trigger but just in case...
        assertthat::assert_that(
            factorValues %>% do.call(rbind,.) %>% dplyr::select(ID,factor.ID) %>% unique %>% .$ID %>% table %>% {all(.==1)},
            msg = "ID's cannot be repeated across factors")
        
        
        
        subset <- differential_expressions %>%   dplyr::filter(result.ID == resultSet) %>% .$subsetFactor %>% unique
        # result set should have the same subset for all contrasts
        assertthat::assert_that(length(subset)==1)
        
        if(nrow(subset[[1]])!=0){
            
            subset_ids <- subset %>% purrr::map('ID') %>% unlist
            
            in_subset <- factorValues %>% purrr::map_lgl(function(x){
                any(x$ID %in% subset_ids)
            })
            
            
            # subset_ids <- subset[[1]] %>%
            #     dplyr::mutate(comb = paste0(ID,'.',factor.ID)) %>% {.$comb}
            # 
            # in_subset <- factorValues %>% purrr::map_lgl(function(x){
            #     x %>%  dplyr::mutate(comb = paste0(ID,'.',factor.ID)) %>% {.$comb} %>%
            #         {all(subset_ids %in% .)}
            # })
            
            out <- out & in_subset
        }
        
        
        if(!is.null(contrast)){
            cn <- differential_expressions %>% dplyr::filter(result.ID == resultSet & contrast.ID == contrast)
            baseline_id <- cn$baseline.factors %>% purrr::map('ID') %>% unlist%>% unique
            baseline_factor_id <- cn$baseline.factors %>% purrr::map('factor.ID') %>% unlist%>% unique
            
            contrast_id <- cn$experimental.factors %>% purrr::map('ID') %>% unlist %>% unique
            contrast_factor_id <-  cn$experimental.factors %>%  purrr::map('factor.ID') %>% unlist %>% unique
            
            contrast_id<- contrast_id[match(baseline_factor_id,contrast_factor_id)]
            
            in_contrast <- factorValues %>% purrr::map_lgl(function(x){
                all(contrast_id %in% x$ID) | 
                    all(baseline_id %in% x$ID) |
                    all(c(contrast_id[1],baseline_id[2]) %in% x$ID) |
                    all(c(contrast_id[2],baseline_id[1]) %in% x$ID)
            })
            
            out <- out & in_contrast
            
        }
    }
    return(out)
}




#' Compile gene expression data and metadata
#'
#' Return an annotated Bioconductor-compatible
#' data structure or a long form tibble of the queried dataset, including
#' expression data and the experimental design.
#'
#' @param type "se"for a SummarizedExperiment or "eset" for Expression Set. We recommend using
#' SummarizedExperiments which are more recent. See the Summarized experiment
#' \href{https://bioconductor.org/packages/release/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html}{vignette}
#' or the ExpressionSet \href{https://bioconductor.org/packages/release/bioc/vignettes/Biobase/inst/doc/ExpressionSetIntroduction.pdf}{vignette}
#' for more details. "tidy" for a long form data frame compatible with tidyverse functions.
#' 'list' to return a list containing individual data frames containing expression values,
#' design and the experiment.
#' @inheritParams memoise
#' @inheritParams get_dataset_expression_for_genes
#' @param metaType How should the metadata information should be included. Can be "text", "uri" or "both". "text" and "uri" options
#' @param resultSets Result set IDs of the a differential expression analysis. Optional. If provided, the output will only include
#' the samples from the subset used in the result set ID.
#' Must be the same length as \code{datasets}.'
#' @param contrasts Contrast IDs of a differential expression contrast. Optional. Need resultSets to be defined to work. If provided, the
#' output will only include samples relevant to the specific contrats.
#'
#' @return A list of \code{\link[SummarizedExperiment]{SummarizedExperiment}}s,
#' \code{\link[Biobase]{ExpressionSet}}s or a tibble containing metadata and
#' expression data for the queried datasets and genes. Metadata will be expanded to include
#' a variable number of factors that annotates samples from a dataset but will
#' always include single "factorValues" column that houses data.tables that
#' include all annotations for a given sample.
#' @keywords dataset
#' @export
#' @examples
#' get_dataset_object("GSE2018")
get_dataset_object <- function(datasets,
                               genes = NULL,
                               keepNonSpecific = FALSE,
                               consolidate = NA_character_,
                               resultSets = NULL,
                               contrasts = NULL,
                               metaType = 'text',
                               type = "se",
                               memoised = getOption("gemma.memoised", FALSE)) {
    if (type != "eset" && type != "se" && type != 'tidy' && type != 'list') {
        stop("Please enter a valid type: 'se' for SummarizedExperiment, 'eset' for ExpressionSet, 'tidy' for a long form tibble, 'list' for an R list")
    }
    unique_sets = unique(datasets)

    metadata <- unique_sets %>% lapply(function(dataset){
        get_dataset_samples(dataset,memoised = memoised)
    })
    names(metadata) <- unique_sets

    if(is.null(genes)){
        expression <- unique_sets %>% lapply(function(dataset){
            exp <- get_dataset_processed_expression(dataset,memoised = memoised)
            meta <- metadata[[as.character(dataset)]]

            # these replicate the arguments for get_dataset_expression_for_genes
            if(!keepNonSpecific){
                exp <- exp[!(grepl("|",exp$GeneSymbol,fixed = TRUE) | exp$GeneSymbol == ""),]
            }
            if(!is.na(consolidate) && consolidate == "pickmax"){
                mean_exp <- exp[,.SD,.SDcols = meta$sample.name] %>% apply(1,function(x){
                    mean(stats::na.omit(x))
                })
                exp <- exp[order(mean_exp,decreasing = TRUE),] %>% {.[!duplicated(.$GeneSymbol),]}
            } else if(!is.na(consolidate) && consolidate == 'pickvar'){
                exp_var <- exp[,.SD,.SDcols = meta$sample.name] %>% apply(1,function(x){
                    stats::var(stats::na.omit(x))
                })
                exp <- exp[order(exp_var,decreasing = TRUE),] %>% {.[!duplicated(.$GeneSymbol),]}
            } else if(!is.na(consolidate) && consolidate == 'average'){
                dups <- exp$GeneSymbol %>% {.[duplicated(.)]} %>% unique
                dup_means <- dups %>% lapply(function(x){
                    dup_subset <- exp[exp$GeneSymbol %in% x,]
                    dup_mean <- dup_subset[,.SD,.SDcols = meta$sample.name] %>% apply(2,mean)
                    probe <- paste0('Averaged from ',paste0(dup_subset$Probe, collapse = ' '))
                    dup_out <- data.frame(Probe = probe,
                                          dup_subset[1,.SD,.SDcols = - c('Probe',meta$sample.Name)],
                                          t(dup_mean),
                                          check.names = FALSE)
                }) %>% do.call(rbind,.)

                exp <- exp[!exp$GeneSymbol %in% dups,]
                exp <- rbind(exp,dup_means)

            }

            # multi platform datasets may have repeated probesets which needs new names
            # most multiplatform datasets were merged into artifical probesets defined
            # within gemma but at the time of writing 365 was an exception
            duplicate_probes <- exp$Probe[duplicated(exp$Probe)]

            for(dp in duplicate_probes){
                to_replace <- exp$Probe[exp$Probe %in% dp]
                to_paste <- sapply(seq_along(to_replace), function(i){
                    if(i == 1){
                        ''
                    }else{
                        paste0('.',i)
                    }
                })

                exp$Probe[exp$Probe %in% dp] <- paste0(to_replace,to_paste)
            }
            return(exp)
        })
        names(expression) <-  unique_sets
    } else{
        expression <- get_dataset_expression_for_genes(unique_sets,
                                                       genes = genes, 
                                                       keepNonSpecific = keepNonSpecific,
                                                       consolidate = consolidate,
                                                       memoised = memoised)
        names(expression) <-  unique_sets
    }

    # bit of a bottleneck
    designs <- metadata %>% lapply(function(meta){
        make_design(meta,metaType)
    })
    
    dat = get_datasets_by_ids(unique_sets,raw = FALSE, memoised = memoised)

    # pack the information that will be included in all outputs
    packed_data <- seq_along(datasets) %>% lapply(function(i){
        dataset <- datasets[i]
        # we don't want to pass data.tables by reference because
        # same datasets might be re-used
        packed_info <-
            list(design = data.table::copy(designs[[as.character(dataset)]]),
                 exp = data.table::copy(expression[[as.character(dataset)]]),
                 result_set = resultSets[i],
                 contrast = contrasts[i],
                 dat =  dat %>% dplyr::filter(experiment.ID==dataset | experiment.shortName == dataset))
        
        # create unique probe ids. needed for rownames and merging
        # probe ids are usually unique but there are exceptions
        unique_probes = packed_info$exp$Probe
        append = integer(length(unique_probes))
        dups = duplicated(unique_probes)
        while(any(dups)){
            append[dups] = append[dups]+1
            dups = duplicated(paste0(unique_probes,append))
        }
        append[append==0] = ""
        unique_probes = paste0(unique_probes,append)
        packed_info$unique_probes = unique_probes

        # reorders the expression to match the metadata
        # no longer necesary
        # data.table::setcolorder(packed_info$exp,c(gene_info,rownames(packed_info$design)))

        if(!is.null(resultSets)){
            
            gene_info <- colnames(packed_info$exp)[!colnames(packed_info$exp) %in% rownames(packed_info$design)]
            
            diff <- get_dataset_differential_expression_analyses(dataset,memoised = memoised)
            
            # passing the original samples is fine since expression data is
            # reordered not design file
            relevant <- subset_factorValues(packed_info$design$factorValues,
                                            differential_expressions = diff,
                                            resultSet = resultSets[i],
                                            contrast = contrasts[i])
            
            packed_info$exp <- packed_info$exp[,.SD,.SDcols = c(gene_info, rownames(packed_info$design)[relevant])]
            packed_info$design <- packed_info$design[relevant,]
        }

        return(packed_info)
    })
    if(!is.null(resultSets)){
        names(packed_data) <- paste0(
            packed_data %>% purrr::map('dat') %>% purrr::map_int('experiment.ID'),
            '.',resultSets)
        if(!is.null(contrasts)){
            names(packed_data) <- paste0(names(packed_data),'.',contrasts)
        }
    } else{
        names(packed_data) <- packed_data %>% purrr::map('dat') %>% purrr::map_int('experiment.ID')
    }

    if (type == 'se'){
        out <- packed_data %>% lapply(function(data){
            exprM <- data$exp
            design <- data$design
            rownames(exprM) <- data$unique_probes
            
            
            genes <- S4Vectors::DataFrame(exprM[,.SD,.SDcols = colnames(exprM)[colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]])
            exprM <- exprM[,.SD,.SDcols = colnames(exprM)[!colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]] %>%
                data.matrix()

            # reordering happens above
            # exprM <- exprM[, match(rownames(design), colnames(exprM))]



            expData <- list(
                title = data$dat$experiment.name,
                abstract = data$dat$experiment.description,
                url = paste0("https://gemma.msl.ubc.ca/expressionExperiment/showExpressionExperiment.html?id=", data$dat$experiment.ID),
                database = data$dat$experiment.database,
                accesion = data$dat$experiment.accession,
                gemmaQualityScore = data$dat$geeq.qScore,
                gemmaSuitabilityScore = data$dat$geeq.sScore,
                taxon = data$dat$taxon.Name
            )


            SummarizedExperiment::SummarizedExperiment(
                assays = list(counts = exprM),
                rowData = genes,
                colData = design,
                metadata = expData
            )
        })

    } else if(type == 'eset'){

        out <- packed_data %>% lapply(function(data){
            exprM <- data$exp
            design <- data$design

            rownames(exprM) <- data$unique_probes
            
            genes <- S4Vectors::DataFrame(exprM[,.SD,.SDcols = colnames(exprM)[colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]])
            exprM <- exprM[,.SD,.SDcols = colnames(exprM)[!colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]] %>%
                data.matrix()

            # reordering happens above
            # exprM <- exprM[, match(rownames(design), colnames(exprM))]

            expData <- Biobase::MIAME(
                title = data$dat$experiment.name,
                abstract = data$dat$experiment.description,
                url =   paste0("https://gemma.msl.ubc.ca/expressionExperiment/showExpressionExperiment.html?id=", data$dat$experiment.ID),
                other = list(
                    database = data$dat$experiment.database,
                    accesion = data$dat$experiment.accession,
                    GemmaQualityScore = data$dat$geeq.qScore,
                    GemmaSuitabilityScore = data$dat$geeq.sScore,
                    taxon = data$dat$taxon.Name
                )
            )

            annots <- data.frame(
                labelDescription = colnames(design),
                row.names = colnames(design)
            )

            phenoData <- Biobase::AnnotatedDataFrame(data = design, varMetadata = annots)

            Biobase::ExpressionSet(
                assayData = exprM,
                phenoData = phenoData,
                experimentData = expData,
                annotation = get_dataset_platforms(data$dat$experiment.ID,memoised = memoised)$platform.shortName
            )
        })
        names(out) <- datasets
    } else if(type == 'tidy'){
       
        out <- packed_data %>% lapply(function(data){
            exprM <- data$exp
            design <- data$design
            exprM$Probe = data$unique_probes
            genes <- exprM[,.SD,.SDcols = colnames(exprM)[colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]]
            exprM <- exprM[,.SD,.SDcols = colnames(exprM)[!colnames(exprM) %in% c('GeneSymbol','GeneName','NCBIid')]]

            exprM <- exprM[match(rownames(design), colnames(exprM)),drop = FALSE]

            design <- tibble::rownames_to_column(design, "Sample")

            frm <- exprM %>% as.data.frame %>%
                tidyr::pivot_longer(-"Probe", names_to = "Sample", values_to = "expression") %>%
                dplyr::inner_join(genes, by ='Probe') %>%
                dplyr::inner_join(design, by = "Sample") %>%
                dplyr::rename(sample = "Sample", probe = "Probe") %>%
                dplyr::mutate(experiment.ID = data$dat$experiment.ID,
                              experiment.shortName = data$dat$experiment.shortName,
                              .before = 1)

            if(!is.null(data$result_set)){
                frm <- dplyr::mutate(frm, result.ID = data$result_set,.before = 3)
            }
            if(!is.null(data$contrast)){
                frm <- dplyr::mutate(frm,  contrast.ID = data$contrast,.before = 3)
            }
            return(frm)

        }) %>% do.call(dplyr::bind_rows,.)

    } else if(type=='list'){
        return(packed_data)
    }

    return(out)
}

#' Retrieve differential expression results
#'
#' Retrieves the differential expression result set(s) associated with the dataset.
#' To get more information about the contrasts in individual resultSets and
#' annotation terms associated them, use [get_dataset_differential_expression_analyses()]
#'
#' In Gemma each result set corresponds to
#' the estimated effects associated with a single factor in the design, and each can have multiple contrasts (for each level compared to baseline).
#' Thus a dataset with a 2x3 factorial design will have two result sets, one of which will have one contrast, and one having two contrasts.
#'
#' The methodology for differential expression is explained in \href{https://doi.org/10.1093/database/baab006}{Curation of over 10000 transcriptomic studies to enable data reuse}.
#' Briefly, differential expression analysis is performed on the dataset based on the annotated
#' experimental design with up two three potentially nested factors.
#' Gemma attempts to automatically assign baseline conditions for each factor.
#' In the absence of a clear control condition, a baseline is arbitrarily selected.
#' A generalized linear model with empirical Bayes shrinkage of t-statistics is fit to the data
#' for each platform element (probe/gene) using an implementation of the limma algorithm. For RNA-seq data,
#' we use weighted regression, applying the
#' voom algorithm to compute weights from the mean–variance relationship of the data.
#' Contrasts of each condition are then computed compared to the selected baseline.
#' In some situations, Gemma will split the data into subsets for analysis.
#' A typical such situation is when a ‘batch’ factor is present and confounded with another factor,
#' the subsets being determined by the levels of the confounding factor.
#'
#' @param dataset A dataset identifier.
#' @param resultSets resultSet identifiers. If a dataset is not provided, all 
#' result sets will be downloaded. If it is provided it will only be used
#' to ensure all result sets belong to the dataset.
#' @param readableContrasts If \code{FALSE} (default), the returned columns will
#' use internal constrasts IDs as names. Details about the contrasts can be accessed
#' using \code{\link{get_dataset_differential_expression_analyses}}. If TRUE IDs will
#' be replaced with human readable contrast information.
#' @inheritParams memoise
#'
#' @return A list of data tables with differential expression
#' values per result set.
#' @keywords dataset
#' @export
#' @examples
#' get_differential_expression_values("GSE2018")
get_differential_expression_values <- function(dataset = NA_character_,
                                               resultSets = NA_integer_,
                                               readableContrasts = FALSE,
                                               memoised = getOption("gemma.memoised", FALSE)) {
    if (is.na(dataset) == FALSE && !isEmpty(resultSets)){
        diffs <- get_dataset_differential_expression_analyses(dataset)
        rss <- diffs$result.ID
        if (!all(resultSets %in% rss)){
            stop("The queried resultSet is not derived from this dataset. Check the available resultSets with `getDatasetResultSets()` or query without the dataset parameter.")
        }
    }
    else if (is.na(dataset) == FALSE && isEmpty(resultSets)){
        diffs <- get_dataset_differential_expression_analyses(dataset)
        resultSets <- diffs$result.ID %>% unique
    } else if (is.na(dataset) == TRUE && !isEmpty(resultSets)){
        resultSets <- resultSets
    } else {
        stop("Specify a dataset or resultSet IDs.")
    }

    rs <- lapply(resultSets, function(x){
        out <- .getResultSets(x,memoised = memoised)
        if(nrow(out)==0){
            msg <- paste0("ResultSet ",x," failed to return a populated table.")
            if(!is.na(dataset)){
                msg <- glue::glue('{msg}\nResult set {x} is part of {dataset}')
            }
            warning(msg)
        }
        if(readableContrasts){
            return(processDEcontrasts(out, x))
        } else{
            return(out)
        }
    })
    names(rs) <- resultSets

    rs
}



#' Get taxa
#'
#' Returns taxa and their versions used in Gemma
#'
#' @inheritParams memoise
#' @return A data frame including the names, IDs and database information
#' about the taxons
#' @keywords misc
#' @export
#' @examples
#' get_taxa()
get_taxa <- function(memoised = getOption("gemma.memoised", FALSE)){
    out <- get_taxa_by_ids(c(9606,
                      10090,
                      10116,
                      4932,
                      7955,
                      7227,
                      6239),memoised = memoised)
    return(out)
}

#' Custom gemma call
#'
#' A minimal function to create custom calls. Can be used to acquire unimplemented
#' endpoints and/or raw output without any processing. Refer to the
#' \href{https://gemma.msl.ubc.ca/resources/restapidocs/}{API documentation}.
#' @param call Gemma API endpoint.
#' @param ... parameters included in the call
#' @param json If `TRUE` will parse the content as a list
#' @keywords misc
#' @return A list if `json = TRUE` and an httr response if `FALSE`
#' @examples
#' # get singular value decomposition for the dataset
#' gemma_call('datasets/{dataset}/svd',dataset = 1)
#' @export
gemma_call <- function(call,...,json = TRUE){
    args <- unlist(list(...))
    args <- args %>% lapply(as.character) %>% lapply(utils::URLencode)
    env = environment()
    lapply(names(args),function(x){
        assign(x,args[[x]],envir = env)
    })

    if (!is.null(getOption('gemma.username')) && !is.null(getOption('gemma.password'))){
        response <- httr::GET(
            glue::glue(paste0(gemmaPath(),call)),
            httr::authenticate(getOption('gemma.username'),
                                 getOption("gemma.password")))
    } else{
        response <- httr::GET(glue::glue(paste0(gemmaPath(),call),.envir = env))
    }

    if (response$status_code == 200) {
        if(json){
            response <- jsonlite::fromJSON(rawToChar(response$content),simplifyVector = FALSE)
        }
        return(response)
    } else if (response$status_code == 403) {
        stop(call,'\n',response$status_code, ": Forbidden. You do not have permission to access this data.")
    } else if (response$status_code == 404) {
        stop(call,'\n',response$status_code, ": Not found. Ensure your parameters are spelled correctly and that you're querying an existing ID.")
    } else if (response$status_code == 500) {
        stop(call,'\n',response$status_code, ": Internal server error.")
    } else if (response$status_code == 503) {
        stop(call,'\n',response$status_code, ": Service Unavailable. Gemma might be under maintenance.")
    } else {
        stop(call, '\n', "HTTP code ", response$status_code)
    }

}


#' Get all pages of a paginated call
#'
#' Given a Gemma.R output from a function with offset and limit arguments,
#' returns the output from all pages. All arguments other than offset, limit
#'
#' @param query Output from a gemma.R function with offset and limit argument
#' @param step_size Size of individual calls to the server. 100 is the maximum value
#' @param binder Binding function for the calls. If \code{raw = FALSE} use \code{rbind} to
#' combine the data.tables. If not, use \code{c} to combine lists
#' @param directory Directory to save the output from the individual calls to. If provided, each page
#' is saved to separate files.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. This function always saves the output as an RDS file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#' @return A data.table or a list containing data from all pages.
#' @keywords misc
#' @export
get_all_pages <- function(query, step_size = 100,binder = rbind,directory  = NULL, file = getOption("gemma.file", NA_character_),overwrite = getOption("gemma.overwrite", FALSE)){
    current_env <-  rlang::env_clone(environment())
    current_env$query <- NULL
    attr <- attributes(query)
    count <- attr$totalElements

    args <- formals(attr$env$fname)
    args_used <- attr$env %>% as.list() %>% {.[names(args)]}
    args_used$limit <- step_size
    args_used$overwrite <- overwrite

    out <- lapply(seq(0,count,step_size),function(offset){
        step_args <- args_used
        step_args$offset <- offset

        if(!is.null(directory)){
            step_args$file <- file.path(directory,offset)
        } else{
            # file argument should not be preserved since it'll overwrite itself in
            # each call
            step_args$file <- NA_character_
        }
        
        do.call(attr$env$fname,step_args)
    }) %>% do.call(binder,.)
    
    if(!is.null(file) && !is.na(file)){
        if (file.exists(file) && !overwrite && !file.info(file)$isdir) {
            warning(file, " exists. Not overwriting.")
        } else{
            dir.create(dirname(file),showWarnings = FALSE,recursive = TRUE)
            saveRDS(out, file)
        }
    }
    attributes(out)$call_origin <- attr$call
    attributes(out)$env_origin <- attr$env
    attributes(out)$env <- current_env
    return(out)
}


#' Return all supported filter properties
#' 
#' Some functions such as \code{\link{get_datasets}} and \code{\link{get_platforms_by_ids}}
#' include a filter argument that allows creation of more complex queries. This
#' function returns a list of supported properties to be used in those filters
#' 
#' @return A list of data.tables that contain supported properties and their data
#' types
#' 
#' 
#' @examples 
#' filter_properties()
#' 
#' @keywords misc
#' @export
filter_properties <- function(){
    api_file <- jsonlite::fromJSON(system.file('script/openapi.json',package = 'gemma.R'),simplifyVector = FALSE)
    
    dataset_filter <- api_file$components$schemas$FilterArgExpressionExperiment$`x-gemma-filterable-properties`
    
    
    dataset_properties <- 
        data.table(properties = dataset_filter %>% accessField('name'),
               type =  dataset_filter %>% accessField('type'),
               description =  dataset_filter %>% accessField('description'))
    
    platform_filter <- api_file$components$schemas$FilterArgArrayDesign$`x-gemma-filterable-properties`
    
    platform_properties <- 
        data.table(properties = platform_filter %>% accessField('name'),
                   type =  platform_filter %>% accessField('type'),
                   description =  platform_filter %>% accessField('description'))
    
    
    resultSet_filter <-  api_file$components$schemas$FilterArgExpressionAnalysisResultSet$`x-gemma-filterable-properties`
    resultSet_properties <- data.table(properties = resultSet_filter %>% accessField('name'),
                                       type =  resultSet_filter %>% accessField('type'),
                                       description =  resultSet_filter %>% accessField('description'))
        
    out <- list(
        dataset = dataset_properties,
        platform = platform_properties,
        resultSet = resultSet_properties
    )
    
    return(out)
}


#' Return child terms of a term
#' 
#' When querying for ontology terms, Gemma propagates these terms to include
#' any datasets with their child terms in the results. This function returns 
#' these children for any number of terms, including all children
#' and the terms itself in the output vector
#' 
#' @param terms An array of terms
#' 
#' @return An array containing descendends of the annotation terms, including
#' the terms themselves
#' 
#' 
#' @examples 
#' get_child_terms("http://purl.obolibrary.org/obo/MONDO_0000408")
#' 
#' @keywords misc
#' @export
get_child_terms <- function(terms){
    output <- get_datasets(uris = terms,limit = 1)
    out <- attributes(output)$filter %>% stringr::str_extract_all('http.*?(?=,|\\))') %>% {.[[1]]}
    if(length(out) == 0){
        out = terms
    }
    return(out)
}



#' Update result
#' 
#' Re-runs the function used to create a gemma.R output 
#' to update the data at hand. Useful if you have a reason
#' to believe parts of the data has changed since your last
#' accession and you wish to update while decoupling the update
#' process from your original code used to generate the data.
#' 
#' Note that if you have used the file and overwrite arguments
#' with the original call, this will also repeat to regenarete
#' the file based on your initial preference
#' 
#' @param query Output from a gemma.R function
#' @keywords misc
#' @examples
#' annots <- get_dataset_annotations(1)
#' # wait for a couple of years..
#' # wonder if the results are the same
#' updated_annots <- update_result(annots)
#' 
#' # also works with outputs of get_all_pages
#' platforms <- get_all_pages(get_platforms_by_ids())
#' updated_platforms <- update_result(platforms)
#' 
#' @export
update_result<- function(query){
    attr <- attributes(query)
    
    # call_origin and env_origin are attached to get_all_pages outputs
    if(!"env_origin" %in% names(attr)){
        args <- formals(attr$env$fname)
        args_used <- attr$env %>% as.list() %>% {.[names(args)]}
        return(do.call(attr$env$fname,args_used))
    } else{
        # the inital call must be repeated since
        # the count may have changes
        args <- formals(attr$env_origin$fname)
        args_used <- attr$env_origin %>% as.list() %>% {.[names(args)]}
        poke_call <- do.call(attr$env_origin$fname,args_used)
        
        pages_args <- formals(get_all_pages)
        pages_args_used <- attr$env %>% as.list %>% {.[names(pages_args)]}
        pages_args_used$query <- poke_call
        
        return(do.call(get_all_pages,pages_args_used))
    }
}


#' Retrieve all result sets matching the provided criteria
#'
#' Returns queried result set
#'
#' @details Output and usage of this function is mostly identical to \code{\link{get_dataset_differential_expression_analyses}}.
#' The principal difference being the ability to restrict your result sets, being able to
#' query across multiple datasets and being able to use the filter argument
#' to search based on result set properties.
#'
#' @param datasets A vector of dataset IDs or short names
#' @param resultSets A resultSet identifier. Note that result set identifiers
#' are not static and can change when Gemma re-runs analyses internally. Whem
#' using these as inputs, try to make sure you access a currently existing
#' result set ID by basing them on result sets returned for a particular dataset or
#' filter used in \code{\link{get_result_sets}}
#' @param filter Filter results by matching expression. Use \code{\link{filter_properties}}
#' function to get a list of all available parameters. These properties can be
#' combined using "and" "or" clauses and may contain common operators such as "=", "<" or "in".
#' (e.g. "taxon.commonName = human", "taxon.commonName in (human,mouse), "id < 1000")
#' @param sort Order results by the given property and direction. The '+' sign
#' indicate ascending order whereas the '-' indicate descending.
#' @param raw \code{TRUE} to receive results as-is from Gemma, or \code{FALSE} to enable
#' parsing. Raw results usually contain additional fields and flags that are
#' omitted in the parsed results.
#' @param memoised Whether or not to save to cache for future calls with the
#' same inputs and use the result saved in cache if a result is already saved.
#' Doing \code{options(gemma.memoised = TRUE)} will ensure that the cache is always
#' used. Use \code{\link{forget_gemma_memoised}} to clear the cache.
#' @param file The name of a file to save the results to, or \code{NULL} to not write
#' results to a file. If \code{raw == TRUE}, the output will be the raw endpoint from the
#' API, likely a JSON or a gzip file. Otherwise, it will be a RDS file.
#' @param overwrite Whether or not to overwrite if a file exists at the specified
#' filename.
#'
#' @inherit processDifferentialExpressionAnalysisResultSetValueObject return
#' @export
#'
#' @keywords misc
#'
#' @examples
#' get_result_sets(dataset = 1)
#' # get all contrasts comparing disease states. use filter_properties to see avaialble options
#' get_result_sets(filter = "baselineGroup.characteristics.value = disease")
get_result_sets <- function(datasets = NA_character_,
                             resultSets = NA_character_,
                             filter = NA_character_,
                             sort = '+id',
                             raw = getOption("gemma.raw", FALSE),
                             memoised = getOption("gemma.memoised", FALSE),
                             file = getOption("gemma.file", NA_character_),
                             overwrite = getOption("gemma.overwrite", FALSE) ){
    # GSE2178 (id 14) is a good example to test this with 78 values in output
    
    output <- .get_result_sets(datasets = datasets,
                               resultSets = resultSets,
                               filter = filter,
                               sort = sort,
                               raw = TRUE,limit = 100,
                               offset = 0,
                               memoised = memoised,
                               file = NA_character_)
    
    needPages <- output %>% purrr::map('experimentalFactors') %>% 
        purrr::map(\(x){x %>% purrr::map('values')}) %>%
        unlist(recursive = FALSE) %>% unlist(recursive = FALSE) %>% length %>%
        {.>=100}
    
    if(needPages){
        all_pages <- output %>% get_all_pages(binder = list)
        # merging last element of the previous page with the first element
        # of the next
        for(i in seq(2,length(all_pages))){
            last_elem <- all_pages[[i-1]][[length(all_pages[[i-1]])]]
            first_elem <- all_pages[[i]][[1]]
            
            # if this is the case the division was perfect
            if(first_elem$id != last_elem$id){
                next
            }
            
            last_elem_ids <- last_elem$experimentalFactors %>% purrr::map_int('id')
            first_elem_ids <- first_elem$experimentalFactors %>% purrr::map_int('id')
            
            common <- intersect(last_elem_ids,first_elem_ids)
            uncommon <- first_elem_ids[!first_elem_ids %in% common]
            
            for(j in common){
                factor_to_fix <- last_elem$experimentalFactors[[which(last_elem_ids == j)]]
                factor_to_add <- first_elem$experimentalFactors[[which(first_elem_ids == j)]]
                testthat::expect_identical(
                    factor_to_fix[names(factor_to_fix)[!names(factor_to_fix) %in%  c('values','factorValues')]], 
                    factor_to_add[names(factor_to_add)[!names(factor_to_add)%in%  c('values','factorValues')]])
                # this output isn't very useful but for completion's sake...
                # the overlap check below isn't included here. TODO
                factor_to_fix$factorValues <- paste(factor_to_fix$factorValues,factor_to_add$factorValues,sep = ', ')
                
                factor_to_add_ids <- factor_to_add$values %>% purrr::map_int('id')
                factor_to_fix_ids <- factor_to_fix$values %>% purrr::map_int('id')
                
                # if there is overlap, they must be identical. pagination is 
                # supposed to prevent overlaps so ask guillaume
                common_vals <- intersect(factor_to_add_ids,factor_to_fix_ids)
                for(k in common_vals){
                    common_in_add <- factor_to_add$values[[which(factor_to_add_ids == k)]]
                    common_in_fix <- factor_to_fix$values[[which(factor_to_fix_ids == k)]]
                    testthat::expect_identical(common_in_add,common_in_fix)
                }
                factor_to_add$values <- factor_to_add$values[!factor_to_add_ids %in% common_vals]
                
                factor_to_fix$values = c(factor_to_fix$values,factor_to_add$values)
                # append the overlapping results to the first page, remove them from the second page
                
                last_elem$experimentalFactors[[which(last_elem_ids == j)]] = factor_to_fix
                
            }
            
            last_elem$experimentalFactors = c(last_elem$experimentalFactors,
                                              first_elem$experimentalFactors[first_elem_ids %in% uncommon])
            
            all_pages[[i-1]][[length(all_pages[[i-1]])]] = last_elem
            all_pages[[i]][[1]] = NULL
        }
        output <- do.call(c,all_pages)
    }
    
    if(!raw){
        output <- processDifferentialExpressionAnalysisResultSetValueObject(output)
    }
    
    if (!is.null(file) && !is.na(file)) {
        if (file.exists(file) && !overwrite && !file.info(file)$isdir) {
            warning(file, " exists. Not overwriting.")
        } else{
            dir.create(dirname(file),showWarnings = FALSE,recursive = TRUE)
            saveRDS(output, file)
        }
    }
    
    return(output)
}
