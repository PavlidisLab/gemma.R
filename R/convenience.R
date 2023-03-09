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
#' @param platform A platform identifier @seealso getPlatforms
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
#' head(get_platform_annotations('Generic_human'))
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


make_design = function(samples,metaType){

    categories <- samples$sample.FactorValues %>% purrr::map('category') %>% unlist %>% unique()
    factorValues <- categories %>% lapply(function(x){
        samples$sample.FactorValues %>% purrr::map_chr(function(y){
            y %>% dplyr::filter(category == x) %>% {.$factorValue} %>% paste(collapse = ',')
        })
    })

    category_uris <- samples$sample.FactorValues %>% purrr::map('categoryURI') %>% unlist %>% unique()
    factorURIs <- category_uris %>% lapply(function(x){
        samples$sample.FactorValues %>% purrr::map_chr(function(y){
            y %>% dplyr::filter(categoryURI == x) %>% {.$factorValueURI} %>% paste(collapse = ',')
        })
    })

    if(metaType == 'text'){
        design_frame <- factorValues %>% as.data.frame()
        colnames(design_frame) = categories
    } else if (metaType == 'uri'){
        design_frame <- factorURIs %>% as.data.frame()
        colnames(design_frame) = category_uris
    } else if (metaType == 'both'){
        design_frame <- seq_along(design_frame) %>% lapply(function(i){
            paste(factorValues[[i]],factorURIs[[i]],sep = '|')
        }) %>% as.data.frame()
        colnames(design_frame) <- paste(categories,category_uris,sep = '|')
    }
    rownames(design_frame) <- samples$sample.Name
    design_frame <- design_frame %>% dplyr::mutate(factorValues = samples$sample.FactorValues, .before = 1)

    return(design_frame)
}


#' Compile gene expression data and metadata
#'
#' Return an annotated Bioconductor-compatible
#' data structure or a long form tibble of the queried dataset, including
#' expression data and the experimental design.
#'
#' @param filter The filtered version corresponds to what is used in most Gemma analyses, removing some probes/elements. Unfiltered includes all elements.
#' @param type "se"for a SummarizedExperiment or "eset" for Expression Set. We recommend using
#' SummarizedExperiments which are more recent. See the Summarized experiment
#' \href{https://bioconductor.org/packages/release/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html}{vignette}
#' or the ExpressionSet \href{https://bioconductor.org/packages/release/bioc/vignettes/Biobase/inst/doc/ExpressionSetIntroduction.pdf}{vignette}
#' for more details.
#' @inheritParams memoise
#' @inheritParams get_dataset_expression_for_genes
#' @param metaType How should the metadata information should be included. Can be "text", "uri" or "both". "text" and "uri" options
#' @param resultSet Result set IDs of the a differential expression analysis. Optional. If provided, the output will only include
#' the samples from the subset used in the result set ID.
#' 
#'  Must be the same length as \code{datasets}.
#'
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
                               filter = FALSE, 
                               metaType = 'text', 
                               type = "se", 
                               memoised = getOption("gemma.memoised", FALSE)) {
    if (type != "eset" && type != "se" && type != 'tidy') {
        stop("Please enter a valid type: 'se' for SummarizedExperiment or 'eset' for ExpressionSet and 'tidy' for a long form tibble.")
    }

    metadata <- unique(datasets) %>% lapply(function(dataset){
        get_dataset_samples(dataset,memoised = memoised)
    })
    names(metadata) = unique(datasets)

    if(is.null(genes)){
        expression <- unique(datasets) %>% lapply(function(dataset){
            exp <- get_dataset_expression(dataset, filter,memoised = memoised)
            meta <- metadata[[as.character(dataset)]]

            # these replicate the arguments for get_dataset_expression_for_genes
            if(!keepNonSpecific){
                exp = exp[!grepl("|",exp$GeneSymbol,fixed = TRUE) | exp$GeneSymbol == "",]
            }
            if(!is.na(consolidate) && consolidate == "pickmax"){
                mean_exp <- exp[,.SD,.SDcols = meta$sample.Name] %>% apply(1,function(x){
                    mean(stats::na.omit(x))
                })
                exp <- exp[order(mean_exp,decreasing = TRUE),] %>% {.[!duplicated(.$GeneSymbol),]}
            } else if(!is.na(consolidate) && consolidate == 'pickvar'){
                exp_var <- exp[,.SD,.SDcols = meta$sample.Name] %>% apply(1,function(x){
                    stats::var(stats::na.omit(x))
                })
                exp <- exp[order(exp_var,decreasing = TRUE),] %>% {.[!duplicated(.$GeneSymbol),]}
            } else if(!is.na(consolidate) && consolidate == 'average'){
                dups <- exp$GeneSymbol %>% {.[duplicated(.)]} %>% unique
                dup_means <- dups %>% lapply(function(x){
                    dup_subset <- exp[exp$GeneSymbol %in% x,]
                    dup_mean <- dup_subset[,.SD,.SDcols = meta$sample.Name] %>% apply(2,mean)
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
        names(expression) = datasets
    } else{
        expression <- get_dataset_expression_for_genes(unique(datasets),genes = genes, keepNonSpecific = keepNonSpecific,consolidate = consolidate,memoised = memoised)
    }

    # bit of a bottleneck
    designs <- metadata %>% lapply(function(meta){
        make_design(meta,metaType)
    })
    
    # pack the information that will be included in all outputs
    packed_data <- seq_along(datasets) %>% lapply(function(i){
        dataset <- datasets[i]
        # we don't want to pass data.tables by reference because 
        # same datasets might be re-used
        packed_info <- 
            list(design = data.table::copy(designs[[as.character(dataset)]]),
                 exp = data.table::copy(expression[[as.character(dataset)]]),
                 result_set = resultSets[i],
                 dat =  get_datasets_by_ids(dataset, raw = FALSE,memoised = memoised))
        

        
        # reorders the expression to match the metadata
        gene_info <- colnames(packed_info$exp)[!colnames(packed_info$exp) %in% rownames(packed_info$design)]
        data.table::setcolorder(packed_info$exp,c(gene_info,rownames(packed_info$design)))

        if(!is.null(resultSets)){
            diff <- get_dataset_differential_expression_analyses(dataset,memoised = memoised)
            subset_category <- diff %>% 
                dplyr::filter(result.ID == resultSets[i]) %>% 
                .$subsetFactor.category %>% unique
            subset_factor <- diff %>% 
                dplyr::filter(result.ID == resultSets[i]) %>% 
                .$subsetFactor.factorValue %>% unique
            
            assertthat::assert_that(length(subset_category)==1)
            assertthat::assert_that(length(subset_factor)==1)
            
            if(!is.na(subset_category)){
                in_subset <- packed_info$design$factorValues %>% purrr::map_lgl(function(x){
                    x %>% dplyr::filter(category %in% subset_category) %>%
                        .$factorValue %in% subset_factor
                })
            } else{
                in_subset <- TRUE
            }
            packed_info$exp <- packed_info$exp[,.SD,.SDcols = c(gene_info, rownames(packed_info$design)[in_subset])]
            packed_info$design <- packed_info$design[in_subset,]
        }
        
        return(packed_info)
    })
    

    if(!is.null(resultSets)){
        names(packed_data) <- paste0(
            packed_data %>% purrr::map('dat') %>% purrr::map_int('experiment.ID'),
            '.',resultSets)
    } else{
        names(packed_data) <- packed_data %>% purrr::map('dat') %>% purrr::map_int('experiment.ID')
    }
    

    if (type == 'se'){
        out <- packed_data %>% lapply(function(data){
            exprM <- data$exp
            design <- data$design


            rownames(exprM) <- exprM$Probe
            genes <- S4Vectors::DataFrame(exprM[,.SD,.SDcols = colnames(exprM)[colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]])
            exprM <- exprM[,.SD,.SDcols = colnames(exprM)[!colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]] %>%
                data.matrix()

            # reordering happens above
            # exprM <- exprM[, match(rownames(design), colnames(exprM))]



            expData <- list(
                title = data$dat$experiment.Name,
                abstract = data$dat$experiment.Description,
                url = paste0("https://gemma.msl.ubc.ca/expressionExperiment/showExpressionExperiment.html?id=", data$dat$experiment.ID),
                database = data$dat$experiment.Database,
                accesion = data$dat$experiment.Accession,
                GemmaQualityScore = data$dat$geeq.qScore,
                GemmaSuitabilityScore = data$dat$geeq.sScore,
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


            rownames(exprM) <- exprM$Probe
            genes <- S4Vectors::DataFrame(exprM[,.SD,.SDcols = colnames(exprM)[colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]])
            exprM <- exprM[,.SD,.SDcols = colnames(exprM)[!colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]] %>%
                data.matrix()
            
            # reordering happens above
            # exprM <- exprM[, match(rownames(design), colnames(exprM))]


            expData <- Biobase::MIAME(
                title = data$dat$experiment.Name,
                abstract = data$dat$experiment.Description,
                url =   paste0("https://gemma.msl.ubc.ca/expressionExperiment/showExpressionExperiment.html?id=", data$dat$experiment.ID),
                other = list(
                    database = data$dat$experiment.Database,
                    accesion = data$dat$experiment.Accession,
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
                annotation = get_dataset_platforms(data$dat$experiment.ID,memoised = memoised)$platform.ShortName
            )
        })
        names(out) <- datasets
    } else if(type == 'tidy'){
        out <- packed_data %>% lapply(function(data){

            exprM <- data$exp
            design <- data$design
            
            rownames(exprM) <- exprM$Probe
            genes <- exprM[,.SD,.SDcols = colnames(exprM)[colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]]
            exprM <- exprM[,.SD,.SDcols = colnames(exprM)[!colnames(exprM) %in% c('Probe','GeneSymbol','GeneName','NCBIid')]] %>%
                data.matrix()

            exprM <- exprM[, match(rownames(design), colnames(exprM)),drop = FALSE]

            design <- tibble::rownames_to_column(design, "Sample")


            frm <- exprM %>% as.data.frame %>%
                tibble::rownames_to_column("Probe") %>%
                tidyr::pivot_longer(-.data$Probe, names_to = "Sample", values_to = "expression") %>%
                dplyr::inner_join(genes, by ='Probe') %>%
                dplyr::inner_join(design, by = "Sample") %>%
                dplyr::rename(sample = .data$Sample, probe = .data$Probe) %>%
                dplyr::mutate(experiment.ID = data$dat$experiment.ID, 
                              experiment.ShortName = data$dat$experiment.ShortName,
                              .before = 1)
            
            if(!is.null(data$result_set)){
                frm <- mutate(frm, result.ID = data$result_set,.before= 3)
            }
            return(frm)

        }) %>% do.call(dplyr::bind_rows,.)

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
#' @param resultSet A resultSet identifier.
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
                                               resultSet = NA_integer_,
                                               readableContrasts = FALSE,
                                               memoised = getOption("gemma.memoised", FALSE)) {
    if (is.na(dataset) == FALSE && is.na(resultSet) == FALSE){
        diffs <- get_dataset_differential_expression_analyses(dataset,raw = TRUE)
        rss <- diffs %>% purrr::map('resultSets') %>% purrr::map(function(x){x %>% purrr::map('id')}) %>% unlist
        if (!(resultSet %in% rss)){
            stop("The queried resultSet is not derived from this dataset. Check the available resultSets with `getDatasetResultSets()` or query without the dataset parameter.")
        }
    }
    else if (is.na(dataset) == FALSE && is.na(resultSet) == TRUE){
        diffs <- get_dataset_differential_expression_analyses(dataset,raw = TRUE)
        resultSet <- diffs %>% purrr::map('resultSets') %>% purrr::map(function(x){x %>% purrr::map('id')}) %>% unlist %>% unique
    } else if (is.na(dataset) == TRUE && is.na(resultSet) == FALSE){
        resultSet <- resultSet
    } else {
        stop("Specify a dataset or a resultSet ID.")
    }

    rs <- lapply(resultSet, function(x){
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
    names(rs) <- resultSet

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
    attach(list(...),warn.conflicts = FALSE)

    if (!is.null(getOption('gemma.username')) && !is.null(getOption('gemma.password'))){
        out <- httr::GET(
            glue::glue(paste0(gemmaPath(),call)),
            httr::authenticate(getOption('gemma.username'),
                                 getOption("gemma.password")))
    } else{
       out <- httr::GET(glue::glue(paste0(gemmaPath(),call)))
    }

    if(json){
        out <- jsonlite::fromJSON(rawToChar(out$content),simplifyVector = FALSE)
    }
    return(out)

}
