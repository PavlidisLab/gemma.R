#' Authentication by user name
#'
#' Allows the user to access information that requires logging in to Gemma. To log out, run `setGemmaUser` without specifying the username or password.
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
    response = gemma_call('',json = FALSE)
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


#' Compile gene expression data and metadata
#'
#' Return an annotated Bioconductor-compatible
#' data structure or a long form tibble of the queried dataset, including 
#' expression data and the experimental design.
#'
#' @param dataset A dataset identifier.
#' @param filter The filtered version corresponds to what is used in most Gemma analyses, removing some probes/elements. Unfiltered includes all elements.
#' @param type "se"for a SummarizedExperiment or "eset" for Expression Set. We recommend using
#' SummarizedExperiments which are more recent. See the Summarized experiment
#' \href{https://bioconductor.org/packages/release/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html}{vignette}
#' or the ExpressionSet \href{https://bioconductor.org/packages/release/bioc/vignettes/Biobase/inst/doc/ExpressionSetIntroduction.pdf}{vignette}
#' for more details.
#' @inheritParams memoise
#'
#' @return A SummarizedExperiment, ExpressionSet or tibble containing metadata and expression data for the queried dataset.
#' @keywords dataset
#' @export
#' @examples
#' get_dataset_object("GSE2018")
get_dataset_object <- function(dataset, filter = FALSE, type = "se", memoised = getOption("gemma.memoised", FALSE)) {
    if (type != "eset" && type != "se" && type != 'tidy') {
        stop("Please enter a valid type: 'se' for SummarizedExperiment or 'eset' for ExpressionSet and 'tidy' for a long form tibble.")
    }
    exprM <- get_dataset_expression(dataset, filter,memoised = memoised)
    
    # multi platform datasets may have repeated probesets which needs new names
    # most multiplatform datasets were merged into artifical probesets defined
    # within gemma but at the time of writing 365 was an exception
    duplicate_probes <- exprM$Probe[duplicated(exprM$Probe)]
    
    for(dp in duplicate_probes){
        to_replace <- exprM$Probe[exprM$Probe %in% dp]
        to_paste <- sapply(seq_along(to_replace), function(i){
          if(i == 1){
              ''
          }else{
              paste0('.',i)
          }
        })
        
        exprM$Probe[exprM$Probe %in% dp] <- paste0(to_replace,to_paste)
    }
    
    
    rownames(exprM) <- exprM$Probe
    genes <- S4Vectors::DataFrame(dplyr::select(exprM, "GeneSymbol", "GeneName", "NCBIid"))
    exprM <- dplyr::select(exprM, -"Probe", -"GeneSymbol", -"GeneName", -"NCBIid") %>%
        data.matrix()
    design <- get_dataset_design(dataset,memoised = memoised)

    # This annotation table is required
    annots <- data.frame(
        labelDescription = colnames(design),
        row.names = colnames(design)
    )
    phenoData <- Biobase::AnnotatedDataFrame(data = design, varMetadata = annots)

    # Reorder expression matrix to match design
    exprM <- exprM[, match(rownames(design), colnames(exprM))]

    # Experiment description
    dat <- get_datasets_by_ids(dataset, raw = TRUE,memoised = memoised) %>% jsonlite:::simplify()
    other <- list(
        database = dat$externalDatabase,
        accesion = dat$accession,
        GemmaQualityScore = dat$geeq$publicQualityScore,
        GemmaSuitabilityScore = dat$geeq$geeq.sScore,
        taxon = dat$taxon
    )

    title <- dat$name
    abstract <- dat$description
    url <- paste0("https://gemma.msl.ubc.ca/expressionExperiment/showExpressionExperiment.html?id=", dat$id)

    if (type == "se") {
        expData <- list(
            title = title,
            abstract = abstract,
            url = url
        )
        expData <- c(expData, other)
        SummarizedExperiment::SummarizedExperiment(
            assays = list(counts = exprM),
            rowData = genes,
            colData = design,
            metadata = expData
        )
    } else if (type == "eset") {
        expData <- Biobase::MIAME(
            title = title,
            abstract = abstract,
            url = url,
            other = other
        )
        Biobase::ExpressionSet(
            assayData = exprM,
            phenoData = phenoData,
            experimentData = expData,
            annotation = get_dataset_platforms(dataset,memoised = memoised)$platform.ShortName
        )
    } else if(type=='tidy'){
        design <- tibble::rownames_to_column(design, "Sample")
        exprM %>% as.data.frame %>% 
            tibble::rownames_to_column("Probe") %>%
            tidyr::pivot_longer(-.data$Probe, names_to = "Sample", values_to = "expression") %>%
            dplyr::inner_join(design, by = "Sample") %>%
            dplyr::rename(sample = .data$Sample, probe = .data$Probe)
    }
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
        rss <- get_result_sets(dataset,memoised = memoised)
        if (!(resultSet %in% rss$resultSet.id)){
            stop("The queried resultSet is not derived from this dataset. Check the available resultSets with `getDatasetResultSets()` or query without the dataset parameter.")
        }
    }
    else if (is.na(dataset) == FALSE && is.na(resultSet) == TRUE){
        rss <- get_result_sets(dataset,memoised = memoised)
         if (nrow(rss) > 1){
            resultSet <- rss$resultSet.id %>% unique()
        } else{
            resultSet <- rss$resultSet.id
        }
    } else if (is.na(dataset) == TRUE && is.na(resultSet) == FALSE){
        resultSet <- resultSet
    } else {
        stop("Specify a dataset or a resultSet ID.")
    }

    rs <- lapply(resultSet, function(x){
        out <- .getResultSets(x,memoised = memoised)
        if(nrow(out)==0){
            msg = paste0("ResultSet ",x," failed to return a populated table.")
            if(!is.na(dataset)){
                msg = glue::glue('{msg}\nResult set {x} is part of {dataset}')
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
