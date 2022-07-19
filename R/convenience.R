#' Authentication
#'
#' Allows the user to access information that requires logging in to Gemma. To log out, run `setGemmaUser` without specifying the username or password.
#'
#' @param username Your username (or empty, if logging out)
#' @param password Your password (or empty, if logging out)
#'
#' @return None
#' @keywords internal
setGemmaUser <- function(username = NULL, password = NULL) {
    options(gemma.username = username)
    options(gemma.password = password)
}

#' Gemma platform annotations
#'
#' Gets Gemma's platform annotation files that can be accessed from https://gemma.msl.ubc.ca/annots/
#'
#' @param platform A platform identifier @seealso getPlatforms
#' @param annotType Which GO terms should the output include
#' @param file Where to save the annotation file to, or empty to just load into memory
#' @param overwrite Whether or not to overwrite an existing file
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always used.
#' Use \code{forgetGemmaMemoised} to clear the cache.
#' @param unzip Whether or not to unzip the file (if @param file is not empty)
#'
#' @return A table of annotations
#' @keywords platform
#' @export
#' @examples
#' dat <- getPlatformAnnotation("GPL96")
#' str(dat)
getPlatformAnnotation <- function(platform,
    annotType = c("bioProcess", "noParents", "allParents"),
    file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE),
    memoised = getOption("gemma.memoise", FALSE),
    unzip = FALSE){

    if (memoised){
        if (!is.na(file)){
            warning("Saving to files is not supported with memoisation.")
        }
        out <- memgetPlatformAnnotation(
            platform = platform,
            annotType = annotType,
            file = NA,
            overwrite = overwrite,
            memoised = FALSE,
            unzip = unzip
        )
        return(out)
    }

    if (!is.numeric(platform)) {
        platforms <- getPlatformsInfo(platform)
        if (!isTRUE(nrow(platforms) == 1)) {
            stop(platform, " is not a valid single platform.")
        }
        platform <- platforms[, "platform.ID"]
    }

    annotType <- match.arg(annotType, c("bioProcess", "noParents", "allParents"))

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

            if (is.tmp || !unzip) {
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
        httr::GET(glue::glue(
            paste0(getOption("gemma.base", "https://gemma.msl.ubc.ca/"),
                "arrays/downloadAnnotationFile.html?id={platform}&fileType={annotType}")),
            httr::write_disk(file))
        doReadFile(file)
    }
}

#' Memoise getPlatformAnnotation
#'
#' @noRd
memgetPlatformAnnotation <- function(platform,
                                     annotType = c("bioProcess", "noParents", "allParents"),
                                     file = getOption("gemma.file", NA_character_),
                                     overwrite = getOption("gemma.overwrite", FALSE),
                                     memoised = getOption("gemma.memoise", FALSE),
                                     unzip = FALSE) {
    mem_call <- memoise::memoise(getPlatformAnnotation, cache = gemmaCache())
    mem_call(
        platform = platform, annotType = annotType, memoised = FALSE, file = file,
        overwrite = overwrite, unzip=unzip
    )
}


#' Dataset expression and design
#'
#' Combines various endpoint calls to return an annotated Bioconductor-compatible
#' data structure of the queried dataset, including expression data and
#' the experimental design.
#'
#' @param dataset A dataset identifier.
#' @param filter The filtered version corresponds to what is used in most Gemma analyses, removing some probes/elements. Unfiltered includes all elements.
#' @param type "se"for a SummarizedExperiment or "eset" for Expression Set. We recommend using
#' SummarizedExperiments which are more recent. See the Summarized experiment
#' \href{https://bioconductor.org/packages/release/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html}{vignette}
#' or the ExpressionSet \href{https://bioconductor.org/packages/release/bioc/vignettes/Biobase/inst/doc/ExpressionSetIntroduction.pdf}{vignette}
#' for more details.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always used.
#' Use \code{forgetGemmaMemoised} to clear the cache.
#' cache. Use \code{forgetGemmaMemoised} to clear the cache.
#'
#' @return A SummarizedExperiment or ExpressionSet of the queried dataset.
#' @keywords dataset
#' @export
#' @examples
#' getDataset("GSE2018")
getDataset <- function(dataset, filter = FALSE, type = "se", memoised = getOption("gemma.memoised", FALSE)) {
    if (type != "eset" && type != "se") {
        stop("Please enter a valid type: 'se' for SummarizedExperiment or 'eset' for ExpressionSet.")
    }
    exprM <- getDatasetExpression(dataset, filter,memoised = memoised)
    rownames(exprM) <- exprM$Probe
    genes <- S4Vectors::DataFrame(dplyr::select(exprM, "GeneSymbol", "GeneName", "NCBIid"))
    exprM <- dplyr::select(exprM, -"Probe", -"GeneSymbol", -"GeneName", -"NCBIid") %>%
        data.matrix()
    design <- getDatasetDesign(dataset,memoised = memoised)

    # This annotation table is required
    annots <- data.frame(
        labelDescription = colnames(design),
        row.names = colnames(design)
    )
    phenoData <- Biobase::AnnotatedDataFrame(data = design, varMetadata = annots)

    # Reorder expression matrix to match design
    exprM <- exprM[, match(rownames(design), colnames(exprM))]

    # Experiment description
    dat <- getDatasetsInfo(dataset, raw = TRUE,memoised = memoised)
    other <- list(
        database = dat$externalDatabase,
        accesion = dat$accession,
        GemmaQualityScore = dat$geeq$publicQualityScore,
        GemmaSuitabilityScore = dat$geeq$geeq.sScore,
        taxon = dat$taxon
    )

    title <- dat$name
    abstract <- dat$description
    url <- paste0("https://gemma.msl.ubc.ca/expressionExperiment/showExpressionExperiment.html?id=", dat$ee.ID)

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
            annotation = getDatasetPlatforms(dataset,memoised = memoised)$platform.ShortName
        )
    }
}

#' Tidy expression and design
#'
#' Combines the expression and design matrices of the queried dataset into a
#' tibble for easy visualization and exploration with ggplot and the rest of the tidyverse.
#'
#' @param dataset A dataset identifier.
#' @param filter The filtered version corresponds to what is used in most Gemma analyses, removing some probes/elements. Unfiltered includes all elements.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the cache is always used.
#' Use \code{forgetGemmaMemoised} to clear the cache.
#'
#' @return A tibble that combines the expression and design matrices.
#' @keywords dataset
#' @export
#' @examples
#'
#' getDatasetTidy("GSE2018")
getDatasetTidy <- function(dataset, filter = FALSE, memoised =  getOption("gemma.memoised", FALSE)) {
    design <- getDatasetDesign(dataset,memoised = memoised) %>%
        tibble::rownames_to_column("Sample")
    # Get expression data, convert to long format and add exp. design
    getDatasetExpression(dataset, filter = filter,memoised = memoised) %>%
        as.data.frame() %>%
        tibble::column_to_rownames("Probe") %>%
        .[, match(design$Sample, colnames(.))] %>% # match sample order
        tibble::rownames_to_column("Probe") %>%
        tidyr::pivot_longer(-.data$Probe, names_to = "Sample", values_to = "expression") %>%
        dplyr::inner_join(design, by = "Sample") %>%
        dplyr::rename(sample = .data$Sample, probe = .data$Probe)
}

#' Dataset differential expression
#'
#' Retrieves the differential expression resultSet(s) associated with the dataset.
#' If there is more than one resultSet, use [getDatasetResultSets()] or [getDatasetDEA()] to see
#' the options and get the ID you want. Alternatively, you can query the resultSet
#' directly if you know its ID beforehand.
#' 
#' Methodology for differential expression is explained in \href{https://doi.org/10.1093/database/baab006}{Curation of over 10 000 transcriptomic studies to enable data reuse}. Specifically, "differential expression analysis is performed on the dataset based on the annotated experimental design. In cases where certain terms are used (e.g. ‘reference substance role’ (OBI_0000025), ‘reference subject role’ (OBI_0000220), ‘initial time point’ (EFO_0004425), ‘wild type genotype’ (EFO_0005168), ‘control’ (EFO_0001461), etc.), Gemma automatically assigns these conditions as the baseline control group; in absence of a clear control condition, a baseline is arbitrarily selected. To perform the analysis, a generalized linear model is fit to the data for each platform element (probe/gene). For RNA-seq data, we use weighted regression, using an in-house implementation of the voom algorithm to compute weights from the mean–variance relationship of the data. Contrasts of each condition are then compared to the selected baseline. In datasets where the ‘batch’ factor is confounded with another factor, separate differential expression analyses are performed on subsets of the data; the subsets being determined by the levels of the confounding factor."
#'
#' @param dataset A dataset identifier.
#' @param resultSet A resultSet identifier.
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the catche is always used.
#' Use \code{forgetGemmaMemoised} to clear the cache.
#'
#' @return A list of data tables with differential expression
#' values per result set.
#' @keywords dataset
#' @export
#' @examples
#' getDatasetDE("GSE2018")
getDatasetDE <- function(dataset = NA_character_, resultSet = NA_integer_, memoised = getOption("gemma.memoised", FALSE)) {
    if (is.na(dataset) == FALSE && is.na(resultSet) == FALSE){
        rss <- getDatasetResultSets(dataset,memoised = memoised)
        if (!(resultSet %in% rss$resultSet.id)){
            stop("The queried resultSet is not derived from this dataset. Check the available resultSets with `getDatasetResultSets()` or query without the dataset parameter.")
        }
    }
    else if (is.na(dataset) == FALSE && is.na(resultSet) == TRUE){
        rss <- getDatasetResultSets(dataset,memoised = memoised)
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
        .getResultSets(x,memoised = memoised) %>%
            processDEcontrasts(x)
    })
    names(rs) <- resultSet
    
    rs
}



#' Get genome versions
#' 
#' Returns the genome version used within Gemma
#' 
#' @param memoised Whether or not to save to cache for future calls with the same inputs
#' and use the result saved in cache if a result is already saved. Doing
#' `options(gemma.memoised = TRUE)` will ensure that the catche is always used.
#' Use \code{forgetGemmaMemoised} to clear the cache.
#' @return A data frame
#' @keywords misc
#' @export
#' @examples
#' getGenomeVersions()
getGenomeVersions <- function(memoised = getOption("gemma.memoised", FALSE)){
    LOOKUP_TABLE <- data.table(
        id = c(1, 2, 3, 11, 12, 13, 14),
        name = c("human", "mouse", "rat", "yeast", "zebrafish", "fly", "worm"),
        scientific = c(
            "Homo sapiens", "Mus musculus", "Rattus norvegicus",
            "Saccharomyces cerevisiae", "Danio rerio", "Drosophila melanogaster",
            "Caenorhabditis elegans"
        ),
        ncbi = c(9606, 
                 10090,
                 10116, 
                 4932,
                 7955, 
                 7227,
                 6239),
        example_gene = c(6125, # this list is created by picking a random gene with homologues in each species
                         100503670,
                         81763,
                         855972,
                         326961,
                         3355124,
                         174371)
    )
    
    LOOKUP_TABLE$genome_version <- sapply(seq_len(nrow(LOOKUP_TABLE)),function(i){
        getGeneLocation(LOOKUP_TABLE$example_gene[i],memoised = memoised)$taxon.Database.Name
    })
    
    LOOKUP_TABLE[,c('name','scientific','ncbi','genome_version')]
}
