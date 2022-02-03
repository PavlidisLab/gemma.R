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

#' Get Gemma platform annotations
#'
#' Gets Gemma's platform annotation files that can be accessed from https://gemma.msl.ubc.ca/annots/
#'
#' @param platform A platform identifier @seealso getPlatforms
#' @param annotType Which GO terms should the output include
#' @param file Where to save the annotation file to, or empty to just load into memory
#' @param overwrite Whether or not to overwrite an existing file
#' @param unzip Whether or not to unzip the file (if @param file is not empty)
#'
#' @return A table of annotations
#' @keywords platform
#' @export
#' @examples
#' dat <- getPlatformAnnotation("GPL96")
#' str(dat)
getPlatformAnnotation <- function(platform, annotType = c("bioProcess", "noParents", "allParents"),
    file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE),
    unzip = FALSE) {
    if (!is.numeric(platform)) {
        platforms <- getPlatforms(platform)
        if (!isTRUE(nrow(platforms) == 1)) {
            stop(paste0(platform, " is not a valid single platform."))
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
                utils::write.table(ret, tools::file_path_sans_ext(file), sep = "\t", quote = FALSE, row.names = FALSE)
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
        warning(paste0(tools::file_path_sans_ext(file), " exists. Not overwriting."))
        doReadFile(file)
    } else {
        synchronise({
            http_get(glue::glue(paste0(getOption("gemma.base", "https://gemma.msl.ubc.ca/"), "arrays/downloadAnnotationFile.html?id={platform}&fileType={annotType}")), file = file)$then(function(...) {
                doReadFile(file)
            })
        })
    }
}

#' Get Bioconductor expression data
#'
#' Combines various endpoint calls to return an annotated Bioconductor-compatible
#' data structure of the queried dataset, including expression data,
#' the experimental design and experiment metadata.
#'
#' @param type "SummarizedExperiment" or "ExpressionSet". We recommend using
#' SummarizedExperiments which are more recent. See the Summarized experiment
#' \href{https://bioconductor.org/packages/release/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html}{vignette}
#' or the ExpressionSet \href{https://bioconductor.org/packages/release/bioc/vignettes/Biobase/inst/doc/ExpressionSetIntroduction.pdf}{vignette}
#' for more details.
#' @param dataset A dataset identifier.
#' @param filter The filtered version corresponds to what is used in most Gemma analyses, removing some probes/elements. Unfiltered includes all elements.
#'
#' @return A \code{\link[SummarizedExperiment]{SummarizedExperiment}} or \code{\link[Biobase]{ExpressionSet}} of the queried dataset.
#' @keywords dataset
#' @export
#' @examples
#' getBioc("ExpressionSet", "GSE2018", filter = TRUE)
getBioc <- function(type, dataset, filter = FALSE) {
    if (type != "ExpressionSet" && type != "SummarizedExperiment"){
       stop("Please enter a valid type: 'ExpressionSet' or 'SummarizedExperiment'")
    }
    exprM <- getDatasetData(dataset, filter)
    rownames(exprM) <- exprM$Probe
    genes <- S4Vectors::DataFrame(dplyr::select(exprM, "GeneSymbol", "GeneName", "NCBIid"))
    exprM <- dplyr::select(exprM, -"Probe", -"GeneSymbol", -"GeneName", -"NCBIid")
    exprM <- data.matrix(exprM)

    design <- getDatasetDesign(dataset)

    # This annotation table is required
    annots <- data.frame(
        labelDescription = colnames(design),
        row.names = colnames(design)
    )
    phenoData <- Biobase::AnnotatedDataFrame(data = design, varMetadata = annots)

    # Reorder expression matrix to match design
    exprM <- exprM[, match(rownames(design), colnames(exprM))]

    # Experiment description
    dat <- getDatasets(dataset, raw = TRUE)
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

    if (type == "SummarizedExperiment") {
        expData <- list(
            title = title,
            abstract = abstract,
            url = url
        )
        expData <- c(expData, other)
        SummarizedExperiment::SummarizedExperiment(
            assays=list(counts=exprM),
            rowData = genes,
            colData = design,
            metadata = expData
        )
    } else if (type == "ExpressionSet") {
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
            annotation = getDatasetPlatforms(dataset)$platform.ShortName
        )
    }
}

#' Get tidy dataset and design
#'
#' Combines the expression and design matrices of the queried dataset into a
#' tibble for easy visualization and exploration with \code{\link[ggplot2]{ggplot}} and the rest of the tidyverse.
#'
#' @param dataset A dataset identifier.
#' @param filter The filtered version corresponds to what is used in most Gemma analyses, removing some probes/elements. Unfiltered includes all elements.
#'
#' @return A \code{\link[tibble]{tibble}}.
#' @keywords dataset
#' @export
#' @examples
#' getTidyDataset("GSE2018")
getTidyDataset <- function(dataset, filter = FALSE){
    design <- getDatasetDesign(dataset) %>%
        tibble::rownames_to_column("Sample")
    # Get expression data, convert to long format and add exp. design
    getDatasetData(dataset, filter = filter) %>%
        as.data.frame() %>%
        tibble::column_to_rownames("Probe") %>%
        .[, match(design$Sample, colnames(.))] %>% # match sample order
        tibble::rownames_to_column("Probe") %>%
        tidyr::pivot_longer(-.data$Probe, names_to = "Sample", values_to = "expression") %>%
        dplyr::inner_join(design, by = "Sample") %>%
        dplyr::rename(sample = .data$Sample, probe = .data$Probe)
}

#' Get Differential Expression
#'
#' Retrieves the differential expression resultSet(s) associated with the dataset.
#' If there is more than one resultSet, it will show the options and prompt the user
#' to choose one.
#'
#' @param dataset A dataset identifier.
#'
#' @return A data table with differential expression values
#' @keywords dataset
#' @export
#' @examples
#' getDatasetDE("GSE2018")
getDatasetDE <- function(dataset){
    rss <- getDatasetResultSets(dataset)
    if (nrow(rss) > 1){
        validID <- FALSE
        while(validID == FALSE){
            print(rss)
            rsID <- readline(prompt = "Enter the ID of the desired differential expression resultSet: ")
            if (!(rsID %in% rs$resultSet.id)){
                warning("The ID you selected was not found for this dataset. Here are the available resultSets:",
                        immediate. = TRUE)
                validID <- FALSE
            }
            else{
                validID <- TRUE
            }
        }
    }
    else {
        rsID <- rss$resultSet.id
    }
    rs <- getResultSets(rsID)

    # Replace factor IDs by the factor names
    factors <- getResultSetFactors(rsID)
    baseline <- getDatasetDEA(dataset) %>%
        .[.data$result.ID == rsID, "cf.Baseline"] %>%
        unique()
    colnames(rs) <- stringr::str_replace(colnames(rs), "log2fc", "logFoldChange")
    colNames <- colnames(rs)
    for (f in factors$id){
        colNames <- stringr::str_replace(colNames, as.character(f), factors[factors$id == f, 2])
    }
    colnames(rs) <- colNames

    cat(paste0("Baseline: ", baseline, "\n"))
    cat(paste0("Factors: ", rss[rss$resultSet.id == rsID, rss$factor.levels], "\n\n"))

    rs
}
