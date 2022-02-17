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
getPlatformAnnotation <- function(platform,
    annotType = c("bioProcess", "noParents", "allParents"),
    file = getOption("gemma.file", NA_character_),
    overwrite = getOption("gemma.overwrite", FALSE),
    unzip = FALSE){
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

#' Get expression data
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
#'
#' @return A SummarizedExperiment or ExpressionSet of the queried dataset.
#' @keywords dataset
#' @export
#' @examples
#' getDataset("GSE2018")
getDataset <- function(dataset, filter = FALSE, type = "se") {
    if (type != "eset" && type != "se") {
        stop("Please enter a valid type: 'se' for SummarizedExperiment or 'eset' for ExpressionSet.")
    }
    exprM <- getDatasetExpression(dataset, filter)
    rownames(exprM) <- exprM$Probe
    genes <- S4Vectors::DataFrame(dplyr::select(exprM, "GeneSymbol", "GeneName", "NCBIid"))
    exprM <- dplyr::select(exprM, -"Probe", -"GeneSymbol", -"GeneName", -"NCBIid") %>%
        data.matrix()
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
    dat <- getDatasetsInfo(dataset, raw = TRUE)
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
            annotation = getDatasetPlatforms(dataset)$platform.ShortName
        )
    }
}

#' Get tidy dataset and design
#'
#' Combines the expression and design matrices of the queried dataset into a
#' tibble for easy visualization and exploration with ggplot and the rest of the tidyverse.
#'
#' @param dataset A dataset identifier.
#' @param filter The filtered version corresponds to what is used in most Gemma analyses, removing some probes/elements. Unfiltered includes all elements.
#'
#' @return A tibble that combines the expression and design matrices.
#' @keywords dataset
#' @export
#' @examples
#' getTidyDataset("GSE2018")
getTidyDataset <- function(dataset, filter = FALSE) {
    design <- getDatasetDesign(dataset) %>%
        tibble::rownames_to_column("Sample")
    # Get expression data, convert to long format and add exp. design
    getDatasetExpression(dataset, filter = filter) %>%
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
#' If there is more than one resultSet, use [getDatasetResultSets()] to see
#' the options and get the ID you want. Alternatively, you can query the resultSet
#' directly if you know its ID beforehand.
#'
#' @param dataset A dataset identifier.
#' @param resultSet A resultSet identifier.
#' @param all If TRUE, will download all differential expression resultSets for the dataset.
#'
#' @return A data table with differential expression values. If there are multiple
#' resultSets and all = TRUE, a list of data tables with differential expression
#' values.
#' @keywords dataset
#' @export
#' @examples
#' getDatasetDE("GSE2018")
getDatasetDE <- function(dataset = NA_character_, resultSet = NA_integer_, all = FALSE) {
    if (is.na(dataset) == FALSE && is.na(resultSet) == FALSE){
        rss <- getDatasetResultSets(dataset)
        if (!(resultSet %in% rss$resultSet.id)){
            stop("The queried resultSet is not derived from this dataset. Check the available resultSets with `getDatasetResultSets()` or query without the dataset parameter.")
        }
    }
    else if (is.na(dataset) == FALSE && is.na(resultSet) == TRUE){
        rss <- getDatasetResultSets(dataset)
        if (nrow(rss) > 1 && all == FALSE){
            stop("There are multiple resultSets for this dataset. Check the available resultSets with `getDatasetResultSets()` or choose all = TRUE")
        } else if (nrow(rss) > 1 && all == TRUE){
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
        .getResultSets(x) %>%
            processDEcontrasts(x)
    })
    if (length(rs) == 1){
        rs <- rs[[1]]
    }
    rs
}
