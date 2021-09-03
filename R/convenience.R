#' Authentication
#'
#' Allows the user to access information that requires logging in to Gemma. To log out, run `setGemmaUser` without specifying the username or password.
#'
#' @param username Your username (or empty, if logging out)
#' @param password Your password (or empty, if logging out)
#'
#' @examples
#' setGemmaUser("username", "password") # login
#' setGemmaUser() # logout
#' @keywords misc
#' @export
setGemmaUser <- function(username = NULL, password = NULL) {
    options(gemma.username = username)
    options(gemma.password = password)
}

# DWR requests seem to have three parts:
# 1. An initial request to establish a trackable ID
# 2. Repeated polling until the request finishes
# 3. Receipt of status (once polling reveals completion)

#' Get differential expression data
#'
#' This occurs asynchronously as data will not always be available upon request. If the data is not available,
#' the function will continue to run and poll for updates every 3 seconds until it becomes available.
#'
#' @param dataset The experiment ID (integer)
#' @param diffExSet The analysis ID for the differential expression analysis we want (integer) @seealso getDatasetDEA for analysis.ID
#'
#' @return A list of `data.table`s that were read in, typically the first one is analysis results (elements, p-values) and the second is full data (elements, FC, t- and p-values)
#'
#' @keywords dataset
#' @export
getDiffExData <- async(function(dataset, diffExSet) {
    REQ1 <- "callCount=1
page=/expressionExperiment/showExpressionExperiment.html?id={dataset}
httpSessionId=
scriptSessionId=1
c0-scriptName=ExpressionExperimentDataFetchController
c0-methodName=getDiffExpressionDataFile
c0-id=0
c0-param0=number:{diffExSet}
batchId=4
"
    REQ2 <- "callCount=1
page=/expressionExperiment/showExpressionExperiment.html?id={dataset}
httpSessionId=
scriptSessionId=1
c0-scriptName=ProgressStatusService
c0-methodName=getSubmittedTask
c0-id=0
c0-param0=string:{taskID}
batchId={countN}"
    REQ3 <- "callCount=1
page=/expressionExperiment/showExpressionExperiment.html?id={dataset}
httpSessionId=
scriptSessionId=1
c0-scriptName=TaskCompletionController
c0-methodName=checkResult
c0-id=0
c0-param0=string:{taskID}
batchId={countN + 1}
"

    # Make the initial request
    http_post(paste0(getOption("gemma.base", "https://gemma.msl.ubc.ca/"), "dwr/call/plaincall/ExpressionExperimentDataFetchController.getDiffExpressionDataFile.dwr"), glue::glue(REQ1))$then(function(response) {
        if (response$status != 200) {
            warning(paste0("Unable to get differential expression data file: ", response$status))
            return(NULL)
        }

        # Receive a task ID to track
        taskID <- gsub('\'|"', "", strsplit(gsub(".*remoteHandleCallback\\(([^\\)]+)\\).*", "\\1", rawToChar(response$content)), ",")[[1]])[3]
        countN <- 5

        requery <- async(function(taskID, countN) {
            # Poll for completion
            http_post(paste0(getOption("gemma.base", "https://gemma.msl.ubc.ca/"), "dwr/call/plaincall/ProgressStatusService.getSubmittedTask.dwr"), glue::glue(REQ2))$then(function(response) {
                if (response$status != 200) {
                    warning(paste0("Unable to get task data: ", response$status))
                    return(NULL)
                }

                mContent <- rawToChar(response$content)
                # Detect completion
                if (grepl('taskStatus:"FAILED"', mContent, fixed = TRUE)) {
                    warning(paste0("Unable to get task data: ", gsub('.*lastLogMessage:"([^"]+)".*', "\\1", mContent)))
                    return(NULL)
                } else if (grepl("done:true", mContent, fixed = TRUE)) {
                    # Query where to find the result
                    http_post(paste0(getOption("gemma.base", "https://gemma.msl.ubc.ca/"), "dwr/call/plaincall/TaskCompletionController.checkResult.dwr"), glue::glue(REQ3))$then(function(response) {
                        if (response$status != 200) {
                            warning(paste0("Unable to get task result: ", response$status))
                            return(NULL)
                        }

                        # Extract the file location
                        fileLoc <- gsub('\'|"', "", strsplit(gsub(".*remoteHandleCallback\\(([^\\)]+)\\).*", "\\1", rawToChar(response$content)), ",")[[1]])[3]

                        # Make a temp file to unzip to
                        tmp <- tempfile()
                        base <- getOption("gemma.base", "https://gemma.msl.ubc.ca/")
                        http_get(paste0(substring(base, 1, nchar(base) - 1), fileLoc), file = tmp)$then(function(...) {
                            filenames <- utils::unzip(tmp, list = TRUE)$Name

                            # Unzip results and fread in
                            ret <- lapply(filenames, function(x) {
                                data.table::fread(
                                    cmd = glue::glue("unzip -p {tmp} {x}"),
                                    colClasses = c(
                                        Element_Name = "character",
                                        Gene_Symbol = "character",
                                        Gene_Name = "character"
                                    )
                                )
                            })
                            names(ret) <- filenames
                            unlink(tmp)
                            ret
                        })
                    })
                } else {
                    # Fail to detect completion, try again in 3 seconds
                    delay(3)$then(function(...) {
                        requery(taskID, countN + 1)
                    })
                }
            })
        })

        requery(taskID, countN)
    })
})

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
        platform <- platforms[, platform.ID]
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

#' Get Bioconductor data structures
#'
#' Combines various endpoint calls to return an annotated Bioconductor-compatible
#' data structure of the queried dataset, including expression data,
#' the experimental design and experiment metadata.
#'
#' @param type "SummarizedExperiment" or "ExpressionSet". We recommend using
#' SummarizedExperiments if you are querying RNA-Seq data. See the Summarized experiment
#' \href{https://bioconductor.org/packages/release/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html}{vignette}
#' or the ExpressionSet \href{https://bioconductor.org/packages/release/bioc/vignettes/Biobase/inst/doc/ExpressionSetIntroduction.pdf}{vignette}
#' for more details.
#' @param dataset A dataset identifier
#' @param filter If TRUE, call returns filtered expression data.
#'
#' @return An annotated \code{\link[SummarizedExperiment]{SummarizedExperiment-class}} or \code{\link[Biobase]{ExpressionSet}} for the queried dataset.
#' @importFrom rlang .data
#' @keywords dataset
#' @export
#' @examples
#' getBioconductor("SummarizedExperiment", "GSE2018", filter = TRUE)
getBioconductor <- function(type, dataset, filter) {
    # Create expression matrix
    expr <- getDatasetData(dataset, filter)
    exprM <- expr[, 7:ncol(expr)] %>% data.matrix()
    rownames(exprM) <- expr$Probe
    colnames(exprM) <- stringr::str_extract(colnames(expr[, 7:ncol(expr)]), "(?<=Name=).*")

    # Create metadata table
    d <- getDatasetDesign(dataset)
    design <- data.frame(d, row.names = stringr::str_extract(d$Bioassay, "(?<=Name=).*")) %>%
        dplyr::select(-c(.data$ExternalID, .data$Bioassay))

    # This annotation table is required
    annots <- data.frame(
        labelDescription = colnames(design),
        row.names = colnames(design)
    )

    phenoData <- Biobase::AnnotatedDataFrame(data = design, varMetadata = annots)

    # Reorder expression matrix to match design
    exprM <- exprM[, match(rownames(design), colnames(exprM))]

    # Create experiment description
    dat <- getDatasets(dataset)
    other <- list(
        database = dat$ee.Database,
        accesion = dat$ee.Accession,
        GemmaQualityScore = dat$geeq.qScore,
        GemmaSuitabilityScore = dat$geeq.sScore,
        taxon = dat$taxon.Name
    )

    title = dat$ee.Name
    abstract = dat$ee.Description
    url = paste0("https://gemma.msl.ubc.ca/expressionExperiment/showExpressionExperiment.html?id=", dat$ee.ID)

    if (type == 'SummarizedExperiment') {
        expData <- list(
            title = title,
            abstract = abstract,
            url = url
        )
        expData <- c(expData, other)
        SummarizedExperiment::SummarizedExperiment(
            assays=list(counts=exprM),
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
    } else {
       stop('miguebo')
    }
}
