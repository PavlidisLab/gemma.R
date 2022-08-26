

#' Replace missing data with NAs
#' @param x Data
#' @return Data or NA in case of an out of bounds error
#'
#' @keywords internal
checkBounds <- function(x){
    tryCatch(x, error = function(e){
        if(e$message == "subscript out of bounds"){
            return(NA)
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
    if (all(is.na(d))) {
        data.table(
            name = NA_character_, URL = NA_character_,
            description = NA_character_, category.Name = NA_character_,
            category.URL = NA_character_, measurement = FALSE, type = NA_character_
        )
    } else {
        data.table(
            name = switch(is.null(d[["factorValue"]]) + 1,
                d[["factorValue"]],
                d[["value"]]
            ),
            URL = d[["valueUri"]],
            description = d[["description"]],
            category.Name = d[["category"]],
            category.URL = d[["categoryUri"]],
            measurement = d[["measurement"]],
            type = d[["type"]]
        )
    }
}

#' Processes JSON as an array
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processGemmaArray <- function(d) {
    data.table(
        array.ShortName = d[["shortName"]],
        array.Name = d[["name"]],
        array.Taxon = d[["taxon"]],
        array.TaxonID = d[["taxonID"]],
        array.Type = d[["technologyType"]],
        array.Description = d[["description"]],
        array.Troubled = d[["troubled"]]
    )
}



#' Processes JSON as a vector of datasets
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processDatasets <- function(d) {
    data.table(
        ee.ShortName = d[["shortName"]],
        ee.Name = d[["name"]],
        ee.ID = d[["id"]],
        ee.Description = d[["description"]],
        ee.Public = d[["isPublic"]],
        ee.Troubled = d[["troubled"]],
        ee.Accession = d[["accession"]],
        ee.Database = d[["externalDatabase"]],
        ee.URL = d[["externalUri"]],
        ee.Samples = d[["bioAssayCount"]],
        ee.LastUpdated = switch(is.character(d[["lastUpdated"]]),
                                lubridate::ymd_hms(d[["lastUpdated"]]), # parse ISO 8601 format
                                as.POSIXct(d[["lastUpdated"]] / 1e3, origin = "1970-01-01")),
        ee.batchEffect = d[["batchEffect"]],
        geeq.batchCorrected = checkBounds(d[["geeq"]][["batchCorrected"]]),
        geeq.qScore = checkBounds(d[["geeq"]][["publicQualityScore"]]),
        geeq.sScore = checkBounds(d[["geeq"]][["publicSuitabilityScore"]]),
        taxon.Name = d[["taxon"]],
        taxon.ID = d[["taxonId"]],
        technology.Type = d[["technologyType"]]
    )
}

#' Processes JSON as an annotation
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processSearchAnnotations <- function(d) {
    data.table(
        category.Name = d[["category"]],
        category.URL = d[["categoryUri"]],
        value.Name = d[["value"]],
        value.URL = d[["valueUri"]]
    )
}

#' Processes JSON as a differential expression analysis
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processDEA <- function(d) {
    # Initialize internal variables to avoid R CMD check notes
    resultIds <- factor.ID <- cf.Baseline <- factorValue <- valueUri <- NULL
    factorId <- id <- result.ID <- analysis.Id <- ee.ID <- cf.Cat <- NULL
    cf.CatLongUri <- cf.Val <- cf.ValLongUri <- sf.Enabled <- NULL
    sf.category.Name <- sf.category.URL <- sf.name <- sf.URL <- stats.DE <- NULL
    stats.Down <- stats.Up <- analysis.Threshold <- probes.Analyzed <- NULL
    genes.Analyzed <- ad.ID <- factors <- cf.BaseLongUri <- analysis.ID <- NULL

    divides <- data.table(
        analysis.ID = d[["id"]],
        ee.ID = ifelse(is.na(d[["sourceExperiment"]]), d[["bioAssaySetId"]], d[["sourceExperiment"]]),
        sf.Enabled = d[["subset"]],
        sf = processGemmaFactor(d[["subsetFactorValue"]]),
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
                        cf.Cat = .[["category"]],
                        cf.CatLongUri = .[["categoryUri"]],
                        cf.Baseline = .[["factorValue"]],
                        cf.BaseLongUri = .[["valueUri"]]
                    )
                }
        ) %>%
            .[, .(factor.ID = unlist(factor.ID)), setdiff(names(.), "factor.ID")]
    }) %>%
        rbindlist() %>%
        .[!is.na(cf.Baseline)]

    rsd <- merge(rs, divides, by = c("result.ID", "analysis.ID"), all = TRUE)
    lapply(unique(rsd[, factor.ID]), function(fid) {
        lapply(d$factorValuesUsed[[as.character(fid)]], function(fv) {
            if (!is.null(fv) && nrow(fv) > 0) {
                as.data.table(fv)[
                    !(factorValue %in% rsd[factor.ID == fid, unique(cf.Baseline)]),
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
            ad.ID = d[["arrayDesignsUsed"]]
        ), by = "analysis.ID", all = TRUE) %>%
        .[, .(
            rsc.ID = paste("RSCID", result.ID, id, sep = "."),
            analysis.ID,
            ee.ID, cf.Cat, cf.CatLongUri, cf.Baseline, cf.BaseLongUri,
            cf.Val, cf.ValLongUri, sf.Subset = sf.Enabled,
            sf.Cat = sf.category.Name, sf.CatLongUri = sf.category.URL,
            sf.Val = sf.name, sf.ValLongUri = sf.URL,
            stats.DE, stats.Down, stats.Up, analysis.Threshold, probes.Analyzed,
            genes.Analyzed, ad.ID
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
#' @return A processed data.table
#'
#' @keywords internal
processDatasetResultSets <- function(d) {
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
#' @return A processed data.table
#'
#' @keywords internal
processAnnotations <- function(d) {
    data.table(
        class.Type = d[["objectClass"]],
        class.Name = d[["className"]],
        class.URL = d[["classUri"]],
        evidence.Code = d[["evidenceCode"]],
        term.Name = d[["termName"]],
        term.URL = d[["termUri"]]
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
#' @return A processed data.table
#'
#' @keywords internal
processSamples <- function(d) {
    data.table(
        bioMaterial.Name = checkBounds(d[["sample"]][["name"]]),
        sample.Name = d[["name"]],
        sample.ID = checkBounds(d[["sample"]][["id"]]),
        sample.Correspondence = checkBounds(d[["sample"]][["description"]]),
        sample.Description = d[["description"]],
        sample.Outlier = d[["outlier"]],
        sample.Accession = checkBounds(d[["accession"]][["accession"]]),
        sample.Database = checkBounds(d[["accession"]][["externalDatabase.name"]]),
        sample.Processed = d[["processingDate"]],
        sample.Characteristics = lapply(checkBounds(d[["sample"]][["characteristics"]]), processGemmaFactor),
        sample.FactorValues = lapply(lapply(checkBounds(d[["sample"]][["factorValueObjects"]]), "[[", "characteristics"), function(x) processGemmaFactor(rbindlist(x))),
        processGemmaArray(d[["arrayDesign"]])
    )
}

#' Processes JSON as a vector of platforms
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processPlatforms <- function(d) {
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
        technology.Type = d[["technologyType"]],
        technology.Color = d[["color"]]
    )
}

#' Processes JSON as a vector of elements
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processElements <- function(d) {
    data.table(
        mapping.Name = d[["name"]],
        mapping.Description = d[["description"]]
    )
}

#' Processes JSON as a vector of genes
#'
#' @param d The JSON to process
#'
#' @return A processed data.table
#'
#' @keywords internal
processGenes <- function(d) {
    data.table(
        gene.Symbol = d[["officialSymbol"]],
        gene.Ensembl = d[["ensemblId"]],
        gene.NCBI = d[["ncbiId"]],
        gene.Name = d[["officialName"]],
        gene.Aliases = d[["aliases"]],
        gene.GO = d[["numGoTerms"]],
        gene.Homologues = d[["homologues"]],
        gene.MFX.Rank = d[["multifunctionalityRank"]],
        taxon.Name = d[["taxonCommonName"]],
        taxon.ID = d[["taxonId"]],
        taxon.Scientific = d[["taxonScientificName"]],
        phenotypes = d[["phenotypes"]]
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
#' @return A processed data.table
#'
#' @keywords internal
processGeneLocation <- function(d) {
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
#' @return A processed data.table
#'
#' @keywords internal
processGO <- function(d) {
    data.table(
        term.Name = d[["term"]],
        term.ID = d[["goId"]],
        term.URL = d[["uri"]]
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

# processSVD <- function(d){
#     d$vMatrix$rawMatrix
#     browser()
# }
