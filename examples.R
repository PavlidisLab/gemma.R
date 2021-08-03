# getDatasets
# example
getDatasets("GSE2018")
getDatasets(c("GSE2018", "GSE2872"))
# value
"If raw is FALSE (default), a data table with information about the queried dataset(s).
If raw is TRUE, a list with the same information."

# searchDatasets
# example
searchDatasets("bipolar")
# value
"If raw is FALSE (default), a data table with information about the queried dataset(s).
If raw is TRUE, a list with the same information."

# getDatasetPlatforms
# example
getDatasetPlatforms("GSE2018")
# value
"If raw is FALSE (default), a data table with information about the platform(s) of the queried dataset.
If raw is TRUE, a list with the same information."

# getDatasetSamples
# example
getDatasetSamples("GSE2018")
# value
"If raw is FALSE (default), a data table with information about the samples of the queried dataset.
If raw is TRUE, a list with the same information."

# getDatasetDEA
# example
getDatasetDEA("GSE2018")
# value
"If raw is FALSE (default), a data table with information about the differential expression analysis of the queried dataset.
If raw is TRUE, a list with the same information."

# getDatasetSVD
# example
getDatasetSVD("GSE2018")
# value
"If raw is FALSE (default), a data table of the support vector decomposition of the queried dataset.
If raw is TRUE, a list with the same information."

# getDatasetAnnotations
# example
getDatasetAnnotations("GSE2018")
# value
"If raw is FALSE (default), a data table with information about the annotations of the queried dataset.
If raw is TRUE, a list with the same information."

# getDatasetData
# example
getDatasetData("GSE2018")
# value
"If raw is FALSE (default), a data table of the expression matrix for the queried dataset.
If raw is TRUE, a list with the expression matrix in binary form."

# getDatasetDesign
# example
getDatasetDesign("GSE2018")
# value
"If raw is FALSE (default), a data table of the design matrix for the queried dataset.
If raw is TRUE, a list with the design matrix in binary form."

# getDatasetPCA
# example
getDatasetPCA("GSE2018")
# value
"If raw is FALSE (default), a data table with the expression values for the selected component.
If raw is TRUE, a list with the same information."

# getDatasetDE
# example
getDatasetDE("GSE2018", diffExSet =  468329)
# value
"If raw is FALSE (default), a data table with the expression values.
If raw is TRUE, a list with the same information."

# getPlatforms
# example
getPlatforms("GPL1355")
getPlatforms(c("GPL1355", "GPL96"))
# value
"If raw is FALSE (default), a data table with information about the queried platform(s).
If raw is TRUE, a list with the same information."

# getPlatformDatasets
# example
getPlatformDatasets("GPL1355")
# value
"If raw is FALSE (default), a data table with information about the datasets associated with the queried platform.
If raw is TRUE, a list with the same information."

# getPlatformElements
# example
getPlatformElements("GPL1355")
# value
"If raw is FALSE (default), a data table with information about the elements (probes or genes) used by the queried platform.
If raw is TRUE, a list with the same information."

# getPlatformElementGenes
# example
getPlatformElementGenes("GPL1355", "AFFX_Rat_beta-actin_M_at")
# value
"If raw is FALSE (default), a data table with information about the gene(s) on the queried platform element.
If raw is TRUE, a list with the same information."

# getGenes
# example
getGenes("DYRK1A")
getGenes("DYRK1A", "PTEN")
# value
"If raw is FALSE (default), a data table with information about the queried gene(s).
If raw is TRUE, a list with the same information."

# getGeneEvidence
# example
getGeneEvidence("DYRK1A")
# value
"If raw is FALSE (default), a data table with information about the disease linkage evidence of the queried gene.
If raw is TRUE, a list with the same information."

# getGeneLocation
# example
getGeneLocation("DYRK1A")
# value
"If raw is FALSE (default), a data table with information about the physical location of the queried gene.
If raw is TRUE, a list with the same information."

# getGeneProbes
# example
getGeneProbes("DYRK1A")
# value
"If raw is FALSE (default), a data table with information about the probes that map to the queried gene.
If raw is TRUE, a list with the same information."

# getGeneGO
# example
getGeneGO("DYRK1A")
# value
"If raw is FALSE (default), a data table with information about the GO terms assigned to the queried gene.
If raw is TRUE, a list with the same information."

# getTaxa
# example
getTaxa("human")
getTaxa(c("human", "rat"))
# value
"If raw is FALSE (default), a data table with information about the queried taxon/taxa.
If raw is TRUE, a list with the same information."

# getTaxonDatasets
# example
getTaxonDatasets("human")
# value
"If raw is FALSE (default), a data table with information about the datasets associated to the queried taxon.
If raw is TRUE, a list with the same information."

# getTaxonPhenotypeCandidates
# example
getTaxonPhenotypeCandidates("human", phenotypes = c("http://purl.obolibrary.org/obo/DOID_11934",
                                                    "http://purl.obolibrary.org/obo/DOID_3119"))
# value
"If raw is FALSE (default), a data table with information about the genes associated with the queried phenotype(s) in the queried taxon.
If raw is TRUE, a list with the same information."

# getGeneOnTaxon
# example
getGeneOnTaxon("human", "DYRK1A")
# value
"If raw is FALSE (default), a data table with information about the queried gene in the queried taxon.
If raw is TRUE, a list with the same information."

# getGeneLocationOnTaxon
# example
getGeneLocationOnTaxon("human", "DYRK1A")
# value
"If raw is FALSE (default), a data table with information about the location of queried gene in the queried taxon.
If raw is TRUE, a list with the same information."

# getGeneAtLocation
# example
getGenesAtLocation("human", chromosome = 21, strand = '+', start = 2, size = 20000)
# value
"If raw is FALSE (default), a data table with information about the location gene(s) overlapping the queried region in the queried taxon.
If raw is TRUE, a list with the same information."
