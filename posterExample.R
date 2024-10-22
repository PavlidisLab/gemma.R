library(gemma.R)
library(dplyr)

plain_text_search <- 
    get_datasets(query = "Parkinson disease")%>%
    get_all_pages


uri_search <- 
    get_datasets(uris = 'http://purl.obolibrary.org/obo/MONDO_0005180') %>% 
    get_all_pages


filter_search <- 
    get_datasets(filter = 'bioAssayCount > 10 and allCharacteristics.valueUri = http://purl.obolibrary.org/obo/MONDO_0005180') %>% 
    get_all_pages

setGemmaPath('prod')
# datasets relevant to X -> table

get_datasets(query = "neurodegeneration") %>% # plain text search
    get_all_pages() 
get_datasets(uris = "http://purl.obolibrary.org/obo/MONDO_0005559") %>% # ontology search
    get_all_pages()
get_datasets(filter = 
                 "experimentalDesign.experimentalFactors.factorValues.characteristics.valueUri = http://purl.obolibrary.org/obo/MONDO_0005559") %>% # specific filters
    get_all_pages()-> ndegen

# expression data for genes of interest in X datasets -> heatmap




# retrive differential expression reslts for disease X -> heatmap

# effect size distrubion -> scater plot
