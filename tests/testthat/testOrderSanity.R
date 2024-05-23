test_that('sample ordering in outputs',{
    samples <- get_dataset_samples(2)
    expression <- get_dataset_processed_expression(2)
    gene_expression <- get_dataset_expression_for_genes(2,282817)
    
    object <- get_dataset_object(2,type = 'list')
    object_genes <- get_dataset_object(2,282817,type = 'list')
    
    exp_cols <- colnames(expression) %>% {.[. %in% samples$sample.name]}
    gene_exp_cols <- colnames(gene_expression[[1]]) %>% {.[. %in% samples$sample.name]}
    expect_true(all(exp_cols == samples$sample.name))
    expect_true(all(gene_exp_cols == samples$sample.name))
    expect_true(all(rownames(object[[1]]$design) == samples$sample.name))
    expect_true(all(rownames(object_genes[[1]]$design) == samples$sample.name))
    object_exp_cols <- colnames(object[[1]]$exp) %>% {.[. %in% samples$sample.name]}
    object_gene_exp_cols <-  colnames(object_genes[[1]]$exp) %>% {.[. %in% samples$sample.name]}
    expect_true(all(object_exp_cols == samples$sample.name))
    expect_true(all(object_gene_exp_cols == samples$sample.name))
})


test_that('Multiple sanity checks for expression/differential expression values and sample metadata',{
    # mouse datasets with biological sex containing expression data for Xist and Kdm5d
    # currently relies on having non-overlapping Xist and Kdm5d expression between sexes
    datasets = c(35175,35171,35087,35032)
    

    for (dt in datasets){
        samples <- get_dataset_samples(dt)
        exp <- get_dataset_processed_expression(dt)
        exp_for_genes <- get_dataset_expression_for_genes(dt,c(213742, #xist
                                                               20592)) #kdm5d
        
        male_s <- samples$sample.characteristics %>% 
            purrr::map_lgl(\(x){any(tolower(x$value) == 'male')}) %>% 
            {samples$sample.name[.]}
        female_s <- samples$sample.characteristics %>% 
            purrr::map_lgl(\(x){any(tolower(x$value) == 'female')}) %>% 
            {samples$sample.name[.]}
        
        
        expect_true(max(exp[GeneSymbol =='Xist', ..male_s] %>% colMeans(),na.rm = TRUE) <
                        min(exp[GeneSymbol =='Xist', ..female_s],na.rm = TRUE))
        
        expect_true(max(exp[GeneSymbol =='Kdm5d', ..female_s] %>% colMeans(),na.rm = TRUE) <
                        min(exp[GeneSymbol =='Kdm5d', ..male_s],na.rm = TRUE))
        
        expect_true(max(exp_for_genes[[1]][GeneSymbol =='Xist', ..male_s] %>% colMeans(),na.rm = TRUE) <
                        min(exp_for_genes[[1]][GeneSymbol =='Xist', ..female_s],na.rm = TRUE))
        
        expect_true(max(exp_for_genes[[1]][GeneSymbol =='Kdm5d', ..female_s] %>% colMeans(),na.rm = TRUE) <
                        min(exp_for_genes[[1]][GeneSymbol =='Kdm5d', ..male_s],na.rm = TRUE))
        
        object <- get_dataset_object(dt)
        object_genes <- get_dataset_object(dt, 
                                           c(213742,20592))
        
        expect_true(
            max(assay(object[[1]],'counts')[rowData(object[[1]])$GeneSymbol=='Xist',
                                            tolower(object[[1]]$`biological sex`) =='male', drop = FALSE] %>% 
                    colMeans(),na.rm=TRUE) < 
                min(assay(object[[1]],'counts')[rowData(object[[1]])$GeneSymbol=='Xist',
                                                tolower(object[[1]]$`biological sex`) =='female', drop = FALSE] %>%
                        colMeans(),na.rm=TRUE)
        )
        
        expect_true(
            max(assay(object[[1]],'counts')[rowData(object[[1]])$GeneSymbol=='Kdm5d',
                                            tolower(object[[1]]$`biological sex`) =='female',drop=FALSE] %>% 
                    colMeans(),na.rm=TRUE) < 
                min(assay(object[[1]],'counts')[rowData(object[[1]])$GeneSymbol=='Kdm5d',
                                                tolower(object[[1]]$`biological sex`) =='male',drop = FALSE] %>%
                        colMeans(),na.rm=TRUE)
        )
        
        
        expect_true(
            max(assay(object_genes[[1]],'counts')[rowData(object_genes[[1]])$GeneSymbol=='Xist',
                                                  tolower(object_genes[[1]]$`biological sex`) =='male', drop = FALSE] %>% 
                    colMeans(),na.rm=TRUE) < 
                min(assay(object_genes[[1]],'counts')[rowData(object_genes[[1]])$GeneSymbol=='Xist',
                                                      tolower(object_genes[[1]]$`biological sex`) =='female', drop = FALSE] %>%
                        colMeans(),na.rm=TRUE)
        )
        
        expect_true(
            max(assay(object_genes[[1]],'counts')[rowData(object_genes[[1]])$GeneSymbol=='Kdm5d',
                                                  tolower(object_genes[[1]]$`biological sex`) =='female',drop=FALSE] %>% 
                    colMeans(),na.rm=TRUE) < 
                min(assay(object_genes[[1]],'counts')[rowData(object_genes[[1]])$GeneSymbol=='Kdm5d',
                                                      tolower(object_genes[[1]]$`biological sex`) =='male',drop = FALSE] %>%
                        colMeans(),na.rm=TRUE)
        )
        
        dif_exp <- get_differential_expression_values(dt)
        dif_exp_meta <- get_dataset_differential_expression_analyses(dt)
        
        sex_res <- dif_exp_meta %>% dplyr::filter(factor.category == 'biological sex') %>% {.$result.ID}
        
        for (rs in sex_res){
            dif_exp[[as.character(rs)]] %>% dplyr::filter(GeneSymbol == "Xist") %>% 
                {expect_true(all(.$pvalue < 0.01))} 
            dif_exp[[as.character(rs)]] %>% dplyr::filter(GeneSymbol == "Kdm5d") %>% 
                {expect_true(all(.$pvalue < 0.01))}
        }
        
    }
    
   
})
