test_that('processor file writing',{
    file = tempfile()
    get_platforms_by_ids(1,file = file)

    expect_warning(get_platforms_by_ids(1,file = file),'exists. Not overwriting')

    file = tempfile()
    gemma.R::get_dataset_processed_expression(dataset = 'GSE2018', file = file)
    expect_true(file.exists(paste0(file)))

    file = tempfile()
    gemma.R::get_dataset_processed_expression(dataset = 'GSE2018', file = file, raw = TRUE)
    expect_true(file.exists(paste0(file)))


    file = tempfile()
    gemma.R::get_dataset_processed_expression(dataset = 'GSE2018', file = file, raw = FALSE)
    expect_true(file.exists(paste0(file)))


})

test_that('processAnnotations reads both Gemma 1.x and Gemma 2.0 field names',{
    # Gemma 2.0 renamed className/classUri/termName/termUri to
    # category/categoryUri/value/valueUri with no aliases, and gemma.R still
    # points at a Gemma 1.x server by default, so both have to parse.
    # The row below is the first of GSE2018's annotations, in each spelling.
    legacy <- list(list(className = 'organism part',
                        classUri = 'http://www.ebi.ac.uk/efo/EFO_0000635',
                        termName = 'lung',
                        termUri = 'http://purl.obolibrary.org/obo/UBERON_0002048',
                        objectClass = 'ExperimentTag'))

    current <- list(list(category = 'organism part',
                         categoryUri = 'http://www.ebi.ac.uk/efo/EFO_0000635',
                         value = 'lung',
                         valueUri = 'http://purl.obolibrary.org/obo/UBERON_0002048',
                         objectClass = 'ExperimentTag'))

    for (payload in list(legacy, current)){
        out <- gemma.R:::processAnnotations(payload)
        expect_s3_class(out, 'data.table')
        expect_equal(out$class.name, 'organism part')
        expect_equal(out$class.URI, 'http://www.ebi.ac.uk/efo/EFO_0000635')
        expect_equal(out$term.name, 'lung')
        expect_equal(out$term.URI, 'http://purl.obolibrary.org/obo/UBERON_0002048')
        expect_equal(out$object.class, 'ExperimentTag')
    }

    # both spellings produce identical output
    expect_equal(gemma.R:::processAnnotations(legacy),
                 gemma.R:::processAnnotations(current))

    # an empty response still yields the columns, as elsewhere in the package
    expect_equal(nrow(gemma.R:::processAnnotations(list())), 0)
})
