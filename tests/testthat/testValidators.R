test_that('validators work',{

    id = gemma.R:::validateID('id',1,2,3)
    expect_true(id == '1,2,3')
    # validateID is supposed to accept homogeneous results
    expect_error(gemma.R:::validateID('id','string',1,2),'do not combine different')

    id = gemma.R:::validateSingleID('id',1)
    expect_true(id == '1')
    # validateSingleID is.. well.. single
    expect_error(gemma.R:::validateSingleID('id',1,2),'one valid identifier')

    expect_true(gemma.R:::validateOptionalTaxon('taxon',NA)=='')
    expect_true(gemma.R:::validateOptionalTaxon('taxon','mouse') == 'mouse')

    expect_error(gemma.R:::validateTaxon('taxon','mouse','human'),'specify only one')
    expect_error(gemma.R:::validateTaxon('taxon','bigfoot'),'must specify a valid taxon')
    expect_true(gemma.R:::validateTaxon('taxon','human') == 'human')

    expect_error(gemma.R:::validateQuery('query'),'specify a query')
    expect_true(gemma.R:::validateQuery('query','something') == 'something')

    expect_error(gemma.R:::validatePositiveInteger('int',-1),'only specify positive')
    expect_error(gemma.R:::validatePositiveInteger('int','notanumber'),'only specify positive')
    expect_true(gemma.R:::validatePositiveInteger('int',1)==1)

    expect_error(gemma.R:::validateLimit('limit',200),'specify a limit between')
    expect_error(gemma.R:::validateLimit('limit',0),'specify a limit between')
    expect_true(gemma.R:::validateLimit('limit',50) == 50)

    expect_true(gemma.R:::validateBoolean('bool',TRUE) == 'true')
    expect_true(gemma.R:::validateBoolean('bool',FALSE) == 'false')
    expect_error(gemma.R:::validateBoolean('bool','TRUE'),'only specify boolean values')

    expect_error(gemma.R:::validateSort('sort',2),'must match')
    expect_error(gemma.R:::validateSort('sort',"+"),'must match')
    expect_true(gemma.R:::validateSort('sort',"+sort") == '+sort')

})
