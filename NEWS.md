# gemma.R 3.0.0

* Column names for the outputs of many functions have changed in this release to be more standardized. Please refer to the function documentation. As a general rule names use camelCase separated by .s to indicate properties of a specific entity (eg. experiment.sampleCount) with the exception of acronyms which are always capitalized (experiment.ID)
* update_results function added which allows re-creation of outputs of gemma.R functions without relying on the original code.
* get_result_sets function added which allows accessing result sets directly, filtering them based on certain filterable properties (see filter_properties()$resultSet).
* gemma_memoise function added which allows setting memoisation options without manually setting options
* get_child_terms function added which returns child terms of an ontology term as inferred by Gemma
* gemma_kable is added which returns tables formatted to fit 

# gemma.R 2.0.0
* Breaking change to `get_dataset_differential_expression_analyses` function in order to return annotations for contrasts with multiple characteristics.

# gemma.R 0.99.44
* Fixes and changes for Bioconductor 3.16 release

# gemma.R 0.99.0
* Submitted to Bioconductor.
