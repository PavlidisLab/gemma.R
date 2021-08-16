
# Gemma API R Wrapper<img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![R build
status](https://github.com/jsicherman/GemmAPI/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/jsicherman/GemmAPI/actions)
[![Codecov test
coverage](https://codecov.io/gh/jsicherman/GemmAPI/branch/master/graph/badge.svg)](https://codecov.io/gh/jsicherman/GemmAPI?branch=master)
[![DOI](https://img.shields.io/badge/doi-10.1093/database/baab006-yellow.svg)](https://doi.org/10.1093/database/baab006)
<!-- badges: end -->

This is an R wrapper for [Gemma](http://gemma.msl.ubc.ca)’s RESTful
[API](https://gemma.msl.ubc.ca/resources/restapidocs/). Gemma is a web
site, database and a set of tools for the meta-analysis, re-use and
sharing of genomics data, currently primarily targeted at the analysis
of gene expression profiles. Gemma contains data from thousands of
public studies, referencing thousands of published papers.

## Installation instructions

### Development

The current development version of GemmaAPI can be installed with:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
}
devtools::install_github("PavlidisLab/gemmaAPI")
```

### Bioconductor (not available yet)

You can install `GemmaAPI` using from
[Bioconductor](http://bioconductor.org/) with the following code:

``` r
# if (!requireNamespace("BiocManager", quietly = TRUE)) {
#     install.packages("BiocManager")
# }
# 
# BiocManager::install("GemmaAPI")
```

## Usage

To get started with the Gemma API wrapper, read the
[vignette](https://pavlidislab.github.io/Gemma-API/articles/gemmaAPI.html).
In addition, you can check the [interactive API
documentation](https://gemma.msl.ubc.ca/resources/restapidocs/) to get
familiar quickly with the endpoints and their output.

## Citation

To cite Gemma, please use: [Lim, N. et al., Curation of over 10 000
transcriptomic studies to enable data reuse, Database,
2021.](https://doi.org/10.1093/database/baab006)

## Code of Conduct

Please note that the `GemmaAPI` project is released with the
[Bioconductor Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

-   Continuous code testing is possible thanks to [GitHub
    actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
    through *[usethis](https://CRAN.R-project.org/package=usethis)*,
    *[remotes](https://CRAN.R-project.org/package=remotes)*, and
    *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)*
    customized to use [Bioconductor’s docker
    containers](https://www.bioconductor.org/help/docker/) and
    *[BiocCheck](https://bioconductor.org/packages/3.13/BiocCheck)*.
-   Code coverage assessment is possible thanks to
    [codecov](https://codecov.io/gh) and
    *[covr](https://CRAN.R-project.org/package=covr)*.
-   The code is styled automatically thanks to
    *[styler](https://CRAN.R-project.org/package=styler)*.
-   The documentation is formatted thanks to
    *[devtools](https://CRAN.R-project.org/package=devtools)* and
    *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.
