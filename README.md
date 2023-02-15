
# gemma.R: A wrapper for Gemma’s Restful API to access curated gene expression data and differential expression analyses <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![R build
status](https://github.com/PavlidisLab/gemma.R/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/PavlidisLab/gemma.R/actions)
[![Codecov test
coverage](https://codecov.io/gh/PavlidisLab/gemma.R/branch/master/graph/badge.svg)](https://codecov.io/gh/PavlidisLab/gemma.R?branch=master)
[![DOI](https://img.shields.io/badge/doi-10.1093/database/baab006-yellow.svg)](https://doi.org/10.1093/database/baab006)
<!-- badges: end -->

This is an R wrapper for [Gemma](http://gemma.msl.ubc.ca)’s RESTful
[API](https://gemma.msl.ubc.ca/rest/v2/). Gemma is a web site, database
and a set of tools for the meta-analysis, re-use and sharing of genomics
data, currently primarily targeted at the analysis of gene expression
profiles. Gemma contains data from thousands of public studies,
referencing thousands of published papers.

## Installation instructions

### Bioconductor

You can install `gemma.R` through
[Bioconductor](http://bioconductor.org/) with the following code:

``` r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("gemma.R")
```

## Usage

To get started with `gemma.R`, read the
[vignette](https://pavlidislab.github.io/gemma.R/articles/gemma.R.html).

## Citation

To cite Gemma, please use: [Lim, N. et al., Curation of over 10 000
transcriptomic studies to enable data reuse, Database,
2021.](https://doi.org/10.1093/database/baab006)

## Code of Conduct

Please note that `gemma.R` is released with the [Bioconductor
Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.
