
# Gemma API <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![R build
status](https://github.com/jsicherman/GemmAPI/workflows/R-CMD-check/badge.svg)](https://github.com/jsicherman/GemmAPI/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/gemmaAPI)](https://CRAN.R-project.org/package=gemmaAPI)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/gemmaAPI?color=blue)](https://cran.r-project.org/package=badger)
<!-- [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) -->
[![Codecov test
coverage](https://codecov.io/gh/jsicherman/GemmAPI/branch/master/graph/badge.svg)](https://codecov.io/gh/jsicherman/GemmAPI?branch=master)
[![DOI](https://img.shields.io/badge/doi-10.1093/database/baab006-yellow.svg)](https://doi.org/10.1093/database/baab006)
<!-- badges: end -->

This is an R wrapper for [Gemma](http://gemma.msl.ubc.ca)’s restful
[API](https://gemma.msl.ubc.ca/resources/restapidocs/).

To cite Gemma, please use: [Lim, N. et al., Curation of over 10 000
transcriptomic studies to enable data reuse, Database,
2021.](https://doi.org/10.1093/database/baab006)

## Installation

You can install the released version of gemmaAPI from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gemmaAPI")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jsicherman/GemmAPI")
```

## Usage

For help on using the Gemma API wrapper, please see the REST [API
docs](https://gemma.msl.ubc.ca/resources/restapidocs/) or the
[quickstart
vignette](https://jsicherman.github.io/GemmAPI/articles/gemmaAPI.html).
