
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exdata v0.0.0.9000

<!-- badges: start -->

[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)]()
![Questioning](https://img.shields.io/badge/lifecycle-questioning-informational)
[![CRAN/METACRAN](https://img.shields.io/cran/v/exdata)](https://cran.r-project.org/package=exdata)
[![dependencies](https://tinyverse.netlify.com/badge/exdata)](https://cran.r-project.org/package=exdata)
[![Downloads](https://cranlogs.r-pkg.org/badges/exdata?color=brightgreen)](https://cran.r-project.org/package=exdata)
<!-- badges: end -->

#### Document Code for Generating Datasets

Documenting how a dataset was generated increases reproducibility.In
addition, documentation for dataset generation can serve as a template
forpeople to create their own datasets in a similar structure, serving
as a wayto bootstrap new analyses. R packages often use the data-raw
directory tocreate datasets, but do not have an easy mechanism for
importing those creationsteps into dataset documentation. exdata
provides a new roxygen2 tag toautomatically copy over the dataset
generation code from the scripts thatcreate the dataset into the
documentation for the dataset.

## Installation

You can install exdata from GitHub with:

``` r
if (!requireNamespace('remotes', quietly = TRUE) {
  install.packages('remotes')
}
remotes::install_github('mojaveazure/exdata')
```
