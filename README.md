
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GiottoUtils <img src="man/figures/logo.png" align="right" alt="" width="160" />

<!-- badges: start -->

![Version](https://img.shields.io/github/r-package/v/drieslab/GiottoUtils)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/drieslab/GiottoUtils/branch/dev/graph/badge.svg)](https://app.codecov.io/gh/drieslab/GiottoUtils?branch=dev)
[![R-CMD-check](https://github.com/drieslab/GiottoUtils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/drieslab/GiottoUtils/actions/workflows/R-CMD-check.yaml)
[![GitHub
issues](https://img.shields.io/github/issues/drieslab/Giotto)](https://github.com/drieslab/Giotto/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/drieslab/GiottoUtils)](https://github.com/drieslab/GiottoUtils/pulls)
<!-- badges: end -->

GiottoUtils exports functionalities that are called internally by many
of the other packages in the Giotto ecosystem. These range from things
such as commonly used imports to pretty prints.

A helpful install utility for the rest of the suite modules is also
provided. See [`suite_install()`](reference/suite_install.html)

## Installation

You can install GiottoUtils like:

``` r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("drieslab/GiottoUtils")
```

There is also a version of GiottoUtils locked at R version 4.4.0 that
can be installed as following:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("drieslab/GiottoUtils@R4.4.0")
```
