
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CodeFactor](https://www.codefactor.io/repository/github/roblbaker/qckit/badge)](https://www.codefactor.io/repository/github/roblbaker/qckit)
<!-- badges: end -->

# QCkit

QCkit is a collection of quality control functions to munge, check,
flag, correct, and summarize data collected by the U.S. National Park
Service Inventory & Monitoring Division.

Functions are typically user generated and so may not have been
thoroughly tested for all use cases.

If you would like to contribute useful functions please initiate a pull
request.

Please request enhancements and bug fixes through
[Issues](https://github.com/nationalparkservice/QCkit/issues).

# Installation

Stand-alone installation:

``` r
# install.packages("devtools")
devtools::install_github("nationalparkservice/QCkit")
library(QCkit)
```

QCkit is also part of the
[NPSdataverse](https://nationalparkservice.github.io/NPSdataverse/) and
can be installed along with the other components of NPSdataverse:

``` r
# install.packages("devtools")
devtools::install_github("nationalparkservice/NPSdataverse")
library(NPSdataverse)
```
