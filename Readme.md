Uncertainty Propagation (GUM & GUM-Supp1) and Sensitivity Analysis
==================

[![Travis-CI Build Status](https://travis-ci.org/ppernot/rgumlib.svg?branch=master)](https://travis-ci.org/ppernot/rgumlib)
[![codecov](https://codecov.io/github/ppernot/rgumlib/branch/master/graphs/badge.svg)](https://codecov.io/github/ppernot/rgumlib) 


## Install

You can install this package in R from GitHub by executing

```r
install.packages("devtools")
devtools::install_github("ppernot/rgumlib")
```
in the R console, and load it the usual way

```r
library("rgumlib")
```

## What is rgumlib ?

`rgumlib` is a set of R functions designed to perform uncertainty propagation
according to the [GUM](http://www.bipm.org/utils/common/documents/jcgm/JCGM_100_2008_F.pdf) 
and [GUM-Supp1](http://www.bipm.org/utils/common/documents/jcgm/JCGM_101_2008_E.pdf) recommendations.

The package implements also various plotting functions, and tools for sensitivity analysis.

It is mostly used as a teaching package.

## Want to test it before installing ?

`rgumlib` is the engine behind the [upsa](https://upsa.shinyapps.io/DynamicUI/) 
Shiny application which implements most of its features.


