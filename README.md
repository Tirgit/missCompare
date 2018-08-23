<!-- rmarkdown v1 -->
---
output: github_document
---
 
---
 
 
---
 
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Licence](https://img.shields.io/badge/licence-MIT +-lightgrey.svg)](http://choosealicense.com/)
[![Build Status](https://travis-ci.com/Tirgit/missCompare.svg?branch=master)](https://travis-ci.com/Tirgit/missCompare)
 
---
 
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/missCompare)](https://cran.r-project.org/package=missCompare)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](commits/master)
 
---
 
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--08--23-yellowgreen.svg)](/commits/master)

<!-- README.md is generated from README.Rmd. Please edit that file -->



# missCompare

This repository contains the missCompare R package, a pipeline for missing data imputation.

## Installation

You can install the released version of missCompare from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("missCompare")
library(missCompare)
```

## Framework

In the framework the following structure is implemented:

1. Cleaning and visualization of data
2. Extracting the characteristics of the data
3. Simulating random data with no missingness based on the original data characteristics
4. Spiking in missingness (based on original missingness structure) in four patterns (missing completely at random [MCAR], missing at random [MAR], missing not at random [MNAR] and optionally, missing in an assumed pattern [MAP])
5. Utilizing a curated list of missing data imputation methods (n=16)
6. Calculating RMSE between the indexed original values and imputed values for each missingness patterns and imputation methods
7. Repeating 4.-5.-6. iteratively *n* times
8. Calculating mean RMSE and 95% CIs for each missingness patterns and imputation methods, visualizing and interpreting results and informing the user on best performing methods.
9. Offering a validation framework in which the user may spike in a small number of missing
data in their original data and perform a similar pipeline of selected missing data imputation algorithms and validate best performing methods.   
10. Imputation of the original dataset using a selected method/selected methods
11. Performing post-imputation diagnostics
