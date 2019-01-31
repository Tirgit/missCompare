## Resubmission
This is a resubmission. In this version I have:

* Updated patch to 1.0.1

* Fixed first sentence and grammar mistake in the Description section in the DESCRIPTION file

* Regarding the question about potential references in the Description section in the DESCRIPTION file: our study group is working on a publication related to this R package, but this is not published yet. As soon as this paper is published, the authors will include this reference in the DESCRIPTION file

* Updated year from 2018 to 2019 in the LICENSE file

* Converted examples to executable examples in functions `clean()`, `get_data()`, `simulate()`, `MCAR()`, `MAR()`, `MNAR()`, `MAP()`, `all_patterns()`,  `test_AmeliaII()`, `test_aregImpute()`, `test_kNN()`, `test_mean_imp()`, `test_median_imp()`, `test_mice_mixed()`, `test_missForest()`, `test_missMDA_EM()`, `test_missMDA_reg()`, `test_pcaMethods_BPCA()`, `test_pcaMethods_Nipals()`, `test_pcaMethods_PPCA()`, `test_pcaMethods_svdImpute()`, `test_random_imp()`. The remaining functions - `test_mi()`, `test_pcaMethods_NLPCA()`, `impute_simulated()`, `impute_data()`, `post_imp_diag()` - remain non-executable due to time constraints. However, the authors made sure that all the functions in the latter category (with non-executed examples) remain to be checked through testthat

* Fixed "No visible binding for global variables" problems using the high level `utils::globalVariables()` function as instructed

## Submission comments
This is this first submission of the missCompare R package to CRAN.    

## Test environments
* local OS X install, R 3.5.2        
* ubuntu 14.04 (on travis-ci), R 3.5.0    
* win-builder (R-devel, R-release)   

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
There are currently no downstream dependencies for this package.    
