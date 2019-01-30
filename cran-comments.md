## Resubmission
This is a resubmission. In this version (1.0.1) I have:

* Fixed first sentence and grammar mistake the Description section in the DESCRIPTION file

* Regarding the the question about potential references in the Description section in the DESCRIPTION file: our study group is working on a publication related to this R package, but this is not published yet. As soon as this paper is published, the authors will include this reference in the DESCRIPTION file

* Updated year from 2018 to 2019 in the LICENSE file

* Converted examples to executable examples in functions *clean()*, *get_data()*, *simulate()*, *MCAR()*, *MAR()*, *MNAR()*, *MAP()*, *all_patterns()* and *all individual method test() functions*. The remaining functions - *impute_simulated()*, *impute_data()*, *post_imp_diag()* - must remain non-executable due to time constraints. However, the authors made sure that all the functions in the latter category (with non-executed examples) remain to be thoroughly checked through testthat

* "No visible binding for global variables" problems now fixed using the high level *utils::globalVariables()* function

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
