## Submission of new version 1.0.2

## Test environments
* local OS X install, R 4.0.3       
* ubuntu 14.04 (on travis-ci), R 4.0.2  
* win-builder (R-devel, R-release)   

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.    

## Further notes on resubmisson to CRAN
In response to Uwe's e-mail, I reduced data size for testing and running less iterations. Hoping this will reduce time for check time. Running the tests takes now 1m 19s on local OS.

Win-devel and Win-release show a NOTE: This note relates to the fact that missCompare was previously on CRAN and it was archived because check problems were not corrected on time. These checks are now corrected and I am now resubmitting the package with these issues fixed as version 1.0.2. The errors needed fixing related to the data.table::melt() function, that has been passed to various categories of objects, e.g. matrices and data.frames - as data.table::melt() currently only has a method for data.tables, objects have either been converted to data.tables or another solutions were found to replace existing code.
