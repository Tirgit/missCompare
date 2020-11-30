## Resubmission of new version 1.0.3

## Test environments
* local OS X install, R 4.0.3       
* ubuntu 14.04 (on travis-ci), R 4.0.2  
* win-builder (R-devel, R-release)   

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.    

## Resubmission
This is a resubmission. In this version I have made the following changes in response to Gregor Seyer's comments:

* Please always write package names, software names and API (application programming interface) names in single quotes in title and description: FIXED. All package names are now marked with single quotation marks in the DESCRIPTION file and in the function titles and details.

* If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form: FIXED. I added five key references to the DESCRIPTION file now.

* Please write TRUE and FALSE instead of T and F. (Please don't use 'T' or 'F' as vector names.): FIXED. All instances in all functions checked, Ts and Fs replaced with TRUEs and FALSEs.

* \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user [...] Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}: FIXED, although with a hack. I really tried using donttest as
you suggested, but it just didn't work - read all material on this in forums and still couldn't get it to
work, so those examples that take too much time are now commented out in the following five functions: impute_data(), impute_simulated(), post_imp_diag(), test_mi() and test_pcaMethods_NLPCA().

* Have the issues why your package was archived been fixed?: FIXED. The package was archived because there were ERRORs that needed fixing related to the data.table::melt() function, that has been passed to various categories of objects, e.g. matrices and data.frames - as data.table::melt() currently only has a method for data.tables, objects have either been converted to data.tables or another solutions were found to replace existing code.
