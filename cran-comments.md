## Test environments
* local OS X install, R 3.3.2    
* win-builder (devel and release)   

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

During the checking process, a group of NOTEs were due to "no visible binding for global    variables". These problems arose due to the use of *dplyr* and *ggplot2* functions.    
If, for instance, NOTEs were generated in a function for **x**, **y** and **z**, because    
these were defined inside a ggplot() function as column names, these issues were eliminated by adding lines:     
x <- y <- y <- NULL    
To the functions before calling ggplot().   
The authors are aware that these lines do not have functional relevance in the functions,   but they strived to eliminate all NOTEs during the process, as suggested by various sources.

## Downstream dependencies
There are currently no downstream dependencies for this package.
