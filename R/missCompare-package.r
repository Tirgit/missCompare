#' missCompare: Missing Data Imputation Comparison Framework
#'
#' The \strong{missCompare} package offers a convenient pipeline to test and compare various missing data
#' imputation algorithms on simulated data. The central assumption behind missCompare is that structurally
#' different datasets (e.g. larger datasets with a large number of correlated variables vs. smaller datasets
#' with non correlated variables and other combinations) will benefit differently from different missing data
#' imputation algorithms. \strong{missCompare} takes measurements of your dataset and sets up a sandbox to try
#' a curated list of standard and sophisticated missing data imputation algorithms and compares them assuming
#' custom set missingness patterns. \strong{missCompare} will give you a comparative analysis of missing data
#' imputation algorithms, offer a report with the best performing algorithms assuming various missing data patterns
#' and publication ready visualizations, impute your dataset for you, assess imputation performance using a validation
#' framework and help you better understand missing data in your dataset.
#'
#' @details
#' \tabular{ll}{
#' Package: \tab missCompare\cr
#' Depends: \tab R (>= 3.5.0)\cr
#' Type: \tab Package\cr
#' Version:  \tab 1.0.1\cr
#' Date:  \tab 2019-01-30\cr
#' License:  \tab MIT\cr
#' LazyLoad:  \tab Yes
#' }
#'
#' @author
#' \itemize{
#'  \item Tibor V. Varga \email{tirgit@@hotmail.com}
#'  \item David Westergaard \email{david.westergaard@@cpr.ku.dk}
#' }
#'
#' @seealso
#' \url{https://github.com/Tirgit/missCompare}
#'
#' @name missCompare
#' @docType package
NULL
