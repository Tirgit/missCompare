#' missCompare: Data Imputation Made Simple
#'
#' The \strong{missCompare} package offers a convenient pipeline to test and compare various missing data
#' imputation algoritms on simulated data. The central assumption behind missCompare is that structurally
#' different datasets (e.g. larger datasets with a large number of correlated variables vs. smaller datasets
#' with non correlated variables) will benefit differently from different missing data imputation algorithms.
#' missCompare takes measurements of your dataset and sets up a sandbox to try a curated list of standard and
#' sophisticated missing data imputation algorithms and missCompare them assuming custom set missingess patterns.
#' missCompare WILL NOT impute your dataset for you. However, missCompare WILL give a comparative analysis of algorithms
#' and offer a report with the best performing algorithms assuming various missing data patterns and publication
#' ready visualizations to help you better understand missing data in your dataset.
#'
#' @details
#' \tabular{ll}{
#' Package \tab missCompare\cr
#' Type \tab Package\cr
#' Version:  \tab 0.1.0\cr
#' Date:  \tab 2018-05-03\cr
#' License:  \tab CC0\cr
#' LazyLoad:  \tab Yes
#' }
#'
#' @author
#' \itemize{
#'  \item Tibor V. Varga \email{tibor.varga@@cpr.ku.dk}
#'  \item David Westergaard \email{david.westergaard@@cpr.ku.dk}
#' }
#'
#' @seealso
#' \url{https://github.com/Tirgit/missingdata}
#'
#' @name missCompare
#' @docType package
NULL
