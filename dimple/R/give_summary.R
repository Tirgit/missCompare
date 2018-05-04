#' @title Creating summary report from the simulated imputation framework
#'
#' @description
#' \code{\link{give_summary}}
#'
#' @details
#'
#'
#' @param impute_simulated_output The saved R object from the \code{\link{impute_simulated}} function (see examples)
#'
#' @name give_summary
#'
#' @return
#' Pdf output with graphs and summary statistics of the performance of the missing data algorithms
#'
#' @examples
#' \dontrun{
#' give_summary(wrap)
#' }
#'
#' @export


#FUNCTION
give_summary <- function(impute_simulated_output) {

  print(paste("In case you assume a missing completely at random (MCAR) missingness pattern, dimple suggests you to use the ",
        wrapper_output$Best_method_MCAR,
        " algorithm for imputation", sep= ""))
  print(paste("In case you assume a missing at random (MAR) missingness pattern, dimple suggests you to use the ",
              wrapper_output$Best_method_MAR,
              " algorithm for imputation", sep= ""))
  print(paste("In case you assume a missing not at random (MNAR) missingness pattern, dimple suggests you to use the ",
              wrapper_output$Best_method_MNAR,
              " algorithm for imputation", sep= ""))

  wrapper_output$Plot

}


#give_summary(wrap)



