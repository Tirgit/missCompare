


#FUNCTION
give_summary <- function(wrapper_output) {

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



