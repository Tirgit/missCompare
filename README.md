## The missCompare R package

This repository contains the missCompare R package, a pipeline for missing data imputation. In the framework the following structure is implemented:

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

## Some examples

cleaned <- missCompare::clean(orig_df)
info <- missCompare::get_data(cleaned$Dataframe_clean)
sim_results <- missCompare::impute_simulated(rownum = info$Rows,
                           colnum = info$Columns,
                           cormat = info$Corr_matrix,
                           missfrac_per_var =  info$Fraction_missingness_per_variable,
                           n.iter = 50)
valid_results <- missCompare::impute_data_validate(orig_df,
                             scale = T,
                             spike.in = 0.01,
                             n.iter = 10,
                             sel_method = c(2,3,14,16))
imputed <- missCompare::impute_data(orig_df, scale = T, n.iter = 10, sel_method = c(14))
diagnostics <- missCompare::post_imp_diag(orig_df, imputed$mean_imputation[[1]], scale=T, n.boot = 100)

## Installation instructions

install.packages("missCompare")
library(missCompare)

## Overview of functions

An overview that describes the main components of the package. For more complex packages, this will point to vignettes for more details.
