# missingdata
Work related to missing data imputation / dimple package

This repository contains the dimple package, a collection of functions related to the topic 'missing data imputation'. In the dimple framework the following structure is implemented:

1. Cleaning and visualization of data
2. Extracting the characteristics of the data
3. Simulating random data with no missingness based on the original data characteristics.
4. Spiking in missingness (based on original missingness structure) in four patterns (missing completely at random [MCAR], missing at random [MAR], missing not at random [MNAR] and missing in an assumed pattern [MAP])
5. Utilizing curated list of missing data imputation methods (n=16)
6. Calculating RMSEs between indexed original values and imputed values for each missingness patterns and imputation methods
7. Repeating 4.-5.-6. iteratively 'n.iter' times
8. Calculating mean RMSE and 95% CIs for each missingness patterns and imputation methods, visualizing and interpreting results, give .pdf summary to user

The package contains additional script for the actual data imputation, post-imputation diagnostics, prediction of missing values, etc.
