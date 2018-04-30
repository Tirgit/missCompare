# missingdata
This repository contains a collection of functions related to the topic 'missing data imputation'. In the dimple framework the following structure is implemented:

1. Clean and visualize data
2. Extract the characteristics of the data
3. Simulate random data with no missingness based on the original data characteristics.
4. Spike in missingness (based on original missingness structure) in four patterns (missing completely at random [MCAR], missing at random [MAR], missing not at random [MNAR] and missing in an assumed pattern [MAP])
5. Utilize curated list of missing data imputation methods (n=14)
6. Calculate RMSEs between indexed original values and imputed values for each missingness patterns and imputation methods
7. Repeat 4.-5.-6. iteratively n times
8. Calculate mean RMSE and 95% CIs for each missingness patterns and imputation methods, visualize and interpret results, give pdf summary to user
