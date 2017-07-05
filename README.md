# missingdata
The dataset "simulated_dataframe.rds" contains information about 21 lipid species at two timepoints. The dataset is constructed based on realistic correlations between these lipids. Follow-up lipids are are correlated (rho=0.9) to their baseline counterparts).

The script aims to test various methods to impute missing data.
The following framework is implemented:

1. Remove x% of data randomly
2. Utilize method for imputation
3. Calculate Pearson correlation coefficient between original dataset and imputed dataset
4. Repeat 1.-2.-3. y times
5. Calculate mean and SE for y correlation coefficients.
