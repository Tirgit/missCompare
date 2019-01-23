#' Clinical dataset with missingness
#'
#' @description
#' \code{\link{clindata_miss}} is a custom made dataframe that resembles a real-life clinical dataset.
#' The correlations between variables, the data means, SDs and ranges are realistic, but
#' the dataset is constructed by simulations and manual data input. The dataset contains
#' missing values (approximately 10\% missing overall), and values are missing in a realistic pattern.
#'
#' @format A data frame with 2500 rows and 12 variables:
#' \describe{
#'   \item{age}{numeric, age, in years, 2.88\% missing - in general, age is not likely have lots of missing data in a realistic dataset, therefore only a few values are missing here randomly, e.g. due to mistakes in data input}
#'   \item{sex}{factor, male=1 and female=2, 2.88\% missing - similar to age, sex information is also not likely have missing data in a realistic dataset, no values are missing here}
#'   \item{waist}{numeric, waist circumference, in cm, 4.12\% missing - anthropometric data is easy to collect, therefore only a small fraction is missing here, often missing together with BMI, the other anthropometric variable}
#'   \item{BMI}{numeric, body mass index, in kg/m2, 4.16\% missing - anthropometric data is easy to collect, therefore only a small fraction is missing here, often missing together with waist, the other anthropometric variable}
#'   \item{SBP}{numeric, systolic blood pressure, in Hgmm, 8.84\% missing - in a realistic fashion, SBP is almost always missing together with DBP}
#'   \item{DBP}{numeric, diastolic blood pressure, in Hgmm, 8.84\% missing - in a realistic fashion, DBP is almost always missing together with SBP}
#'   \item{FG}{numeric, blood fasting glucose concentration, in mmol/dl, 5.84\% missing - often missing together with other clinical variables}
#'   \item{PPG}{numeric, blood postprandial glucose concentration, in mmol/dl, 53.2\% missing - in this simulated dataset, only less than half of the participants had postprandial glucose measurements}
#'   \item{TC}{numeric, blood total cholesterol concentration, in mmol/dl, 7.2\% missing - often missing together with other lipids, TG and HDL-C}
#'   \item{TG}{numeric, blood triglycerides concentration, in mmol/dl, 7.48\% missing - often missing together with other lipids, TC and HDL-C, due to the sensitivity of a hypothetical machine, values below 0.6 are set to -9, upon conversion from -9s to NAs, the missingness fraction is 10.6\%}
#'   \item{HDL}{numeric, blood high density lipoprotein cholesterol concentration, in mmol/dl, 10.76\% missing - often missing together with other lipids, TG and TC, due to the sensitivity of a hypothetical machine, values below 0.05 are set to -9, upon conversion from -9s to NAs, the missingness fraction is 13.72\%}
#'   \item{education}{factor, primary school=1, secondary school=2, bsc degree=3, msc degree=4, phd degree=5, 7.16\% missing - self reported education missing in a not random fashion, those with lower education are less likely to report their education status}
#' }
#'
#' @source The dataset is simulated and undergone manual configuration.
"clindata_miss"
