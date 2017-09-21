library(MASS)
library(Matrix)
library(missForest)

#EXAMPLE USING A SMALL, POSITIVE DEFINITIVE CORRELATION MATRIX
N <- 1000
M <- 3
mu <- rep(0,M)
stddev <- rep(1,M)
corMat <- matrix(c(1, 0.78, 0.23,
                   0.78, 1, 0.27,
                   0.23, 0.27, 1),
                 ncol = M)
covMat <- stddev %*% t(stddev) * corMat
X_hat <- mvrnorm(n=N, mu=mu, Sigma=covMat, empirical=TRUE) # Simulated values
#compare correlation coefficients:
corMat
cor(X_hat)

#ESTIMATING NEAREST POSITIVE DEFINITIVE MATRIX ON KNOWN CORRELATION STRUCTURE
N <- 10000
x <- read.csv("/Users/med-tv_/Documents/Projects/DPP_NMR_lipidomics/DPPcsv.csv", header = F, sep = ";")
corr_matrix <- Matrix(as.matrix(x))
pd_corr_matrix <- nearPD(corr_matrix, keepDiag=T, conv.tol = 1e-7, corr=T)
M <- ncol(pd_corr_matrix$mat)
mu <- rep(0,M)
stddev <- rep(1,M)
covMat <- stddev %*% t(stddev) * pd_corr_matrix$mat
X_hat <- mvrnorm(n=N, mu=mu, Sigma=covMat, empirical=TRUE) # Simulated values
#compare correlation coefficients:
corr_matrix[1:5,1:5]
cor(X_hat)[1:5,1:5]

#PIPELINE USING REAL DATASET WITH MISSINGNESS
df <- read.delim("/Volumes/External/LOCUS/pheno/LOCUS_pheno.txt", stringsAsFactors = F)
N <- nrow(df)
df <- df[,!sapply(df, is.character)]
df$patid <- NULL
df$matid <- NULL
df_miss <- prodNA(df, noNA = 0.2)
x <- cor(df_miss, use = "pairwise.complete.obs")
corr_matrix <- Matrix(as.matrix(x))
pd_corr_matrix <- nearPD(corr_matrix, keepDiag=T, conv.tol = 1e-7, corr=T)
M <- ncol(pd_corr_matrix$mat)
mu <- rep(0,M)
stddev <- rep(1,M)
covMat <- stddev %*% t(stddev) * pd_corr_matrix$mat
X_hat <- mvrnorm(n=N, mu=mu, Sigma=covMat, empirical=TRUE) # Simulated values
cor(X_hat)
#compare correlation coefficients:
corr_matrix[1:5,1:5]
cor(X_hat)[1:5,1:5]




