#################################################################################################################################################
#
#This function uses the generated simulated matrix and generates missing datapoints in MAR (missing at random) pattern per variable.
#This is achieved by sorting each columns and removing the top/bottom of the data (according to the original missingness).
#Selection from top or bottom is random.
#
#Inputs are:
#
#simulated matrix
#missing fraction per variable
#
#The function outputs a matrix with missing values and a summary of the matrix.
#
#################################################################################################################################################

###PACKAGES
library(missForest)


###FUNCTION
dimple_MAR <- function(X_hat, missfrac_per_var) {
  
  logi <- sample(0:1, length(missfrac_per_var), replace = T)
  
  for (i in 1:length(missfrac_per_var)) {
    if (logi[i]==1) X_hat <- X_hat[order(X_hat[,i], decreasing = T),] else X_hat <- X_hat[order(X_hat[,i], decreasing = F),]
    threshold_for_excl <- X_hat[,i][ceiling(missfrac_per_var[i]*nrow(X_hat))]
    if (logi[i]==1) X_hat[,i] <- ifelse(X_hat[,i]>threshold_for_excl, NA, X_hat[,i]) else X_hat[,i] <- ifelse(X_hat[,i]<threshold_for_excl, NA, X_hat[,i]) 
  }
  
  matrix_summary <- summary(X_hat)
  
  list(MAR_matrix = X_hat, Summary = matrix_summary)
  
}













###LAB
missfrac_per_var <- y$Fraction_missingness_per_variable
X_hat <- yy$Simulated_matrix


if (logi[1]==0) X_hat <- X_hat[order(X_hat[,1], decreasing = T),] else X_hat <- X_hat[order(X_hat[,1], decreasing = F),]
threshold_for_excl <- X_hat[,1][ceiling(missfrac_per_var[1]*nrow(X_hat))]
X_hat[,1] <- ifelse(X_hat[,1]>threshold_for_excl, NA, X_hat[,1]) 



X_hat[,1]
sum(is.na(X_hat[,1]))

View(cbind(id, week, cond, base, y, y.nmar))


sample(0:1, 1)

sort(as.matrix(X_hat[,1]))


for (i in 1:length(missfrac_per_var)) {
  X_hat[,i] <- prodNA(as.matrix(X_hat[,i]), noNA = missfrac_per_var[i])
}

summary(X_hat)



res <- dimple_MNAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)





ni     = 100  # 100 people
nj     =  10  # 10 week study
id     = rep(1:ni, each=nj)
cond   = rep(c("control", "diet"), each=nj*(ni/2))
base   = round(rep(rnorm(ni, mean=250, sd=10), each=nj))
week   = rep(1:nj, times=ni)
y      = round(base + rnorm(ni*nj, mean=0, sd=1))
prop.m = .07

sort.y = sort(y, decreasing=TRUE)
nmar   = sort.y[ceiling(prop.m*length(y))]
y.nmar = ifelse(y>nmar, NA, y)  # doesn't show up when heavier
View(cbind(id, week, cond, base, y, y.nmar))





