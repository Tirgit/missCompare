





###PACKAGES



#FUNCTION


median_imp <- function(X) {
  for(i in 1:ncol(X)) {
    X[is.na(X[,i]), i] <- median(X[,i], na.rm = TRUE)
  }
  
  list(Imputed = X)
}

results <- lapply(res, median_imp)




#LAB
res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable)




median(res$MCAR_matrix[,1], na.rm = TRUE)

for(i in 1:ncol(res$MCAR_matrix)) {
  res$MCAR_matrix[is.na(res$MCAR_matrix[,i]), i] <- median(res$MCAR_matrix[,i], na.rm = TRUE)
}

results <- lapply(res, median_imp)


#using NA index to identify the original values (later set to missing)
original <- yy$Simulated_matrix[is.na(res$MCAR_matrix)]

#using NA index to identify the imputed values
imputed <- results$MCAR_matrix$Imputed[is.na(res$MCAR_matrix)]
error <- original-imputed

rmse <_ sqrt(mean(error^2))




lapply(res, median_imp)










###Simulation, MEDIAN imputation
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  for(i in 1:ncol(real_matrix_miss)){
    res$MCAR_matrix[is.na(res$MCAR_matrix[,1]), 1] <- median(real_matrix_miss[,i], na.rm = TRUE)
  }
  imp_matrix <- real_matrix_miss
  n <- cor(c(X_hat), c(imp_matrix))
  ns <- c(ns, n) 
  count <- count+1
  if(count > numsim) {
    break
  }
}
meanvalue <- mean(ns)
sevalue <- sd(ns)/sqrt(length(ns))
values <- c(meanvalue,sevalue)
outputvalues[nrow(outputvalues) + 1, ] <- values