

dimple_post_imp() <- function(x) {
  
  
  
}


finalLE$name <- 'imputed_LE'
trueLE$name <- 'real_LE'
histDataLE <- rbind(finalLE, trueLE)

finalNP$name <- 'imputed_NP'
trueNP$name <- 'real_NP'
histDataNP <- rbind(finalNP, trueNP)


ggplot(histDataLE, aes(values, fill = name)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth = 0.05, position = 'identity')

ggplot(histDataNP, aes(values, fill = name)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth = 0.2, position = 'identity')