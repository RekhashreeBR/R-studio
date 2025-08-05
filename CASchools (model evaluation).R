# ols estimator
attach(CASchools)
beta_1 <- sum((STR - mean(STR)) * (score - mean(score))) / sum((STR - mean(STR))^2) 
beta_0 <- mean(score) - beta_1 * mean(STR) 
beta_1 
beta_0 
 
#linear model
linear_model <- lm(score ~ STR, data = CASchools)
linear_model

#scatter plot od lm

plot(score ~ STR, 
     data = CASchools,
     main = "Scatterplot of Test Score and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)",
     xlim = c(10, 30),
     ylim = c(600, 720))
abline(linear_model) 

n<- nrow(CASchools)
#std error of regression
SER <- sqrt(SSR / (n-2))
#Application to the Test Score Data
mod_summary <- summary(linear_model)
mod_summary

# compute R^2 manually 
SSR <- sum(mod_summary$residuals^2) 
TSS <- sum((score - mean(score))^2) 
R2 <- 1 - SSR/TSS 
R2 

# compute SER manually 
n <- nrow(CASchools)
SER <- sqrt(SSR / (n-2)) 
SER

