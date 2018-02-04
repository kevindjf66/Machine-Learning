rm(list=ls())
# Problem 1
# a). Set the random seed to 5072
set.seed(5072)

# b). Create vector x
x <- rnorm(n = 100, mean = 0, sd = 1)

# c). Create vector eps
eps <- rnorm(n = 100, mean = 0, sd = 0.5)

#     Create linear model
y <- 0.5*x - 1 + eps

# d). Display length of y
length(y)

# e). Beta is -1 and beta1 is 0.5

# f). Create a scatterplot displaying the realtionship between x and y
plot(x, y)

mean(y)
var(y)
# g). The relationship is positive, high linearity and mean of y is -0.929, the variance of y is 0.461 

# h). Fit a least squares linear 
lm1 <- lm(y~x)
coef(lm1)

# i). beta2 is -1.0029820 and beta3 is 0.4352255
#     They are almost the same with the beta0 and beta1

# j). Display the least squares line on the scatterplot
abline(lm1, lwd = 1, col = "black")
# k). Display the population line on the scatterplot
abline(a = -1, b = 0.5, lwd = 1, col = "red")
legend('bottomright', c('Least Squares', 'Population'), col = c('black', 'red'), lty = 1)

# m). Fit a polynomial regression model that predicts y using x and x^2
lm2 <- lm(y ~ x + I(x^2))
anova(lm2, lm1)

# n).
anova(lm2, lm1)$F[2]
#Because the F value of the model is 0.3277, so this model is not better than the previous one.

# o). Changing the variance 0.25 to 1 of the model
eps <- rnorm(n = 100, mean = 0, sd = 0.1^0.5)
y <- 0.5*x - 1 + eps
plot(x, y)
lm3 <- lm(y~x)
abline(a = -1, b = 0.5, lwd = 1, col = "red")
abline(lm3, lwd = 1, col = "black")

# p). Changing the variance from 0.25 to 0.5 of the model
eps <- rnorm(n = 100, mean = 0, sd = 0.5^0.5)
y <- 0.5*x - 1 + eps
plot(x, y)
lm4 <- lm(y~x)
abline(a = -1, b = 0.5, lwd = 1, col = "red")
abline(lm4, lwd = 1, col = "black")

# q).
# From my observation, the best model is the lm2, the average level is lm1, and the worst model is lm3.

# r).
# Display the 95% confidence intervals for beta0 and ??1 based on the original data set
confint(lm1, level=.95)
confint(lm3, level=.95)
confint(lm4, level=.95)
# s).
# Acording to observations, the bigger is eps's variance, the wider is the confidence interval

#Problem 2
# a). 
rm(list = ls())
set.seed (5072)
x1 = runif (100)
x2 = 0.5 * x1 + rnorm (100) /10
y = 2 + 2* x1 + 0.3* x2 + rnorm (100)

# c). beta0 is 2, beta1 is 2, and beta2 is 0.3
cor(y, x1)
cor(y, x2)
cor(x1, x2)

# d). 
pairs(data.frame(y, x1, x2))

# e).
#    The correlation between y and x1 is 0.510176
#    The correlation between y and x2 is 0.4087993
#    The correlation between x1 and x2 is 0.58442246

# f). Fit a least squares regression model
lm.fit.both <- lm(y ~ x1 + x2)
coef(lm.fit.both)
# g). The beta0 is 2.0408539, beta1 is 2.3410129, and beta2 is -0.4961621

# h). Among the three betas, beta0 and beta 1 has more significant influence on y but beta2 only has little influence on y.

# i). Yes, I can reject the null hypothesis H0.
#     Because the p-values are still pretty small.

# j). Fit a least squares regression model called lm.fit.justx1 to predict y using only x1
lm.fit.justx1 <- lm(y ~ x1)
summary(lm.fit.both)
summary(lm.fit.justx1)
# k). The model lm.fit.justx1 is better because the adjusted R-squared is bigger.
#     Yes, I can reject H0 because p-value is small.

# l). Fit a least squares regression model called lm.fit.justx2 to predict y using only x2
lm.fit.justx2 <- lm(y ~ x2)
summary(lm.fit.justx2)
# m). The model lm.fit.justx2 is worser than lm.fit.both because the adjusted R-squared is smaller.

# n). 
#   No, it is not contradicted because their R-squared values are still pretty small and their p-values are also small

# o). Add points to x1, x2 and y
x1=c(x1 , 0.1)
x2=c(x2 , 0.8)
y=c(y,6)

# p). Re-fit the linear models from f)-m) using this new data
lm.fit.both1 <- lm(y ~ x1 + x2)
summary(lm.fit.both1)
lm.fit.justx11 <- lm(y ~ x1)
summary(lm.fit.justx11)
lm.fit.justx21 <- lm(y ~ x2)
summary(lm.fit.justx21)
#     The new observation makes the model lm.just.x1 worse but makes the other two models better.

# r).
#    Set the graphics window to 2x2
par(mfrow=c(2,2))
plot(lm.fit.both1)
#    For model lm.fit.both1, we can see that the point 101 is outside of the clulster of other poionts
#    which means that the point 101 is an outliner. Also, the point 101 cook distance is greater
#    than 1, so the point 101 is both of outliner and high-leverage point.
plot(lm.fit.justx11)
#    For model lm.fit.justx11, we can see that the point 101 is outside of the clulster of other poionts
#    which means that the point 101 is an outliner. Also, the point 101 cook distance is less
#    than 1, so the point 101 is an outliner but not a high-leverage point.
plot(lm.fit.justx21)
#    For model lm.fit.justx21, we can see that the point 101 is inside of the clulster of all poionts
#    which means that the point 101 is not an outliner. Also, the point 101 cook distance is less
#    than 1, so the point 101 is neither an outliner or a high-leverage point.

#    Reset the graphics window to 1x1 
par(mfrow=c(1,1))

#Problem 3
rm(list = ls())
# a). 
library(MASS)
set.seed(5072)
ncol(Boston)
#   Build some empty vectors to save data such as beta later
singlebeta0 <- rep(0, 12)
singlebeta1 <- rep(0, 12)
fstatistic <- rep(0, 12)
pvalue <- rep(0,12)
#   Delete the column crim and column chas
Boston1 <- Boston[,-4]
Boston2 <- Boston1[,-1]
predictor <- colnames(Boston2)
# c). Change the graphics window to a 4x3 grid
par(mfrow=c(3,4))
#   Design a for loop to draw all the plots and save all beta0 and beta1
for (i in 1:ncol(Boston2)){
  x <- Boston2[, i]
  lm.fit1 <- lm(Boston$crim ~ x)
  fstatistic[i] <- summary(lm.fit1)$fstatistic[[1]]
  pvalue[i] <- anova(lm.fit1)$'Pr(>F)'[1]
  # c). Draw the plots
  plot(x = x, y = Boston$crim, main = colnames(Boston2[i]))
  abline(lm.fit1, lwd = 1, col = 'red')
  singlebeta0[i] <- coef(lm.fit1)[1]
  singlebeta1[i] <- coef(lm.fit1)[2]
}
#   Display these values in a table (one row for each predictor)
predtable1 <- data.frame(predictor, fstatistic, pvalue, singlebeta0, singlebeta1)
print (predtable1)
# b). 
#    At alpha = 0.05 level, all variables' p-value is less than 0.05, 
#    so every model has significant association between predictor and the response.

# c). Reset the graphics window to 1*1
par(mfrow=c(1,1))

# d). Use all variables to build a new model
fit.all <- lm(crim ~ ., data = Boston1)
allbeta1 <- coef(summary(fit.all))

# e). 
#    Display only those predictors which are significant at a level of alpha = 0.05
for (i in 2:13){
  if (allbeta1[,4][[i]] < 0.05){
    print (row.names(allbeta1)[i])
  }
}
#    As a result, the predictors which is significant at a level of alpha = 0.5 are: zn, nox, dis, rad, black and medv

# f). 
#    Create a plot displaying the univariate regression coefficients
plot(x = singlebeta1, y = allbeta1, xlab = 'Simple', ylab = 'Multiple')

#    Simple regression approaches are more accurate because from the plot, we can see that each variable has less
#    contribution to the model under the multiple situation. However, when it comes to simple linear model, every variable
#    seems to have greater contribution to model.

# g). For each predictor X, fit a polynomial model
fstat <- rep(0, 12)
pvalueofFstat <- rep(0, 12)
for (i in 1:ncol(Boston2)){
  x <- Boston2[, i]
  lm.fit1 <- lm(Boston$crim ~ x)
  lm.fit2 <- lm(Boston$crim ~ poly(x, 3))
  fstat[i] <- anova(lm.fit1, lm.fit2)$F[2]
  pvalueofFstat[i] <- anova(lm.fit1, lm.fit2)$'Pr(>F)'[2]
}
#     Display the table
predtable2 <- data.frame(predictor, fstat, sort(pvalueofFstat))
print (predtable2)

#     For predictors medv, dis, nox, indus, age, tax, ptratio, rm, zn, rad and lstat, each of these predictors' p-value 
#     is smaller than 0.05, so we could say that we can reject the null hypothesis for all thses predictors.
