rm(list=ls())
library(ISLR)
library(MASS)
library(class)
library(glmnet)
library(boot)
####################
#### QUESTION 1 #### 
####################
weekly <- Weekly
# a) Set the random seed to 5072
set.seed(5072)
# b) Use the full data set to perform a logistic regression
lm.model <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = binomial, data = weekly)
summary(lm.model)
# c) Create and display a confusion matrix
lm.probs <- predict(lm.model, type="response")
lm.pred = rep("Down", length(lm.probs))
lm.pred[lm.probs > 0.5] = 'Up'
mytable <- table(weekly$Direction, lm.pred)
mytable
# d) From the confusion matrix, compute and display the following performance statistics:
  # The overall fraction of correct predictions.
mean(lm.pred == weekly$Direction)
  # The overall error rate
mean(lm.pred != weekly$Direction)
  # Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
  # The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
  # The Precision of the model
precision <- mytable["Up", "Up"] / sum(mytable[, "Up"])
precision

# e) Fit a logistic regression model using a training data period from 1990 to 2008
weekly.train <- weekly[weekly$Year <= 2008,]
weekly.test <- weekly[weekly$Year > 2008,]
lm.model <- glm(Direction~Lag2, family = binomial, data = weekly.train)
lm.probs <- predict(lm.model, type="response", newdata = weekly.test)
lm.pred = rep("Down", length(lm.probs))
lm.pred[lm.probs > 0.5] = 'Up'
mytable <- table(weekly.test$Direction, lm.pred)
mytable
# f) compute the same five performance statistics
  # The overall fraction of correct predictions.
mean(lm.pred == weekly.test$Direction)
  # The overall error rate
mean(lm.pred != weekly.test$Direction)
  # Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
  # The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
  # The Precision of the model
precision <- mytable["Up", "Up"] / sum(mytable[, "Up"])
precision

# g) Repeat e) and f) using LDA.
lda.fit=lda(Direction ~ Lag2, data = weekly.train)
lda.pred = predict(lda.fit, weekly.test)
mytable <- table(weekly.test$Direction, lda.pred$class)
mytable
  # The overall fraction of correct predictions.
mean(lda.pred$class == weekly.test$Direction)
  # The overall error rate
mean(lda.pred$class != weekly.test$Direction)
  # Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
  # The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
  # The Precision of the model
precision <- mytable["Up", "Up"] / sum(mytable[, "Up"])
precision

# h) Repeat e) and f) using QDA.
qda.fit = qda(Direction ~ Lag2, data = weekly.train)
qda.pred = predict(qda.fit, weekly.test)
mytable <- table(weekly.test$Direction, qda.pred$class)
mytable
  # The overall fraction of correct predictions.
mean(qda.pred$class == weekly.test$Direction)
  # The overall error rate
mean(qda.pred$class != weekly.test$Direction)
  # Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
  # The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
  # The Precision of the model
precision <- mytable["Up", "Up"] / sum(mytable[, "Up"])
precision

# i) Repeat e) and f) using KNN with k = 1.
train.x <- data.frame(weekly.train$Lag2)
train.y <- weekly.train$Direction
test.x <- data.frame(weekly.test$Lag2)
test.y <- weekly.test$Direction
knn.pred <- knn(train = train.x, test = test.x, cl = train.y, k = 1)
mytable <- table(test.y, knn.pred)
mytable

  # The overall fraction of correct predictions.
mean(knn.pred == test.y)
  # The overall error rate
mean(knn.pred != test.y)
  # Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
  # The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
  # The Precision of the model
precision <- mytable["Up", "Up"] / sum(mytable[, "Up"])
precision

# j) Repeat e) and f) using KNN with k = 5.
knn.pred <- knn(train = train.x, test = test.x, cl = train.y, k = 5)
mytable <- table(test.y, knn.pred)
mytable
  # The overall fraction of correct predictions.
mean(knn.pred == test.y)
  # The overall error rate
mean(knn.pred != test.y)
  # Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
  # The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
  # The Precision of the model
precision <- mytable["Up", "Up"] / sum(mytable[, "Up"])
precision

# k) which of these methods appears to provide the best results on this data?
'According to the confusion matrixces, logistc regression model and LDA both '
'provide the best results becuse their overall error rates are both the smallest.'

####################
#### QUESTION 2 #### 
####################
# a. Set the random seed to 5072.
rm(list=ls())
set.seed(5072)

# b. Create a binary variable
auto <- Auto
mpg01 <- rep(0, nrow(auto))
mpg01[auto$mpg > median(auto$mpg)] <- 1
auto1 <- data.frame(mpg01, auto[,-1])

# c. Split the data into a training set and a test set using the sample() function as usual.
n <- nrow(auto1)
trainprop <- 0.8
train <- sample(n, trainprop*n)
trainset <- auto1[train,]
testset <- auto1[-train,]

# d. Perform logistic regression on the training data
glm.model <- glm(mpg01~cylinders+displacement+weight, family = binomial, data = trainset)
glm.probs <- predict(glm.model, type="response", newdata = testset)
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mytable <- table(testset$mpg01, glm.pred)
mytable

# e. For the test set, use the model just created to construct and display a confusion matrix in the format outlined in Question 1
# The overall fraction of correct predictions.
mean(glm.pred == testset$mpg01)
# The overall error rate
mean(glm.pred != testset$mpg01)
# Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
# The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
# The Precision of the model
precision <- mytable[2, 2] / sum(mytable[, 2])
precision

# f. Repeat d) and e) using LDA.
lda.fit=lda(mpg01~cylinders+displacement+weight, data = trainset)
lda.pred = predict(lda.fit, testset)
mytable <- table(testset$mpg01, lda.pred$class)
mytable
# The overall fraction of correct predictions.
mean(lda.pred$class == testset$mpg01)
# The overall error rate
mean(lda.pred$class != testset$mpg01)
# Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
# The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
# The Precision of the model
precision <- mytable[2, 2] / sum(mytable[, 2])
precision

# g. Repeat d) and e) using QDA.
qda.fit=qda(mpg01~cylinders+displacement+weight, data = trainset)
qda.pred = predict(qda.fit, testset)
mytable <- table(testset$mpg01, qda.pred$class)
# The overall fraction of correct predictions.
mean(qda.pred$class == testset$mpg01)
# The overall error rate
mean(qda.pred$class != testset$mpg01)
# Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
# The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
# The Precision of the model
precision <- mytable[2, 2] / sum(mytable[, 2])
precision

# h. Repeat d) and e) using KNN with k = 1.
train.x <- data.frame(trainset$cylinders, trainset$displacement, trainset$weight)
test.x <- data.frame(testset$cylinders, testset$displacement, testset$weight)
train.y <- trainset$mpg01
test.y <- testset$mpg01
knn.pred <- knn(train = train.x, test = test.x, cl = train.y, k = 1)
mytable <- table(knn.pred, test.y)
mytable

  # The overall fraction of correct predictions.
mean(knn.pred == test.y)
  # The overall error rate
mean(knn.pred != test.y)
  # Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
  # The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
specificityTrueNegative <- mytable[1, 1] / sum(mytable[1, ])
specificityTrueNegative
  # The Precision of the model
precision <- mytable[2, 2] / sum(mytable[, 2])
precision

# i. Repeat d) and e) using KNN, with various values of k. Choose the model that performs best.
k <- 1:5
mse <- rep(0, 5)
for (i in k){
  knn.pred <- knn(train = train.x, test = test.x, cl = train.y, k = i)
  mse[i] <- mean(knn.pred != test.y)
}
best.k = which.min(mse)
'The best k is 3'

# j. Based on the confusion matrices, which of these methods appears to provide the best results on this data?
'According to the confusion matrixces, QDA model provides '
'the best results becuse its overall error rate is the smallest.'

####################
#### QUESTION 3 #### 
####################
# a. Set the random seed to 5072.
rm(list=ls())
set.seed(5072)

# b. Create a binary variable
boston <- Boston
crim01 <- rep(0, nrow(boston))
crim01[boston$crim > median(boston$crim)] <- 1
boston1 <- data.frame(crim01, boston[,-1])

# c. Split the data into a training set and a test set using the sample() function as usual.
n <- nrow(boston1)
trainprop <- 0.8
train <- sample(n, trainprop*n)
trainset <- boston1[train,]
testset <- boston1[-train,]

# d. Perform logistic regression on the training data
lm.model <- glm(crim01~nox+rad+dis, family = binomial, data = trainset)
lm.probs <- predict(lm.model, type="response", newdata = testset)
lm.pred = rep(0, length(lm.probs))
lm.pred[lm.probs > 0.5] = 1
mytable <- table(testset$crim01, lm.pred)
mytable

# e. For the test set, use the model just created to construct and display a confusion matrix in the format outlined in Question 1
# The overall fraction of correct predictions.
mean(lm.pred == testset$crim01)
# The overall error rate
mean(lm.pred != testset$crim01)
# Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
# The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
# The Precision of the model
precision <- mytable[2, 2] / sum(mytable[, 2])
precision

# f. Repeat d) and e) using LDA.
lda.fit=lda(crim01~nox+rad+dis, data = trainset)
lda.pred = predict(lda.fit, testset)
mytable <- table(testset$crim01, lda.pred$class)
mytable
# The overall fraction of correct predictions.
mean(lda.pred$class == testset$crim01)
# The overall error rate
mean(lda.pred$class != testset$crim01)
# Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
# The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
# The Precision of the model
precision <- mytable[2, 2] / sum(mytable[, 2])
precision

# h. Repeat d) and e) using KNN.
train.x <- data.frame(trainset$nox, trainset$rad, trainset$dis)
test.x <- data.frame(testset$nox, testset$rad, testset$dis)
train.y <- trainset$crim01
test.y <- testset$crim01
# Find the best k
k <- 1:10
mse <- rep(0, 10)
for (i in k){
  knn.pred <- knn(train = train.x, test = test.x, cl = train.y, k = i)
  mse[i] <- mean(knn.pred != test.y)
}
best.k = which.min(mse)
knn.pred <- knn(train = train.x, test = test.x, cl = train.y, k = best.k)
mytable <- table(test.y, knn.pred)
mytable
# The overall fraction of correct predictions.
mean(knn.pred == test.y)
# The overall error rate
mean(knn.pred != test.y)
# Type I and Type II error rates
type1ErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1ErrorRate
type2ErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2ErrorRate
# The Power of the model
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
# The Precision of the model
precision <- mytable[2, 2] / sum(mytable[, 2])
precision


####################
#### QUESTION 4 #### 
####################
rm(list=ls())
# a) Generate a simulated data set as follows:
set.seed(5072)
x=rnorm(100)
y = x - 2 * x^2 + rnorm(100)

# b) Create a data frame containing these x and y variables in named columns X and Y
mydata <- data.frame(x, y)
names(mydata) <- c('X', 'Y')

# c) Create a scatterplot of X against Y.
plot(x = x, y = y)

# d) Set the random seed to 123, then compute the LOOCV errors
set.seed(123)
cv.error <- rep(0, 4)
for (i in 1:4){
  glm.fit <- glm(Y~poly(X,i), data=mydata)
  cv.error[i] <- cv.glm(mydata, glm.fit)$delta[1]
}
cv.error

# e) Repeat d) using random seed 456 and report your LOOCV errors as above
set.seed(456)
for (i in 1:4){
  glm.fit <- glm(Y~poly(X,i), data=mydata)
  cv.error[i] <- cv.glm(mydata, glm.fit)$delta[1]
}
cv.error
'Yes, the results are the same. Because there is no randomness in the training set splits'

# f) Which of the models in (d) had the smallest LOOCV error? 
#    Is this what you expected? Explain your answer.
'The modle ii has the smallest LOOCV error.'
'Yes the result is also what I expected. By observing the scatter plot of X and Y,'
'I think the model would be very similar with quadratic formula. So I expect model ii'
'is the best model for the dataset.'

# g) Comment on the statistical significance of the coefficient estimates (citing p-values) that 
#    results from fitting each of the models in d) using least squares. 
#    Do these results agree with the conclusions drawn based on the cross-validation results?
lm.mse <- rep(0, 4)
for (i in 1:4){
  lm.fit <- lm(Y~poly(X, i), data = mydata)
  lm.mse[i] <- mean((predict(lm.fit, type='response', newdata = mydata) - mydata$Y)^2)
}
summary(lm.fit)
lm.mse
'Yes these results agree with the conclusions! Because in the summary shows us that the x^1 and x^2 are significant'