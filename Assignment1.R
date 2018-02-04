rm(list = ls())
#install and load the package we need to answer following questions
library("ISLR")
library("FNN")

#####################
#### QUESTION 1 #####
#####################

#read the table
homeprice = read.table("HomePrices.txt", sep = '\t', header = T)
summary(homeprice)

#calculate MSE and variance of medv
n = nrow(homeprice)
MSE <- mean((homeprice$medv - mean(homeprice$medv))^2)
var(homeprice$medv)*((n - 1) / n)

#build the scale data frame
homeprice_sca = scale(homeprice[-13])
homeprice2 <- cbind(homeprice_sca, homeprice[13])
head(homeprice2, 6)

#set seed 5072
set.seed(5072)

#create training, validate and test data set
trainprop <- 0.75
validateprop <- 0.15
train <- sample(n, trainprop * n)
validate <- sample(setdiff(1:n, train), validateprop * n)
test <- setdiff(setdiff(1:n, train), validate)
trainset <- homeprice2[train,]
validateset <- homeprice2[validate,]
testset <- homeprice2[test,]
head(trainset, 1)
head(validateset, 1)
head(testset, 1)

#create following 6 data frames
train.x <- trainset[-13]
train.y <- trainset$medv
validate.x <- validateset[-13]
validate.y <- validateset$medv
test.x <- testset[-13]
test.y <- testset$medv

#Construct 10 models with k equals 19, 17, ... 3, 1
nums <- c(seq(19, 1, -2))
validate.mses <- rep(0, 10)
train.mses <- rep(0, 10)
for (i in 1:10) {
  knn.pred <- knn.reg(train.x, validate.x, train.y, k = nums[i])
  validate.mses[i] <- mean((knn.pred$pred - validate.y)^2)
  
  knn.pred <- knn.reg(train.x, train.x, train.y, k = nums[i])
  train.mses[i] <- mean((knn.pred$pred - train.y)^2)
}
#plot the training and validate MSE
plot(NULL, NULL, type='n', xlim=c(19, 1), ylim=c(0,max(c(validate.mses, train.mses))), xlab='Increasing Flexibility (Decreasing k)', ylab='MSE', main='MSEs as a Function of \n Flexibility for KNN Classification')
lines(nums, validate.mses[1:length(validate.mses)], type='b', col=2, pch=16)
lines(nums, train.mses[1:length(train.mses)], type='b', col=1, pch=16)
legend("topright", legend = c("Validate MSES", "Train MSES"), col=c(2, 1), cex=.75, pch=16)
minvalidatemse <- nums[which.min(validate.mses)]

#print out the minimum mse and its k
print(paste('My best validate MSE occurred with k =', nums[which.min(validate.mses)], 'and produced a valid MSE of', validate.mses[which.min(validate.mses)]))

#Predict medv for the test set
knn.pred <- knn.reg(train.x, test.x, train.y, k = nums[which.min(validate.mses)])
testmse <- mean((knn.pred$pred - test.y)^2)
print(paste('The test mse is:', testmse))

#####################
#### QUESTION 2 #####
#####################
rm(list=ls())
#read the file
loandata = read.csv("LoanData.csv", header = T)
error.rate = sum(loandata$loan.repaid == 'No') / nrow(loandata)
print(paste("The error rate result from always predicting Yes is:", error.rate))
loandata$loan.repaid<-ifelse(loandata$loan.repaid=='Yes', 1,0)
#Scale the data frame
loan_sca = scale(loandata[-8])
loan = cbind(loan_sca, loandata[8])
#set seed
set.seed(5072)
#Create training, validate and test data frames
n = nrow(loan)
trainprop <- 0.75
validateprop <- 0.15
train <- sample(n, trainprop * n)
validate <- sample(setdiff(1:n, train), validateprop * n)
test <- setdiff(setdiff(1:n, train), validate)
trainset <- loan[train,]
validateset <- loan[validate,]
testset <- loan[test,]
head(trainset, 1)
head(validateset, 1)
head(testset, 1)

#create following 6 data frames
train.x <- trainset[-8]
train.y <- trainset$loan.repaid
validate.x <- validateset[-8]
validate.y <- validateset$loan.repaid
test.x <- testset[-8]
test.y <- testset$loan.repaid

#Construct 10 models with k equals 19, 17, ... 3, 1
nums <- seq(1, 19, 2)
validate.errors <- rep(0, 10)
train.errors <- rep(0, 10)
for(i in 1:10) {
  knn.pred <- knn(train.x, validate.x, train.y, k = nums[i])
  validate.errors[i] <- mean(validate.y != knn.pred)
  
  knn.pred <- knn(train.x, train.x, train.y, k = nums[i])
  train.errors[i] <- mean(train.y != knn.pred)    
}
print(paste('My best validate error occurred with k =', nums[which.min(validate.errors)], 'and produced a valid error of', validate.errors[which.min(validate.errors)]))

#Draw the plot
plot(NULL, NULL, type='n', xlim=c(19, 1), ylim=c(0,max(c(validate.errors, train.errors))), xlab='Increasing Flexibility (Decreasing k)', ylab='Error Rates', main='Error Rates as a Function of \n Flexibility for KNN Classification')
lines(nums, validate.errors[1:length(validate.errors)], type='b', col=2, pch=16)
lines(nums, train.errors[1:length(train.errors)], type='b', col=1, pch=16)
legend("topleft", legend = c("Validation Error Rate", "Train Error Rate"), col=c(2, 1), cex=.75, pch=16)

#predict the loan.repaid for test set
knn.pred <- knn(train.x, test.x,  train.y, k = nums[which.min(validate.errors)])
test.error <- mean(test.y != knn.pred)
print(paste('The test error is:', test.error))

#####################
#### QUESTION 3 #####
#####################
rm(list=ls())
#read the table
homeprice = read.table("HomePrices.txt", header = T)

#build the scale data frame
homeprice_sca = scale(homeprice[-13])
homeprice2 <- cbind(homeprice_sca, homeprice[13])

#set seed 5072
set.seed(5072)
trainprop <- 0.75
validateprop <- 0.15
n = nrow(homeprice)
nums <- c(19, 17, 15, 13, 11, 9, 7, 5, 3, 1)

#repeat 50 times
validate.mses <- rep(0, 10)
validate.mses1 <- rep(0, 50)
test.mses <- rep(0, 50)
for (i in 1:50) {
  #create training, validate and test data set
  train <- sample(n, trainprop * n)
  validate <- sample(setdiff(1:n, train), validateprop * n)
  test <- setdiff(setdiff(1:n, train), validate)
  trainset <- homeprice2[train,]
  validateset <- homeprice2[validate,]
  testset <- homeprice2[test,]
  
  #create following 6 data frames
  train.x <- trainset[-13]
  train.y <- trainset$medv
  validate.x <- validateset[-13]
  validate.y <- validateset$medv
  test.x <- testset[-13]
  test.y <- testset$medv
  
  #find the best k 
  for (j in 1:10){
    knn.pred <- knn.reg(train.x, validate.x, train.y, k = nums[j])
    validate.mses[j] <- mean((knn.pred$pred - validate.y)^2)
  }
  validate.mses1[i] <- min(validate.mses)
  #calculate the test mses
  knn.pred <- knn.reg(train.x, test.x, train.y, k = nums[which.min(validate.mses)])
  test.mses[i] <- mean((knn.pred$pred - test.y)^2)
}
print(paste('Mean validate MSE:', mean(validate.mses1)))
print(paste('Sd validate MSE: ', sd(validate.mses1)))
print(paste('Mean test MSE:', mean(test.mses)))
print(paste('Sd test MSE:', sd(test.mses)))

#Draw the plot
plot(NULL, NULL, type='n', xlim=c(0, 50), ylim=c(0,max(c(validate.mses, test.mses))), xlab='Replication', ylab='MSEs', main='Test and Best Validation MSEs for Many Partitionings of the Data')
lines(1:50, test.mses[1:50], type='b', col=1, pch=16)
lines(1:50, validate.mses1[1:50], type='b', col=2, pch=16)
lines(1:50, rep(mean(validate.mses1), 50), lty = 2, lwd = 3, col=2)
lines(1:50, rep(mean(test.mses), 50), lty = 2, lwd = 3, col=1)
legend("topright", legend = c("Validation MSEs", "Test MSEs", "Validate MSEs mean", "Test MSEs mean"), lty = c(1, 1, 2, 2), col=c(2, 1), cex=.75, pch=16)

#From the plot we can see that for most time, the test mse has big differences with the validate mse. So I think we are not unlucky in Question 1.

#####################
#### QUESTION 4 #####
#####################
rm(list = ls())
#read the file
collegeinfo = read.csv("applications.train.csv", header = T)

#set seed
set.seed(5072)

#Create training, validate and test data frames
n = nrow(collegeinfo)
trainprop <- 0.7
validateprop <- 0.2
train <- sample(n, trainprop * n)
validate <- sample(setdiff(1:n, train), validateprop * n)
test <- setdiff(setdiff(1:n, train), validate)
trainset <- collegeinfo[train,]
validateset <- collegeinfo[validate,]
testset <- collegeinfo[test,]

#Create following 6 data frames
#After trying many times, I believe the columns of Spending On students, Accepted and Top 10% Percent are the most relevant data
train.x <- trainset[c(6, 10, 15)]
train.y <- trainset$Applications
validate.x <- validateset[c(6, 10, 15)]
validate.y <- validateset$Applications
test.x <- testset[c(6, 10, 15)]
test.y <- testset$Applications

#find the best k
numreps <- 100
validate.mses <- rep(0, numreps)

for(i in 1:numreps) {
  knn.pred <- knn.reg(train.x, validate.x, train.y, k = i)
  validate.mses[i] <- mean((validate.y - knn.pred$pred)^2)
}
#Use the best k for the test dataset:
knn.pred <- knn.reg(train.x, test.x, train.y, k = which.min(validate.mses))
test.mses <- mean((test.y - knn.pred$pred)^2)
print(paste("The minimum test mse is:", min(test.mses)))
##So, the best k should be:
print(paste("The best k = ", which.min(validate.mses)))

