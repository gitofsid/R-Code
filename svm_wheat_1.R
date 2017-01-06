#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(rpart)
library(rpart.plot)

wheatData <- read.table("wheat.csv", header=TRUE, sep=",", na.strings=" ")
head(wheatData)

set.seed(67982193)
wheatData$classnum <- as.numeric(wheatData$class)
perm <- sample(x=nrow(wheatData))
set1.wheat <- wheatData[which(perm <= 200), -1]
set2.wheat <- wheatData[which(perm > 200), -1]

# For the Wheat data, model on the training set and test final errors of the test set:
#   (a) Use SVM with Gaussian radial basis function kernel to classify these data. Tune
# the predictor using CV (explain what you tried) and report the test error.
# (b) Repeat using polynomial kernel.
# (c) Compare the results to the previous results

wheat.svm.tune.rad <- tune.svm(data=set1.wheat, type ~ ., kernel="radial", gamma = 10^(-3:-1), cost = 10^(2:7), cross=10)
wheat.svm.rad.gamma <- wheat.svm.tune.rad$best.parameters$gamma
wheat.svm.rad.cost <- wheat.svm.tune.rad$best.parameters$cost

wheat.svm.rad.final <- svm(data=set1.wheat, type ~ ., kernel="radial", gamma = wheat.svm.rad.gamma, cost = wheat.svm.rad.cost, cross=10)  
wheat.pred.svm.rad.tr <- predict(wheat.svm.rad.final, newdata=set1.wheat)
wheat.pred.svm.rad.te <- predict(wheat.svm.rad.final, newdata=set2.wheat)

wheat.svm.train.rad.error <- mean(ifelse(wheat.pred.svm.rad.tr == set1.wheat$type, yes=0, no=1))
wheat.svm.test.rad.error <- mean(ifelse(wheat.pred.svm.rad.te == set2.wheat$type, yes=0, no=1))

wheat.svm.tune.poly <- tune.svm(data=set1.wheat, type ~ ., kernel="polynomial", gamma = 10^(-3:-1), cost = 10^(2:7), cross=10)
wheat.svm.poly.gamma <- wheat.svm.tune.poly$best.parameters$gamma
wheat.svm.poly.cost <- wheat.svm.tune.poly$best.parameters$cost

wheat.svm.poly.final <- svm(data=set1.wheat, type ~ ., kernel="polynomial", gamma = wheat.svm.poly.gamma, cost = wheat.svm.poly.cost, cross=10)  
wheat.pred.svm.poly.tr <- predict(wheat.svm.poly.final, newdata=set1.wheat)
wheat.pred.svm.poly.te <- predict(wheat.svm.poly.final, newdata=set2.wheat)

wheat.svm.train.poly.error <- mean(ifelse(wheat.pred.svm.poly.tr == set1.wheat$type, yes=0, no=1))
wheat.svm.test.poly.error <- mean(ifelse(wheat.pred.svm.poly.te == set2.wheat$type, yes=0, no=1))


wheat.svm.train.rad.error
wheat.svm.test.rad.error
wheat.svm.train.poly.error
wheat.svm.test.poly.error



