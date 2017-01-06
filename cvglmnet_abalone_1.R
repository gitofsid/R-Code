#################################################
## Name        -   Siddharth Baronia       ######
#################################################


abalone <-  read.table("abalone.csv", header=FALSE, sep=",", na.strings=" ")
names(abalone) <- c("sex","length","diameter","height","wweight","sweight","vweight","shweight","rings")
abalone <- abalone[(0 < abalone$height)&(abalone$height < 0.5),]
head(abalone)

library(dummies)
sex.dummy <- dummy(abalone$sex,fun = as.integer)

abalone$female <- sex.dummy[,1]
abalone$male <- sex.dummy[,3]

abalone.final <- abalone[,c(9,2:8,10:11)]
head(abalone.final)

set.seed(29003092)
abalone.final$set <- ifelse(U <- runif(n=nrow(abalone.final))>0.75, yes=2, no=1)

y.train <- abalone.final[which(abalone.final$set==1),1]
x.train <- as.matrix(abalone.final[which(abalone.final$set==1),c(2:10)])
xs.train <- scale(x.train)
y.test <- abalone.final[which(abalone.final$set==2),1]
x.test <- as.matrix(abalone.final[which(abalone.final$set==2),c(2:10)])
xs.test <- scale(x.test)

# predict both halves using training data
# this will give the optimal results which will be same as at lambda.1se, so no need to explicitly find for lambda.1se
cv.lasso.train <- cv.glmnet(y=y.train, x= xs.train, family="gaussian")
predict.tr.tr <- predict(cv.lasso.train, newx=xs.train) # predicts y.train
predict.tr.te <- predict(cv.lasso.train, newx=xs.test) # predicting y.test using training model, will use xs.test as input
sMSE.lasso.tr <- mean((y.train - predict.tr.tr)^2)
MSPE.lasso.tr <- mean((y.test - predict.tr.te)^2)

# finding training and testing error using lambda.min
predict.tr.tr.min <- predict(cv.lasso.train, newx=xs.train, s = "lambda.min") 
predict.tr.te.min <- predict(cv.lasso.train, newx=xs.test, s = "lambda.min") 
sMSE.lasso.tr.min <- mean((y.train - predict.tr.tr.min)^2)
MSPE.lasso.tr.min <- mean((y.test - predict.tr.te.min)^2)

# predict both halves using testing data
# this will give the optimal results which will be same as at lambda.1se, so no need to explicitly find for lambda.1se
cv.lasso.test <- cv.glmnet(y=y.test, x= xs.test, family="gaussian")
predict.te.te <- predict(cv.lasso.test, newx=xs.test) # predicts y.test
predict.te.tr <- predict(cv.lasso.test, newx=xs.train) # predicting y.train using training model, will use xs.train as input
sMSE.lasso.te <- mean((y.test - predict.te.te)^2)
MSPE.lasso.te <- mean((y.train - predict.te.tr)^2)

# finding training and testing error using lambda.min
predict.te.te.min <- predict(cv.lasso.test, newx=xs.test, s = "lambda.min") # predicts y.test
predict.te.tr.min <- predict(cv.lasso.test, newx=xs.train, s = "lambda.min") # predicting y.train using training model, will use xs.train as input
sMSE.lasso.te.min <- mean((y.test - predict.te.te.min)^2)
MSPE.lasso.te.min <- mean((y.train - predict.te.tr.min)^2)




# Results
plot(cv.lasso.train, main="training")
plot(cv.lasso.test, main="testing")

cv.lasso.train$lambda.min
cv.lasso.train$lambda.1se
cv.lasso.test$lambda.min
cv.lasso.test$lambda.1se

coef(cv.lasso.train) # train coefficients at optimal lambda or lambda.1se
coef(cv.lasso.train, s="lambda.min")
coef(cv.lasso.test)  # test coefficients at optimal lambda or lambda.1se
coef(cv.lasso.test, s = "lambda.min")

sMSE.lasso.tr # sMSE at optimal lambda or lambda.1se for training
MSPE.lasso.tr # MSPE at optimal lambda or lambda.1se for training
sMSE.lasso.tr.min # sMSE at lambda.min for training
MSPE.lasso.tr.min # MSPE at lambda.min for training

sMSE.lasso.te # sMSE at optimal lambda or lambda.1se for testing
MSPE.lasso.te # MSPE at optimal lambda or lambda.1se for testing
sMSE.lasso.te.min # sMSE at lambda.min for testing
MSPE.lasso.te.min # MSPE at lambda.min for testing