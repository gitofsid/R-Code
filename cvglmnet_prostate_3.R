#################################################
## Name        -   Siddharth Baronia       ######
#################################################

# LASSO on prostate data using glmnet package 
#  (THERE IS ANOTHER PACKAGE THAT DOES LASSO.  WE WILL SEE IT LATER)
# Splitting the data in half and modeling each half separately.


prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

library(glmnet)

# Splitting data in half using random uniform selection to make two "set"s.

#set.seed(120401002)
set.seed(9267926) # comment this and uncomment line above for question 1
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

# glmnet() requires x to be in matrix class, so saving out 
#   the separate variables to be used as Y and X.
# Also standardizing columns of X to have unit variance (and mean 0)
#   (However, this is not necessary unless you want to see the 
#   coefficients plotted in the coefficient path on a comparable scale)
#   glmnet() scales all columns of x in its calculations, but presents 
#   unscaled versions of coefficients.  

y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
xs.1 <- scale(x.1)
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])
xs.2 <- scale(x.2)

# Fit LASSO by glmnet(y=, x=). Gaussian is default, but other families are available  
#  Function produces series of fits for many values of lambda.  

# First half of data 
lasso.1 <- glmnet(y=y.1, x= x.1, family="gaussian")
#("%Dev" in output below is R-square in linear regression, its called deviance and same to 1-RSS/SSTo)
lasso.1
plot(lasso.1, label = TRUE) # Plots coefficient path
##### Note that these are on original scales, even though LASSO scaled variables 
coef(lasso.1) # Lists out coefficients for each lambda
##### Note that glmnet() does not necessarily go all the way out to the LSEs
#####But they are close

#Using scaled variables gives a better coefficient path because 
#  coefficients are measured in the same units
lasso.1s <- glmnet(y=y.1, x= xs.1, family="gaussian")
plot(lasso.1s, label = TRUE) 
lasso.1s
coef(lasso.1s)

# cv.glmnet() uses crossvalidation to estimate optimal lambda
#  We haven't talked about this yet, so don't worry about it.
cv.lasso.1 <- cv.glmnet(y=y.1, x= xs.1, family="gaussian")
cv.lasso.1
plot(cv.lasso.1) # Plot CV-MSPE
coef(cv.lasso.1) # Print out coefficients at optimal lambda
coef(lasso.1s, s=cv.lasso.1$lambda.min) # Another way to do this.
# Using the "+1SE rule" (see later) produces a sparser solution
coef(lasso.1s, s=cv.lasso.1$lambda.1se) # Another way to do this.

# Predict both halves using first-half fit
predict.1.1 <- predict(cv.lasso.1, newx=xs.1) # predicts y1
predict.1.2 <- predict(cv.lasso.1, newx=xs.2) # predicting 2 ie y2 using 1, will use xs.2 as input
sMSE.lasso.1 <- mean((y.1 - predict.1.1)^2)
MSPE.lasso.1 <- mean((y.2 - predict.1.2)^2)

# finding training and testing error for lambda.min
predict.1.1.min <- predict(cv.lasso.1, newx=xs.1, s = "lambda.min") # predicts y1
predict.1.2.min <- predict(cv.lasso.1, newx=xs.2, s = "lambda.min") # predicting 2 ie y2 using 1, will use xs.2 as input
sMSE.lasso.1.min <- mean((y.1 - predict.1.1.min)^2)
MSPE.lasso.1.min <- mean((y.2 - predict.1.2.min)^2)

# finding training and testing error for lambda.1se
predict.1.1.1se <- predict(cv.lasso.1, newx=xs.1, s = "lambda.1se") # predicts y1
predict.1.2.1se <- predict(cv.lasso.1, newx=xs.2, s = "lambda.1se") # predicting 2 ie y2 using 1, will use xs.2 as input
sMSE.lasso.1.1se <- mean((y.1 - predict.1.1.1se)^2)
MSPE.lasso.1.1se <- mean((y.2 - predict.1.2.1se)^2)

# Repeat for second half of data
lasso.2 <- glmnet(y=y.2, x= xs.2, family="gaussian")
plot(lasso.2, label = TRUE)
cv.lasso.2 <- cv.glmnet(y=y.2, x= xs.2, family="gaussian")
plot(cv.lasso.2)
coef(cv.lasso.2)
coef(lasso.2, s=cv.lasso.2$lambda.min)
coef(lasso.2, s=cv.lasso.2$lambda.1se)

# Predict both halves using second-half fit
predict.2.1 <- predict(cv.lasso.2, newx=xs.1) # predicting 1 ie y1 using lasso2 using xs.1
predict.2.2 <- predict(cv.lasso.2, newx=xs.2) # predict y2
sMSE.lasso.2 <- mean((y.2 - predict.2.2)^2)
MSPE.lasso.2 <- mean((y.1 - predict.2.1)^2)

# finding training and testing error for lambda.min
predict.2.1.min <- predict(cv.lasso.2, newx=xs.1, s = "lambda.min") # predicting 1 ie y1 using lasso2 using xs.1
predict.2.2.min <- predict(cv.lasso.2, newx=xs.2, s = "lambda.min") # predict y2
sMSE.lasso.2.min <- mean((y.2 - predict.2.2.min)^2)
MSPE.lasso.2.min <- mean((y.1 - predict.2.1.min)^2)

# finding training and testing error for lambda.1se
predict.2.1.1se <- predict(cv.lasso.2, newx=xs.1, s = "lambda.1se") # predicting 1 ie y1 using lasso2 using xs.1
predict.2.2.1se <- predict(cv.lasso.2, newx=xs.2, s = "lambda.1se") # predict y2
sMSE.lasso.2.1se <- mean((y.2 - predict.2.2.1se)^2)
MSPE.lasso.2.1se <- mean((y.1 - predict.2.1.1se)^2)

# Results
cv.lasso.1$lambda.min
cv.lasso.1$lambda.1se
sMSE.lasso.1.min
MSPE.lasso.1.min
sMSE.lasso.1.1se
MSPE.lasso.1.1se
sMSE.lasso.1
MSPE.lasso.1
coef(cv.lasso.1) #optimal lambda
coef(cv.lasso.1, s="lambda.1se") # = coef(cv.lasso.1) = coef(lasso.1s, s=cv.lasso.1$lambda.1se)
coef(cv.lasso.1, s="lambda.min") # = coef(lasso.1s, s=cv.lasso.1$lambda.min)
coef(lasso.1s, s=cv.lasso.1$lambda.min)
coef(lasso.1s, s=cv.lasso.1$lambda.1se)

cv.lasso.2$lambda.min
cv.lasso.2$lambda.1se
sMSE.lasso.2.min
MSPE.lasso.2.min
sMSE.lasso.2.1se
MSPE.lasso.2.1se
sMSE.lasso.2
MSPE.lasso.2
coef(cv.lasso.2) #optimal lambda, looks like optimal lambda is lambda.1se
coef(cv.lasso.2, s="lambda.1se") # = coef(cv.lasso.2) = coef(lasso.2, s=cv.lasso.2$lambda.1se)
coef(cv.lasso.2, s="lambda.min") # = coef(lasso.2, s=cv.lasso.2$lambda.min)
coef(lasso.2, s=cv.lasso.2$lambda.min)
coef(lasso.2, s=cv.lasso.2$lambda.1se)

# How well do the two models agree? Compare predicted values.  
plot(x=c(predict.1.1,predict.1.2), y=c(predict.2.1,predict.2.2), 
     main = "Comparison of predictions from the two models",
     xlab="Predictions using Set 1 model", ylab="predictions using Set 2 model")
abline(a=0,b=1)
