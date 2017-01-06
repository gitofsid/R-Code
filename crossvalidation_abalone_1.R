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
y.test <- abalone.final[which(abalone.final$set==2),1]
x.test <- as.matrix(abalone.final[which(abalone.final$set==2),c(2:10)])

# Model fit function
lasso.fit <- function(x,y){
  cv.glmnet(y=y, x= x, family="gaussian")
}
# Prediction function
lasso.pred.1se <- function(fit,x){
  predict(fit, x) # or predict(fit, x, s="lambda.1se") as lambda.1se is the optimal lambda
}
# Prediction function
lasso.pred.min <- function(fit,x){
  predict(fit, x, s="lambda.min")
}

######################
###### part a ########
######################

# training set -  to keep it standard using V=10
library(glmnet)
library(bootstrap)
lasso.1se_10.tr <- crossval(x=x.train, y=y.train, theta.fit=lasso.fit, theta.predict=lasso.pred.1se, ngroup=10)
lasso.min_10.tr <- crossval(x=x.train, y=y.train, theta.fit=lasso.fit, theta.predict=lasso.pred.min, ngroup=10)
lasso.1se_10.te <- crossval(x=x.test, y=y.test, theta.fit=lasso.fit, theta.predict=lasso.pred.1se, ngroup=10)
lasso.min_10.te <- crossval(x=x.test, y=y.test, theta.fit=lasso.fit, theta.predict=lasso.pred.min, ngroup=10)

mspe.lasso.1se_10.tr <- mean((y.train - lasso.1se_10.tr$cv.fit)^2)
mspe.lasso.min_10.tr <- mean((y.train - lasso.min_10.tr$cv.fit)^2)

# All Subset BIC using least BIC on trainign and testting set
library(leaps)
allsub.tr <- regsubsets(x=x.train, y=y.train, nbest=1)
allsub.te <- regsubsets(x=x.test,y=y.test, nbest=1)
summ_allsub.tr <- summary(allsub.tr)
summ_allsub.te <- summary(allsub.te)
min_bic_index.te <- which.min(summ_allsub.te$bic) 
min_bic_index.tr <- which.min(summ_allsub.tr$bic) # finding index of minimum bic and use that model for fitting purposes
allbic.te <- lm(rings ~ ., data=abalone.final[which (abalone.final$set==2), summ_allsub.te$which[min_bic_index.te,]])
allbic.tr <- lm(rings ~ ., data=abalone.final[which (abalone.final$set==1), summ_allsub.tr$which[min_bic_index.tr,]])

pred_allsub.tr.tr <- predict(allbic.tr, newdata=abalone.final [which (abalone.final$set==1),])
pred_allsub.te.te <- predict(allbic.te, newdata=abalone.final [which (abalone.final$set==2),])
pred_allsub.tr.te <- predict(allbic.tr, newdata=abalone.final [which (abalone.final$set==2),])
pred_allsub.te.tr <- predict(allbic.te, newdata=abalone.final [which (abalone.final$set==1),])

mspe.allsub.tr.tr <-  mean((pred_allsub.tr.tr-abalone.final [which (abalone.final$set==1),]$rings)^2)


mspe.allsub.tr.te <-  mean((pred_allsub.tr.te-abalone.final [which (abalone.final$set==2),]$rings)^2)
mspe.allsub.te.tr <-  mean((pred_allsub.te.tr-abalone.final [which (abalone.final$set==1),]$rings)^2)

smse.allsub.te.te <- summary(allbic.te)$sigma^2
smse.allsub.tr.tr <- summary(allbic.tr)$sigma^2

# sMSE, MSPE on testing set using V=10
fit.10.te <- lasso.fit(x.test,y.test)
fit.10.tr <- lasso.fit(x.train,y.train)
lpred.1se_10.te.te <- lasso.pred.1se(fit.10.te,x.test)
lpred.min_10.te.te <- lasso.pred.min(fit.10.te,x.test)
lpred.1se_10.te.tr <- lasso.pred.1se(fit.10.te,x.train)
lpred.min_10.te.tr <- lasso.pred.min(fit.10.te,x.train)

smse.lasso.1se_10.te.te <- mean((y.test - lpred.1se_10.te.te)^2)
smse.lasso.min_10.te.te <- mean((y.test - lpred.min_10.te.te)^2)
mspe.lasso.1se_10.te.tr <- mean((y.train - lpred.1se_10.te.tr)^2)
mspe.lasso.min_10.te.tr <- mean((y.train - lpred.min_10.te.tr)^2)

# results 
# part a
mspe.lasso.1se_10.tr
mspe.lasso.min_10.tr
mspe.allsub.tr.tr

smse.allsub.te.te
smse.lasso.min_10.te.te
smse.lasso.1se_10.te.te
mspe.lasso.min_10.te.tr
mspe.lasso.1se_10.te.tr
mspe.allsub.te.tr


