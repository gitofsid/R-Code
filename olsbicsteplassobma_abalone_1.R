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

##################
##### part c #####
##################

library(glmnet)
library(bootstrap)
library(MASS)

R <- 20

sMSE_tr <- matrix (NA , nrow=R, ncol =7)
sMSE_te <- matrix (NA , nrow=R, ncol =7)
colnames ( sMSE_tr ) <- c(" OLS ", " ALLBIC ", "ALLAIC", "STEPBIC", "LASSOMIN ", " LASSO1SE", "BMABIC")
MSPE_tr <- matrix (NA , nrow =R, ncol =7)
MSPE_te <- matrix (NA , nrow =R, ncol =7)
resc_MSPE_tr <- matrix (NA , nrow =R, ncol =7)
resc_MSPE_te <- matrix (NA , nrow =R, ncol =7)
colnames ( sMSE_te ) <- colnames ( sMSE_tr )
colnames ( MSPE_tr ) <- colnames ( sMSE_tr )
colnames ( MSPE_te ) <- colnames ( sMSE_tr )
colnames (resc_MSPE_tr) <- colnames ( sMSE_tr )
colnames (resc_MSPE_te) <- colnames ( sMSE_tr )


counter <- 1
for (r in 1:R){
  new <- ifelse ( runif (n= nrow ( abalone.final )) <.75 , yes =1, no =2)
  y.1n <- abalone.final [ which ( new ==1) , 1]
  x.1n <- as.matrix ( abalone.final [ which ( new ==1) , 2:10])
  y.2n <- abalone.final [ which ( new ==2) , 1]
  x.2n <- as.matrix ( abalone.final [ which ( new ==2) , 2:10])
  
  # ols
  ols_full.tr <- lsfit (x.1n,y.1n)
  ols_full.te <- lsfit(x.2n,y.2n)
  sMSE_tr [counter,1] <- mean ((y.1n - cbind (1,x.1n) %*% ols_full.tr$coef )^2)
  MSPE_tr [counter,1] <- mean ((y.2n - cbind (1,x.2n) %*% ols_full.tr$coef )^2)
  sMSE_te [counter,1] <- mean ((y.2n - cbind (1,x.2n) %*% ols_full.te$coef )^2)
  MSPE_te [counter,1] <- mean ((y.1n - cbind (1,x.1n) %*% ols_full.te$coef )^2)
  
  # All Subset BIC
  library(leaps)
  allsub_tr.1 <- regsubsets(x=x.1n, y=y.1n, nbest=1)
  allsub_te.1 <- regsubsets(x=x.2n, y=y.2n, nbest=1)
  summ.tr.1 <- summary(allsub_tr.1)
  summ.te.1 <- summary(allsub_te.1)
  min_bic_index.tr.1 <- which.min(summ.tr.1$bic)
  min_bic_index.te.1 <- which.min(summ.te.1$bic)
  allbic.tr.1 <- lm(rings ~ ., data=abalone.final [ which ( new ==1), summ.tr.1$which[min_bic_index.tr.1,]])
  allbic.te.1 <- lm(rings ~ ., data=abalone.final [ which ( new ==2), summ.te.1$which[min_bic_index.te.1,]])
  sMSE_tr [counter,2] <- summary(allbic.tr.1)$sigma^2
  sMSE_te [counter,2] <- summary(allbic.te.1)$sigma^2
  pred.tr.te.1 <- predict(allbic.tr.1, newdata=abalone.final [which( new ==2),])
  pred.te.tr.1 <- predict(allbic.te.1, newdata=abalone.final [which( new ==1),])
  MSPE_tr [counter,2] <- mean((pred.tr.te.1-abalone.final [which( new ==2),]$rings)^2)
  MSPE_te [counter,2] <- mean((pred.te.tr.1-abalone.final [which( new ==1),]$rings)^2)
  
  # All Subset AIC
  # this is going to have same value as Allsubset BIC as
  # AIC = BIC + 2k - log(n)k
  # so AIC will be smallest for smallest value of BIC
  sMSE_tr [counter,3] <- sMSE_tr [counter,2]
  MSPE_tr [counter,3] <- MSPE_tr [counter,2]
  sMSE_te [counter,3] <- sMSE_te [counter,2]
  MSPE_te [counter,3] <- MSPE_te [counter,2]
  
  # stepwise/BIC
  initial.tr.fit <- lm(data=abalone.final[which(new==1),], formula=rings ~ 1)
  final.tr.fit <- lm(data=abalone.final[which(new==1),], formula=rings ~ .)
  initial.te.fit <- lm(data=abalone.final[which(new==2),], formula=rings ~ 1)
  final.te.fit <- lm(data=abalone.final[which(new==2),], formula=rings ~ .)
  step.tr <- step(object=initial.tr.fit, scope=list(upper=final.tr.fit),k = log(nrow(abalone.final[which(new==1),])))
  step.te <- step(object=initial.te.fit, scope=list(upper=final.te.fit),k = log(nrow(abalone.final[which(new==2),])))
  pred.tr.tr <- predict(step.tr, newdata=abalone.final[which(new==1),])
  pred.te.te <- predict(step.te, newdata=abalone.final[which(new==2),])
  pred.tr.te <- predict(step.tr, newdata=abalone.final[which(new==2),])
  pred.te.tr <- predict(step.te, newdata=abalone.final[which(new==1),])
  sMSE_tr [counter,4] <- mean((y.1n - pred.tr.tr)^2)
  MSPE_tr[counter,4] <- mean((y.2n - pred.tr.te)^2)
  sMSE_te [counter,4] <- mean((y.2n - pred.te.te)^2)
  MSPE_te[counter,4] <- mean((y.1n - pred.te.tr)^2)
  
  # LASSO 
  library(glmnet)
  xs.1n <- scale(x.1n)
  xs.2n <- scale(x.2n)
  cv.lasso.1n <- cv.glmnet(y=y.1n, x= xs.1n, family="gaussian")
  cv.lasso.2n <- cv.glmnet(y=y.2n, x= xs.2n, family="gaussian")
  
  #with lambda.min
  predict.1n.1n.min <- predict(cv.lasso.1n, newx=xs.1n, s = "lambda.min") 
  predict.1n.2n.min <- predict(cv.lasso.1n, newx=xs.2n, s = "lambda.min") 
  predict.2n.2n.min <- predict(cv.lasso.2n, newx=xs.2n, s = "lambda.min") 
  predict.2n.1n.min <- predict(cv.lasso.2n, newx=xs.1n, s = "lambda.min")
  sMSE_tr [counter,5] <- mean((y.1n - predict.1n.1n.min)^2)
  MSPE_tr [counter,5] <- mean((y.2n - predict.1n.2n.min)^2)
  sMSE_te [counter,5] <- mean((y.2n - predict.2n.2n.min)^2)
  MSPE_te [counter,5] <- mean((y.1n - predict.2n.1n.min)^2)
  
  # LASSO with lamda.1se
  predict.1n.1n.1se <- predict(cv.lasso.1n, newx=xs.1n, s = "lambda.1se") 
  predict.1n.2n.1se <- predict(cv.lasso.1n, newx=xs.2n, s = "lambda.1se") 
  predict.2n.2n.1se <- predict(cv.lasso.2n, newx=xs.2n, s = "lambda.1se") 
  predict.2n.1n.1se <- predict(cv.lasso.2n, newx=xs.1n, s = "lambda.1se") 
  sMSE_tr [counter,6] <- mean((y.1n - predict.1n.1n.1se)^2)
  MSPE_tr [counter,6] <- mean((y.2n - predict.1n.2n.1se)^2)
  sMSE_te [counter,6] <- mean((y.2n - predict.2n.2n.1se)^2)
  MSPE_te [counter,6] <- mean((y.1n - predict.2n.1n.1se)^2)
  
  # BMA/BIC
  bayesian.mod.tr <- bicreg(x = x.1n, y = y.1n, strict = FALSE, OR = 80)
  bayesian.mod.te <- bicreg(x = x.2n, y = y.2n, strict = FALSE, OR = 80)
  bayesian.min.bic.tr <- which.min(bayesian.mod.tr$bic)
  bayesian.min.bic.te <- which.min(bayesian.mod.te$bic)
  bma.bic.tr <- lm(formula=abalone.final [ which ( new ==1), 1] ~ ., data=abalone.final [ which ( new ==1), bayesian.mod.tr$which[bayesian.min.bic.tr,]])
  bma.bic.te <- lm(formula=abalone.final [ which ( new ==2), 1] ~ ., data=abalone.final [ which ( new ==2), bayesian.mod.te$which[bayesian.min.bic.te,]])
  sMSE_tr [counter,7] <- summary(bma.bic.tr)$sigma^2
  sMSE_te [counter,7] <- summary(bma.bic.te)$sigma^2
  pred.bma_tr <- predict(bma.bic.tr, newdata=abalone.final [which( new ==2),])
  pred.bma_te <- predict(bma.bic.te, newdata=abalone.final [which( new ==1),])
  MSPE_tr [counter,7] <- mean((pred.bma_tr-abalone.final [which( new ==2),]$rings)^2)
  MSPE_te [counter,7] <- mean((pred.bma_te-abalone.final [which( new ==1),]$rings)^2)
  
  counter <- counter +1
}


boxplot(sqrt(MSPE_te), at=1:7, las=2, xlim = c(0,8), ylim=c(2,2.6), names = c("ols", "allsubbic","allsubaic","stepbic","lassomin","lasso1se","bmabic"), 
        main="boxplot square-root MSPE for test set")

minimum_test_tr <- apply(X=MSPE_tr, MARGIN=1, FUN=min)
minimum_test_te <- apply(X=MSPE_te, MARGIN=1, FUN=min)
counter <- 1
for (r in 1:R){
  resc_MSPE_te [counter,] <- MSPE_te[counter,] / minimum_test_te[counter]
  resc_MSPE_tr [counter,] <- MSPE_tr[counter,] / minimum_test_tr[counter]
  counter <- counter + 1
}

boxplot(sqrt(resc_MSPE_te), at=1:7, las=2, xlim = c(0,8),  ylim=c(1,1.1), names = c("ols", "allsubbic","allsubaic","stepbic","lassomin","lasso1se","bmabic"), 
        main="boxplot rescaled square-root MSPE for test set")

boxplot(sqrt(resc_MSPE_tr), at=1:7, las=2, xlim = c(0,8), ylim=c(0.9,1.06), names = c("ols", "allsubbic","allsubaic","stepbic","lassomin","lasso1se","bmabic"), 
        main="boxplot rescaled square-root MSPE for training set")




