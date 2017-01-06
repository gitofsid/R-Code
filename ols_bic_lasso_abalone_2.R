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

R =20
set.seed (890987665)

sMSE <- matrix (NA , nrow =R, ncol =4)
colnames ( sMSE ) <- c(" OLS ", " ALLBIC ", " LASSOMIN ", " LASSO1SE ")
MSPE <- matrix (NA , nrow =R, ncol =4)
colnames ( MSPE ) <- colnames ( sMSE )
resc_MSPE <- matrix (NA , nrow =R, ncol =4)
colnames(resc_MSPE) <- colnames ( sMSE )

counter <- 1
for (r in 1:R){
  new <- ifelse ( runif (n= nrow ( abalone.final )) <.75 , yes =1, no =2)
  y.1n <- abalone.final [ which ( new ==1) , 1]
  x.1n <- as.matrix ( abalone.final [ which ( new ==1) , -1])
  y.2n <- abalone.final [ which ( new ==2) , 1]
  x.2n <- as.matrix ( abalone.final [ which ( new ==2) , -1])
  
  # Lin Reg via lsfit
  olsn <- lsfit (x.1n,y.1n)
  sMSE [ counter ,1] <- mean ((y.1n - cbind (1,x.1n) %*% olsn$coef )^2)
  MSPE [ counter ,1] <- mean ((y.2n - cbind (1,x.2n) %*% olsn$coef )^2)
  
  # All Subset BIC
  library(leaps)
  allsub1 <- regsubsets(x=x.1n, y=y.1n, nbest=1)
  summ.1 <- summary(allsub1)
  summ.1$bic
  min_bic_index <- which.min(summ.1$bic)
  allbic <- lm(rings ~ ., data=abalone.final [ which ( new ==1), summ.1$which[min_bic_index,]])
  sMSE [ counter ,2] <- summary(allbic)$sigma^2
  pred2 <- predict(allbic, newdata=abalone.final [ which ( new ==2),])
  MSPE [ counter ,2] <- mean((pred2-abalone.final [ which ( new ==2),]$rings)^2)
  
  # LASSO 
  library(glmnet)
  xs.1n <- scale(x.1n)
  xs.2n <- scale(x.2n)
  cv.lasso.1n <- cv.glmnet(y=y.1n, x= xs.1n, family="gaussian")
  
  #with lambda.min
  predict.1n.1n.min <- predict(cv.lasso.1n, newx=xs.1n, s = "lambda.min") 
  predict.1n.2n.min <- predict(cv.lasso.1n, newx=xs.2n, s = "lambda.min") 
  sMSE [ counter, 3] <- mean((y.1n - predict.1n.1n.min)^2)
  MSPE [ counter, 3] <- mean((y.2n - predict.1n.2n.min)^2)
  
  
  # LASSO with lamda.1se
  predict.1n.1n.1se <- predict(cv.lasso.1n, newx=xs.1n, s = "lambda.1se") 
  predict.1n.2n.1se <- predict(cv.lasso.1n, newx=xs.2n, s = "lambda.1se") 
  sMSE [ counter, 4] <- mean((y.1n - predict.1n.1n.1se)^2)
  MSPE [ counter, 4] <- mean((y.2n - predict.1n.2n.1se)^2)
  
  counter <- counter +1
}

# Results
# ols allbic lassomin lasso1se
mean(sqrt(sMSE[,1])) # ols
mean(sqrt(MSPE[,1])) # ols
t.test(sqrt(sMSE[,1])) # 95% ols
t.test(sqrt(MSPE[,1])) # 95% ols

mean(sqrt(sMSE[,2])) # allbic
mean(sqrt(MSPE[,2])) # allbic
t.test(sqrt(sMSE[,2])) # 95% allbic
t.test(sqrt(MSPE[,2])) # 95% allbic

mean(sqrt(sMSE[,3])) # lassomin
mean(sqrt(MSPE[,3])) # lassomin
t.test(sqrt(sMSE[,3])) # 95% lassomin
t.test(sqrt(MSPE[,3])) # 95% lassomin

mean(sqrt(sMSE[,4])) # lasso1se
mean(sqrt(MSPE[,4])) # lasso1se
t.test(sqrt(sMSE[,4])) # 95% lasso1se
t.test(sqrt(MSPE[,4])) # 95% lasso1se

boxplot(sqrt(sMSE), at=1:4, las=2, xlim = c(0,9), ylim=c(2,2.4), names = c("ols_tr", "allsub_tr","lassomin_tr","lasso1se_tr"), main="boxplot square-root train & test error")
boxplot(sqrt(MSPE), at=5:8, add=TRUE, las=2, names = c("ols_te", "allsub_te","lassomin_te","lasso1se_te"))


minimum_test <- apply(X=MSPE, MARGIN=1, FUN=min)
counter <- 1
for (r in 1:R){
  resc_MSPE [counter,] <- MSPE[counter,] / minimum_test[counter]
  counter <- counter + 1
}

boxplot(sqrt(resc_MSPE), at=1:4, las=2, xlim = c(0,5), names = c("ols_te", "allsub_te","lassomin_te","lasso1se_te"), main="boxplot rescaled square-root test error")
