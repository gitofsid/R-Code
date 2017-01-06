#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(rpart)
library(dummies)
library(earth)

R <- 20
set.seed (890987665)

abalone <-  read.table("abalone.csv", header=TRUE, sep=",", na.strings=" ")
abalone <- abalone[(0 < abalone$Height)&(abalone$Height < 0.5),]
head(abalone)


sex.dummy <- dummy(abalone$Sex,fun = as.integer)
abalone$female <- sex.dummy[,1]
abalone$male <- sex.dummy[,3]
abalone <- abalone[,c(9,1:8,10:11)]

MSPE <- matrix (NA , nrow =R, ncol =13)
colnames ( MSPE ) <- c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3", "mars_1", "mars_2", "mars_3", "mars_best")
rMSPE <- matrix (NA , nrow =R, ncol =13)
colnames(rMSPE) <- colnames ( MSPE )

counter <- 1
for (r in 1:R){
  new <- ifelse ( runif (n= nrow ( abalone )) <.75 , yes =1, no =2)
  y.1n <- abalone[ which ( new == 1) , 1]
  x.1n_i <- as.matrix ( abalone[ which ( new == 1), c(3:11)])
  x.1n_f <- as.matrix ( abalone[ which ( new == 1), c(2:9)])
  y.2n <- abalone [ which ( new == 2) , 1]
  x.2n_i <- as.matrix ( abalone [ which ( new == 2), c(3:11)])
  x.2n_f <- as.matrix ( abalone[ which ( new == 2), c(2:9)])
  
  # full regression tree
  abalone.tree.full <- rpart(data=abalone[ which(new==1),c(1:9)], Rings~Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, method="anova",cp=0)
  cpt.abalone.tree.full <- abalone.tree.full$cptable
  pred.abalone.tree.full <- predict(abalone.tree.full,newdata=abalone[ which(new==2),c(1:9)])
  MSPE[counter,1] <- mean((pred.abalone.tree.full-y.2n)^2)
  
  # min-cp pruned
  minrow <- which.min(cpt.abalone.tree.full[,"xerror"])
  cplow.min <- cpt.abalone.tree.full[minrow,"CP"]
  cpup.min <- ifelse(minrow==1, yes=1, no=cpt.abalone.tree.full[minrow-1,"CP"])
  cp.min <- sqrt(cplow.min*cpup.min)
  
  abalone.tree.cp.min <- prune(abalone.tree.full,cp=cp.min)
  pred.abalone.tree.cp.min <- predict(abalone.tree.cp.min,newdata=abalone[ which(new==2),c(1:9)])
  MSPE[counter,2] <- mean((pred.abalone.tree.cp.min-y.2n)^2)
  
  
  # 1se pruned 
  se.row <- min(which(cpt.abalone.tree.full[,"xerror"] < cpt.abalone.tree.full[minrow,"xerror"]+cpt.abalone.tree.full[minrow,"xstd"]))
  cplow.1se <- cpt.abalone.tree.full[se.row,"CP"]
  cpup.1se <- ifelse(se.row==1, yes=1, no=cpt.abalone.tree.full[se.row-1,"CP"])
  cp.1se <- sqrt(cplow.1se*cpup.1se)
  
  abalone.tree.cp.1se <- prune(abalone.tree.full,cp=cp.1se)
  pred.abalone.tree.cp.1se <- predict(abalone.tree.cp.1se,newdata=abalone[ which(new==2),c(1:9)])
  MSPE[counter,3] <- mean((pred.abalone.tree.cp.1se-y.2n)^2)
  
  # ols
  ols_full <- lsfit (x.1n_i,y.1n)
  MSPE [counter,4] <- mean ((y.2n - cbind (1,x.2n_i) %*% ols_full$coef )^2)
  
  # All Subset BIC
  library(leaps)
  allsub <- regsubsets(x=x.1n_i, y=y.1n, nbest=1)
  summ <- summary(allsub)
  min_bic_index <- which.min(summ$bic)
  allbic <- lm(Rings ~ Length+Diameter+Height+Whole+Shucked+Viscera+Shell+female+male, data=abalone [ which ( new ==1), summ$which[min_bic_index,]])
  pred.bic <- predict(allbic, newdata=abalone [which( new ==2),c(1,3:11)])
  MSPE[counter,5] <- mean((pred.bic-abalone [which( new ==2),]$Rings)^2)
  
  # LASSO with lambda.min
  library(glmnet)
  xs.1n <- scale(x.1n_i)
  xs.2n <- scale(x.2n_i)
  cv.lasso <- cv.glmnet(y=y.1n, x= xs.1n, family="gaussian")
  predict.lasso.min <- predict(cv.lasso, newx=xs.2n, s = "lambda.min") 
  MSPE [counter,6] <- mean((y.2n - predict.lasso.min)^2)
  
  # BMA/BIC
  library(BMA)
  bayesian.mod <- bicreg(x = x.1n_i, y = y.1n, strict = FALSE, OR = 80)
  bayesian.min.bic <- which.min(bayesian.mod$bic)
  bma.bic <- lm(formula=abalone [ which ( new ==1), 1] ~ Length+Diameter+Height+Whole+Shucked+Viscera+Shell+male, data=abalone [ which ( new ==1), bayesian.mod$which[bayesian.min.bic,]])
  pred.bma <- predict(bma.bic, newdata=abalone [which( new ==2),c(1,3:11)])
  MSPE [counter,7] <- mean((pred.bma-abalone [which( new ==2),]$Rings)^2)
  
  # gam
  library(mgcv)
  gam1 <- gam(data=abalone[ which(new==1),c(1:9)], Rings~Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, 
              family=gaussian(link=identity)) 
  summary(gam1)
  pred_gam <- predict(gam1,newdata=as.data.frame(abalone[ which ( new == 2), c(2:9)]))
  MSPE [ counter ,8] <- mean((pred_gam-y.2n)^2)
  
  # ppr3
  ppr3 <- ppr(data=abalone[ which(new==1),c(1:9)], Rings~Sex+Length+Diameter+Height+Whole
              +Shucked+Viscera+Shell, nterms=3, optlevel=3) 
  summary(ppr3)
  pred_ppr3 <- predict(ppr3,newdata=as.data.frame(abalone[ which ( new == 2), c(2:9)]))
  MSPE [ counter ,9] <- mean((pred_ppr3-y.2n)^2)
  
  # MARS degree = 1
  earth.1 <- earth(Rings~Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, data=abalone[which(new==1),c(1:9)], 
                       trace=3, degree=1)
  summary(earth.1)
  pred.earth.1 <- predict(earth.1,newdata=as.data.frame(abalone[ which ( new == 2), c(2:9)]))
  MSPE [ counter ,10] <- mean((pred.earth.1-y.2n)^2)
  
  # MARS degree = 2
  earth.2 <- earth(Rings~Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, data=abalone[which(new==1),c(1:9)], 
                   trace=3, degree=2)
  summary(earth.2)
  pred.earth.2 <- predict(earth.2,newdata=as.data.frame(abalone[ which ( new == 2), c(2:9)]))
  MSPE [ counter ,11] <- mean((pred.earth.2-y.2n)^2)
  
  # MARS degree = 3
  earth.3 <- earth(Rings~Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, data=abalone[which(new==1),c(1:9)], 
                   trace=3, degree=3)
  summary(earth.3)
  pred.earth.3 <- predict(earth.3,newdata=as.data.frame(abalone[ which ( new == 2), c(2:9)]))
  MSPE [ counter ,12] <- mean((pred.earth.3-y.2n)^2)
  
  # MARS degree = best
  best.grsq.index <- which.min(c(earth.1$grsq,earth.3$grsq,earth.3$grsq))
  if (best.grsq.index == '1') {
    MSPE [ counter ,13] <- mean((pred.earth.1-y.2n)^2)
  } else if (best.grsq.index == '2') {
    MSPE [ counter ,13] <- mean((pred.earth.2-y.2n)^2)
  } else {
    MSPE [ counter ,13] <- mean((pred.earth.3-y.2n)^2)
  }
  
  
  counter <- counter + 1
}

minimum_test <- apply(X=MSPE, MARGIN=1, FUN=min)
counter <- 1
for (r in 1:R){
  rMSPE [counter,] <- MSPE[counter,] / minimum_test[counter]
  counter <- counter + 1
}

x11(pointsize=9)
boxplot(sqrt(MSPE), at=1:13, las=2, xlim = c(0,14), names = c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3", "mars_1","mars_2","mars_3","mars_best"), main="boxplot root MSPE")

x11(pointsize=9)
boxplot(sqrt(rMSPE), at=1:13, las=2, xlim = c(0,14), names = c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3","mars_1","mars_2","mars_3","mars_best"), main="boxplot relative root MSPE")


## adding nnet to this for graphs
## did nnet before - uncomment only if nnet code is run before
#MSPE.temp <- MSPE
#MSPE.temp <- cbind(MSPE.temp,MSPR.best.f[,-c(1:2)])
#rMSPE.temp <- rMSPE
#rMSPE.temp <- cbind(rMSPE.temp,MSPR.best.r[,-c(1:2)])


## plots for nnet
#x11(pointsize=9)
#boxplot(sqrt(MSPE.temp), at=1:14, las=2, xlim = c(0,15), names = c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3", "mars_1","mars_2","mars_3","mars_best","nnet"), main="boxplot root MSPE")

#x11(pointsize=9)
#boxplot(sqrt(rMSPE.temp), at=1:14, las=2, xlim = c(0,15), names = c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3","mars_1","mars_2","mars_3","mars_best","nnet"), main="boxplot relative root MSPE")


