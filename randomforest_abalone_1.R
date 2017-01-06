#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(randomForest)

R <- 20
set.seed (890987665)

abalone <-  read.table("abalone.csv", header=TRUE, sep=",", na.strings=" ")
abalone <- abalone[(0 < abalone$Height)&(abalone$Height < 0.5),]
abalone <- abalone[,c(9,1:8)]
head(abalone)

val.mtry <- c(1,3,6,9)
val.nodesize <- c(2.5,5,10)
best.m.n <- matrix(NA, nrow=R, ncol=3)
colnames(best.m.n) <- c("mtry","nodesize","ooberror")

counter <- 1
for (r in 1:R){
  new <- ifelse ( runif (n= nrow ( abalone )) <.75 , yes =1, no =2)
  y.1n <- abalone[ which ( new == 1) , 1]
  x.1n <- as.matrix ( abalone[ which ( new == 1), c(2:9)])
  y.2n <- abalone [ which ( new == 2) , 1]
  x.2n <- as.matrix ( abalone[ which ( new == 2), c(2:9)])
  
  oob.final.temp.abalone <- 9e99
  
  for ( m in val.mtry) {
    for ( n in val.nodesize) {
      abalone.rf.temp <- randomForest(data=abalone[ which ( new == 1),], 
                                       Rings ~ Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, importance=TRUE, ntree=1000, 
                                       mtry=m,nodesize=n,keep.forest=TRUE)
      
      oob.rf.temp <- mean((y.1n - abalone.rf.temp$predicted)^2)
      
      if (oob.rf.temp < oob.final.temp.abalone) {
        oob.final.temp.abalone <- oob.rf.temp
        final.m <- m
        final.n <- n
      }
    }
  }
  
  abalone.rf.final.temp <- randomForest(data=abalone[ which ( new == 1),], 
                                   Rings ~ Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, importance=TRUE, ntree=1000, 
                                   mtry=final.m,nodesize=final.n,keep.forest=TRUE)
  
  best.m.n[counter,] <- c(final.m,final.n,oob.final.temp.abalone)
  
  counter <- counter + 1
}

min.mspe.temp.index.en <- which.min(best.m.n[,3])
best.mtry.val.en <- best.m.n[min.mspe.temp.index.en,1]
best.nodesize.val.en <- best.m.n[min.mspe.temp.index.en,2]

MSPE.randomf <- matrix (NA , nrow =R, ncol=1)
MSPE.randomf.default <- matrix (NA , nrow =R, ncol=1)
colnames ( MSPE.randomf ) <- c("randomforest")
colnames(MSPE.randomf.default) <- c ( "randomforestdef" )

rMSPE.randomf <- matrix (NA , nrow =R, ncol =1)
rMSPE.randomf.default <- matrix (NA , nrow =R, ncol =1)
colnames(rMSPE.randomf) <- c("randomforest")
colnames(rMSPE.randomf.default) <- c ( "randomforestdef")

counter <- 1
for (r in 1:R){
  new <- ifelse ( runif (n= nrow ( abalone )) <.75 , yes =1, no =2)
  y.1n <- abalone[ which ( new == 1) , 1]
  x.1n <- as.matrix ( abalone[ which ( new == 1), c(2:9)])
  y.2n <- abalone [ which ( new == 2) , 1]
  x.2n <- as.matrix ( abalone[ which ( new == 2), c(2:9)])
  
  abalone.rf.final.en <- randomForest(data=abalone[ which ( new == 1),], 
                                  Rings ~ Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, importance=TRUE, ntree=1000, 
                                  mtry=best.mtry.val.en,nodesize=best.nodesize.val.en,keep.forest=TRUE)
  
  abalone.rf.final.en.pred <- predict(abalone.rf.final.en,newdata=abalone[ which ( new == 2),])
  MSPE.randomf[counter,] <- mean((y.2n - abalone.rf.final.en.pred)^2)
  
  abalone.rf.default.en <- randomForest(data=abalone[ which ( new == 1),], 
                                      Rings ~ Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, importance=TRUE,keep.forest=TRUE)
  
  abalone.rf.default.en.pred <- predict(abalone.rf.default.en,newdata=abalone[ which ( new == 2),])
  MSPE.randomf.default[counter,] <- mean((y.2n - abalone.rf.default.en.pred)^2)
  
  counter <- counter + 1
}

minimum_test.rf <- apply(X=MSPE.randomf, MARGIN=2, FUN=min)
minimum_test.rf.def <- apply(X=MSPE.randomf.default, MARGIN=2, FUN=min)
counter <- 1
for (r in 1:R){
  rMSPE.randomf[counter,] <- MSPE.randomf[counter,] / minimum_test.rf
  rMSPE.randomf.default[counter,] <- MSPE.randomf.default[counter,] / minimum_test.rf.def
  counter <- counter + 1
}

MSPE.temp <- cbind(MSPE.temp,MSPE.randomf,MSPE.randomf.default)
rMSPE.temp <- cbind(rMSPE.temp,rMSPE.randomf,rMSPE.randomf.default)

## plots for random forest
x11(pointsize=9)
boxplot(sqrt(MSPE.temp), at=1:19, las=2, xlim = c(0,21), names = c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3", "mars_1","mars_2","mars_3","mars_best","nnet","bestboost","boostingdef","bartdef","rf","rfdef"), main="boxplot root MSPE")

x11(pointsize=9)
boxplot(sqrt(rMSPE.temp), at=1:19, las=2, xlim = c(0,21), names = c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3","mars_1","mars_2","mars_3","mars_best","nnet","bestboost","boostingdef","bartdef","rf","rfdef"), main="boxplot relative root MSPE")

