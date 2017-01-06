#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(gbm)

R <- 20
set.seed (890987665)

abalone <-  read.table("abalone.csv", header=TRUE, sep=",", na.strings=" ")
abalone <- abalone[(0 < abalone$Height)&(abalone$Height < 0.5),]
abalone <- abalone[,c(9,1:8)]
head(abalone)

combinations.matrix <- expand.grid(tree = c(5000,10000,15000), depth = c(1,2,6,8,10), shrink = c(0.1,0.01,0.001), bag = c(0.5,0.8))
best.combination.abalone <- matrix(NA, nrow=R, ncol=6)
colnames ( best.combination.abalone ) <- c("itr","tree","depth","shrinkage","bag","mspetemp")

counter <- 1
for (r in 1:R){
  new <- ifelse ( runif (n= nrow ( abalone )) <.75 , yes =1, no =2)
  y.1n <- abalone[ which ( new == 1) , 1]
  x.1n <- as.matrix ( abalone[ which ( new == 1), c(2:9)])
  y.2n <- abalone [ which ( new == 2) , 1]
  x.2n <- as.matrix ( abalone[ which ( new == 2), c(2:9)])
  
  MSE.final.temp.abalone <- 9e99
  
  for (j in 1:nrow(combinations.matrix)) {
    abalone.boost.temp <- gbm(data=abalone[ which ( new == 1),], Rings ~ Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, distribution="gaussian", 
                               n.trees=combinations.matrix[j,1], interaction.depth=combinations.matrix[j,2], 
                               shrinkage=combinations.matrix[j,3],bag.fraction=combinations.matrix[j,4], cv.folds=0)
    
    abalone.boost.pred.temp = matrix(predict(abalone.boost.temp, newdata=abalone[ which ( new == 2),], n.trees=2000, 
                                              type="response"),nrow=nrow(abalone[ which ( new == 2),]))
    
    mspe.temp.abalone <- mean((y.2n - abalone.boost.pred.temp )^2)
    
    if (mspe.temp.abalone < MSE.final.temp.abalone) {
      MSE.final.temp.abalone <- mspe.temp.abalone
      index.temp <- j
    }
  }
  
  best.combination.abalone[r,] <- c(r,combinations.matrix[index.temp,1],combinations.matrix[index.temp,2],
                            combinations.matrix[index.temp,3],combinations.matrix[index.temp,4],
                            MSE.final.temp.abalone)
}

min.mspe.temp.index.ab <- which.min(best.combination.abalone[,6])
best.tree.val.ab <- best.combination.abalone[min.mspe.temp.index.ab,2]
best.depth.val.ab <- best.combination.abalone[min.mspe.temp.index.ab,3]
best.shrinkage.val.ab <- best.combination.abalone[min.mspe.temp.index.ab,4]
best.bag.val.ab <- best.combination.abalone[min.mspe.temp.index.ab,5]


MSPE.boost <- matrix (NA , nrow =R, ncol=1)
MSPE.boost.default <- matrix (NA , nrow =R, ncol=1)
colnames ( MSPE.boost ) <- c("boosting")
colnames(MSPE.boost.default) <- c ( "boostingdef" )

rMSPE.boost <- matrix (NA , nrow =R, ncol =1)
rMSPE.boost.default <- matrix (NA , nrow =R, ncol =1)
colnames(rMSPE.boost) <- colnames (MSPE.boost)
colnames(rMSPE.boost.default) <- c ( "boostingdef")

counter <- 1
for (r in 1:R){
  new <- ifelse ( runif (n= nrow ( abalone )) <.75 , yes =1, no =2)
  y.1n <- abalone[ which ( new == 1) , 1]
  x.1n <- as.matrix ( abalone[ which ( new == 1), c(2:9)])
  y.2n <- abalone [ which ( new == 2) , 1]
  x.2n <- as.matrix ( abalone[ which ( new == 2), c(2:9)])
  
  abalone.boost.final <- gbm(data=abalone[ which ( new == 1),], Rings ~ Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, distribution="gaussian", 
                             n.trees=best.tree.val.ab, interaction.depth=best.depth.val.ab, 
                             shrinkage=best.shrinkage.val.ab,bag.fraction=best.bag.val.ab, cv.folds=10)
  
  abalone.boost.pred.final = matrix(predict(abalone.boost.final, newdata=abalone[ which ( new == 2),], n.trees=2000, 
                                            type="response"),nrow=nrow(abalone[ which ( new == 2),]))
  
  MSPE.boost[counter,] <- mean((y.2n - abalone.boost.pred.final)^2)
  counter <- counter + 1
}
  
counter <- 1
for (r in 1:R){  
  #### default #####
  new <- ifelse ( runif (n= nrow ( abalone )) <.75 , yes =1, no =2)
  y.1n <- abalone[ which ( new == 1) , 1]
  x.1n <- as.matrix ( abalone[ which ( new == 1), c(2:9)])
  y.2n <- abalone [ which ( new == 2) , 1]
  x.2n <- as.matrix ( abalone[ which ( new == 2), c(2:9)])
  abalone.boost.default <- gbm(data=abalone[ which ( new == 1),], Rings ~ Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, distribution="gaussian")
              
  
  abalone.boost.pred.default = matrix(predict(abalone.boost.default, newdata=abalone[ which ( new == 2),], n.trees=20, 
                                              type="response"),nrow=nrow(abalone[ which ( new == 2),]))
  
  
  MSPE.boost.default[counter,] <- mean((y.2n - abalone.boost.pred.default)^2)
  
  counter <- counter + 1
  
}

minimum_test.boost <- apply(X=MSPE.boost, MARGIN=2, FUN=min)
minimum_test.boost.def <- apply(X=MSPE.boost.default, MARGIN=2, FUN=min)
counter <- 1
for (r in 1:R){
  rMSPE.boost[counter,] <- MSPE.boost[counter,] / minimum_test.boost
  rMSPE.boost.default[counter,] <- MSPE.boost.default[counter,] / minimum_test.boost.def
  counter <- counter + 1
}


MSPE.temp <- cbind(MSPE.temp,MSPE.boost,MSPE.boost.default)
rMSPE.temp <- cbind(rMSPE.temp,rMSPE.boost,rMSPE.boost.default)

## plots for boosting
x11(pointsize=9)
boxplot(sqrt(MSPE.temp), at=1:16, las=2, xlim = c(0,17), names = c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3", "mars_1","mars_2","mars_3","mars_best","nnet","bestboost","boostingdef"), main="boxplot root MSPE")

x11(pointsize=9)
boxplot(sqrt(rMSPE.temp), at=1:16, las=2, xlim = c(0,17), names = c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3","mars_1","mars_2","mars_3","mars_best","nnet","bestboost","boostingdef"), main="boxplot relative root MSPE")







