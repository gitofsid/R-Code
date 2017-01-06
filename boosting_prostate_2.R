#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(gbm)

prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

set.seed(120401002) 
reps=10

combinations.matrix <- expand.grid(tree = c(5000,10000,15000), depth = c(1,2,6,8), shrink = c(0.1,0.01,0.001), bag = c(0.5,0.8))
best.combination <- matrix(NA, nrow=reps, ncol=6)
colnames ( best.combination ) <- c("itr","tree","depth","shrinkage","bag","mspetemp")

for ( i in 1:reps) {
  MSE.final.temp <- 9e99
  prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)
  resamp <- sample.int(n=nrow(prostate[which(prostate$set==1),]), size=nrow(prostate[which(prostate$set==1),]), replace=TRUE)
  
  prostate.train.temp <- prostate [resamp, c(2:10)]
  prostate.test.temp <- prostate [-unique(resamp), c(2:10)]
  y.test.temp <- prostate [-unique(resamp),10]
  x.test.temp <- prostate [-unique(resamp),c(2:9)]
  
  for (j in 1:nrow(combinations.matrix)) {

    prostate.boost.temp <- gbm(data=prostate.train.temp, lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, distribution="gaussian", 
                          n.trees=combinations.matrix[j,1], interaction.depth=combinations.matrix[j,2], 
                          shrinkage=combinations.matrix[j,3],bag.fraction=combinations.matrix[j,4], cv.folds=0)
    
    prostate.boost.pred.temp = matrix(predict(prostate.boost.temp, newdata=prostate.test.temp, n.trees=1000, 
                                        type="response"),nrow=nrow(prostate.test.temp))
    
    
    mspe.temp <- mean((y.test.temp - prostate.boost.pred.temp )^2)
    
    if (mspe.temp < MSE.final.temp) {
      MSE.final.temp <- mspe.temp
      index.temp <- j
      best.pred <- summary(prostate.boost.temp)
    }
  }
  
  best.combination[i,] <- c(i,combinations.matrix[index.temp,1],combinations.matrix[index.temp,2],
                            combinations.matrix[index.temp,3],combinations.matrix[index.temp,4],
                            MSE.final.temp)
  
}

min.mspe.temp.index <- which.min(best.combination[,6])
best.tree.val <- best.combination[min.mspe.temp.index,2]
best.depth.val <- best.combination[min.mspe.temp.index,3]
best.shrinkage.val <- best.combination[min.mspe.temp.index,4]
best.bag.val <- best.combination[min.mspe.temp.index,5]
y.test <- prostate[which(prostate$set==2),10]


prostate.boost.final <- gbm(data=prostate[which(prostate$set==1),c(2:10)], lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, distribution="gaussian", 
                            n.trees=best.tree.val, interaction.depth=best.depth.val, 
                            shrinkage=best.shrinkage.val,bag.fraction=best.bag.val, cv.folds=10)

gbm.perf(prostate.boost.final, method="cv" )
summary(prostate.boost.final)

plot(prostate.boost.final, i.var=1)
plot(prostate.boost.final, i.var=2)
plot(prostate.boost.final, i.var=3)
plot(prostate.boost.final, i.var=4)
plot(prostate.boost.final, i.var=5)
plot(prostate.boost.final, i.var=6)
plot(prostate.boost.final, i.var=7)
plot(prostate.boost.final, i.var=8)

prostate.boost.pred.final <- matrix(predict(prostate.boost.final, newdata=prostate[which(prostate$set==2),c(2:10)], n.trees=1000, 
                                    type="response"),nrow=nrow(prostate[which(prostate$set==2),c(2:10)]))


mean(prostate.boost.final$train.error)
mean((y.test - prostate.boost.pred.final)^2)
