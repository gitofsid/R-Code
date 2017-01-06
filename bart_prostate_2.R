#################################################
## Name        -   Siddharth Baronia       ######
#################################################
library(BayesTree)

prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

set.seed(120401002)
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)
y.train <- prostate[which(prostate$set==1),10]
x.train <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.test <- prostate[which(prostate$set==2),10]
x.test <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

reps=10 # Feel free to increase this to tolerable value

combinations.matrix.p <- expand.grid(k = c(1,2,3), ntree = c(100,200,300), sigdf = c(3,6,9), sigquant = c(.90,.95,.99))
best.combination.p <- matrix(NA, nrow=reps, ncol=6)
colnames (best.combination.p) <- c("itr","k","ntree","sigdf","sigquant","mspetemp")


for(r in 1:reps){
  MSE.final.temp <- 9e99
  resamp <- sample.int(n=nrow(prostate[which(prostate$set==1),]), size=nrow(prostate[which(prostate$set==1),]), replace=TRUE)
  prostate.train.temp <- prostate [resamp, c(2:10)]
  prostate.test.temp <- prostate [-unique(resamp), c(2:10)]
  x.train.temp <- prostate[resamp, c(2:9)]
  y.train.temp <- prostate[resamp, 10]
  y.test.temp <- prostate [-unique(resamp),10]
  x.test.temp <- prostate [-unique(resamp),c(2:9)]
  
  
  for (j in 1:nrow(combinations.matrix.p)) {
    bart.temp <- bart(y.train=y.train.temp, x.train=x.train.temp, x.test=x.test.temp,
                      k=combinations.matrix.p[j,1], ntree=combinations.matrix.p[j,2], sigdf=combinations.matrix.p[j,3], 
                      sigquant=combinations.matrix.p[j,4], verbose=FALSE)
    mspr.bart.temp <- mean((y.test.temp - bart.temp$yhat.test.mean)^2)
    
    if (mspr.bart.temp < MSE.final.temp) {
      MSE.final.temp <- mspr.bart.temp
      index.temp <- j
    }
    
    best.combination.p[r,] <- c(r,combinations.matrix.p[index.temp,1],combinations.matrix.p[index.temp,2],
                                combinations.matrix.p[index.temp,3],combinations.matrix.p[index.temp,4],
                                MSE.final.temp)
  }
}
  
  
min.mspr.temp.index <- which.min(best.combination.p[,6])
best.k <- best.combination.p[min.mspr.temp.index,2]
best.ntree <- best.combination.p[min.mspr.temp.index,3]
best.sigdf <- best.combination.p[min.mspr.temp.index,4]
best.sigquant <- best.combination.p[min.mspr.temp.index,5]

bart.best <- bart(y.train=y.train, x.train=x.train, x.test=x.test,
                  k=best.k, ntree=best.ntree, sigdf=best.sigdf, 
                  sigquant=best.sigquant, verbose=FALSE)

bart.best
summary(bart.best)


mspr.final <- mean((y.test - bart.best$yhat.test.mean)^2)
mse.final <- mean((y.train - bart.best$yhat.train.mean)^2)
boxplot.matrix(x=bart.best$varcount, las=2, names = c("lcavol","lweight","age","lbph","svi","lcp","gleason","pgg45"),main="var importance")  
  
  
  
  
  
  
  