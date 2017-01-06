#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(randomForest)


prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

set.seed(120401002)
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

y.train <- prostate[which(prostate$set==1),10]
x.train <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.test <- prostate[which(prostate$set==2),10]
x.test <- as.matrix(prostate[which(prostate$set==2),c(2:9)])
samplesize <- c(20,30,40,50)

error.mat <- matrix(NA, nrow=16, ncol=4)
colnames ( error.mat ) <- c("sampsize","mvalues","ooberror","mspe")

counter <- 1
for (s in samplesize) {
  prostate.rf.1 <- randomForest(data=prostate[which(prostate$set==1),c(2:10)], 
                                lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, importance=TRUE, ntree=5000, 
                                mtry=1,sampsize=s, replace=TRUE, keep.forest=TRUE)
  prostate.rf.1
  importance(prostate.rf.1)
  varImpPlot(prostate.rf.1)
  
  prostate.rf.1.pred <- predict(prostate.rf.1,newdata=prostate[which(prostate$set==2),c(2:10)])
  oob.rf.1 <- mean((y.train - prostate.rf.1$predicted)^2)
  mspe.rf.1 <- mean((y.test - prostate.rf.1.pred)^2)
  error.mat[counter,] <- c(s,"1",oob.rf.1,mspe.rf.1)
  counter <- counter + 1
  
  
  prostate.rf.p3 <- randomForest(data=prostate[which(prostate$set==1),c(2:10)], 
                                 lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, importance=TRUE, ntree=5000, 
                                 mtry=3,sampsize=s, replace=TRUE, keep.forest=TRUE)
  
  prostate.rf.p3
  importance(prostate.rf.p3)
  varImpPlot(prostate.rf.p3)
  
  prostate.rf.p3.pred <- predict(prostate.rf.p3,newdata=prostate[which(prostate$set==2),c(2:10)])
  oob.rf.p3 <- mean((y.train - prostate.rf.p3$predicted)^2)
  mspe.rf.p3 <- mean((y.test - prostate.rf.p3.pred)^2)
  error.mat[counter,] <- c(s,"p/3",oob.rf.p3,mspe.rf.p3)
  counter <- counter + 1
  
  prostate.rf.2p3 <- randomForest(data=prostate[which(prostate$set==1),c(2:10)], 
                                  lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, importance=TRUE, ntree=5000, 
                                  mtry=6,sampsize=s, replace=TRUE, keep.forest=TRUE)
  prostate.rf.2p3
  importance(prostate.rf.2p3)
  varImpPlot(prostate.rf.2p3)
  
  prostate.rf.2p3.pred <- predict(prostate.rf.2p3,newdata=prostate[which(prostate$set==2),c(2:10)])
  oob.rf.2p3 <- mean((y.train - prostate.rf.2p3$predicted)^2)
  mspe.rf.2p3 <- mean((y.test - prostate.rf.2p3.pred)^2)
  error.mat[counter,] <- c(s,"2p/3",oob.rf.2p3,mspe.rf.2p3)
  counter <- counter + 1
  
  prostate.rf.p <- randomForest(data=prostate[which(prostate$set==1),c(2:10)], 
                                lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, importance=TRUE, ntree=5000, 
                                mtry=9,sampsize=s, replace=TRUE, keep.forest=TRUE)
  prostate.rf.p
  importance(prostate.rf.p)
  varImpPlot(prostate.rf.p)
  
  
  prostate.rf.p.pred <- predict(prostate.rf.p,newdata=prostate[which(prostate$set==2),c(2:10)])
  oob.rf.p <- mean((y.train - prostate.rf.p$predicted)^2)
  mspe.rf.p <- mean((y.test - prostate.rf.p.pred)^2)
  error.mat[counter,] <- c(s,"p",oob.rf.p,mspe.rf.p)
  counter <- counter + 1
}


### fitting with nodesize = 2.5 and nodesize = 10
prostate.rf.2.5 <- randomForest(data=prostate[which(prostate$set==1),c(2:10)], 
                              lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, importance=TRUE, ntree=5000, 
                              mtry=3,nodesize=2.5,keep.forest=TRUE)
prostate.rf.2.5.pred <- predict(prostate.rf.2.5,newdata=prostate[which(prostate$set==2),c(2:10)])
oob.rf.2.5 <- mean((y.train - prostate.rf.2.5$predicted)^2)
mspe.rf.2.5 <- mean((y.test - prostate.rf.2.5.pred)^2)


prostate.rf.10 <- randomForest(data=prostate[which(prostate$set==1),c(2:10)], 
                                lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, importance=TRUE, ntree=5000, 
                                mtry=3,nodesize=10,keep.forest=TRUE)
prostate.rf.10.pred <- predict(prostate.rf.10,newdata=prostate[which(prostate$set==2),c(2:10)])
oob.rf.10 <- mean((y.train - prostate.rf.10$predicted)^2)
mspe.rf.10 <- mean((y.test - prostate.rf.10.pred)^2)