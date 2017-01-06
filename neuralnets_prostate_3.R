#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(nnet)

prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

# scaling the data to lie withing [0,1]
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

run.nn <- function(siz, dec){
  MSE.final <- 9e99 # initialize to a big number
  #  check <- MSE.final
  for(i in 1:100){
    nn <- nnet(y=y.train, x=x.train, linout=TRUE, size=siz, decay=dec, maxit=500, trace=FALSE)
    MSE <- nn$value/nrow(x.train)
    if(MSE < MSE.final){ 
      MSE.final <- MSE
      nn.final <- nn
    }
    #    check <- c(check,MSE.final)
  }
  #  check
  nn.final
}

set.seed(120401002) 
siz <- c(1,2,3,4,5)
dec <- c(0,0.001,0.01,0.1,1)
reps=10 # Boot Reps, Feel free to increase this to tolerable value
nets=100 # Number of nnets per setting.
MSPR <- matrix(NA, nrow=length(siz)*length(dec), ncol=reps+2)
sMSE <- matrix(NA, nrow=length(siz)*length(dec), ncol=reps+2)

for(r in 1:reps){
  
  prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)
  
  y.train <- prostate[which(prostate$set==1),10]
  x.train.un <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
  y.test <- prostate[which(prostate$set==2),10]
  x.test.un <- as.matrix(prostate[which(prostate$set==2),c(2:9)])
  # scaled training and testing set
  x.train <- rescale(x.train.un,x.train.un)
  x.test <- rescale(x.test.un,x.train.un)
  
  # Set counter for storage of results
  qq <- 1
  # Cycle over all parameter values
  for(s in siz){
    for(d in dec){
      MSPR[qq,1:2] <- c(s,d)
      sMSE[qq,1:2] <- c(s,d)
      # Run nnet and get MSPE and MSE from run
      MSEmin <- 9e99
      for(i in 1:nets){
        nn <- nnet(y=y.train, x=x.train, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
        MS <- nn$value/nrow(x.train)
        if(MS < MSEmin){ 
          MSEmin <- MS
          nn.final <- nn
        }
      }
      # Save results in new column of matrix
      MSPR[qq, r+2] <- mean((y.test - predict(nn.final, newdata=x.test))^2)
      sMSE[qq, r+2] <- MSEmin
      # Increment counter for next row
      qq <- qq + 1
    }
  }
}


# Compute mean, minimum, and maximum
(MSPR.min <- cbind(MSPR[,1:2], apply(X=MSPR[,-c(1,2)], MARGIN=1, FUN=min)))
(MSPR.max <- cbind(MSPR[,1:2], apply(X=MSPR[,-c(1,2)], MARGIN=1, FUN=max)))

(sMSE.min <- cbind(sMSE[,1:2], apply(X=sMSE[,-c(1,2)], MARGIN=1, FUN=min)))
(sMSE.max <- cbind(sMSE[,1:2], apply(X=sMSE[,-c(1,2)], MARGIN=1, FUN=max)))

cbind(MSPR[,1:2],MSPR.max[,-c(1,2)])

# Plot results. 
siz.dec <- paste(MSPR[,1],MSPR[,2])
x11(pointsize=9)
boxplot.matrix(x=sqrt(MSPR[,-c(1,2)]), use.cols=FALSE, names=siz.dec, las=2, main="rMSPE for all combination")

x11(pointsize=9)
boxplot.matrix(x=sqrt(MSPR[which(MSPR.max[,3]<2),-c(1,2)]), use.cols=FALSE, names=siz.dec[which(MSPR.max[,3]<2)], las=2,main="rMSPE for all combination no extreme")

# Make plot relative to best
best <- apply(X=MSPR[,-c(1,2)], MARGIN=2, FUN=min)
x11(pointsize=9)
boxplot.matrix(x=sqrt(t(t(MSPR[which(MSPR.max[,3]<2),-c(1:2)])/best)), use.cols=FALSE, names=siz.dec[which(MSPR.max[,3]<2)], las=2, main="rMSPE for all combination relative to best")


### boundary extension
siz <- c(4,5,6,7,8,9)
dec <- c(1,1.5,2,2.5)
reps=10 # Boot Reps, Feel free to increase this to tolerable value
nets=100 # Number of nnets per setting.
MSPR1 <- matrix(NA, nrow=length(siz)*length(dec), ncol=reps+2)
sMSE1 <- matrix(NA, nrow=length(siz)*length(dec), ncol=reps+2)

for(r in 1:reps){
  
  prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)
  
  y.train <- prostate[which(prostate$set==1),10]
  x.train.un <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
  y.test <- prostate[which(prostate$set==2),10]
  x.test.un <- as.matrix(prostate[which(prostate$set==2),c(2:9)])
  # scaled training and testing set
  x.train <- rescale(x.train.un,x.train.un)
  x.test <- rescale(x.test.un,x.train.un)
  
  # Set counter for storage of results
  qq <- 1
  # Cycle over all parameter values
  for(s in siz){
    for(d in dec){
      MSPR1[qq,1:2] <- c(s,d)
      sMSE1[qq,1:2] <- c(s,d)
      # Run nnet and get MSPE and MSE from run
      MSEmin <- 9e99
      for(i in 1:nets){
        nn <- nnet(y=y.train, x=x.train, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
        MS <- nn$value/nrow(x.train)
        if(MS < MSEmin){ 
          MSEmin <- MS
          nn.final <- nn
        }
      }
      # Save results in new column of matrix
      MSPR1[qq, r+2] <- mean((y.test - predict(nn.final, newdata=x.test))^2)
      sMSE1[qq, r+2] <- MSEmin
      # Increment counter for next row
      qq <- qq + 1
    }
  }
}


# Compute mean, minimum, and maximum
(MSPR1.min <- cbind(MSPR1[,1:2], apply(X=MSPR1[,-c(1,2)], MARGIN=1, FUN=min)))
(MSPR1.max <- cbind(MSPR1[,1:2], apply(X=MSPR1[,-c(1,2)], MARGIN=1, FUN=max)))

(sMSE1.min <- cbind(sMSE1[,1:2], apply(X=sMSE1[,-c(1,2)], MARGIN=1, FUN=min)))
(sMSE1.max <- cbind(sMSE1[,1:2], apply(X=sMSE1[,-c(1,2)], MARGIN=1, FUN=max)))

cbind(MSPR1[,1:2],MSPR1.max[,-c(1,2)])

# Plot results. 
siz.dec.1 <- paste(MSPR1[,1],MSPR1[,2])
x11(pointsize=9)
boxplot.matrix(x=sqrt(MSPR1[,-c(1,2)]), use.cols=FALSE, names=siz.dec.1, las=2,main="rMSPE for all combination extension")

x11(pointsize=9)
boxplot.matrix(x=sqrt(MSPR1[which(MSPR1.max[,3]<2),-c(1,2)]), use.cols=FALSE, names=siz.dec.1[which(MSPR1.max[,3]<2)], las=2,main="rMSPE for all combination no extreme extension")

# Make plot relative to best
best <- apply(X=MSPR1[,-c(1,2)], MARGIN=2, FUN=min)
x11(pointsize=9)
boxplot.matrix(x=sqrt(t(t(MSPR1[which(MSPR1.max[,3]<2),-c(1:2)])/best)), use.cols=FALSE, names=siz.dec.1[which(MSPR1.max[,3]<2)], las=2,main="rMSPE for all combination relative to best extension")

# best comes out to be size = 4 and decay = 1
nn.best <- run.nn(4,1)
MSPE.best <- mean((y.test - predict(nn.best,x.test))^2)
