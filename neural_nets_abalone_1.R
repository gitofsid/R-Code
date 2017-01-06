#################################################
## Name        -   Siddharth Baronia       ######
################################################# 

abalone <-  read.table("abalone.csv", header=TRUE, sep=",", na.strings=" ")
abalone <- abalone[(0 < abalone$Height)&(abalone$Height < 0.5),]
head(abalone)

library(dummies)
library(nnet)
sex.dummy <- dummy(abalone$Sex,fun = as.integer)

abalone$Female <- sex.dummy[,1]
abalone$Male <- sex.dummy[,3]

abalone.final <- abalone[,c(9,2:8,10:11)]
head(abalone.final)

R =20
set.seed (890987665)

# scaling the data to lie withing [0,1]
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
     a <- min(x2[,col])
     b <- max(x2[,col])
     x1[,col] <- (x1[,col]-a)/(b-a)
     }
  x1
}

run.nn <- function(siz, dec, xdata, ydata){
  MSE.final.1 <- 9e99 # initialize to a big number
  #  check <- MSE.final
  for(i in 1:10){
    nn.1 <- nnet(y=ydata, x=xdata, linout=TRUE, size=siz, decay=dec, maxit=500, trace=FALSE)
    MSE.1 <- nn.1$value/nrow(xdata)
    if(MSE.1 < MSE.final.1){ 
      MSE.final.1 <- MSE.1
      nn.final.1 <- nn.1
    }
    #    check <- c(check,MSE.final)
  }
  #  check
  nn.final.1
}
 
 
 
siz <- c(1,2,3,4,5)
dec <- c(0,0.001,0.01,0.1,1)
nets=10 # Number of nnets per setting.
MSPR.temp <- matrix(NA, nrow=length(siz)*length(dec), ncol=R+2)
best.s.d <- matrix(NA, nrow=R, ncol=2)

 
for (r in 1:R){
   new <- ifelse ( runif (n= nrow ( abalone.final )) <.75 , yes =1, no =2)
   resamp <- sample.int(n=nrow(abalone.final[which(new==1),]), size=nrow(abalone.final[which(new==1),]), replace=TRUE)
   y.train.temp <- abalone.final [resamp,1]
   x.train.temp_un <- abalone.final [resamp, -1]
   x.train.temp <- rescale(x.train.temp_un,x.train.temp_un)
   y.test.temp <- abalone.final [-unique(resamp),1]
   x.test.temp_un <- abalone.final [-unique(resamp), -1]
   x.test.temp <- rescale(x.test.temp_un,x.train.temp_un)
   
   y.train <- abalone.final [which(new==1),1]
   x.train.un <- abalone.final [which(new==1),-1]
   x.train <- rescale(x.train.un,x.train.un)
   y.test <- abalone.final [which(new==2),1]
   x.test.un <- abalone.final [which(new==2),-1]
   x.test <- rescale(x.test.un,x.train.un)
  
   # Set counter for storage of results
   qq <- 1
   # Cycle over all parameter values
   for(s in siz){
     for(d in dec){
       MSPR.temp[qq,1:2] <- c(s,d)
       # Run nnet and get MSPE and MSE from run
       MSEmin <- 9e99
       for(i in 1:nets){
         nn <- nnet(y=y.train.temp, x=x.train.temp, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
         MS <- nn$value/nrow(x.train.temp)
         if(MS < MSEmin){ 
           MSEmin <- MS
           nn.final <- nn
         }
       }
       # Save results in new column of matrix
       MSPR.temp[qq, r+2] <- mean((y.test.temp - predict(nn.final, newdata=x.test.temp))^2)
       # Increment counter for next row
       qq <- qq + 1
     }
   }
   
   best.temp.index <- which.min(MSPR.temp[,r+2])
   s.best <- MSPR.temp[best.temp.index,1]
   d.best <- MSPR.temp[best.temp.index,2]
   best.s.d[r,] <- c(s.best,d.best) # best s and d for each iteration in R
}  
 

############################
### plot for part a ########
############################
siz <- c(1,2,3,4,5)
dec <- c(0,0.001,0.01,0.1,1) 
MSPR.a.all <- matrix(NA, nrow=length(siz)*length(dec), ncol=R+2)

for (r in 1:R){
  new <- ifelse ( runif (n= nrow ( abalone.final )) <.75 , yes =1, no =2)
  y.train <- abalone.final [which(new==1),1]
  x.train.un <- abalone.final [which(new==1),-1]
  x.train <- rescale(x.train.un,x.train.un)
  y.test <- abalone.final [which(new==2),1]
  x.test.un <- abalone.final [which(new==2),-1]
  x.test <- rescale(x.test.un,x.train.un)
  
  qq <- 1
  # Cycle over all parameter values
  for(s in siz){
    for(d in dec){
      MSPR.a.all[qq,1:2] <- c(s,d)
      nn.final.a <- run.nn(s,d,x.train,y.train)
      # Save results in new column of matrix
      MSPR.a.all[qq, r+2] <- mean((y.test - predict(nn.final.a, newdata=x.test))^2)
      # Increment counter for next row
      qq <- qq + 1
    }
  }
}

siz.dec.a.all <- paste(MSPR.a.all[,1],MSPR.a.all[,2])
x11(pointsize=9)
boxplot.matrix(x=sqrt(MSPR.a.all[,-c(1,2)]), ylim=c(1,3), use.cols=FALSE, names=siz.dec.a.all, las=2, main="rMSPE for all combination")

MSPR.best.a <- matrix(NA, nrow=R, ncol=R+2)
MSPR.best.a[,c(1,2)] <- best.s.d[,c(1,2)]
for (r in 1:R){
  new <- ifelse ( runif (n= nrow ( abalone.final )) <.75 , yes =1, no =2)
  y.train <- abalone.final [which(new==1),1]
  x.train.un <- abalone.final [which(new==1),-1]
  x.train <- rescale(x.train.un,x.train.un)
  y.test <- abalone.final [which(new==2),1]
  x.test.un <- abalone.final [which(new==2),-1]
  x.test <- rescale(x.test.un,x.train.un)
  
  for (i in 1:length(best.s.d[,1])) {
    nn.best.a <- run.nn(best.s.d[i,1],best.s.d[i,2],x.train,y.train)
    MSPR.best.a[i,r+2] <- mean((y.test - predict(nn.best.a,x.test))^2)
  }
}

siz.dec.a.best <- paste(MSPR.best.a[,1],MSPR.best.a[,2])
x11(pointsize=9)
boxplot.matrix(x=sqrt(MSPR.best.a[,-c(1,2)]), ylim=c(1,3), use.cols=FALSE, names=siz.dec.a.best, las=2, main = "rMSPE for best combination per split")


######################
### part b plots  ####
######################
best.a.all <- apply(X=MSPR.a.all[,-c(1,2)], MARGIN=2, FUN=min)
x11(pointsize=9)
boxplot.matrix(x=sqrt(t(t(MSPR.a.all[,-c(1:2)])/best.a.all)), ylim=c(1,2), use.cols=FALSE, names=siz.dec.a.all,las=2, main="rrMSPE for all combination")

best.a.best <- apply(X=MSPR.best.a[,-c(1,2)], MARGIN=2, FUN=min)
x11(pointsize=9)
boxplot.matrix(x=sqrt(t(t(MSPR.best.a[,-c(1:2)])/best.a.best)), ylim=c(1,2), use.cols=FALSE, names=siz.dec.a.best,las=2, main="rrMSPE for best combination per split")

#####################
## part c plots #####
#####################
best.i <- which.min(best.a.best) + 2
MSPR.best.f <- t(MSPR.best.a[which.min(MSPR.best.a[,best.i]),])
siz.dec.best <- paste(MSPR.best.f[,1],MSPR.best.f[,2])
x11(pointsize=9)
boxplot.matrix(x=sqrt(t(MSPR.best.f[,-c(1:2)])), ylim=c(1,2.6), use.cols=FALSE, names=siz.dec.best, las=2, main = "rMSPE for best model overall")

#####################
### part d plots ####
#####################
minimum_test <- min(MSPR.best.f[,-c(1,2)])
counter <- 1
MSPR.best.r <- matrix(NA, nrow=1, ncol=R+2)
MSPR.best.r[,c(1,2)] <- MSPR.best.f[,c(1,2)]
for (r in 1:R){
  MSPR.best.r [,r+2] <- MSPR.best.f[,r+2] / minimum_test
  counter <- counter + 1
}
x11(pointsize=9)
boxplot.matrix(x=sqrt(t(MSPR.best.r[,-c(1:2)])),  ylim=c(1,1.25), use.cols=FALSE, names=siz.dec.best, las=2, main = "usual rrMSPE for best model overall")
