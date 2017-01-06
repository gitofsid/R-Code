#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(nnet)

prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

y.train <- prostate[which(prostate$set==1),10]
x.train.un <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.test <- prostate[which(prostate$set==2),10]
x.test.un <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

# scaling the data to lie withing [0,1]
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

# scaled training and testing set
x.train <- rescale(x.train.un,x.train.un)
x.test <- rescale(x.test.un,x.train.un)


# neural nets that runs 100 times on 
# size = (1,2,3,4,5) and decay = (0,0.001,0.01,0.1,1)
# defining generic function
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

siz <- c(1,2,3,4,5)
dec <- c(0,0.001,0.01,0.1,1)
sMSE <- matrix(NA, nrow=length(siz), ncol=length(dec), 
              dimnames=list(c("siz_1","siz_2","siz_3","siz_4","siz_5"),
                            c("dec_0","dec_0.001","dec_0.01","dec_0.1","dec_1")))
MSPR <- matrix(NA, nrow=length(siz), ncol=length(dec), 
               dimnames=list(c("siz_1","siz_2","siz_3","siz_4","siz_5"),
                             c("dec_0","dec_0.001","dec_0.01","dec_0.1","dec_1")))

# part a and b
# calculate sMSE and MSPE for every variance of size and 
i <- 1
for (s in siz) {
  j <- 1
  for (d in dec) {
    nn_temp <- run.nn(s,d)
    sMSE[i,j] <- nn_temp$value/nrow(x.train)
    MSPR[i,j] <- mean((y.test - predict(nn_temp,x.test))^2)
    j <- j + 1
  }
  i <- i + 1
}

# Results
sMSE
MSPR