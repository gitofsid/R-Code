#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(BayesTree)

R <- 20
set.seed (890987665)

abalone <-  read.table("abalone.csv", header=TRUE, sep=",", na.strings=" ")
abalone <- abalone[(0 < abalone$Height)&(abalone$Height < 0.5),]
abalone <- abalone[,c(9,1:8)]
head(abalone)

MSPE.bart <- matrix (NA , nrow =R, ncol=1)
colnames ( MSPE.bart ) <- c("bartdef")
rMSPE.bart <- matrix (NA , nrow =R, ncol=1)
colnames ( rMSPE.bart ) <- c("bartdef")

counter <- 1
for (r in 1:R) { 
	  new <- ifelse ( runif (n= nrow ( abalone )) <.75 , yes =1, no =2)
  	y.1n <- as.numeric(abalone[ which ( new == 1) , 1])
  	x.1n <- data.matrix ( abalone[ which ( new == 1), c(2:9)])
  	y.2n <- as.numeric(abalone [ which ( new == 2) , 1])
  	x.2n <- data.matrix ( abalone[ which ( new == 2), c(2:9)])

  	bart.abalone.1 <- bart(y.train=y.1n, x.train=x.1n, x.test=x.2n)
  	MSPE.bart[counter,] <- mean((y.2n - bart.abalone.1$yhat.test.mean)^2)

  	counter <- counter + 1

}

minimum_test.bart <- apply(X=MSPE.bart, MARGIN=2, FUN=min)

counter <- 1
for (r in 1:R){
  rMSPE.bart[counter,] <- MSPE.bart[counter,] / minimum_test.bart
  counter <- counter + 1
}

MSPE.temp <- cbind(MSPE.temp,MSPE.bart)
rMSPE.temp <- cbind(rMSPE.temp,rMSPE.bart)

## plots for bart
x11(pointsize=9)
boxplot(sqrt(MSPE.temp), at=1:17, las=2, xlim = c(0,18), names = c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3", "mars_1","mars_2","mars_3","mars_best","nnet","bestboost","boostingdef","bartdef"), main="boxplot root MSPE")

x11(pointsize=9)
boxplot(sqrt(rMSPE.temp), at=1:17, las=2, xlim = c(0,18), names = c("full","cp.min","cp.1se","ols", "allbic", "lassomin", "bmabic", "gam", "ppr3","mars_1","mars_2","mars_3","mars_best","nnet","bestboost","boostingdef","bartdef"), main="boxplot relative root MSPE")

