#################################################
## Name        -   Siddharth Baronia       ######
#################################################


abalone <-  read.table("abalone.csv", header=TRUE, sep=",", na.strings=" ")
abalone <- abalone[(0 < abalone$Height)&(abalone$Height < 0.5),]
head(abalone)

library(dummies)
sex.dummy <- dummy(abalone$Sex,fun = as.integer)

abalone$Female <- sex.dummy[,1]
abalone$Male <- sex.dummy[,3]

abalone.final <- abalone[,c(9,2:8,10:11)]
head(abalone.final)

R =20
set.seed (890987665)


MSPE <- matrix (NA , nrow =R, ncol =4)
colnames ( MSPE ) <- c("gam","ppr1","ppr2","ppr3")
rMSPE <- matrix (NA , nrow =R, ncol =4)
colnames(resc_MSPE) <- colnames ( MSPE )

counter <- 1
for (r in 1:R){
  new <- ifelse ( runif (n= nrow ( abalone.final )) <.75 , yes =1, no =2)
  y.1n <- abalone.final [ which ( new ==1) , 1]
  x.1n <- as.matrix ( abalone.final [ which ( new == 1) , -1])
  y.2n <- abalone.final [ which ( new ==2) , 1]
  x.2n <- as.matrix ( abalone.final [ which ( new == 2) , -1])
  x.2n_dframe <- data.frame(abalone.final [ which ( new == 2) , -1])
  
  gam1 <- gam(data=abalone.final [ which ( new ==1),], Rings~s(Length)+s(Diameter)+s(Height)+s(Whole)
              +s(Shucked)+s(Viscera)+s(Shell), family=gaussian(link=identity))
  summary(gam1)
  pred_gam1 <- predict(gam1,newdata=x.2n_dframe)
  MSPE [ counter ,1] <- mean((pred_gam1-y.2n)^2)
  
  ppr1 <- ppr(data=abalone.final [ which ( new ==1),], Rings~Length+Diameter+Height+Whole
              +Shucked+Viscera+Shell, nterms=1, optlevel=3) 
  summary(ppr1)
  pred_ppr1 <- predict(ppr1,newdata=x.2n)
  MSPE [ counter ,2] <- mean((pred_ppr1-y.2n)^2)
  
  ppr2 <- ppr(data=abalone.final [ which ( new ==1),], Rings~Length+Diameter+Height+Whole
              +Shucked+Viscera+Shell, nterms=2, optlevel=3) 
  summary(ppr2)
  pred_ppr2 <- predict(ppr2,newdata=x.2n)
  MSPE [ counter ,3] <- mean((pred_ppr2-y.2n)^2)
  
  ppr3 <- ppr(data=abalone.final [ which ( new ==1),], Rings~Length+Diameter+Height+Whole
              +Shucked+Viscera+Shell, nterms=3, optlevel=3) 
  summary(ppr3)
  pred_ppr3 <- predict(ppr3,newdata=x.2n)
  MSPE [ counter ,4] <- mean((pred_ppr3-y.2n)^2)
  
  counter <- counter +1
  
}

minimum_test <- apply(X=MSPE, MARGIN=1, FUN=min)
counter <- 1
for (r in 1:R){
  rMSPE [counter,] <- MSPE[counter,] / minimum_test[counter]
  counter <- counter + 1
}

boxplot(sqrt(MSPE), at=1:4, las=2, xlim = c(0,5), ylim = c(1.9,2.4), names = c("gam", "ppr1","ppr2","ppr3"), main="boxplot root MSPE")
boxplot(sqrt(rMSPE), at=1:4, las=2, xlim = c(0,5), ylim = c(1,1.08), names = c("gam", "ppr1","ppr2","ppr3"), main="boxplot relative root MSPE")
