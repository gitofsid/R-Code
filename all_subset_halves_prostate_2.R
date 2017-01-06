#################################################
## Name        -   Siddharth Baronia       ######
#################################################

prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)
# Splitting data in half using random uniform selection to make two "set"s.

#set.seed(120401002)
set.seed(9267926) #uncomment for part b and comment seed above

prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1) # randomly assign to one half

library(leaps)
# All subsets regression using the "regsubsets" function from "leaps"
#  Note: default is to limit to 8-variable models.  Add nvmax argument to increase.
allsub1 <- regsubsets(x=prostate[which(prostate$set==1),2:9], 
                      y=prostate[which(prostate$set==1),10], nbest=1) # x from lcalvol to pgg45 and y is lpsa
allsub2 <- regsubsets(x=prostate[which(prostate$set==2),2:9], 
                      y=prostate[which(prostate$set==2),10], nbest=1)

# Store summary() so we can see BICs (not comparable across different data sets)
summ.1 <- summary(allsub1)
summ.2 <- summary(allsub2)

summ.1
summ.2

names(summ.1)
summ.1$bic
summ.2$bic

# Plot of results in a special form
win.graph(h=7, w=10, pointsize=12)
par(mfrow=c(1,2))
plot(allsub1, main="All Subsets on half of Prostate data")
plot(allsub2, main="All Subsets on other half of Prostate data")

# Fitting the models in succession from smallest to largest.  
# Fit one-var model. then update to 2-var model.  Could keep going.
# Each time computing sample-MSE (sMSE), BIC, and mean squared pred. error (MSPE). 

results1 <- matrix(data=NA, nrow=9, ncol=4)
mod1 <- lm(lpsa ~ 1, data=prostate[which(prostate$set==1),])
sMSE <- summary(mod1)$sigma^2
BIC <- extractAIC(mod1, k=log(nrow(prostate[which(prostate$set==1),])))
pred2 <- predict(mod1, newdata=prostate[which(prostate$set==2),])
MSPE <- mean((pred2-prostate[which(prostate$set==2),]$lpsa)^2)
results1[1,] <- c(0, sMSE, BIC[2], MSPE)

#Get rid of superfluous variables so that I can call the right variables into the data set each time.
# Also move response to 1st column to be included every time below.
prostate2 <- prostate[,c(10,2:9)]


for(v in 1:8){
  mod1 <- lm(lpsa ~ ., data=prostate2[which(prostate$set==1), summ.1$which[v,]])
  sMSE <- summary(mod1)$sigma^2
  BIC <- extractAIC(mod1, k=log(nrow(prostate2[which(prostate$set==1),])))
  pred2 <- predict(mod1, newdata=prostate2[which(prostate$set==2),])
  MSPE <- mean((pred2-prostate2[which(prostate$set==2),]$lpsa)^2)
  results1[v+1,] <- c(v, sMSE, BIC[2], MSPE)
}

results1


# All 3 plots together
x11(width=10,height=5,pointsize=18)
par(mfrow=c(1,3))
plot(x=results1[,1], y=results1[,2], xlab="Vars in model", ylab="sample-MSE",
     main="SampleMSE vs Vars: 1st", type="b")
plot(x=results1[,1], y=results1[,3], xlab="Vars in model", ylab="BIC",
     main="BIC vs Vars: 1st", type="b")
plot(x=results1[,1], y=results1[,4], xlab="Vars in model", ylab="MSPE",
     main="MSPE vs Vars: 1st", type="b")

##########
# Repeat for second data set


# Fitting the models in succession from smallest to largest.  
# Fit one-var model. then update to 2-var model.  Could keep going.
# Each time computing sample-MSE (sMSE), BIC, and mean squared pred. error (MSPE). 

results2 <- matrix(data=NA, nrow=9, ncol=4)
mod1 <- lm(lpsa ~ 1, data=prostate[which(prostate$set==2),])
sMSE <- summary(mod1)$sigma^2
BIC <- extractAIC(mod1, k=log(nrow(prostate[which(prostate$set==2),])))
pred1 <- predict(mod1, newdata=prostate[which(prostate$set==1),])
MSPE <- mean((pred1-prostate[which(prostate$set==1),]$lpsa)^2)
results2[1,] <- c(0, sMSE, BIC[2], MSPE)

#Get rid of superfluous variables so that I can call the right variables into the data set each time.
# Also move response to 1st column to be included every time below.
prostate2 <- prostate[,c(10,2:9)]


for(v in 1:8){
  mod1 <- lm(lpsa ~ ., data=prostate2[which(prostate$set==2), summ.2$which[v,]])
  sMSE <- summary(mod1)$sigma^2
  BIC <- extractAIC(mod1, k=log(nrow(prostate2[which(prostate$set==2),])))
  pred1 <- predict(mod1, newdata=prostate2[which(prostate$set==1),])
  MSPE <- mean((pred1-prostate2[which(prostate$set==1),]$lpsa)^2)
  results2[v+1,] <- c(v, sMSE, BIC[2], MSPE)
}

results2


# All 3 plots together
x11(width=10,height=5,pointsize=18)
par(mfrow=c(1,3))
plot(x=results2[,1], y=results2[,2], xlab="Vars in model", ylab="sample-MSE",
     main="SampleMSE vs Vars: 2nd", type="b")
plot(x=results2[,1], y=results2[,3], xlab="Vars in model", ylab="BIC",
     main="BIC vs Vars: 2nd", type="b")
plot(x=results2[,1], y=results2[,4], xlab="Vars in model", ylab="MSPE",
     main="MSPE vs Vars: 2nd", type="b")


# Results - v1=var, v2=sMSE (train), v3=BIC, v4=MSPE (test)
index1 <- which.min(results1[,3]) # minimum BIC
no.of.var.1 <- results1[index1,1]
sMSE.1 <- results1[index1,2]
BIC.1 <- results1[index1,3]
MSPE.1 <- results1[index1,4]
var.1 <- summ.1$which[no.of.var.1,]
# print
no.of.var.1
sMSE.1
BIC.1
MSPE.1
var.1

index2 <- which.min(results2[,3]) # minimum BIC
no.of.var.2 <- results2[index2,1]
sMSE.2 <- results2[index2,2]
BIC.2 <- results2[index2,3]
MSPE.2 <- results2[index2,4]
var.2 <- summ.2$which[no.of.var.2,]
# print
no.of.var.2
sMSE.2
BIC.2
MSPE.2
var.2



