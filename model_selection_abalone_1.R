#################################################
## Name        -   Siddharth Baronia       ######
#################################################

abalone <-  read.table("abalone.csv", header=FALSE, sep=",", na.strings=" ")
names(abalone) <- c("sex","length","diameter","height","wweight","sweight","vweight","shweight","rings")
abalone <- abalone[(0 < abalone$height)&(abalone$height < 0.5),]
head(abalone)

library(dummies)
sex.dummy <- dummy(abalone$sex,fun = as.integer)

abalone$female <- sex.dummy[,1]
abalone$male <- sex.dummy[,3]

abalone.final <- abalone[,c(9,2:8,10:11)]
head(abalone.final)

# a - scatterplot matrix
plot(abalone.final, main="Scatterplot Matrix")

# b - seed and split
set.seed(29003092) 
abalone.final$set <- ifelse(U <- runif(n=nrow(abalone.final))>0.75, yes=2, no=1)

initial.tr.fit <- lm(data=abalone.final[which(abalone.final$set==1),], formula=rings ~ 1)
final.tr.fit <- lm(data=abalone.final[which(abalone.final$set==1),], formula=rings ~ .)
initial.te.fit <- lm(data=abalone.final[which(abalone.final$set==2),], formula=rings ~ 1)
final.te.fit <- lm(data=abalone.final[which(abalone.final$set==2),], formula=rings ~ .)

y.train <- abalone.final[which(abalone.final$set==1),1]
x.train <- as.matrix(abalone.final[which(abalone.final$set==1),c(2:10)])
y.test <- abalone.final[which(abalone.final$set==2),1]
x.test <- as.matrix(abalone.final[which(abalone.final$set==2),c(2:10)])

# c - forward stepwise regression with BIC
step.tr.forward <- step(object=initial.tr.fit, scope=list(upper=final.tr.fit), direction = "forward",
              k = log(nrow(abalone.final[which(abalone.final$set==1),])))
summary(step.tr.forward)
pred.tr.tr.fw <- predict(step.tr.forward, newdata=abalone.final[which(abalone.final$set==1),])
pred.tr.te.fw <- predict(step.tr.forward, newdata=abalone.final[which(abalone.final$set==2),])
sMSE.tr.fw <- mean((y.train - pred.tr.tr.fw)^2)
MSPE.tr.fw <- mean((y.test - pred.tr.te.fw)^2)

# d - step wise regression with BIC
step.tr <- step(object=initial.tr.fit, scope=list(upper=final.tr.fit),
                        k = log(nrow(abalone.final[which(abalone.final$set==1),])))
summary(step.tr)
pred.tr.tr <- predict(step.tr, newdata=abalone.final[which(abalone.final$set==1),])
pred.tr.te <- predict(step.tr, newdata=abalone.final[which(abalone.final$set==2),])
sMSE.tr <- mean((y.train - pred.tr.tr)^2)
MSPE.tr <- mean((y.test - pred.tr.te)^2)

# e - forward selection with 
step.tr.forward.nopenalty <- step(object=initial.tr.fit, scope=list(lower=initial.tr.fit, upper=final.tr.fit), 
                                  k=0, direction = "forward")
summary(step.tr.forward.nopenalty)
pred.tr.tr.fw.np <- predict(step.tr.forward.nopenalty, newdata=abalone.final[which(abalone.final$set==1),])
pred.tr.te.fw.np <- predict(step.tr.forward.nopenalty, newdata=abalone.final[which(abalone.final$set==2),])
sMSE.tr.fw.np <- mean((y.train - pred.tr.tr.fw.np)^2)
MSPE.tr.fw.np <- mean((y.test - pred.tr.te.fw.np)^2)

# forward selection all models
results.fw.np <- matrix(data=NA, nrow=9, ncol=4)
for(v in 1:9){
  vars <- c(names(abalone.final)[2:v+1])
  final.tr.fit.1 <- lm(data=abalone.final[which(abalone.final$set==1),], formula=as.formula(paste("rings~",paste(vars, collapse = "+"))))
  step.tr.forward.nopenalty.1 <- step(object=initial.tr.fit, scope=list(lower=initial.tr.fit, upper=final.tr.fit.1), 
                                    k=0, direction = "forward")
  pred.tr.tr.fw.np.1 <- predict(step.tr.forward.nopenalty.1, newdata=abalone.final[which(abalone.final$set==1),])
  pred.tr.te.fw.np.1 <- predict(step.tr.forward.nopenalty.1, newdata=abalone.final[which(abalone.final$set==2),])
  sMSE.tr.fw.np.1 <- mean((y.train - pred.tr.tr.fw.np.1)^2)
  MSPE.tr.fw.np.1 <- mean((y.test - pred.tr.te.fw.np.1)^2)
  BIC.tr.fw.np.1 <- step.tr.forward.nopenalty.1$anova$AIC
  results.fw.np[v,] <- c(v, sMSE.tr.fw.np.1, BIC.tr.fw.np.1[2], MSPE.tr.fw.np.1)
}

# f - all subset best model
library(leaps)
allsub <- regsubsets(x=x.train, y=y.train, nbest=1)
summ.allsub <- summary(allsub)
summ.allsub$bic
min_bic_index <- which.min(summ.allsub$bic) # find the index of minimum bic
allbic <- lm(rings ~ ., data=abalone.final [which (abalone.final$set==1), summ.allsub$which[min_bic_index,]])
summary(allbic)
sMSE.allsub <- summary(allbic)$sigma^2
pred.allsub <- predict(allbic, newdata=abalone.final [which (abalone.final$set==2),])
MSPE.allsub <- mean((pred.allsub-abalone.final [which (abalone.final$set==2),]$rings)^2)

# allsubset all models
results.allsub <- matrix(data=NA, nrow=9, ncol=4)
for(v in 1:9){
  allbic.1 <- lm(rings ~ ., data=abalone.final [which (abalone.final$set==1), summ.allsub$which[v,]])
  sMSE.allsub.1 <- summary(allbic.1)$sigma^2
  BIC.1 <- extractAIC(allbic.1, k=log(nrow(abalone.final [which (abalone.final$set==1),])))
  pred.allsub.1 <- predict(allbic.1, newdata=abalone.final [which (abalone.final$set==2),])
  MSPE.allsub.1 <- mean((pred.allsub.1-abalone.final [which (abalone.final$set==2),]$rings)^2)
  results.allsub[v,] <- c(v, sMSE.allsub.1, BIC.1[2], MSPE.allsub.1)
}


# Results
step.tr.forward$coefficients # best model variable for c
sMSE.tr.fw # sMSE for forward stepwise c
MSPE.tr.fw # MSPE for forward stepwise c

step.tr$coefficients # best model variable for stepwise reg d
sMSE.tr # sMSE for stepwise regression d
MSPE.tr # MSPE for stepwise regression d

step.tr.forward.nopenalty$coefficients # best model for foward with no penalty e
sMSE.tr.fw.np # sMSE no penalty e
MSPE.tr.fw.np # MSPE no penalty e
results.fw.np

allbic$coefficients # best model for allsubst f
sMSE.allsub # sMSE for allsub f
MSPE.allsub #MSPE for allsub f
results.allsub