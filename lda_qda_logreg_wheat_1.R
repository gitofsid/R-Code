#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(nnet)
library(MASS)
library(car)

wheatData <- read.table("wheat.csv", header=TRUE, sep=",", na.strings=" ")
head(wheatData)

set.seed(67982193)
wheatData$classnum <- as.numeric(wheatData$class)
perm <- sample(x=nrow(wheatData))
set1.wheat <- wheatData[which(perm <= 200), -1]
set2.wheat <- wheatData[which(perm > 200), -1]

rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

set1.wheat.rescale <- data.frame(cbind(rescale(set1.wheat[,c(2:6,8)], set1.wheat[,c(2:6,8)]), class=set1.wheat$type))
set2.wheat.rescale <- data.frame(cbind(rescale(set2.wheat[,c(2:6,8)], set1.wheat[,c(2:6,8)]), class=set2.wheat$type))

#########################
## b - scatterplot  #####
#########################
x11(pointsize = 6)
class.col.wh <- ifelse(wheatData$type == "Healthy", y=393,n=
                ifelse(wheatData$type =="Sprout", y=68,n=203))

plot(wheatData[,c(3:9)],main="scatterplot matrix",col=colors()[class.col.wh])



##############################################
### c - run principal components analysis  ###
##############################################
wheatPCA <-  prcomp(x=set1.wheat[,c(2:6,8)], scale.=TRUE)
summary(wheatPCA)
wheatPCA
x11(pointsize = 9)
plot(wheatPCA,main="PCA on wheat")

xi.1.wheat <- data.frame(wheatPCA$x,class=set1.wheat$type)
model.fit.pc.wheat <- multinom(data=xi.1.wheat,formula=class ~ ., maxit=3000,trace=TRUE)
model.fit.pc.wheat
summary(model.fit.pc.wheat)

pred.class.pc1.wheat <- predict(model.fit.pc.wheat,newdata=xi.1.wheat)
pred.prob.pc1.wheat <- round(predict(model.fit.pc.wheat, newdata=xi.1.wheat, type="probs"),digits=3)
(pca.misclass.train.wheat <- mean(ifelse(pred.class.pc1.wheat == set1.wheat$type , yes=0, no=1)))


######################################
## d - lda on type                ####
######################################

# run lda on type, use numeric class, compare class means 
# plot results 
# report training and testing
# confusion matrix and interprest nature of misclassification


lda.fit.wheat <- lda(x=set1.wheat[,c(2:6,8)],grouping=set1.wheat$type)
lda.fit.wheat
summary(lda.fit.wheat)

set1s.wheat <- apply(set1.wheat[,c(2:6,8)], 2, scale)
lda.fit.wheat.s <- lda(x=set1s.wheat,grouping=set1.wheat$type)
lda.fit.wheat.s
summary(lda.fit.wheat.s)
lda.fit.wheat.s$means

x11(pointsize = 9)
plot(lda.fit.wheat,dimen = 1,type="histogram", main="first discriminant function lda")

pred.lda.wheat <- predict(lda.fit.wheat, newdata = set1.wheat[,c(2:6,8)])

# confusion matrix in training set
table(set1.wheat$type, pred.lda.wheat$class, dnn=c("observed_tr","predicted_tr"))

lda.pred.train.wheat <- pred.lda.wheat$class
lda.pred.test.wheat <- predict(lda.fit.wheat, newdata=set2.wheat[,c(2:6,8)])$class
(lmisclass.train.wheat <- mean(ifelse(lda.pred.train.wheat == set1.wheat$type, yes=0, no=1)))
(lmisclass.test.wheat <- mean(ifelse(lda.pred.test.wheat == set2.wheat$type, yes=0, no=1)))

# confusion matrix in testing set
table(set2.wheat$type, lda.pred.test.wheat, dnn=c("observed","predicted"))



###############################
#### e - qda            #######
###############################
# report trainig an ftesting error and compare
# confusion matrix

qda.fit.wheat <- qda(x=set1.wheat[,c(2:6,8)], grouping=set1.wheat$type)
qda.fit.wheat
summary(qda.fit.wheat)
qda.fit.wheat$means

qda.pred.train.wheat <- predict(qda.fit.wheat, newdata=set1.wheat[,c(2:6,8)])$class
qda.pred.test.wheat <- predict(qda.fit.wheat, newdata=set2.wheat[,c(2:6,8)])$class
(qmisclass.train.wheat <- mean(ifelse(qda.pred.train.wheat == set1.wheat$type, yes=0,no=1)))
(qmisclass.test.wheat <- mean(ifelse(qda.pred.test.wheat == set2.wheat$type, yes=0,no=1)))


# training set confusion matrix
table(set1.wheat$type, qda.pred.train.wheat, dnn=c("observed_tr","predicted_tr"))

# testing set confusion matrix
table(set2.wheat$type, qda.pred.test.wheat, dnn=c("observed_te","predicted_te"))


#################################
##### f - logistic regression ###
#################################

# converged in default
log.reg.fit.wheat <- multinom(data=set1.wheat.rescale, formula=class ~ ., trace=TRUE)
log.reg.fit.wheat
summ.log.reg <- summary(log.reg.fit.wheat)
summ.log.reg$coefficients

# LR tests
Anova(log.reg.fit.wheat)

# misclassification error
lreg.pred.class.wheat.1 <- predict(log.reg.fit.wheat, newdata=set1.wheat.rescale)
lreg.pred.prob.wheat.1 <- round(predict(log.reg.fit.wheat, newdata=set1.wheat.rescale, type="probs"),digits=3)

lreg.pred.class.wheat.2 <- predict(log.reg.fit.wheat, newdata=set2.wheat.rescale)
lreg.pred.prob.wheat.2 <- round(predict(log.reg.fit.wheat, newdata=set2.wheat.rescale, type="probs"),digits=3)

(lreg.misclass.train <- mean(ifelse(lreg.pred.class.wheat.1 == set1.wheat.rescale$class, yes=0, no=1)))
(lreg.misclass.test <- mean(ifelse(lreg.pred.class.wheat.2 == set2.wheat.rescale$class, yes=0, no=1)))


## training set confusion matrix
table(set1.wheat$type,lreg.pred.class.wheat.1, dnn=c("Observed_tr","Predicted_tr") )

## testing set confusion matrix
table(set2.wheat$type,lreg.pred.class.wheat.2, dnn=c("Observed_te","Predicted_te") )

x11(pointsize = 9)
plot(wheatData[,8], wheatData[,3],  ylab="density", xlab= "type", col=colors()[class.col.wh])

x11(pointsize = 9)
plot(wheatData[,8], wheatData[,6],  ylab="weight", xlab= "type", col=colors()[class.col.wh])

log.reg.fit.wheat$edf
