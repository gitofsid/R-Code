#################################################
## Name        -   Siddharth Baronia       ######
#################################################


library(nnet)
library(sm)
library(e1071)
library(klaR)
library(mgcv)


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

set1.wheat.healthy <- ifelse(set1.wheat$type=="Healthy", y=1, n=0)
set2.wheat.healthy <- ifelse(set2.wheat$type=="Healthy", y=1, n=0)

# i - logistic regreeion with density only
log.reg.fit.wheat.density <- multinom(data=set1.wheat.rescale, formula=set1.wheat.healthy ~ density, trace=TRUE)
log.reg.fit.wheat.density
summ.log.reg.density <- summary(log.reg.fit.wheat.density)
summ.log.reg.density$coefficients

lreg.pred.class.wheat.density.1 <- predict(log.reg.fit.wheat.density, newdata=set1.wheat.rescale)
lreg.pred.prob.wheat.density.1 <- round(predict(log.reg.fit.wheat.density, newdata=set1.wheat.rescale, type="probs"),digits=3)

lreg.pred.class.wheat.density.2 <- predict(log.reg.fit.wheat.density, newdata=set2.wheat.rescale)
lreg.pred.prob.wheat.density.2 <- round(predict(log.reg.fit.wheat.density, newdata=set2.wheat.rescale, type="probs"),digits=3)

(lreg.misclass.wheat.den.train <- mean(ifelse(lreg.pred.class.wheat.density.1 == set1.wheat.healthy, yes=0, no=1)))
(lreg.misclass.wheat.den.test <- mean(ifelse(lreg.pred.class.wheat.density.2 == set2.wheat.healthy, yes=0, no=1)))

x11(pointsize = 9)
plot(set1.wheat.rescale$density, lreg.pred.prob.wheat.density,  ylab="healthy", xlab= "density", main="pred healthy vs density")

# ii - smooth binomial regression 
x11(pointsize = 9)
sbino.wheat.1 <- sm.binomial(x=set1.wheat$density, y=set1.wheat.healthy, h=5, xlab="h=5", ylab = "density")
sbino.wheat.2 <- sm.binomial(x=set2.wheat$density, y=set2.wheat.healthy, h=5, xlab="h=5", ylab = "density")

# iii - log odds
lodds.sbino.wheat.1 <- log(sbino.wheat.1$eval.points)
lodds.lreg.wheat.1 <- log(lreg.pred.prob.wheat.density)

# iv
x11(pointsize = 6)
plot(y=sbino.wheat.1$linear.predictor, x=sbino.wheat.1$eval.points)

# v
sbino.misclass.wheat.den.train <- mean(sbino.wheat.1$se)
sbino.misclass.wheat.den.test <- mean(sbino.wheat.2$se)


# vi - gam not including classnum
gam.wheat.tr <- gam(data=set1.wheat, type~  s(density) + 
              s(hardness) + s(size) + s(weight) + s(moisture), 
            family=binomial(link=logit)) 

summary(gam.wheat.tr)

x11(pointsize=12)
par(mfrow=c(3,3))
plot(gam.wheat.tr, main="GAM marginal splines",scale=0)


gam.pred.prob.wheat.1 <- predict(gam.wheat.tr, newdata=set1.wheat[,c(2:6,8)], type="response")
gam.pred.class.wheat.1 <- as.numeric(predict(gam.wheat.tr, newdata=set1.wheat[,c(2:6,8)], type="link") > 0)
head(cbind(round(gam.pred.prob.wheat.1, digits=3), gam.pred.class.wheat.1))

x11(pointsize = 6)
plot(set1.wheat$density,gam.pred.prob.wheat.1)

gam.pred.prob.wheat.2 <- predict(gam.wheat.tr, newdata=set2.wheat[,c(2:6,8)], type="response")
gam.pred.class.wheat.2 <- as.numeric(predict(gam.wheat.tr, newdata=set2.wheat[,c(2:6,8)], type="link") > 0)
head(cbind(round(gam.pred.prob.wheat.2, digits=3), gam.pred.class.wheat.2))

x11(pointsize = 6)
plot(set2.wheat$density,gam.pred.prob.wheat.2)

# Training error is zero, but test errors are pretty good.  
misclass.gam.prob.tr <- mean(ifelse(gam.pred.class.wheat.1 == set1.wheat.healthy, yes=0, no=1))
misclass.gam.prob.te <- mean(ifelse(gam.pred.class.wheat.2 == set2.wheat.healthy, yes=0, no=1))









##################################
### b -kernel density estimates ##
##################################
kd.wheat.den.tr <- density(set1.wheat[which(set1.wheat$type=="Healthy"),2], kernel="gaussian", bw=6)
kd.wheat.den.hr <- density(set1.wheat[which(set1.wheat$type=="Healthy"),3], kernel="gaussian", bw=4)
kd.wheat.den.sz <- density(set1.wheat[which(set1.wheat$type=="Healthy"),4], kernel="gaussian", bw=6)
kd.wheat.den.wt <- density(set1.wheat[which(set1.wheat$type=="Healthy"),5], kernel="gaussian", bw=8)
kd.wheat.den.mo <- density(set1.wheat[which(set1.wheat$type=="Healthy"),6], kernel="gaussian", bw=12)
kd.wheat.den.cl <- density(set1.wheat[which(set1.wheat$type=="Healthy"),8], kernel="gaussian", bw=8)



x11(pointsize = 9)
plot(kd.wheat.den.tr, main="Gaussian Kernel", col=colors()[53], lwd=2)
lines(kd.wheat.den.hr, col=colors()[68], lwd=2)
lines(kd.wheat.den.sz, col=colors()[203], lwd=2)
lines(kd.wheat.den.wt, col=colors()[12], lwd=2)
lines(kd.wheat.den.mo, col=colors()[342], lwd=2)
lines(kd.wheat.den.cl, col=colors()[112], lwd=2)
legend(x = 15, y = 0.06, legend = c("density","hardness","size","weight","moisture","type","classnum"), lty = "solid",
       col=c(colors()[c(53,68,203,464,342,112)]), cex=0.8, bty="n")


#####################################
#### c - naive bayes classifier   ###
#####################################

## Naivebayes classifier using Gaussian
nbg.fit.wheat <- NaiveBayes(x=set1.wheat[,c(2:6,8)],grouping=set1.wheat$type,usekernel = TRUE)
pred.nbg.wheat_tr <- predict(nbg.fit.wheat, newdata=set1.wheat[,c(2:6,8)], type="class")
pred.nbg.wheat_te <- predict(nbg.fit.wheat, newdata=set2.wheat[,c(2:6,8)], type="class")

misclass.wheat.gauss.tr <- mean(ifelse(pred.nbg.wheat_tr$class == set1.wheat$type, yes=0, no=1))
misclass.wheat.gauss.te <- mean(ifelse(pred.nbg.wheat_te$class == set2.wheat$type, yes=0, no=1))

## naivebaye kernel without pca
nbk.fit.wheat <- naiveBayes(x=set1.wheat[,c(2:6,8)], y=set1.wheat$type)
pred.nbk.wheat_tr <- predict(nbk.fit.wheat, newdata=set1.wheat[,c(2:6,8)], type="class")
pred.nbk.wheat_te <- predict(nbk.fit.wheat, newdata=set2.wheat[,c(2:6,8)], type="class")

misclass.wheat.kernel.tr <- mean(ifelse(pred.nbk.wheat_tr == set1.wheat$type, yes=0, no=1))
misclass.wheat.kernel.te <- mean(ifelse(pred.nbk.wheat_te == set2.wheat$type, yes=0, no=1))

## naivebaye kernel with pca
naiveb.pc.wheat <- prcomp(x=set1.wheat[,c(2:6,8)], scale.=TRUE)
nb.xi.wheat.1 <- data.frame(naiveb.pc.wheat$x, class = as.factor(set1.wheat$type))
nb.xi.wheat.2 <- data.frame(predict(naiveb.pc.wheat, newdata=set2.wheat[,c(2:6,8)]), class = as.factor(set2.wheat$type))

nbk_pc.fit.wheat <- naiveBayes(x=nb.xi.wheat.1, y=nb.xi.wheat.1$class)
pred.nbk_pc.wheat_tr <- predict(nbk_pc.fit.wheat, newdata=nb.xi.wheat.1, type="class")
pred.nbk_pc.wheat_te <- predict(nbk_pc.fit.wheat, newdata=nb.xi.wheat.2, type="class")

misclass.training.kernel_pc.tr <- mean(ifelse(pred.nbk_pc.wheat_tr == nb.xi.wheat.1$class, yes=0, no=1))
misclass.testing.kernel_pc.te <- mean(ifelse(pred.nbk_pc.wheat_te == nb.xi.wheat.2$class, yes=0, no=1))

