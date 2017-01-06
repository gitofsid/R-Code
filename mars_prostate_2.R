#################################################
## Name        -   Siddharth Baronia       ######
#################################################

prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)


set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

x1.sd <- as.vector(apply(X=x.1, MARGIN=2, FUN=sd))
x1.bar <- as.vector(apply(X=x.1, MARGIN=2, FUN=mean))

prostate1.scale1 <- as.data.frame(cbind(lpsa=y.1,scale(x.1, center=TRUE, scale=x1.sd)))
prostate2.scale1<- as.data.frame(cbind(lpsa=y.2, scale(x.2, center=x1.bar, scale=x1.sd)))

# MARS degree = 1
p.earth.1 <- earth(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=prostate1.scale1,
                   trace=3, degree=1)
summary(p.earth.1)
p.pred.earth.1 <- predict(p.earth.1,newdata=prostate2.scale1[c(2:9)])
p.MSPE.1 <- mean((p.pred.earth.1-prostate2.scale1$lpsa)^2)

# MARS degree = 2
p.earth.2 <- earth(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=prostate1.scale1,
                   trace=3, degree=2)
summary(p.earth.2)
p.pred.earth.2 <- predict(p.earth.2,newdata=prostate2.scale1[c(2:9)])
p.MSPE.2 <- mean((p.pred.earth.2-prostate2.scale1$lpsa)^2)

# MARS degree = 3
p.earth.3 <- earth(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=prostate1.scale1,
                   trace=3, degree=3)
summary(p.earth.3)
p.pred.earth.3 <- predict(p.earth.3,newdata=prostate2.scale1[c(2:9)])
p.MSPE.3 <- mean((p.pred.earth.3-prostate2.scale1$lpsa)^2)

# backward elimination
best.grsq.index.1 <- which.min(c(p.earth.1$grsq,p.earth.3$grsq,p.earth.3$grsq))
p.earth.back <- earth(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=prostate1.scale1,
                      trace=3, degree=best.grsq.index.1, pmethod="backward")
summary(p.earth.back)
p.pred.earth.back <- predict(p.earth.back,newdata=prostate2.scale1[c(2:9)])
p.MSPE.back <- mean((p.pred.earth.back-prostate2.scale1$lpsa)^2)

## Results
p.earth.1$grsq
p.MSPE.1
p.earth.2$grsq
p.MSPE.2
p.earth.3$grsq
p.MSPE.3
p.earth.back$grsq
p.MSPE.back

p.earth.1$coefficients
p.earth.2$coefficients
p.earth.3$coefficients
p.earth.back$coefficients

