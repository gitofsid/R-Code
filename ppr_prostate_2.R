#################################################
## Name        -   Siddharth Baronia       ######
#################################################

prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

# Splitting data in half using random uniform selection to make two "set"s.
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

# with 1, 2 and 3 terms
ppr1 <- ppr(data=prostate1.scale1, lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, nterms=1, optlevel=3, sm.method="gcvspline") 
plot(ppr1, main="PPR 1", col="orange", ylim=c(-2,8))
pred_ppr1 <- predict(ppr1,newdata=prostate2.scale1[c(2:9)])
mspe_ppr1 <- mean((pred_ppr1-prostate2.scale1$lpsa)^2)


ppr2 <- ppr(data=prostate1.scale1, lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, nterms=2, optlevel=3, sm.method="gcvspline") 
plot(ppr2, main="PPR 2", col="orange", ylim=c(-2,8))
pred_ppr2 <- predict(ppr2,newdata=prostate2.scale1[c(2:9)])
mspe_ppr2 <- mean((pred_ppr2-prostate2.scale1$lpsa)^2)

ppr3 <- ppr(data=prostate1.scale1, lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, nterms=3, optlevel=3, sm.method="gcvspline") 
plot(ppr3, main="PPR 3", col="orange", ylim=c(-2,8))
pred_ppr3 <- predict(ppr3,newdata=prostate2.scale1[c(2:9)])
mspe_ppr3 <- mean((pred_ppr3-prostate2.scale1$lpsa)^2)


# now with 1 term and maxterm of 4,5,6
ppr1_4 <- ppr(data=prostate1.scale1, lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, nterms=1, optlevel=3, maxterms=4, sm.method="gcvspline") 
pred_ppr1_4 <- predict(ppr1_4,newdata=prostate2.scale1[c(2:9)])
mspe_ppr1_4 <- mean((pred_ppr1_4-prostate2.scale1$lpsa)^2)

ppr1_5 <- ppr(data=prostate1.scale1, lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, nterms=1, optlevel=3, maxterms=5, sm.method="gcvspline") 
pred_ppr1_5 <- predict(ppr1_5,newdata=prostate2.scale1[c(2:9)])
mspe_ppr1_5 <- mean((pred_ppr1_5-prostate2.scale1$lpsa)^2)

ppr1_6 <- ppr(data=prostate1.scale1, lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, nterms=1, optlevel=3, maxterms=6, sm.method="gcvspline") 
pred_ppr1_6 <- predict(ppr1_6,newdata=prostate2.scale1[c(2:9)])
mspe_ppr1_6 <- mean((pred_ppr1_6-prostate2.scale1$lpsa)^2)


# Results
summary(ppr1)
mspe_ppr1
summary(ppr2)
mspe_ppr2
summary(ppr3)
mspe_ppr3
summary(ppr1_4)
mspe_ppr1_4
summary(ppr1_5)
mspe_ppr1_5
summary(ppr1_6)
mspe_ppr1_6



