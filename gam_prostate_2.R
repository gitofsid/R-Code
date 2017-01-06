#################################################
## Name        -   Siddharth Baronia       ######
#################################################

prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
# head(prostate)

# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)


y.1 <- prostate[which(prostate$set==1),10]
x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
y.2 <- prostate[which(prostate$set==2),10]
x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])

library(mgcv)

#  Generalized additive model as alternative to multivariate splines
gam1 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight)+s(age)+s(lbph)
            +svi+gleason+s(pgg45), family=gaussian(link=identity)) 
summary(gam1)

alpha <- mean(y.1)

# replacing pgg45 with lcp as pgg45 was not required from last run
gam1 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight)+s(age)+s(lbph)
            +svi+s(lcp)+gleason, family=gaussian(link=identity))
summary(gam1)


# backward elimination
gam_j1 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight)+s(age)+s(lbph)
              +svi+s(lcp)+gleason, family=gaussian(link=identity))
summary(gam_j1)
bic_j1 <- extractAIC(gam_j1,k=log(length(prostate$set==1)))[2]
edf_j1 <- extractAIC(gam_j1,k=log(length(prostate$set==1)))[1]

# removing least significant lbph
gam_j2 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight)+s(age)
              +svi+s(lcp)+gleason, family=gaussian(link=identity))
summary(gam_j2)
bic_j2 <- extractAIC(gam_j2,k=log(length(prostate$set==1)))[2]
edf_j2 <- extractAIC(gam_j2,k=log(length(prostate$set==1)))[1]

# Rsq increased from original so putting lbph back and taking out lcp
gam_j3 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight)+s(age)+s(lbph)
              +svi+gleason, family=gaussian(link=identity))
summary(gam_j3)
bic_j3 <- extractAIC(gam_j3,k=log(length(prostate$set==1)))[2]
edf_j3 <- extractAIC(gam_j3,k=log(length(prostate$set==1)))[1]

#Rsq decreased from  original so now taking svi out
gam_j4 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight)+s(age)+s(lbph)
              +gleason, family=gaussian(link=identity))
summary(gam_j4)
bic_j4 <- extractAIC(gam_j4,k=log(length(prostate$set==1)))[2]
edf_j4 <- extractAIC(gam_j4,k=log(length(prostate$set==1)))[1]

#Rsq decreased fruther taking lbph out
gam_j5 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight)+s(age)
              +gleason, family=gaussian(link=identity))
summary(gam_j5)
bic_j5 <- extractAIC(gam_j5,k=log(length(prostate$set==1)))[2]
edf_j5 <- extractAIC(gam_j5,k=log(length(prostate$set==1)))[1]

#Rsq decreased age out
gam_j6 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+s(lweight)
              +gleason, family=gaussian(link=identity))
summary(gam_j6)
bic_j6 <- extractAIC(gam_j6,k=log(length(prostate$set==1)))[2]
edf_j6 <- extractAIC(gam_j6,k=log(length(prostate$set==1)))[1]

#rsq decreased lweight out as removing lweight reduces rsq more than removing gleason
gam_j7 <- gam(data=prostate[which(prostate$set==1),], lpsa~s(lcavol)+gleason
              , family=gaussian(link=identity))
summary(gam_j7)
bic_j7 <- extractAIC(gam_j7,k=log(length(prostate$set==1)))[2]
edf_j7 <- extractAIC(gam_j7,k=log(length(prostate$set==1)))[1]

## Results
sum(gam_j1$edf)
bic_j1
edf_j1
sum(gam_j2$edf)
bic_j2
edf_j2
sum(gam_j3$edf)
bic_j3
edf_j3
sum(gam_j4$edf)
bic_j4
edf_j4
sum(gam_j5$edf)
bic_j5
edf_j5
sum(gam_j6$edf)
bic_j6
edf_j6
sum(gam_j7$edf)
bic_j7
edf_j7

# 7th model seems the best by BIC
pred_gam_j7 <- predict(gam_j7,newdata=data.frame(x.2))
MSPE = mean((pred_gam_j7-y.2)^2)
MSPE

# replacing with parametric linear terms
gam_j7_linear <- gam(data=prostate[which(prostate$set==1),], lpsa~lcavol+gleason,family=gaussian(link=identity))
summary(gam_j7_linear)
bic_j7_linear <- extractAIC(gam_j7_linear,k=log(length(prostate$set==1)))[2]
bic_j7_linear
