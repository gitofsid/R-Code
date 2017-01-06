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

set.seed(29003092)
abalone.final$set <- ifelse(U <- runif(n=nrow(abalone.final))>0.75, yes=2, no=1)

y.train <- abalone.final[which(abalone.final$set==1),1]
x.train <- as.matrix(abalone.final[which(abalone.final$set==1),c(2:10)])
xs.train <- scale(x.train) #standardized training vars
y.test <- abalone.final[which(abalone.final$set==2),1]
x.test <- as.matrix(abalone.final[which(abalone.final$set==2),c(2:10)])
xs.test <- scale(x.test) # standardized testing vars

#########################
####### part b ##########
#########################
library(glmnet)
library(bootstrap)
library(MASS)

# bayesian
library(BMA)
bayesian.mod <- bicreg(x = xs.train, y = y.train, strict = FALSE, OR = 80)
bayesian.min.bic <- which.min(bayesian.mod$bic)
bayesian.mod$which[bayesian.min.bic,] # variables selcted in best model

# ols
ols <- lsfit(xs.train,y.train)

# lasso-min
cv.lasso <- cv.glmnet(y=y.train, x= xs.train, family="gaussian")

# ridge
ridge.fit <- lm.ridge(formula=y.train ~ xs.train,data=abalone.final[which(abalone.final$set==1),], lambda = cv.lasso$lambda.min)

# results
# part b
summary(bayesian.mod) # find the coefficients for bayesian
ols$coefficients # coefficients for ols
ridge.fit$coef # coefficients for ridge
coef(cv.lasso, s="lambda.min") # coefficients for LASSO-min

sum(abs(ols$coefficients)) - abs(ols$coefficients[1])
sum(ols$coefficients^2) - ols$coefficients[1]^2
sum(abs(ridge.fit$coef))
sum(ridge.fit$coef^2)
sum(abs(coef(cv.lasso, s="lambda.min"))) - abs(coef(cv.lasso, s="lambda.min")[1])
sum(coef(cv.lasso, s="lambda.min")^2) - coef(cv.lasso, s="lambda.min")[1]^2
