#################################################
## Name        -   Siddharth Baronia       ######
#################################################

abalone <-  read.table("abalone.csv", header=TRUE, sep=",", na.strings=" ")
abalone <- abalone[(0 < abalone$Height)&(abalone$Height < 0.5),]
head(abalone)


set.seed (890987665)
abalone$set <- ifelse(U <- runif(n=nrow(abalone))>0.75, yes=2, no=1)

abalone <- abalone[,c(9,1:8,10)]

y.train <- abalone[which(abalone$set==1),1]
x.train <- as.matrix(abalone[which(abalone$set==1),c(2:10)])
y.test <- abalone[which(abalone$set==2),1]
x.test <- as.matrix(abalone[which(abalone$set==2),c(2:10)])

library(rpart)
library(rpart.plot)
abalone.tree <- rpart(data=abalone[ which(abalone$set==1),], Rings~Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, method="anova")
abalone.tree
summary(abalone.tree)
cpt.abalone.tree <- abalone.tree$cptable
printcp(abalone.tree)

prp(abalone.tree, type=1, extra=1, main="Abalone - original full tree")
 

pred.abalone.tree <- predict(abalone.tree,newdata=abalone[ which(abalone$set==2),])
mspe.abalone.tree <- mean((pred.abalone.tree-y.test)^2)

# Find location of minimum error
minrow <- which.min(cpt.abalone.tree[,"xerror"])
cplow.min <- cpt.abalone.tree[minrow,"CP"]
cpup.min <- ifelse(minrow==1, yes=1, no=cpt.abalone.tree[minrow-1,"CP"])
cp.min <- sqrt(cplow.min*cpup.min)

# Find smallest row where error is below +1SE
se.row <- min(which(cpt.abalone.tree[,"xerror"] < cpt.abalone.tree[minrow,"xerror"]+cpt.abalone.tree[minrow,"xstd"]))
cplow.1se <- cpt.abalone.tree[se.row,"CP"]
cpup.1se <- ifelse(se.row==1, yes=1, no=cpt.abalone.tree[se.row-1,"CP"])
cp.1se <- sqrt(cplow.1se*cpup.1se)

abalone.tree.cp.min <- rpart(data=abalone[ which(abalone$set==1),], Rings~Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, 
                             method="anova",cp=cp.min)
summary(abalone.tree.cp.min)
printcp(abalone.tree.cp.min)
library(rpart.plot)
prp(abalone.tree.cp.min, type=1, extra=1, main="Abalone - cp.min tree")
pred.abalone.tree.cp.min <- predict(abalone.tree.cp.min,newdata=abalone[ which(abalone$set==2),])
mspe.abalone.tree.cp.min <- mean((pred.abalone.tree.cp.min-y.test)^2)

abalone.tree.cp.1se <- rpart(data=abalone[ which(abalone$set==1),], Rings~Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, 
                             method="anova",cp=cp.1se)
summary(abalone.tree.cp.1se)
printcp(abalone.tree.cp.1se)
library(rpart.plot)
prp(abalone.tree.cp.1se, type=1, extra=1, main="Abalone - cp.1se tree")
pred.abalone.tree.cp.1se <- predict(abalone.tree.cp.1se,newdata=abalone[ which(abalone$set==2),])
mspe.abalone.tree.cp.1se <- mean((pred.abalone.tree.cp.1se-y.test)^2)

###########################
##### with cp = 0 #########
###########################
abalone.tree.0 <- rpart(data=abalone[ which(abalone$set==1),], Rings~Sex+Length+Diameter+Height+Whole+Shucked+Viscera+Shell, 
                      method="anova",cp=0)
cpt.abalone.tree.0 <- abalone.tree.0$cptable
printcp(abalone.tree.0)

pred.abalone.tree.0 <- predict(abalone.tree.0,newdata=abalone[ which(abalone$set==2),])
mspe.abalone.tree.0 <- mean((pred.abalone.tree.0-y.test)^2)

# Find location of minimum error
minrow.0 <- which.min(cpt.abalone.tree.0[,"xerror"])
cplow.min.0 <- cpt.abalone.tree.0[minrow.0,"CP"]
cpup.min.0 <- ifelse(minrow.0==1, yes=1, no=cpt.abalone.tree.0[minrow.0-1,"CP"])
cp.min.0 <- sqrt(cplow.min.0*cpup.min.0)

# Find smallest row where error is below +1SE
se.row.0 <- min(which(cpt.abalone.tree.0[,"xerror"] < cpt.abalone.tree.0[minrow.0,"xerror"]+cpt.abalone.tree.0[minrow.0,"xstd"]))
cplow.1se.0 <- cpt.abalone.tree.0[se.row.0,"CP"]
cpup.1se.0 <- ifelse(se.row.0==1, yes=1, no=cpt.abalone.tree.0[se.row.0-1,"CP"])
cp.1se.0 <- sqrt(cplow.1se.0*cpup.1se.0)

abalone.tree.cp.min.0 <- prune(abalone.tree.0,cp=cp.min.0)
printcp(abalone.tree.cp.min.0)
pred.abalone.tree.cp.min.0 <- predict(abalone.tree.cp.min.0,newdata=abalone[ which(abalone$set==2),])
mspe.abalone.tree.cp.min.0 <- mean((pred.abalone.tree.cp.min.0-y.test)^2)

abalone.tree.cp.1se.0 <- prune(abalone.tree.0, cp=cp.1se.0)
printcp(abalone.tree.cp.1se.0)
pred.abalone.tree.cp.1se.0 <- predict(abalone.tree.cp.1se.0,newdata=abalone[ which(abalone$set==2),])
mspe.abalone.tree.cp.1se.0 <- mean((pred.abalone.tree.cp.1se.0-y.test)^2)

