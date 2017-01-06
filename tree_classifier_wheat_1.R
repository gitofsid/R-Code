#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(rpart)
library(rpart.plot)

wheatData <- read.table("wheat.csv", header=TRUE, sep=",", na.strings=" ")
head(wheatData)

set.seed(67982193)
wheatData$classnum <- as.numeric(wheatData$class)
perm <- sample(x=nrow(wheatData))
set1.wheat <- wheatData[which(perm <= 200), -1]
set2.wheat <- wheatData[which(perm > 200), -1]

#####################
## a - 1se, min   ###
#####################

wheat.tree <- rpart(data=set1.wheat, type ~ ., method = "class", cp = 0.001)
print(wheat.tree)
printcp(wheat.tree)

x11(pointsize = 9)
prp(wheat.tree, type=1, extra=1, main="original full tree")

x11(pointsize = 9)
plotcp(wheat.tree)


wheat.tree$variable.importance
wheat.tree$cptable

val.tune <- function(obj, valid, G, grid) {
  cp <- matrix(0, ncol=2, nrow=grid)
  for (x in c(1:grid)){
    cp[x,1] <- x/grid  
    pred <- predict(prune(obj, cp=x/grid), newdata=valid, type="class")
    cp[x,2] <- mean(ifelse(pred == G, yes=0, no=1))
  }
  cp
}

wheat.valtree <- val.tune(obj=wheat.tree, valid=set1.wheat, G=set1.wheat$type, grid=500)
# Returns optimal cp and misclassification rate there.
wheat.valtree[which.min(wheat.valtree[,2]), ]


wheat.prune.val <- prune(wheat.tree, cp=0.002)
wheat.prune.cv.1se <- prune(wheat.tree, cp=0.1)
wheat.prune.cv.min <- prune(wheat.tree, cp=0.041)

x11(h=10, w=18)
par(mfrow=c(1,3))
prp(wheat.prune.cv.1se, type=1, extra=1, main="Pruned CV-1SE tree")
prp(wheat.prune.cv.min, type=1, extra=1, main="Pruned CV-min tree")
prp(wheat.prune.val, type=1, extra=1, main="Pruned Val tree")

wheat.pred.test.cv.1se <- predict(wheat.prune.cv.1se, newdata=set2.wheat,type="class")
wheat.pred.test.cv.min <- predict(wheat.prune.cv.min, newdata=set2.wheat, type="class")
wheat.pred.test.val <- predict(wheat.prune.val, newdata=set2.wheat, type="class")
wheat.pred.test.full <- predict(wheat.tree, newdata=set2.wheat, type="class")

(wheat.misclass.test.cv.1se <- mean(ifelse(wheat.pred.test.cv.1se == set2.wheat$type, yes=0, no=1)))
(wheat.misclass.test.cv.min <- mean(ifelse(wheat.pred.test.cv.min == set2.wheat$type, yes=0, no=1)))
(wheat.misclass.test.val <- mean(ifelse(wheat.pred.test.val == set2.wheat$type, yes=0, no=1)))
(wheat.misclass.test.full <- mean(ifelse(wheat.pred.test.full == set2.wheat$type, yes=0, no=1)))

# Find an optimal (best number of variables) random forest based on OOB error.
# Interpret the importance of the variables and report the test error.
####################
##  b - optimal   ##
####################

# based on minimum from wheat.valtree
wheat.prune.val$variable.importance
(wheat.misclass.test.val <- mean(ifelse(wheat.pred.test.val == set2.wheat$type, yes=0, no=1)))
x11(pointsize = 9)
prp(wheat.prune.val, type=1, extra=1, main="Pruned Val tree")