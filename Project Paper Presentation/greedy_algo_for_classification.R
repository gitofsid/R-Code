#################################################
## Name        -   Siddharth Baronia       ######
## Assignment  -   Project 2               ######
#################################################

library(Kendall)
library(Metrics)
library(e1071)

wheatData <- read.table("wheat.csv", header=TRUE, sep=",", na.strings=" ")
head(wheatData)
wheatData <- wheatData[,c(2:8)]

set.seed(67982193)
wheatData$classnum <- as.numeric(wheatData$class)
wheatData$type <- ifelse(wheatData$type=="Healthy",y="Healthy",n="Unhealthy")
wheatData$type <- as.factor(wheatData$type)
wheatData <- wheatData[,c(8,2:7)]

### making test and training 
perm <- sample(x=nrow(wheatData))
set1.train <- wheatData[which(perm <= 200),]
set2.test <- wheatData[which(perm > 200),]


# calculating similarity between feature by using Kendall package
# and getting Tau values from it for each pair of features
features.1 <- c("classnum",  "density", "hardness", "size", "weight", "moisture")
feat.matrix <- t(combn(features.1,2))
kendall.value <- matrix(0, nrow=nrow(feat.matrix), ncol=1)

feat.kendall <- cbind(feat.matrix,kendall.value)
colnames(feat.kendall) <- c("feat1","feat2","kendallvalue")
feat.kendall.df <- as.data.frame(feat.kendall)

for (i in 1:nrow(feat.kendall)) {
   kendall.val <- Kendall(wheatData[,feat.kendall.df[i,1]],wheatData[,feat.kendall.df[i,2]])
   feat.kendall[i,3] <- round(kendall.val$tau[1], digits = 4)
}

## if value is 1 or -1 it means that variables are closely related (direct or inversely)
## if variables are independent then the coefficient is approx 0
feat.kendall.df <- as.data.frame(feat.kendall)

### Checking how individual feature effects the prediction 
## by tuning svm using one feature at a time using training data
## and finding best gamma and cost. Using these values fit svm
## on training data and get training set prediction

precision.matrix <- matrix(NA, nrow=6, ncol=2)
colnames(precision.matrix) <- c("feature","MAP")
precision.matrix <- as.data.frame(precision.matrix)

## classnum ####
wheat.svm.classnum <- tune.svm(data=set1.train, type ~ classnum, kernel="radial", gamma = 10^(-2:-1), cost = 10^(2:3), cross=10)
wheat.svm.classnum.gamma <- wheat.svm.classnum$best.parameters$gamma
wheat.svm.classnum.cost <- wheat.svm.classnum$best.parameters$cost

wheat.svm.rad.classnum <- svm(data=set1.train, type ~ classnum, kernel="radial", gamma = wheat.svm.classnum.gamma, cost = wheat.svm.classnum.cost, cross=10)  
wheat.pred.svm.rad.classnum.tr <- predict(wheat.svm.rad.classnum, newdata=set1.train, type="class")

### density ###
wheat.svm.density <- tune.svm(data=set1.train, type ~ density, kernel="radial", gamma = 10^(-2:-1), cost = 10^(2:3), cross=10)
wheat.svm.density.gamma <- wheat.svm.density$best.parameters$gamma
wheat.svm.density.cost <- wheat.svm.density$best.parameters$cost

wheat.svm.rad.density <- svm(data=set1.train, type ~ density, kernel="radial", gamma = wheat.svm.density.gamma, cost = wheat.svm.density.cost, cross=10)  
wheat.pred.svm.rad.density.tr <- predict(wheat.svm.rad.density, newdata=set1.train, type="class")

## hardness ##
wheat.svm.hardness <- tune.svm(data=set1.train, type ~ hardness, kernel="radial", gamma = 10^(-2:-1), cost = 10^(2:3), cross=10)
wheat.svm.hardness.gamma <- wheat.svm.hardness$best.parameters$gamma
wheat.svm.hardness.cost <- wheat.svm.hardness$best.parameters$cost

wheat.svm.rad.hardness <- svm(data=set1.train, type ~ hardness, kernel="radial", gamma = wheat.svm.hardness.gamma, cost = wheat.svm.hardness.cost, cross=10)  
wheat.pred.svm.rad.hardness.tr <- predict(wheat.svm.rad.hardness, newdata=set1.train, type="class")

## size ##
wheat.svm.size <- tune.svm(data=set1.train, type ~ size, kernel="radial", gamma = 10^(-2:-1), cost = 10^(2:3), cross=10)
wheat.svm.size.gamma <- wheat.svm.size$best.parameters$gamma
wheat.svm.size.cost <- wheat.svm.size$best.parameters$cost

wheat.svm.rad.size <- svm(data=set1.train, type ~ size, kernel="radial", gamma = wheat.svm.size.gamma, cost = wheat.svm.size.cost, cross=10)  
wheat.pred.svm.rad.size.tr <- predict(wheat.svm.rad.size, newdata=set1.train, type="class")

## weight ##
wheat.svm.weight <- tune.svm(data=set1.train, type ~ weight, kernel="radial", gamma = 10^(-2:-1), cost = 10^(2:3), cross=10)
wheat.svm.weight.gamma <- wheat.svm.weight$best.parameters$gamma
wheat.svm.weight.cost <- wheat.svm.weight$best.parameters$cost

wheat.svm.rad.weight <- svm(data=set1.train, type ~ weight, kernel="radial", gamma = wheat.svm.weight.gamma, cost = wheat.svm.weight.cost, cross=10)  
wheat.pred.svm.rad.weight.tr <- predict(wheat.svm.rad.weight, newdata=set1.train, type="class")

## mositure ##
wheat.svm.moisture <- tune.svm(data=set1.train, type ~ moisture, kernel="radial", gamma = 10^(-2:-1), cost = 10^(2:3), cross=10)
wheat.svm.moisture.gamma <- wheat.svm.moisture$best.parameters$gamma
wheat.svm.moisture.cost <- wheat.svm.moisture$best.parameters$cost

wheat.svm.rad.moisture <- svm(data=set1.train, type ~ moisture, kernel="radial", gamma = wheat.svm.moisture.gamma, cost = wheat.svm.moisture.cost, cross=10)  
wheat.pred.svm.rad.moisture.tr <- predict(wheat.svm.rad.moisture, newdata=set1.train, type="class")

### calculating MAP - mean average precision by using predicted and actual value 
### and for every feature we calculate MAP whch act as its weight in undirected graph
wheat.train.predict <- as.data.frame(cbind(wheat.pred.svm.rad.classnum.tr,wheat.pred.svm.rad.density.tr,
                             wheat.pred.svm.rad.hardness.tr,wheat.pred.svm.rad.size.tr,
                             wheat.pred.svm.rad.weight.tr,wheat.pred.svm.rad.moisture.tr,set1.train$type))
colnames(wheat.train.predict) <- c("pred.classnum","pred.density","pred.hardness","pred.size","pred.weight","pred.moisture","act.set1.type")

for (j in 1:nrow(precision.matrix)) {
  precision.matrix[j,] <- c(features.1[j],round(mapk(nrow(wheat.train.predict),wheat.train.predict[,j],wheat.train.predict[,7]), digits = 4))
}


## for greedy search algorithm this are 
## values of correlation or similarity between two features using kendall and 
## importance factor for a feature (MAP) found from training set
precision.matrix
feat.kendall.df

precision.feature <- precision.matrix[,1]
feat.map <- precision.matrix[,2]
names(feat.map) <- precision.feature


imp.factor <- matrix(0, nrow=nrow(feat.kendall.df), ncol=2)
colnames(imp.factor) <- c("MAP.feat1","MAP.feat2")

## final matrix with similarity values and importance meaures of each features
precision.similarity.matrix <- as.data.frame(cbind(feat.kendall.df,imp.factor))

for (m in 1:nrow(precision.similarity.matrix)) {
  precision.similarity.matrix[m,4] <- feat.map[[precision.similarity.matrix[m,1]]]
  precision.similarity.matrix[m,5] <- feat.map[[precision.similarity.matrix[m,2]]]
}


########################################
## Greedy Search Algorithms ############
########################################

### we are trying to maximize (importance measure - correlation * 2c)
## we will use c = 1 which is param to balance

### It is an undirected graph, so we select node with biggest weight
### then we recalualte weight of rest of nodes using current weight
### and similarity value. Once recalculation is done we remove the 
### selected node out from the graph and then repeat the process

final.features <- c()
final.weight <- c()
temp.prec.matrix <- precision.matrix

for (r in 1:6) {
  max.map.feat <- temp.prec.matrix[which.max(temp.prec.matrix[,2]),1]
  final.features <- c(final.features,max.map.feat)
  final.weight <- c(final.weight,max(temp.prec.matrix[,2]))
  for (t in 1:nrow(temp.prec.matrix)) {
    for (h in 1:nrow(feat.kendall.df)) {
      if ((feat.kendall.df[h,1] == temp.prec.matrix[t,1] && feat.kendall.df[h,2] == max.map.feat) || (feat.kendall.df[h,2] == temp.prec.matrix[t,1] && feat.kendall.df[h,1] == max.map.feat)) {
        temp.prec.matrix[t,2] <- as.numeric(temp.prec.matrix[t,2]) - 2*1*as.numeric(feat.kendall.df[h,3])
      }
    }
  }
  temp.prec.matrix <- temp.prec.matrix[which(temp.prec.matrix$feature != max.map.feat),]
  print(temp.prec.matrix)
}

### order of importance of features and their weights

# final.features final.weight
# 1        density       0.8057
# 2       moisture     -21.3829
# 3           size     -43.3829
# 4       classnum     -45.3829
# 5       hardness     -57.3829
# 6         weight     -63.3829

importance.factor <- as.data.frame(cbind(final.features,final.weight))

### now fitting svm model on training set again while decreasing 
### the features one by one and refittign. Calcuating traning error
### for each new fit. I found the the best model comes with 
### 4 most impoirtant features - density,moisture,size,classnum
### Final model is made with these 4 features and then testing error 
### is calculated

# (density+moisture+size+classnum+hardness+weight) =  0.16
# (density+moisture+size+classnum+hardness) = 0.16
# (density+moisture+size+classnum) = 0.13  <<<<<<<<< best
# (density+moisture+size) = 0.1942857
# (density+moisture) = 0.1828571
# (density) = 0.1942857

wheat.svm.train.feat <- tune.svm(data=set1.train, type ~ density+moisture+size+classnum, kernel="radial", gamma = 10^(-2:-1), cost = 10^(2:4), cross=10)
wheat.svm.train.gamma <- wheat.svm.train.feat$best.parameters$gamma
wheat.svm.classnum.cost <- wheat.svm.train.feat$best.parameters$cost

wheat.svm.rad.train <- svm(data=set1.train, type ~ density+moisture+size+classnum, kernel="radial", gamma = wheat.svm.train.gamma, cost = wheat.svm.classnum.cost, cross=10)  
wheat.pred.svm.rad.train <- predict(wheat.svm.rad.train, newdata=set1.train, type="class")
wheat.pred.svm.rad.test <- predict(wheat.svm.rad.train, newdata=set2.test, type="class")

wheat.svm.train.error.2 <- mean(ifelse(wheat.pred.svm.rad.train == set1.train$type, yes=0, no=1))
wheat.svm.test.error.2 <- mean(ifelse(wheat.pred.svm.rad.test == set2.test$type, yes=0, no=1))

# training error
wheat.svm.train.error.2

# testing error
wheat.svm.test.error.2

table(set1.train$type, wheat.pred.svm.rad.train, dnn=c("observed_tr","predicted_tr"))
table(set2.test$type, wheat.pred.svm.rad.test, dnn=c("observed_te","predicted_te"))

#                 predicted_tr
# observed_tr   Healthy Unhealthy
#   Healthy        61        11
#   Unhealthy      15        113

                # predicted_te
# observed_te   Healthy Unhealthy
#   Healthy        17        7
#   Unhealthy      13        38

