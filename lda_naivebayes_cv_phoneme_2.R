#################################################
## Name        -   Siddharth Baronia       ######
#################################################
library(MASS)
library(glmnet)
library(e1071)
library(klaR)
library(rpart)
library(randomForest)

phonemeData <- read.table("phoneme.csv", header=TRUE, sep=",", na.strings=" ")
phonemeData <- phonemeData[,c(2:258)]
head(phonemeData)

# col x.1 to x.256 (2-257), respond var g, speaker 
# aa   ao dcl   iy  sh 


#####################
## a - LDA       ####
#####################

lda.fit.phoneme <- lda(x=phonemeData[,-257],grouping=phonemeData$g)
lda.fit.phoneme
summary(lda.fit.phoneme)


## fit using scale
phonemeData.scale <- apply(phonemeData[,-257], 2, scale)
lda.fit.phoneme.s <- lda(x=phonemeData.scale,grouping=phonemeData$g)
lda.fit.phoneme.s
summary(lda.fit.phoneme.s)
lda.fit.phoneme.s$means


x11(pointsize = 9)
class.col.ph <- ifelse(phonemeData$g == "sh", y=393,n=
                         ifelse(phonemeData$g=="iy", y=68,n=
                                  ifelse(phonemeData$g=="dcl",y=203,n=
                                           ifelse(phonemeData$g=="aa",y=385,n=464))))
plot(lda.fit.phoneme, col=colors()[class.col.ph])

pred.lda.phoneme <- predict(lda.fit.phoneme, newdata = phonemeData[,-257])

# confusion matrix for entire set
table(phonemeData$g, pred.lda.phoneme$class, dnn=c("observed_tr","predicted_tr"))

lda.pred.train.phoneme <- pred.lda.phoneme$class
(lmisclass.train.phoneme <- mean(ifelse(lda.pred.train.phoneme == phonemeData$g, yes=0, no=1)))


################################
#### b - bootstrap resmapling ##
################################

rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

reps <- 4

combination.matrix.index <- matrix(NA, nrow=nrow(phonemeData), ncol=4)
misclass.training <- matrix(NA, nrow=reps, ncol=4)
misclass.testing <- matrix(NA, nrow=reps, ncol=4)
relative.misclass.testing <- matrix(NA, nrow=reps, ncol=4)
colnames(misclass.training) <- c("lda","qda","mlr","lasso")
colnames(misclass.testing) <- c("lda","qda","mlr","lasso")
colnames(relative.misclass.testing) <- c("lda","qda","mlr","lasso")

counter <- 1
for (i in 1:reps) {
  new <- ifelse ( runif (n= nrow ( phonemeData )) <.75 , yes =1, no =2)
  set1.phoneme <- phonemeData[which(new==1),]
  set2.phoneme <- phonemeData[which(new==2),]
  combination.matrix.index[,i] <- new
  
  ## lda ###
  bs.lda.fit.phoneme <- lda(x=set1.phoneme[,-257],grouping=set1.phoneme$g)
  bs.pred.lda.phoneme_tr <- predict(bs.lda.fit.phoneme, newdata = set1.phoneme[,-257])$class
  bs.pred.lda.phoneme_te <- predict(bs.lda.fit.phoneme, newdata = set2.phoneme[,-257])$class
  
  misclass.training[counter, 1] <- mean(ifelse(bs.pred.lda.phoneme_tr == set1.phoneme$g, yes=0, no=1))
  misclass.testing [counter, 1] <- mean(ifelse(bs.pred.lda.phoneme_te == set2.phoneme$g, yes=0, no=1))
  
  ### qda
  bs.qda.fit.phoneme <- qda(x=set1.phoneme[,-257],grouping=set1.phoneme$g)
  bs.pred.qda.phoneme_tr <- predict(bs.qda.fit.phoneme, newdata = set1.phoneme[,-257])$class
  bs.pred.qda.phoneme_te <- predict(bs.qda.fit.phoneme, newdata = set2.phoneme[,-257])$class
  
  misclass.training[counter, 2] <- mean(ifelse(bs.pred.qda.phoneme_tr == set1.phoneme$g, yes=0, no=1))
  misclass.testing [counter, 2] <- mean(ifelse(bs.pred.qda.phoneme_te == set2.phoneme$g, yes=0, no=1))
  
  ## multinomial logistic regression
  bs.logit.fit.phoneme <- glmnet(x=as.matrix(set1.phoneme[,1:256]), y=set1.phoneme[,257], family="multinomial")
  bs.pred.logit.phoneme_tr <- predict(object=bs.logit.fit.phoneme,newx=as.matrix(set1.phoneme[,1:256]),c=0,type="class")
  bs.pred.logit.phoneme_te <- predict(object=bs.logit.fit.phoneme,newx=as.matrix(set2.phoneme[,1:256]),c=0,type="class")
  
  misclass.training[counter, 3] <- mean(ifelse(bs.pred.logit.phoneme_tr == set1.phoneme$g, yes=0, no=1))
  misclass.testing [counter, 3] <- mean(ifelse(bs.pred.logit.phoneme_te == set2.phoneme$g, yes=0, no=1))
  
  ## lasso
  bs.lass.fit.phoneme <- cv.glmnet(x=as.matrix(set1.phoneme[,1:256]), y=set1.phoneme[,257], family="multinomial")
  bs.pred.lasso.phoneme_tr <- predict(object=bs.lass.fit.phoneme, newx=as.matrix(set1.phoneme[,1:256]), s=bs.lass.fit.phoneme$lambda.min, type="class")
  bs.pred.lasso.phoneme_te <- predict(bs.lass.fit.phoneme, newx=as.matrix(set2.phoneme[,1:256]), s=bs.lass.fit.phoneme$lambda.min, type="class")
  
  misclass.training[counter, 4] <- mean(ifelse(bs.pred.lasso.phoneme_tr == set1.phoneme$g, yes=0, no=1))
  misclass.testing [counter, 4] <- mean(ifelse(bs.pred.lasso.phoneme_te == set2.phoneme$g, yes=0, no=1))
  
  counter <- counter + 1
  
}

###### Naive Bayes ###########

misclass.training.naiveb <- matrix(NA, nrow=reps, ncol=4)
colnames(misclass.training.naiveb) <- c("nb_g","nb_g_pca","nb_k","nb_k_pca")
misclass.testing.naiveb <- matrix(NA, nrow=reps, ncol=4)
colnames(misclass.testing.naiveb) <- c("nb_g","nb_g_pca","nb_k","nb_k_pca")
relative.misclass.testing.naiveb <- matrix(NA, nrow=reps, ncol=4)
colnames(relative.misclass.testing.naiveb) <- c("nb_g","nb_g_pca","nb_k","nb_k_pca")

misclass.training <- cbind(misclass.training,misclass.training.naiveb)
misclass.testing <- cbind(misclass.testing,misclass.testing.naiveb)
relative.misclass.testing <- cbind(relative.misclass.testing,relative.misclass.testing.naiveb)


counter <- 1
for (i in 1:reps) {
  set1.phoneme <- phonemeData[which(combination.matrix.index[,i]==1),]
  set2.phoneme <- phonemeData[which(combination.matrix.index[,i]==2),]
  
  ## naivebaye gaussian without pca
  bs.nbg.fit.phoneme <- NaiveBayes(x=set1.phoneme[,-257],grouping=set1.phoneme$g,usekernel = TRUE)
  bs.pred.nbg.phoneme_tr <- predict(bs.nbg.fit.phoneme, newdata=set1.phoneme[,-257], type="class")
  bs.pred.nbg.phoneme_te <- predict(bs.nbg.fit.phoneme, newdata=set2.phoneme[,-257], type="class")
  
  misclass.training[counter, 5] <- mean(ifelse(bs.pred.nbg.phoneme_tr$class == set1.phoneme$g, yes=0, no=1))
  misclass.testing [counter, 5] <- mean(ifelse(bs.pred.nbg.phoneme_te$class == set2.phoneme$g, yes=0, no=1))
  
  ## naivebaye gaussian with pca
  naiveb.pc <- prcomp(x=set1.phoneme[,-257], scale.=TRUE)
  nb.xi.1 <- data.frame(naiveb.pc$x, class = as.factor(set1.phoneme$g))
  nb.xi.2 <- data.frame(predict(naiveb.pc, newdata=set2.phoneme), class = as.factor(set2.phoneme$g))
  
  bs.nbg_pc.fit.phoneme <- NaiveBayes(x=nb.xi.1[,-257],grouping=nb.xi.1$class, usekernel = TRUE)
  bs.pred_pc.nbg.phoneme_tr <- predict(bs.nbg_pc.fit.phoneme, newdata=nb.xi.1[,-257], type="class")
  bs.pred.nbg_pc.phoneme_te <- predict(bs.nbg_pc.fit.phoneme, newdata=nb.xi.2[,-257], type="class")
  
  misclass.training[counter, 6] <- mean(ifelse(bs.pred_pc.nbg.phoneme_tr$class == nb.xi.1$class, yes=0, no=1))
  misclass.testing [counter, 6] <- mean(ifelse(bs.pred.nbg_pc.phoneme_te$class == nb.xi.2$class, yes=0, no=1))
  
  ## naivebaye kernel without pca
  bs.nbk.fit.phoneme <- naiveBayes(x=set1.phoneme[,-257], y=set1.phoneme$g)
  bs.pred.nbk.phoneme_tr <- predict(bs.nbk.fit.phoneme, newdata=set1.phoneme[,-257], type="class")
  bs.pred.nbk.phoneme_te <- predict(bs.nbk.fit.phoneme, newdata=set2.phoneme[,-257], type="class")
  
  misclass.training[counter, 7] <- mean(ifelse(bs.pred.nbk.phoneme_tr == set1.phoneme$g, yes=0, no=1))
  misclass.testing [counter, 7] <- mean(ifelse(bs.pred.nbk.phoneme_te == set2.phoneme$g, yes=0, no=1))
  
  ## naivebaye kernel with pca
  bs.nbk_pc.fit.phoneme <- naiveBayes(x=nb.xi.1[,-257], y=nb.xi.1$class)
  bs.pred.nbk_pc.phoneme_tr <- predict(bs.nbk_pc.fit.phoneme, newdata=nb.xi.1[,-257], type="class")
  bs.pred.nbk_pc.phoneme_te <- predict(bs.nbk_pc.fit.phoneme, newdata=nb.xi.2[,-257], type="class")
  
  misclass.training[counter, 8] <- mean(ifelse(bs.pred.nbk_pc.phoneme_tr == nb.xi.1$class, yes=0, no=1))
  misclass.testing [counter, 8] <- mean(ifelse(bs.pred.nbk_pc.phoneme_te == nb.xi.2$class, yes=0, no=1))
  
  counter <- counter + 1
}

mean(misclass.training[,5])
mean(misclass.training[,6])
mean(misclass.training[,7])
mean(misclass.training[,8])

mean(misclass.testing[,5])
mean(misclass.testing[,6])
mean(misclass.testing[,7])
mean(misclass.testing[,8])

mean(relative.misclass.testing[,5])
mean(relative.misclass.testing[,6])
mean(relative.misclass.testing[,7])
mean(relative.misclass.testing[,8])

############################
## cv-min, cv-1se, RF ######
############################

misclass.training.trees <- matrix(NA, nrow=reps, ncol=4)
colnames(misclass.training.trees) <- c("cv-min","cv-1se","cv-def","randomforest")
misclass.testing.trees <- matrix(NA, nrow=reps, ncol=4)
colnames(misclass.testing.trees) <- c("cv-min","cv-1se","cv-def","randomforest")
relative.misclass.testing.trees <- matrix(NA, nrow=reps, ncol=4)
colnames(relative.misclass.testing.trees) <- c("cv-min","cv-1se","cv-def","randomforest")

misclass.training <- cbind(misclass.training,misclass.training.trees)
misclass.testing <- cbind(misclass.testing,misclass.testing.trees)
relative.misclass.testing <- cbind(relative.misclass.testing,relative.misclass.testing.trees)


for (i in 1:reps) {
  set1.phoneme <- phonemeData[which(combination.matrix.index[,i]==1),]
  set2.phoneme <- phonemeData[which(combination.matrix.index[,i]==2),]
  
  # "cv-min","cv-1se","cv-def","randomforest"
  phoneme.tree <- rpart(data=set1.phoneme, g ~ ., method = "class", cp = 0.001)
  
  x11(pointsize = 9)
  plotcp(phoneme.tree, main=i)
}

cvmin.list <- c(0.0020,0.0022,0.0015,0.0026)
cv1se.list <- c(0.0025,0.0025,0.0018,0.0038)

rf.ntree <- c(1000,1500,2000,2500)
rf.maxnodes <- c(20,40,60,80)
misclass.training.trees <- matrix(NA, nrow=16, ncol=1)
rf.tune.comb <- expand.grid(rf.ntree,rf.maxnodes)

for (i in 1:16) {
  phoneme.rf.temp <- randomForest(data=set1.phoneme, g ~ ., maxnodes=rf.tune.comb[i,2],
                                  importance=TRUE, ntree=rf.tune.comb[i,1], keep.forest=TRUE)
  phoneme.pred.rf.temp <- predict(phoneme.rf.temp, newdata=set2.phoneme[,-257], type="response")
  misclass.training.trees[i,1] <- mean(ifelse(phoneme.pred.rf.temp == set2.phoneme$g, yes=0, no=1))
}

counter <- 1
for (i in 1:reps) {
  set1.phoneme <- phonemeData[which(combination.matrix.index[,i]==1),]
  set2.phoneme <- phonemeData[which(combination.matrix.index[,i]==2),]
  
  # "cv-min","cv-1se","cv-def","randomforest"
  phoneme.tree.def <- rpart(data=set1.phoneme, g ~ ., method = "class", cp = 0.001)
  phoneme.tree.cv1se <- rpart(data=set1.phoneme, g ~ ., method = "class", cp = cv1se.list[i])
  phoneme.tree.cvmin <- rpart(data=set1.phoneme, g ~ ., method = "class", cp = cvmin.list[i])
  
  phoneme.pred.tr.cv.1se <- predict(phoneme.tree.cv1se, newdata=set1.phoneme[,-257],type="class")
  phoneme.pred.tr.cv.min <- predict(phoneme.tree.cvmin, newdata=set1.phoneme[,-257],type="class")
  phoneme.pred.tr.def <- predict(phoneme.tree.def, newdata=set1.phoneme[,-257],type="class")
  
  phoneme.pred.te.cv.1se <- predict(phoneme.tree.cv1se, newdata=set2.phoneme[,-257],type="class")
  phoneme.pred.te.cv.min <- predict(phoneme.tree.cvmin, newdata=set2.phoneme[,-257],type="class")
  phoneme.pred.te.def <-    predict(phoneme.tree.def, newdata=set2.phoneme[,-257],type="class")
  
  
  phoneme.rf.tr <- randomForest(data=set1.phoneme, g ~ ., maxnodes=maxnode.rf.temp,
                                importance=TRUE, ntree=ntree.rf.temp, keep.forest=TRUE)
  phoneme.pred.rf.tr <- predict(phoneme.rf.tr, newdata=set1.phoneme[,-257], type="response")
  phoneme.pred.rf.te <- predict(phoneme.rf.tr, newdata=set2.phoneme[,-257], type="response")
  # cv-min cv-1se cv-def randomforest
  
  misclass.training[counter, 9] <- mean(ifelse(phoneme.pred.tr.cv.min == set2.phoneme$g, yes=0, no=1))
  misclass.testing [counter, 9] <- mean(ifelse(phoneme.pred.te.cv.min == set2.phoneme$g, yes=0, no=1))
  
  misclass.training[counter, 10] <- mean(ifelse(phoneme.pred.tr.cv.1se == set2.phoneme$g, yes=0, no=1))
  misclass.testing [counter, 10] <- mean(ifelse(phoneme.pred.te.cv.1se == set2.phoneme$g, yes=0, no=1))
  
  misclass.training[counter, 11] <- mean(ifelse(phoneme.pred.tr.def == set2.phoneme$g, yes=0, no=1))
  misclass.testing [counter, 11] <- mean(ifelse(phoneme.pred.te.def == set2.phoneme$g, yes=0, no=1))
  
  misclass.training[counter, 12] <- mean(ifelse(phoneme.pred.rf.tr == set2.phoneme$g, yes=0, no=1))
  misclass.testing [counter, 12] <- mean(ifelse(phoneme.pred.rf.te == set2.phoneme$g, yes=0, no=1))
  
  counter <- counter + 1
}


#################################
####       SVM          #########
#################################

misclass.training.svm <- matrix(NA, nrow=reps, ncol=2)
colnames(misclass.training.svm) <- c("svm-def","svm-tuned")
misclass.testing.svm <- matrix(NA, nrow=reps, ncol=2)
colnames(misclass.testing.svm) <- c("svm-def","svm-tuned")
relative.misclass.testing.svm <- matrix(NA, nrow=reps, ncol=2)
colnames(relative.misclass.testing.svm) <- c("svm-def","svm-tuned")

misclass.training <- cbind(misclass.training,misclass.training.svm)
misclass.testing <- cbind(misclass.testing,misclass.testing.svm)
relative.misclass.testing <- cbind(relative.misclass.testing,relative.misclass.testing.svm)

phoneme.tune.val.temp <-  tune.svm(data=set1.phoneme, g ~ ., 
                          tunecontrol=tune.control(sampling="fix"), 
                          validation.x=set2.phoneme[,-257], validation.y=set2.phoneme[,257], 
                          gamma = 10^(-3:-1), cost = 10^(2:7))
phoneme.tune.svm.val.temp <- phoneme.tune.val.temp

svm.best.gamma <- phoneme.tune.svm.val.temp$best.parameters$gamma
svm.best.cost <- phoneme.tune.svm.val.temp$best.parameters$cost

counter <- 1
for (i in 1:reps) {
  set1.phoneme <- phonemeData[which(combination.matrix.index[,i]==1),]
  set2.phoneme <- phonemeData[which(combination.matrix.index[,i]==2),]
  
  # "svm-def","svm-tuned"
  # For the Phoneme data, include both default and tuned SVM in your analysis. Explain
  # clearly what you did to tune, including the algorithm and version of validation error
  # used and the parameter combinations tried. Report results.
  phoneme.svm.def <- svm(data=set1.phoneme, g ~ ., kernel="radial")
  phoneme.pred.svm.def.tr <- predict(phoneme.svm.def, newdata=set1.phoneme)
  phoneme.pred.svm.def.te <- predict(phoneme.svm.def, newdata=set2.phoneme)
  
  phoneme.svm.tuned <- svm(data=set1.phoneme, g ~ ., kernel="radial", gamma=svm.best.gamma, cost=svm.best.cost)
  phoneme.pred.svm.tune.tr <- predict(phoneme.svm.tuned, newdata=set1.phoneme)
  phoneme.pred.svm.tune.te <- predict(phoneme.svm.tuned, newdata=set2.phoneme)
  
  misclass.training[counter, 13] <- mean(ifelse(phoneme.pred.svm.def.tr == set1.phoneme$g, yes=0, no=1))
  misclass.testing[counter, 13] <- mean(ifelse(phoneme.pred.svm.def.te == set2.phoneme$g, yes=0, no=1))
  
  misclass.training[counter, 14] <- mean(ifelse(phoneme.pred.svm.tune.tr == set1.phoneme$g, yes=0, no=1))
  misclass.testing[counter, 14] <- mean(ifelse(phoneme.pred.svm.tune.te == set2.phoneme$g, yes=0, no=1))

  counter <- counter + 1
}



minimum_test.class <- apply(X=misclass.testing, MARGIN=1, FUN=min)
counter <- 1
for (r in 1:reps){
  relative.misclass.testing [counter,] <- misclass.testing[counter,] / minimum_test.class[counter]
  counter <- counter + 1
}




x11(pointsize = 9)
boxplot(misclass.training, at=1:14, las=2, names = c("lda","qda","mlr","lasso","nb_g","nb_g_pca","nb_k","nb_k_pca","cv-min","cv-1se","cv-def","randomforest","svm-def","svm-tuned"), main="boxplot training")

x11(pointsize = 9)
boxplot(misclass.testing, at=1:14, las=2, names = c("lda","qda","mlr","lasso","nb_g","nb_g_pca","nb_k","nb_k_pca","cv-min","cv-1se","cv-def","randomforest","svm-def","svm-tuned"), main="boxplot testing")

x11(pointsize = 9)
boxplot(relative.misclass.testing, at=1:14, las=2, names = c("lda","qda","mlr","lasso","nb_g","nb_g_pca","nb_k","nb_k_pca","cv-min","cv-1se","cv-def","randomforest","svm-def","svm-tuned"), main="boxplot relative testing")






