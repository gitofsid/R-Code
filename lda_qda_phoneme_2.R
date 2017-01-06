#################################################
## Name        -   Siddharth Baronia       ######
#################################################
library(MASS)
library(glmnet)
library(e1071)
library(klaR)

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
  
minimum_test.class <- apply(X=misclass.testing, MARGIN=1, FUN=min)
counter <- 1
for (r in 1:reps){
  relative.misclass.testing [counter,] <- misclass.testing[counter,] / minimum_test.class[counter]
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

x11(pointsize = 9)
boxplot(misclass.training, at=1:8, las=2, names = c("lda","qda","mlr","lasso","nb_g","nb_g_pca","nb_k","nb_k_pca"), main="boxplot training")

x11(pointsize = 9)
boxplot(misclass.testing, at=1:8, las=2, names = c("lda","qda","mlr","lasso","nb_g","nb_g_pca","nb_k","nb_k_pca"), main="boxplot testing")

x11(pointsize = 9)
boxplot(relative.misclass.testing, at=1:8, las=2, names = c("lda","qda","mlr","lasso","nb_g","nb_g_pca","nb_k","nb_k_pca"), main="boxplot relative testing")






