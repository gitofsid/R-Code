#################################################
## Name        -   Siddharth Baronia       ######
#################################################

prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

library(rpart)
library(rpart.plot)
prostate.tree <- rpart(data=prostate, lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, method="anova")
prostate.tree
summary(prostate.tree)


prp(prostate.tree, type=1, extra=1, main="Prostate - original full tree")
plotcp(prostate.tree)
meanvar(prostate.tree)
printcp(prostate.tree)

cpt_all <- prostate.tree$cptable
minrow <- which.min(cpt_all[,"xerror"])
se.row <- min(which(cpt_all[,"xerror"] < cpt_all[minrow,"xerror"]+cpt_all[minrow,"xstd"]))
cplow.1se <- cpt_all[se.row,1]
cpup.1se <- ifelse(se.row==1, yes=1, no=cpt_all[se.row-1,1])
cp.1se <- sqrt(cplow.1se*cpup.1se)


prostate.prune <- prune(prostate.tree, cp=cp.1se)
prp(prostate.prune, type=1, extra=1, main="Prostate - final pruned tree")
prostate.prune
cpt_prune <- prostate.prune$cptable
summary(prostate.prune)
meanvar(prostate.prune)
printcp(prostate.prune)

# using 10 folds default CV
rss_root <- 1.3187 #found from printcp for both and its same
sMSE_all <- rss_root*min(cpt_all[,"rel error"])
MSPE_all <- rss_root*min(cpt_all[,"xerror"])
sMSE_prune <- rss_root*min(cpt_prune[,"rel error"])
MSPE_prune <- rss_root*min(cpt_prune[,"xerror"])

# Rsults
prostate.tree$splits
prostate.prune$splits
sMSE_all
sMSE_prune
MSPE_all
MSPE_prune

