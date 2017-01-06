#################################################
## Name        -   Siddharth Baronia       ######
#################################################

library(datasets)
library(splines)
head(discoveries)

# converting to dataframe 
discoveries_dframe <- data.frame(discoveries_num=as.matrix(discoveries),year=as.matrix(time(discoveries)))

# a - plotting the discoveries to see the trend
plot (discoveries,main="# of inventions/year")

######################################
####     Natural Splines          ####
######################################

# c - fitting natural cubic splines
plot(discoveries, main="# of inventions/year Natural Splines")
legend("topright",legend=c("df3","df5","df7","df11","df15","df23","df29","df35","df41"), lty="solid",
       col=colors()[c(121,145,84,90,50,111,135,27,55)], lwd=2)

# 3 DF natural spline
head(ns(discoveries_dframe$year,df=3))
nat.spl.3 <- lm(data=discoveries_dframe, discoveries_num ~ ns(discoveries_dframe$year,df=3))
summary(nat.spl.3)
lines(x=discoveries_dframe$year, y=predict(nat.spl.3, newdata=discoveries_dframe), col=colors()[121], lwd=2)

# 5 DF natural spline
head(ns(discoveries_dframe$year,df=5))
nat.spl.5 <- lm(data=discoveries_dframe, discoveries_num ~ ns(discoveries_dframe$year,df=5))
summary(nat.spl.5)
lines(x=discoveries_dframe$year, y=predict(nat.spl.5, newdata=discoveries_dframe), col=colors()[145], lwd=2)

# 7 DF natural spline
head(ns(discoveries_dframe$year,df=7))
nat.spl.7 <- lm(data=discoveries_dframe, discoveries_num ~ ns(discoveries_dframe$year,df=7))
summary(nat.spl.7)
lines(x=discoveries_dframe$year, y=predict(nat.spl.7, newdata=discoveries_dframe), col=colors()[84], lwd=2)

# 11 DF natural spline
head(ns(discoveries_dframe$year,df=11))
nat.spl.11 <- lm(data=discoveries_dframe, discoveries_num ~ ns(discoveries_dframe$year,df=11))
summary(nat.spl.11)
lines(x=discoveries_dframe$year, y=predict(nat.spl.11, newdata=discoveries_dframe), col=colors()[90], lwd=2)

# 15 DF natural spline
head(ns(discoveries_dframe$year,df=15))
nat.spl.15 <- lm(data=discoveries_dframe, discoveries_num ~ ns(discoveries_dframe$year,df=15))
summary(nat.spl.15)
lines(x=discoveries_dframe$year, y=predict(nat.spl.15, newdata=discoveries_dframe), col=colors()[50], lwd=2)

# 23 DF natural spline
head(ns(discoveries_dframe$year,df=23))
nat.spl.23 <- lm(data=discoveries_dframe, discoveries_num ~ ns(discoveries_dframe$year,df=23))
summary(nat.spl.23)
lines(x=discoveries_dframe$year, y=predict(nat.spl.23, newdata=discoveries_dframe), col=colors()[111], lwd=2)

# 29 DF natural spline
head(ns(discoveries_dframe$year,df=29))
nat.spl.29 <- lm(data=discoveries_dframe, discoveries_num ~ ns(discoveries_dframe$year,df=29))
summary(nat.spl.29)
lines(x=discoveries_dframe$year, y=predict(nat.spl.29, newdata=discoveries_dframe), col=colors()[135], lwd=2)

# 35 DF natural spline
head(ns(discoveries_dframe$year,df=35))
nat.spl.35 <- lm(data=discoveries_dframe, discoveries_num ~ ns(discoveries_dframe$year,df=35))
summary(nat.spl.35)
lines(x=discoveries_dframe$year, y=predict(nat.spl.35, newdata=discoveries_dframe), col=colors()[27], lwd=2)

# 41 DF natural spline
head(ns(discoveries_dframe$year,df=41))
nat.spl.41 <- lm(data=discoveries_dframe, discoveries_num ~ ns(discoveries_dframe$year,df=41))
summary(nat.spl.41)
lines(x=discoveries_dframe$year, y=predict(nat.spl.41, newdata=discoveries_dframe), col=colors()[55], lwd=2)


######################################
####     Smoothing Splines        ####
######################################


plot(discoveries, main="# of inventions/year Smoothing Splines")
legend("topright",legend=c("df3","df5","df7","df11","df15","df23","df29","df35","df41"), lty="solid",
       col=colors()[c(121,145,84,90,50,111,135,27,55)], lwd=2)

# 3 DF smoothing spline
sm.spl.opt3 <- smooth.spline(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, cv=FALSE, df=3)
sm.spl.opt3
lines(sm.spl.opt3, col=colors()[121], lwd=2)

# 5 DF smoothing spline
sm.spl.opt5 <- smooth.spline(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, cv=FALSE, df=5)
sm.spl.opt5
lines(sm.spl.opt5, col=colors()[145], lwd=2)

# 7 DF smoothing splines
sm.spl.opt7 <- smooth.spline(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, cv=FALSE, df=7)
sm.spl.opt7
lines(sm.spl.opt7, col=colors()[84], lwd=2)

# 11 DF smoothing splines
sm.spl.opt11 <- smooth.spline(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, cv=FALSE, df=11)
sm.spl.opt11
lines(sm.spl.opt11, col=colors()[90], lwd=2)

#15 DF smoothing splines
sm.spl.opt15 <- smooth.spline(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, cv=FALSE, df=15)
sm.spl.opt15
lines(sm.spl.opt15, col=colors()[50], lwd=2)

# 23 DF smoothing splines
sm.spl.opt23 <- smooth.spline(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, cv=FALSE, df=23)
sm.spl.opt23
lines(sm.spl.opt23, col=colors()[111], lwd=2)

# 29 DF smooting splines
sm.spl.opt29 <- smooth.spline(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, cv=FALSE, df=29)
sm.spl.opt29
lines(sm.spl.opt29, col=colors()[135], lwd=2)

# 35 DF smoothing splines
sm.spl.opt35 <- smooth.spline(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, cv=FALSE, df=35)
sm.spl.opt35
lines(sm.spl.opt35, col=colors()[27], lwd=2)

# 41 DF smooting splines
sm.spl.opt41 <- smooth.spline(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, cv=FALSE, df=41)
sm.spl.opt41
lines(sm.spl.opt41, col=colors()[55], lwd=2)

