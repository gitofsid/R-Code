#################################################
## Name        -   Siddharth Baronia       ######
#################################################


library(datasets)
head(discoveries)

# converting to dataframe 
discoveries_dframe <- data.frame(discoveries_num=as.matrix(discoveries),year=as.matrix(time(discoveries)))

############################
## Loess Kernel Smoothers ##
############################

plot(discoveries, main="# of inventions/year Loess Kernel Smoothers")
legend("topright",legend=c("100.0","100.1","100.2","75.0","75.1","75.2","50.0","50.1","50.2","25.0","25.1","25.2"), lty="solid",
       col=colors()[c(121,145,84,90,50,111,135,27,55,34,93,81)], lwd=2)

# span of 1 and degree = 0,1,2
lo.100.0 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=1, degree=0)
summary(lo.100.0)
lines(x=discoveries_dframe$year, y=predict(lo.100.0, newdata=discoveries_dframe), col=colors()[121])

lo.100.1 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=1, degree=1)
summary(lo.100.1)
lines(x=discoveries_dframe$year, y=predict(lo.100.1, newdata=discoveries_dframe), col=colors()[145])

lo.100.2 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=1, degree=2)
summary(lo.100.2)
lines(x=discoveries_dframe$year, y=predict(lo.100.2, newdata=discoveries_dframe), col=colors()[84])

# span of .75 and degree = 0,1,2
lo.75.0 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=.75, degree=0)
summary(lo.75.0)
lines(x=discoveries_dframe$year, y=predict(lo.75.0, newdata=discoveries_dframe), col=colors()[90])

lo.75.1 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=.75, degree=1)
summary(lo.75.1)
lines(x=discoveries_dframe$year, y=predict(lo.75.1, newdata=discoveries_dframe), col=colors()[50])

lo.75.2 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=.75, degree=2)
summary(lo.75.2)
lines(x=discoveries_dframe$year, y=predict(lo.75.2, newdata=discoveries_dframe), col=colors()[111])

# span of .50 and degree = 0,1,2
lo.50.0 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=.50, degree=0)
summary(lo.50.0)
lines(x=discoveries_dframe$year, y=predict(lo.50.0, newdata=discoveries_dframe), col=colors()[135])

lo.50.1 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=.50, degree=1)
summary(lo.50.1)
lines(x=discoveries_dframe$year, y=predict(lo.50.1, newdata=discoveries_dframe), col=colors()[27])

lo.50.2 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=.50, degree=2)
summary(lo.50.2)
lines(x=discoveries_dframe$year, y=predict(lo.50.2, newdata=discoveries_dframe), col=colors()[55])

# span of .25 and degree = 0,1,2
lo.25.0 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=.25, degree=0)
summary(lo.25.0)
lines(x=discoveries_dframe$year, y=predict(lo.25.0, newdata=discoveries_dframe), col=colors()[34])

lo.25.1 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=.25, degree=1)
summary(lo.25.1)
lines(x=discoveries_dframe$year, y=predict(lo.25.1, newdata=discoveries_dframe), col=colors()[93])

lo.25.2 <- loess(data=discoveries_dframe, discoveries_num ~ discoveries_dframe$year, span=.25, degree=2)
summary(lo.25.2)
lines(x=discoveries_dframe$year, y=predict(lo.25.2, newdata=discoveries_dframe), col=colors()[81])


###############################
### Normal Kernel      ########
###############################


plot(discoveries, main="# of inventions/year Normal Kernel Smoothers")
legend("topright",legend=c("50.0","50.1","50.2","1.0","1.1","1.2","opt.0","opt.1","opt.2"), lty="solid",
       col=colors()[c(121,145,84,90,50,111,135,27,55)], lwd=2)

library(KernSmooth)

# bandwidth of .50 and degree 0,1,2
lp.50.0 <- locpoly(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, bandwidth=.50, degree=0)
lines(lp.50.0, col=colors()[121])

lp.50.1 <- locpoly(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, bandwidth=.50, degree=1)
lines(lp.50.1, col=colors()[145])

lp.50.2 <- locpoly(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, bandwidth=.50, degree=2)
lines(lp.50.2, col=colors()[84])

# bandwidth of 1 and degree 0,1,2
lp.100.0 <- locpoly(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, bandwidth=1, degree=0)
lines(lp.100.0, col=colors()[90])

lp.100.1 <- locpoly(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, bandwidth=1, degree=1)
lines(lp.100.1, col=colors()[50])

lp.100.2 <- locpoly(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, bandwidth=1, degree=2)
lines(lp.100.2, col=colors()[111])

# optimal bandwidth and degree 0,1,2
lambda = dpill(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num)
lambda

lp.opt.0 <- locpoly(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, bandwidth=lambda, degree=0)
lines(lp.opt.0, col=colors()[135])

lp.opt.1 <- locpoly(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, bandwidth=lambda, degree=1)
lines(lp.opt.1, col=colors()[27])

lp.opt.2 <- locpoly(x=discoveries_dframe$year, y=discoveries_dframe$discoveries_num, bandwidth=lambda, degree=2)
lines(lp.opt.2, col=colors()[55])



