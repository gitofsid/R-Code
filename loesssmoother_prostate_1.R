#################################################
## Name        -   Siddharth Baronia       ######
#################################################

prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)

# 3D plot
library(rgl)  
open3d()
plot3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

# Loess smoother, degree 0,1,2 and span=1
lo.1.0 <- loess(data=prostate, lpsa~ lcavol + lweight, span=1, degree=0)
lo.1.1 <- loess(data=prostate, lpsa~ lcavol + lweight, span=1, degree=1)
lo.1.2 <- loess(data=prostate, lpsa~ lcavol + lweight, span=1, degree=2)

# Loess smoother, degree 0,1,2 and span=.75
lo.75.0 <- loess(data=prostate, lpsa~ lcavol + lweight, span=.75, degree=0)
lo.75.1 <- loess(data=prostate, lpsa~ lcavol + lweight, span=.75, degree=1)
lo.75.2 <- loess(data=prostate, lpsa~ lcavol + lweight, span=.75, degree=2)

# Loess smoother, degree 0,1,2 and span=.50
lo.50.0 <- loess(data=prostate, lpsa~ lcavol + lweight, span=.50, degree=0)
lo.50.1 <- loess(data=prostate, lpsa~ lcavol + lweight, span=.50, degree=1)
lo.50.2 <- loess(data=prostate, lpsa~ lcavol + lweight, span=.50, degree=2)

# Loess smoother, degree 0,1,2 and span=.25
lo.25.0 <- loess(data=prostate, lpsa~ lcavol + lweight, span=.25, degree=0)
lo.25.1 <- loess(data=prostate, lpsa~ lcavol + lweight, span=.25, degree=1)
lo.25.2 <- loess(data=prostate, lpsa~ lcavol + lweight, span=.25, degree=2)

x1 <- seq(from=-1.5, to=4, by=.1)
xy1 <- expand.grid(lcavol=seq(from=-1.5, to=4, by=.1), 
                    lweight=seq(from=2, to=5, by=.1))

####### plot span 1 degree 0,1,2 ########
surface = predict(lo.1.0, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

surface = predict(lo.1.1, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

surface = predict(lo.1.2, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

###### plot span .75 degree 0,1,2 #########
surface = predict(lo.75.0, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

surface = predict(lo.75.1, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

surface = predict(lo.75.2, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")


###### plot span .50 degree 0,1,2 #######
surface = predict(lo.50.0, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

surface = predict(lo.50.1, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

surface = predict(lo.50.2, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")


####### plot span .25 degree 0,1,2 ######
surface = predict(lo.25.0, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

surface = predict(lo.25.1, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")

surface = predict(lo.25.2, newdata=xy1)
# 3D plot.
open3d()
persp3d(x = seq(from=-1.5, to=4, by=.1), y = seq(from=2, to=5, by=.1), 
        z = surface,col = "orange", xlab="lcavol", ylab="lweight", 
        zlab="lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$lweight, col="blue")