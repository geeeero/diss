################################################################################
# Code for durham interview talk graphics
################################################################################

setwd("/home/gero/work/_diss/v1/R")
source("/home/gero/work/_diss/v1/R/boatfunctions.R")
install.packages("luck", repos="http://R-Forge.R-project.org")
library("luck")

boat1 <- list(xp = c(1,6), a = 2, b = 0.8, yc = 0.5, data = list(tau = 4, n = 8))
res <- normalplotter(boat1, minmax=T)
# there is somewhere a bug...
miktonormal <- function(xylist){
  x <- xylist$x + 2
  y <- xylist$y/(xylist$x + 2) + 0.5
  list(x = x, y = y)
}
lm1 <- LuckModel(n0=c(3,8),y0=c(res$lower[2],res$upper[2]), data=list(tau=4, n=8))
#lm1 <- LuckModel(n0=c(1,6),y0=c(res$lower[2],res$upper[2]), data=list(tau=8, n=8))


postscript("boatshape-durham0913.ps", width=6, height=4.5)
par(mar=c(5,4.5,4,2)+0.1)
#
normalplotter(boat1, minmax=T, xlims=c(0,25))
plot(lm1, add=T, lty=2, control=controlList(polygonCol=NA, annotate=F))
#points(n0(lm1)[2]*c(1,1), y0(lm1), cex = 1.5)
#
normalplotter(boat1, minmax=T, prior=F, add=T)
plot(lm1, add=T, lty=2, control=controlList(polygonCol=NA, annotate=F, posterior=T))
points(updateLuckN(n0(lm1)[2], n(data(lm1)))*c(1,1), #
       updateLuckY(n0(lm1)[2], y0(lm1), tau(data(lm1)), n(data(lm1))), cex = 1.5)
#
boat1$data <- list(tau = 8, n = 16)
data(lm1)  <- list(tau = 8, n = 16)
normalplotter(boat1, minmax=T, prior=F, add=T)
plot(lm1, add=T, lty=2, control=controlList(polygonCol=NA, annotate=F, posterior=T))
points(updateLuckN(n0(lm1)[2], n(data(lm1)))*c(1,1), #
       updateLuckY(n0(lm1)[2], y0(lm1), tau(data(lm1)), n(data(lm1))), cex = 1.5)
#
boat1$data <- list(tau = 16, n = 16)
data(lm1)  <- list(tau = 16, n = 16)
normalplotter(boat1, minmax=T, prior=F, add=T)
plot(lm1, add=T, lty=2, control=controlList(polygonCol=NA, annotate=F, posterior=T))
points(updateLuckN(n0(lm1), n(data(lm1))), #
       updateLuckY(n0(lm1), c(y0(lm1)[2],y0(lm1)[1]), tau(data(lm1)), n(data(lm1))), cex = 1.5)
#
dev.off()


boat1$data <- list(tau = 20, n = 40)
data(lm1)  <- list(tau = 20, n = 40)
# strange behaviour of minmax
normalplotter(boat1, minmax=T, prior=F, xlims=c(40,50), ylims=c(0.4,0.6), minmaxtol=1e-3)
plot(lm1, add=T, lty=2, control=controlList(polygonCol=NA, annotate=F, posterior=T))




