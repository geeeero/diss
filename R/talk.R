################################################################################
# Code for talk graphics
################################################################################

# LuckModel package luck
setwd("/home/gero/work/_diss/v1/R")
source("/home/gero/work/QdC+n/R/package/luck/R/00-01_LuckModelData.r")
source("/home/gero/work/QdC+n/R/package/luck/R/00-02_LuckModel.r")
source("/home/gero/work/QdC+n/R/package/luck/R/00-03_utilLuckModel.r")
source("/home/gero/work/QdC+n/R/package/luck/R/00-04_unionHdiLuckModel.r")
source("/home/gero/work/QdC+n/R/package/luck/R/00-05_plotLuckModel.r")
source("/home/gero/work/QdC+n/R/package/luck/R/00-06_cdfplotLuckModel.r")
source("/home/gero/work/QdC+n/R/package/luck/R/01-01_ScaledNormalData.r")
source("/home/gero/work/QdC+n/R/package/luck/R/01-02_ScaledNormal.r")

# betashape functions
betafu <- function(betaobj, n0, wh){
  betaobj$yl[wh] + (betaobj$yr[wh] - betaobj$yl[wh])*((n0-betaobj$n[1])/(betaobj$n[2]-betaobj$n[1]))^betaobj$beta[wh]
}
betaplotter <- function(betaobj, prior = TRUE, ylims = c(0,1), minmax = TRUE, seqn = 100, data = list(tau = 2, n = 8), fillcol = "gray", add = FALSE, xylabs = TRUE, ...){
  nseqf <- seq(betaobj$n[1], betaobj$n[2], length = seqn)
  nseqb <- seq(betaobj$n[2], betaobj$n[1], length = seqn)
  lower <- betafu(betaobj = betaobj, n0 = nseqf, wh = 1)
  upper <- betafu(betaobj = betaobj, n0 = nseqb, wh = 2)
  if(!prior){
    lower <- updateLuckY(nseqf, lower, data$tau, data$n)
    upper <- updateLuckY(nseqb, upper, data$tau, data$n)
    nseqf <- updateLuckN(nseqf, data$n)
    nseqb <- updateLuckN(nseqb, data$n)
  }
  # if(prior) xlabs <- "$\\nz$" else xlabs <- "$\\nn$"
  # if(prior) ylabs <- "$\\yz$" else ylabs <- "$\\yn$"
  if(prior) xlabs <- bquote(n^{(0)}) else xlabs <- bquote(n^{(n)})
  if(prior) ylabs <- bquote(y^{(0)}) else ylabs <- bquote(y^{(n)})
  if(!xylabs){
    xlabs = ""
    ylabs = ""
  }
  if(!add){
    plot(x = c(nseqf, nseqb), y = c(lower, upper), ylim = ylims, type = "n",
         xlab = xlabs, ylab = ylabs, ...)
  }
  polygon(c(nseqf, nseqb), c(lower, upper), col = fillcol, ...)
  minmaxn <- c(nseqf[which.min(lower)], nseqb[which.max(upper)])
  minmaxy <- c(min(lower), max(upper))
  if(minmax)
    points(minmaxn, minmaxy, cex = 1.5)
}


par(lwd = 3, mar=c(3,3,0,0))
#pdf("betashape11.pdf", width = 8, height = 4)
#dev.off()


lm0 <- LuckModel(n0=5, y0=0.35, data=LuckModelData(tau=9,n=10))

pdf("shape0.pdf", width = 6, height = 5)
plot(lm0, control=controlList(annotate=F), ylim=c(0,1), xlim=c(1,18))
mtext(text = bquote(n^(0)), 1, line = 2, adj = 0.5, col="blue") 
mtext(text = bquote(y^(0)), 2, line = 2, adj = 0.5, col="red")
plot(lm0, control=controlList(annotate=F, posterior=T), add=T)
points(c(5,15),c(0.35,updateLuckY(5, 0.35, 9, 10)), pch=c(20,20))
dev.off()

lm1 <- LuckModel(n0=5, y0=c(0.1,0.6), data=LuckModelData(tau=9,n=10))

pdf("shape1.pdf", width = 6, height = 5)
plot(lm1, control=controlList(annotate=F), ylim=c(0,1), xlim=c(1,18))
mtext(text = bquote(n^(0)), 1, line = 2, adj = 0.5, col="blue") 
mtext(text = bquote(y^(0)), 2, line = 2, adj = 0.5, col="red")
plot(lm1, control=controlList(annotate=F, posterior=T), add=T)
dev.off()

lm2 <- LuckModel(n0=c(1,8), y0=c(0.1,0.6), data=LuckModelData(tau=9,n=10))

pdf("shape2.pdf", width = 6, height = 5)
plot(lm2, control=controlList(annotate=F), ylim=c(0,1), xlim=c(1,18))
mtext(text = bquote(n^(0)), 1, line = 2, adj = 0.5, col="blue") 
mtext(text = bquote(y^(0)), 2, line = 2, adj = 0.5, col="red")
plot(lm2, control=controlList(annotate=F, posterior=T), add=T)
dev.off()

eggplant <- list(n = c(1,8), yr = c(0.25,0.3), yl = c(0.1, 0.6), beta = c(8,8))
data1 <- list(tau = 9, n = 10)

pdf("shape3.pdf", width = 6, height = 5)
betaplotter(eggplant, xlim=c(1,18), minmax=F, xylab=F)
mtext(text = bquote(n^(0)), 1, line = 2, adj = 0.5, col="blue") 
mtext(text = bquote(y^(0)), 2, line = 2, adj = 0.5, col="red")
betaplotter(eggplant, prior=F, data = data1, add=T, minmax=F)
dev.off()




#