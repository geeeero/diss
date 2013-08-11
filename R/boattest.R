###########################################
# test for boat shapes in Mik's world #
###########################################

# display the domain \Eta
postscript("boatshape-domain.ps" width=5, height=5)
domainplotter(xlims=c(-2,8), ylims=c(-5,5))
dev.off()

bsp1 <- list(xp = c( 1,6), a = 2, b = 1/4, yc = 0.6, data = list(tau = 0, n = 0))
bsp1 <- list(xp = c(-1,7), a = 1, b = 1/2, yc = 0.5, data = list(tau = 6, n = 10))

boatplotter(bsp1)
boatplotter(bsp1, prior=F)
boatplotter(bsp1, add=T)

boatplotter(bsp1, prior = F, add=T, col = 2)

normalplotter(bsp1)
normalplotter(bsp1, prior=F)
normalplotter(bsp1, add=T)


# Beispiel, bei dem priori und posteriori touchpoints in der Mitte der Kontur sind (nicht an Enden)
bsp1 <- list(xp = c(0,14), a = 2, b = 1/4, yc = 0.6, data = list(tau = 2.5, n = 5))

#par(mar=c(4,5,0,0))
pdf("boatshape-talk130306.pdf", width=8, height=4)
talk <- list(xp = c(1,6), a = 2, b = 1/2, yc = 0.5, data = list(tau = 8, n = 8))
normalplotter(talk, prior=F, xlims=c(0,23), xlabs="", ylabs="")
normalplotter(talk, add=T)
talk$data <- list(tau = 4, n = 8)
normalplotter(talk, prior=F, add=T)
talk$data <- list(tau = 8, n = 16)
normalplotter(talk, prior=F, add=T)
talk$data <- list(tau = 16, n = 16)
normalplotter(talk, prior=F, add=T)
mtext(text = bquote(n^(0)), 1, line = 2, adj = 0.5, col="blue") 
mtext(text = bquote(y^(0)), 2, line = 2, adj = 0.5, col="red")
dev.off()

#