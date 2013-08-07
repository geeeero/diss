# strange behaviour of conjugate HD intervals in comparison to whitcomb:
# checking variance of Gamma prior and pdc posterior with n=10
varfun <- function(n0y0,posterior=F){
  n0 <- n0y0[1]
  y0 <- n0y0[2]
  if(posterior){
    n0p <- n0+10
    y0p <- (n0*y0 + 1.25)/(n0+10)
    n0 <- n0p
    y0 <- y0p
  }
  (n0+1)/(n0^2*y0^2)
} 
#prior n0 = [2.39,2.85], y0=[2.91,4.08]
res <- optim(par=c(2.5,3), fn=varfun, method="L-BFGS-B", lower=c(2.39,2.91), upper=c(2.85,4.08))$value
res <- optim(par=c(2.5,3), fn=varfun, method="L-BFGS-B", lower=c(2.39,2.91), upper=c(2.85,4.08),
             control=list(fnscale=-1))$value
#posterior
res <- optim(par=c(12.5,0.75), fn=varfun, method="L-BFGS-B", lower=c(12.39,0.66), upper=c(12.85,1),
             posterior=T)$value
res <- optim(par=c(12.5,0.75), fn=varfun, method="L-BFGS-B", lower=c(12.39,0.66), upper=c(12.85,1),
             control=list(fnscale=-1),posterior=T)$value

#