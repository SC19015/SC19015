## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
load('A-19015-2019-12-06.RData')
exp(log(20)) == log(exp(20))
all.equal(exp(log(20)),log(exp(20)))

## -----------------------------------------------------------------------------
# gammaratio <- function(k){
#   if (k %% 2==0) 
#     return(exp(sum(log((k/2-1):1))-sum(log(seq(1,k-3,2)/2))-log(gamma(1/2))))
#   else
#     return(exp(sum(log(seq(1,k-2,2)/2))+log(gamma(1/2))-sum(log(((k-1)/2-1):1))))
# }   ### Gamma ratio computing for large k
# g<- function(a,k){
#   f <- function(x, N) (1 + x^2/N)^(-(N + 1)/2)
#   I1 <- integrate(f,lower = 0,upper = sqrt(a^2*(k-1)/(k-a^2)),rel.tol=.Machine$double.eps^0.25,N = k-1 )$value
#   I2 <- integrate(f,lower = 0,upper = sqrt(a^2*k/(k+1-a^2)),rel.tol=.Machine$double.eps^0.25,N = k )$value
#   return(2*gammaratio(k)*I1/sqrt(k-1)-2*gammaratio(k+1)*I2/sqrt(k))
# }   # the equation in 11.5
# f1 <- function(x,k) pt(sqrt(x^2*(k-1)/(k-x^2)),k-1)-pt(sqrt(x^2*k/(k+1-x^2)),k)
# ##  computing method in 11.4
# a <- seq(0.001,6,0.0001)
# y1 <-numeric(length(a))
# for ( i in 1:length(a)){
#   y1[i] <- g(a[i],100)
# }
plot(cbind(a,y1),type = "l",xlab="a",ylab = "value")

## -----------------------------------------------------------------------------
# nk <- c(4:25,100,500,1000)
# ak <-numeric(25)
# qk <-numeric(25)
# for (i in 1:25) {
#   ak[i] <- uniroot(g,c(0.00001,ifelse(nk[i] <=7,sqrt(nk[i])-0.01,2.5)),k=nk[i])$root # the solution of the equation
#   qk[i] <- uniroot(f1,c(0.00001,ifelse(nk[i] <=7,sqrt(nk[i])-0.01,2.5)),k=nk[i])$root # the solution of 11.4
# }
cbind(ak,qk)
plot(cbind(nk,ak),xlab = "k",ylab = "A(k)")
lines(cbind(nk,ak),lwd =2,col = "red")

## -----------------------------------------------------------------------------
# loglike <- function(pq,ng){
#   p <- pq[1]
#   q <- pq[2]
#   r <- 1-p -q
#   a <- c(p^2,q^2,r^2,2*p*r,2*q*r,2*p*q)
#   return(-sum(log(a)*ng))
# }
# na <- 28
# nb <- 24
# noo <- 41
# nab <- 70
# n <-na + nb +noo+nab
# eloglike <- function(pq){
#   p <- pq[1]
#   q <- pq[2]
#   r <- 1-p -q
#   a <- c(p^2+2*p*r,q^2+2*q*r,r^2,2*p*q)
#   ng <- c(na,nb,noo,nab)
#   return(-sum(log(a)*ng))
# }
# r <- sqrt(noo/n)
# f2 <- function(x) x^2 +2*x*r - na/n
# p <- uniroot(f2,c(0,1))$root
# q <- 1-p-r
# m <- 200
# pqr <- matrix(rep(0,m*3),nrow = m)
# ll <- numeric(m)
# pqr[1,]<-c(p,q,r)
# for (i in 2:m) {
#   p <- pqr[i-1,1]
#   q <- pqr[i-1,2]
#   r <- pqr[i-1,3]
#   naa <- p/(p+2*r)*na
#   nao <- 2*r/(p+2*r)*na
#   nbb <- q/(q+2*r)*nb
#   nbo <- 2*r/(q+2*r)*nb
#   ng <- c(naa,nbb,noo,nao,nbo,nab)  #estimation step
#   res <- optim(c(p,q),loglike,method="Nelder-Mead",ng=ng) #maximum step
#   pqr[i,]<-c(res$par,1-res$par[1]-res$par[2])
#   ll[i] <- -eloglike(res$par)
# }

## -----------------------------------------------------------------------------
pqr[200,]

## -----------------------------------------------------------------------------
# res <- optim(c(p,q),eloglike,method="Nelder-Mead")
res$par
plot(ll[-1],type = "l",ylab = "log-likelihood")
abline(a=ll[200],b=0,col="red")

