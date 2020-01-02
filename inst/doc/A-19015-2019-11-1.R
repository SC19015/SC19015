## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
load("A-19015-2019-11-1.RData")
# sk <- function(x) {
#   #computes the sample skewness coeff.
#   xbar <- mean(x)
#   m3 <- mean((x - xbar)^3)
#   m2 <- mean((x - xbar)^2)
#   return( m3 / m2^1.5 )
# }

## -----------------------------------------------------------------------------
# n <- 50
# m <- 2500
# epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
# N <- length(epsilon)
# pwr.beta <- numeric(N)
# pwr.t <- numeric(N)
# #critical value for the skewness test
# cv <- qnorm(0.975, 0, sqrt(6 / n))
# for (j in 1:N) { #for each epsilon
#   e <- epsilon[j]
#   sktests <- numeric(m)
#   for (i in 1:m) { #for each replicate
#     a <- rbinom(n,1,prob = 1-e)
#     x <- rbeta(n, 2, 2)*a+rbeta(n,100, 100)*(1-a)
#     sktests[i] <- as.integer(abs(sk(x)) >= cv)
#   }
#   pwr.beta[j] <- mean(sktests)
# }

## -----------------------------------------------------------------------------
# for (j in 1:N) { #for each epsilon
#   e <- epsilon[j]
#   sktests <- numeric(m)
#   for (i in 1:m) { #for each replicate
#     a <- rbinom(n,1,prob = 1-e)
#     x <- rt(n, 5)*a+rt(n, 10)*(1-a)
#     sktests[i] <- as.integer(abs(sk(x)) >= cv)
#   }
#   pwr.t[j] <- mean(sktests)
# }

## -----------------------------------------------------------------------------
#plot power vs epsilon
plot(epsilon, pwr.beta, type = "l",
     xlab = bquote(epsilon), ylim = c(0,1),ylab = "power")
lines(epsilon, pwr.t,col = "red")
legend("topright",legend=c("beta","t"),col=c("black","red"),lty=1,lwd=2) 


## -----------------------------------------------------------------------------
# num<-c(50,100,200,500,1000) # Estimate the Type-I error for different sizes.
# m<-10000
# 
# er<-NULL
# for (n in num){
#   cv<-qt(0.975,n-1)
#   er1<-mean(sapply(1:m,FUN = function(o){
#   x<-rchisq(n,1)
#   m<-mean(x)
#   se<-sqrt(var(x))
#   abs((m-1)*sqrt(n)/se)>=cv
#   }))  # Estimate the Type-I error for chi-square distribution
#   er2<-mean(sapply(1:m,FUN = function(o){
#   x<-runif(n,0,2)
#   m<-mean(x)
#   se<-sqrt(var(x))
#   abs((m-1)*sqrt(n)/se)>=cv
#   }))   # Estimate the Type-I error for uniform distribution
#   er3<-mean(sapply(1:m,FUN = function(o){
#   x<-rexp(n,1)
#   m<-mean(x)
#   se<-sqrt(var(x))
#   abs((m-1)*sqrt(n)/se)>=cv
#   }))  # Estimate the Type-I error for exp distribution
#   er<-cbind(er,c(er1,er2,er3))
# }
# colnames(er)<-num
# rownames(er)<-c("chi(1)","U(0,2)","exp(1)")
knitr::kable(er)

