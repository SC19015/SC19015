## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
load('A-19015-2019-12-20.RData')
# library(Rcpp)
# sourceCpp("metr.cpp")
# lapla <- function(x) 1/2*exp(-abs(x))
# rw.Metropolis <- function(sigma, x0, N) {
#   x <- numeric(N)
#   x[1] <- x0
#   u <- runif(N)
#   k <- 0
#   for (i in 2:N) {
#     y <- rnorm(1, x[i-1], sigma)
#     if (u[i] <= (lapla(y) / lapla(x[i-1])))
#       x[i] <- y else {
#         x[i] <- x[i-1]
#         k <- k + 1
#       }
#   }
#   return(list(x=x, k=k))
# }

## -----------------------------------------------------------------------------
# N <- 2000
# sigma <- c(.05, .5,1, 2, 5,16)
# x0 <- 0
# rwc1 <- Metr(sigma[1],N)
# rwc2 <- Metr(sigma[2], N)
# rwc3 <- Metr(sigma[3], N)
# rwc4 <- Metr(sigma[4], N)
# rwc5 <- Metr(sigma[5], N)
# rwc6 <- Metr(sigma[6], N)
# rw1 <- rw.Metropolis(sigma[1], x0, N)
# rw2 <- rw.Metropolis(sigma[2], x0, N)
# rw3 <- rw.Metropolis(sigma[3], x0, N)
# rw4 <- rw.Metropolis(sigma[4], x0, N)
# rw5 <- rw.Metropolis(sigma[5], x0, N)
# rw6 <- rw.Metropolis(sigma[6], x0, N)
par(mfrow=c(2,3))
qqplot(rwc1$sample[500:2000],rw1$x[500:2000],xlab = "cpp",ylab = "r",main = bquote(sigma == .(round(sigma[1],3))))
qqplot(rwc2$sample[500:2000],rw2$x[500:2000],xlab = "cpp",ylab = "r",main = bquote(sigma == .(round(sigma[2],3))))
qqplot(rwc3$sample[500:2000],rw3$x[500:2000],xlab = "cpp",ylab = "r",main = bquote(sigma == .(round(sigma[3],3))))
qqplot(rwc4$sample[500:2000],rw4$x[500:2000],xlab = "cpp",ylab = "r",main = bquote(sigma == .(round(sigma[4],3))))
qqplot(rwc5$sample[500:2000],rw5$x[500:2000],xlab = "cpp",ylab = "r",main = bquote(sigma == .(round(sigma[5],3))))
qqplot(rwc6$sample[500:2000],rw6$x[500:2000],xlab = "cpp",ylab = "r",main = bquote(sigma == .(round(sigma[6],3))))
par(mfrow = c(1,1))

## -----------------------------------------------------------------------------
# library(microbenchmark)
# ts <- microbenchmark(MetroR=rw.Metropolis(sigma[3], x0,N),Metrocppp=Metr(sigma[3], N))
summary.ts

