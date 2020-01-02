## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning = FALSE,message = FALSE)

## -----------------------------------------------------------------------------
load('A-19015-2019-11-29.RData')
# lapla <- function(x) 1/2*exp(-abs(x))
# 
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
# 
# N <- 2000
# sigma <- c(.05, .5,1, 2, 5,16)
# x0 <- 0
# rw1 <- rw.Metropolis(sigma[1], x0, N)
# rw2 <- rw.Metropolis(sigma[2], x0, N)
# rw3 <- rw.Metropolis(sigma[3], x0, N)
# rw4 <- rw.Metropolis(sigma[4], x0, N)
# rw5 <- rw.Metropolis(sigma[5], x0, N)
# rw6 <- rw.Metropolis(sigma[6], x0, N)


## -----------------------------------------------------------------------------
print(c(rw1$k, rw2$k, rw3$k, rw4$k,rw5$k,rw6$k))

## -----------------------------------------------------------------------------
par(mfrow=c(2,3)) #display 6 graphs together
refline <- c(log(0.05),-log(0.05))
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x,rw5$x,rw6$x)
colnames(rw) <- sigma
for (j in 1:6) {
  plot(rw[,j], type="l",xlab=bquote(sigma == .(round(sigma[j],3))),  ylab="X", ylim=range(rw[,j]))
  abline(h=refline)
}
par(mfrow=c(1,1)) #reset to default
par(mfrow=c(2,3)) 
for (j in 1:6) {
  hist(rw[501:N,j],probability =  TRUE,ylim = c(0,0.55),breaks = 50,main = paste("sigma = ",sigma[j]), xlab = "x")
  curve(lapla,add = TRUE,lwd=2,col="red")
}
par(mfrow=c(1,1))
# a <- c(.05, seq(.1, .9, .1), .95)
# Q <- ifelse(a<0.5,log(2*a),-log(2*(1-a)))
# mc <- rw[501:N, ]
# Qrw <- apply(mc, 2, function(x) quantile(x, a))
print(round(cbind(Q, Qrw), 3)) 
par(mfrow=c(2,3)) 
for (j in 1:6) {
  plot(Q,Qrw[,j],main = "Q-Q plots")
  abline(a=0,b=1,add = TRUE,lwd=2,col="red")
}
par(mfrow=c(1,1))

