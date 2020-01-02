## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning = FALSE,message = FALSE)

## -----------------------------------------------------------------------------
load("A-19015-2019-11-15.RData")
# library(bootstrap)
# sc<-scor
# theta<-function(x){ # function used to compute theta
#   sigma<-cov(x)
#   pca.sigma<-prcomp(sigma)
#   theta<-pca.sigma$sdev[1]/sum(pca.sigma$sdev)
#   theta
# }
# n<-dim(sc)[1]
# theta.j<- numeric(n)
# for (i in 1:n){    # compute theta for each leave-one-out sample
#   theta.j[i]<-theta(sc[-i,])
# }
# theta.hat<-theta(sc)
# bias<-(n-1)*(mean(theta.j)-theta.hat) #BIAS
# ss<-(n-1)*var(theta.j)
# se<-sqrt(ss) #SE
c(BIAS=bias,SE=se)

## -----------------------------------------------------------------------------
library(DAAG,quietly=TRUE); attach(ironslag)
a <- seq(10, 40, .1)     #sequence for plotting fits
r<-numeric(4)  
L1 <- lm(magnetic ~ chemical)
plot(chemical, magnetic, main="Linear", pch=16)
yhat1 <- L1$coef[1] + L1$coef[2] * a
lines(a, yhat1, lwd=2)
r[1]<-summary(L1)$adj.r.squared

L2 <- lm(magnetic ~ chemical + I(chemical^2))
plot(chemical, magnetic, main="Quadratic", pch=16)
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
lines(a, yhat2, lwd=2)
r[2]<-summary(L2)$adj.r.squared

L3 <- lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential", pch=16)
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)
lines(a, yhat3, lwd=2)
r[3]<-summary(L3)$adj.r.squared

L4 <- lm(magnetic ~ chemical + I(chemical^2)+ I(chemical^3))
plot(chemical, magnetic, main="Cubic", pch=16)
hat4 <- L4$coef[1] + L4$coef[2] * a + L4$coef[3] * a^2 + L4$coef[4] * a^3
lines(a, hat4, lwd=2)
r[4]<-summary(L4)$adj.r.squared

## -----------------------------------------------------------------------------
# n <- length(magnetic) #in DAAG ironslag
# e1 <- e2 <- e3 <- e4 <- numeric(n)
# # for n-fold cross validation
# # fit models on leave-one-out samples
# for (k in 1:n) {
#   y <- magnetic[-k]
#   x <- chemical[-k]
#   J1 <- lm(y ~ x)
#   yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
#   e1[k] <- magnetic[k] - yhat1
#   
#   J2 <- lm(y ~ x + I(x^2))
#   yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
#     J2$coef[3] * chemical[k]^2
#   e2[k] <- magnetic[k] - yhat2
#   
#   J3 <- lm(log(y) ~ x)
#   logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
#   yhat3 <- exp(logyhat3)
#   e3[k] <- magnetic[k] - yhat3
#   
#   J4 <- lm(y ~ x + I(x^2)+I(x^3))
#   yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k] +J4$coef[3] * chemical[k]^2 +J4$coef[3] * chemical[k]^3
#   e4[k] <- magnetic[k] - yhat4
# }

## -----------------------------------------------------------------------------
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## -----------------------------------------------------------------------------
r

