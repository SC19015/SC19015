## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
  load('A-19015-2019-10-18.RData')
#   set.seed(123)
#   n<-20
#   c<-qt(0.975,n-1) # 0.975 quantile of t-distribution
# cv.t<-sapply(1:1000,FUN= function(o){
#   x<-rchisq(n,2)
#   m<-mean(x) # estimate of mean
#   se<-sqrt(var(x)) # estimate of standard error
#   as.numeric((m-c*se/sqrt(n)<2)&(m+c*se/sqrt(n)>2)) # ci
# })
mean(cv.t) # mean of  Monte Carlo experiment

## -----------------------------------------------------------------------------
  # alpha <- .05
  # UCL <- replicate(1000, expr = {
  # x <- rchisq(n,2)
  # (n-1) * var(x) / qchisq(alpha, df = n-1)
  # } )
#count the number of intervals that contain sigma^2=4
sum(UCL > 4)/1000

## -----------------------------------------------------------------------------
# n2<-100
# res<-sapply(1:1000,FUN = function(o){
#   x<-rnorm(n2)
#   m<-mean(x)
#   s<-var(x)
#   bs<-mean((x-m)^3)/s^(1.5)
# })

## -----------------------------------------------------------------------------
  # q<-quantile(res,probs = seq(0, 1, 0.025))[c(2,3,39,40)]
q

## -----------------------------------------------------------------------------
# p<-c(0.025,0.05,0.95,0.975)
# d<-dnorm(q,0,6/n) # Compute the density of q as N(0,6/n)
# vq<-(1-p)*p/(n*d^2) # compute the variance using the formula above
sqrt(vq)

## -----------------------------------------------------------------------------
#  q.real<-qnorm(p,0,6/n)
# out<-data.frame(rbind(q,q.real),row.names = c("Estimates","True value"))
knitr::kable(out)

