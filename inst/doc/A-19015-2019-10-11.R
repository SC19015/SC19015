## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
load("A-19015-2019-10-11.RData")
#   set.seed(2333)
# x1<-runif(1000,0,pi/3)
# theta1<-pi/3*mean(sin(x1)) #estimate
# theta1.real<-1-cos(pi/3) #true value


## ----echo=FALSE---------------------------------------------------------------
  c(theta1,theta1.real)

## -----------------------------------------------------------------------------
# f<-function(x) exp(-x)/(1+x^2)
# res2<-sapply(1:1000,FUN = function(o){
#   x2<-runif(1000)
#   MC1<-f(c(x2,runif(1000))) #norm sequence
#   MC2<-f(c(x2,1-x2)) # norm sequence plus reverse sequence
#   c(mean(MC1),mean(MC2))
# })

## ----echo=FALSE---------------------------------------------------------------
c(mean(res2[1,]),mean(res2[2,]))
c(sd(res2[1,]),sd(res2[2,]))

## -----------------------------------------------------------------------------
# M<-5000
# N<-1000
# k<-5
# expge<-function(n,a,b){  #Generate r.v.s from [a,b] ~ exp(1)
#   u<-runif(n)
#   x<--log(exp(-a)-(exp(-a)-exp(-b))*u)
#   x
# } 
# 
# res3<-sapply(1:N,FUN = function(o){
#   x<-expge(M,0,1)
#   M1<-mean((1-exp(-1))/(1+x^2))
#   M2<-numeric(k)
#   for (j in 0:(k-1)){
#     a<-j/k
#     b<-(j+1)/k
#     xj<-expge(M/k,a,b)
#     M2[j+1]<-mean((exp(-a)-exp(-b))/(1+xj^2))
#     
#   }
#   c(M1,sum(M2))
# })
c(sd(res3[1,]),sd(res3[2,]))

