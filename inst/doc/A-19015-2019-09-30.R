## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
  load('A-19015-2019-09-30.RData')
  # sigma<-1
  # f1<-function(x){
  #   x/sigma^2*exp(-x^2/(2*sigma^2))
  # }
  # The Rayleigh density

## -----------------------------------------------------------------------------
  # F1<-runif(10000)

## -----------------------------------------------------------------------------
  # x<-sqrt(-2*sigma^2*log(1-F1))

## -----------------------------------------------------------------------------
hist(x,breaks = 100,probability = T,main = 'Rayleigh distribution,σ=1')
y<-seq(0,8,0.1)
lines(y,f1(y),col='red')

## ----echo=FALSE---------------------------------------------------------------
  sigma<-0.5
  F1<-runif(10000)
  x<-sqrt(-2*sigma^2*log(1-F1))
  hist(x,breaks = 100,probability = T,main = 'Rayleigh distribution,σ=0.5')
  y<-seq(0,5,0.1)
  lines(y,f1(y),col='red')
  sigma<-2
  F1<-runif(10000)
  x<-sqrt(-2*sigma^2*log(1-F1))
  hist(x,breaks = 100,probability = T,main = 'Rayleigh distribution,σ=2')
  y<-seq(0,10,0.1)
  lines(y,f1(y),col='red')


## -----------------------------------------------------------------------------
# mixnormhist<-function(p1){
#   f2<-function(x){
#     p1*dnorm(x)+(1-p1)*dnorm(x,3,1)
#   }
#   #Mix-normal distribution
#   p<-rbinom(10000,1,prob = p1) #Generate the choices of 10000 samples (0:N(3,1),1:N(0,1))
#   x1<-rnorm(10000)
#   x2<-rnorm(10000,3,1)
#   x<-p*x1+(1-p)*x2 #Generate  samples
#   title<-paste('p1 = ',p1)
#   hist(x,breaks = 200,probability = T,main = title)
#   y<-seq(-3,6,0.1)
#   lines(y,f2(y),col='red')
# }
mixnormhist(0.75)

## -----------------------------------------------------------------------------
  for (p1 in seq(0.1,0.9,0.1)){
    mixnormhist(p1)
  }

## ----echo =FALSE--------------------------------------------------------------
n<-10
d<-3
Sigma<-rbind(c(2,-1,2),c(-1,2,-1),c(2,-1,2))
L<-t(chol(Sigma)) # L is the Choleski factorization of Sigma

## -----------------------------------------------------------------------------
# A<-matrix(rep(0,d^2),nrow = d) # Initial
# A[1,1]<-sqrt(rchisq(1,n))
# for (i in 2:d){
#   A[i,i]<-sqrt(rchisq(1,n-i+1))
#   A[i,1:(i-1)]<-rnorm(i-1)
# } #Bartlett’s  decomposition
# S<-L%*%A%*%t(A)%*%t(L)
S

