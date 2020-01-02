## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning = FALSE)

## -----------------------------------------------------------------------------
load("A-19015-2019-11-08.RData")
# library(bootstrap)
# library(boot)
# set.seed(1234)
# sc<-scor
pairs(sc)
# p.cor<-function(x,i){  ## cor function
#   rr<-cor(x[i,])
#   c(rr[1,2],rr[3,4],rr[3,5],rr[4,5])
# }
# sc.boot<-boot(data = sc,statistic = p.cor,R=2000)
apply(sc.boot$t,2,FUN=sd)

## -----------------------------------------------------------------------------
# sk <- function(x) {
#   #computes the sample skewness coeff.
#   xbar <- mean(x)
#   m3 <- mean((x - xbar)^3)
#   m2 <- mean((x - xbar)^2)
#   return( m3 / m2^1.5 )
# }
# mu1<-0;n<-1e1;m<-1e3;library(boot);
# boot.sk <- function(x,i) sk(x[i])
# ci1.norm<-ci1.basic<-ci1.perc<-ci1.bca<-matrix(NA,m,2)
# for(i in 1:m){
#   U<-rnorm(n)
#   de <- boot(data=U,statistic=boot.sk, R = 999)
#   ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
#   ci1.norm[i,]<-ci$norm[2:3];ci1.basic[i,]<-ci$basic[4:5]
#   ci1.perc[i,]<-ci$percent[4:5];ci1.bca[i,]<-ci$bca[4:5]
# }
cat('norm =',mean(ci1.norm[,1]<=mu1 & ci1.norm[,2]>=mu1),
    'basic =',mean(ci1.basic[,1]<=mu1 & ci1.basic[,2]>=mu1),
    'perc =',mean(ci1.perc[,1]<=mu1 & ci1.perc[,2]>=mu1),
    'BCa =',mean(ci1.bca[,1]<=mu1 & ci1.bca[,2]>=mu1))

## -----------------------------------------------------------------------------
# mu<-sqrt(8/5)
# ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,m,2)
# for(i in 1:m){
#   U<-rchisq(n,5)
#   de <- boot(data=U,statistic=boot.sk, R = 999)
#   ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
#   ci.norm[i,]<-ci$norm[2:3];ci.basic[i,]<-ci$basic[4:5]
#   ci.perc[i,]<-ci$percent[4:5];ci.bca[i,]<-ci$bca[4:5]
# }
cat('norm =',mean(ci.norm[,1]<=mu & ci.norm[,2]>=mu),
    'basic =',mean(ci.basic[,1]<=mu & ci.basic[,2]>=mu),
    'perc =',mean(ci.perc[,1]<=mu & ci.perc[,2]>=mu),
    'BCa =',mean(ci.bca[,1]<=mu & ci.bca[,2]>=mu))


