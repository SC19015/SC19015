## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(SC19015)
a <- rchisq(100,1)
kerh<-function(x,y) max(x,y)/2
JEL(X=a,theta = 0.83, kerh = kerh)
a <- rchisq(100,1)
kerh<-function(x,y) max(x,y)/2
woodel(X=a,theta = 0.83, kerh = kerh)

