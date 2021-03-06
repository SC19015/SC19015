## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning = FALSE,message = FALSE)

## -----------------------------------------------------------------------------
load("A-19015-2019-11-22.RData")
# library(parallel)
# set.seed(1234)
# cl <- makeCluster(8)
# maxout <- function(x, y) {
#   X <- x - mean(x)
#   Y <- y - mean(y)
#   outx <- sum(X > max(Y)) + sum(X < min(Y))
#   outy <- sum(Y > max(X)) + sum(Y < min(X))
#   as.numeric(max(c(outx, outy))<=5)
# }  ## simple count5
# count5test<-function(X,Y){
#   X <- X-mean(X)
#   Y <- Y-mean(Y)
#   n1 <- length(X)
#   n2 <- length(Y)
#   if (n1==n2) {    
#     outx <- sum(X > max(Y)) + sum(X < min(Y))
#     outy <- sum(Y > max(X)) + sum(Y < min(X))
#     return(as.numeric(max(outx,outy)<=5))
#   }
#   if (n1>n2) {    
#     res<-sapply(1:1000,FUN=function(o){
#       k<-sample(1:n1,n2,replace = FALSE)
#       x<-X[k]
#       y<-Y
#       outx <- sum(x > max(y)) + sum(x < min(y))
#       outy <- sum(y > max(x)) + sum(y < min(x))
#       max(outx,outy)
#     })
#     return(as.numeric(mean(res)<=5))
#   }
#   if (n1<n2) {    
#     res<-sapply(1:10000,FUN=function(o){
#       k<-sample(1:n2,n1,replace = FALSE)
#       x<-X
#       y<-Y[k]
#       outx <- sum(x > max(y)) + sum(x < min(y))
#       outy <- sum(y > max(x)) + sum(y < min(x))
#       max(outx,outy)
#     })
#     return(as.numeric(mean(res)<=5)) ## compare whether the average maxout is larger than 5 
#   }
# }  ## permute small sample size in large sample size



## -----------------------------------------------------------------------------
# maxout20<-function(X,Y){
#   X <- X-mean(X)
#   Y <- Y-mean(Y)
#   n1 <- length(X)
#   n2 <- length(Y)
#   res<-sapply(1:1000,FUN=function(o){
#     k1<-sample(1:n1,20,replace = FALSE)
#     k2<-sample(1:n2,20,replace = FALSE)
#     x<-X[k1]
#     y<-Y[k2]
#     outx <- sum(x > max(y)) + sum(x < min(y))
#     outy <- sum(y > max(x)) + sum(y < min(x))
#     max(outx,outy)
#   })
#   return(as.numeric(mean(res)<=5))
# }  ## permute fixed sample size (20) in each sample

## -----------------------------------------------------------------------------
# count5test1<-function(X,Y){
#   X <- X-mean(X)
#   Y <- Y-mean(Y)
#   n1 <- length(X)
#   n2 <- length(Y)
#   n  <- min(n1%/%2,n2%/%2)
#   res<-sapply(1:1000,FUN=function(o){
#     k1<-sample(1:n1,n,replace = FALSE)
#     k2<-sample(1:n2,n,replace = FALSE)
#     x<-X[k1]
#     y<-Y[k2]
#     outx <- sum(x > max(y)) + sum(x < min(y))
#     outy <- sum(y > max(x)) + sum(y < min(x))
#     max(outx,outy)
#   })
#   return(as.numeric(mean(res)<=5))
# } ## permute half of the small sample size in each sample

## -----------------------------------------------------------------------------
# mu1 <- mu2 <-0
# s1 <-1
# s2 <- 1
# clusterExport(cl,"count5test",envir = environment())
# clusterExport(cl,"count5test1",envir = environment())
# clusterExport(cl,"maxout",envir = environment())
# clusterExport(cl,"maxout20",envir = environment())
# #clusterExport(cl,"n1",envir = environment())
# #clusterExport(cl,"n2",envir = environment())
# clusterExport(cl,"mu1",envir = environment())
# clusterExport(cl,"mu2",envir = environment())
# clusterExport(cl,"s1",envir = environment())
# clusterExport(cl,"s2",envir = environment())
# n<-matrix(c(20,20,50,50,100,100,500,500,20,50,20,100,50,100,50,500,100,500),ncol=2,byrow = TRUE)
# res<-matrix(rep(0,36),ncol = 4)
# for (i in 1:9) {
#   n1<-n[i,1]
#   n2<-n[i,2]
#   clusterExport(cl,"n1",envir = environment())
#   clusterExport(cl,"n2",envir = environment())
#   I<-parSapply(cl,1:1000,FUN= function(o){
#     X <- rnorm(n1,mu1,s1)
#     #X<- rexp(n1,rate = s1)
#     #Y<- rexp(n2,rate = s2)
#     Y <- rnorm(n2,mu2,s2)
#     c(maxout(X,Y),count5test(X,Y),count5test1(X,Y),maxout20(X,Y))
#   })
#  res[i,] <- 1-apply(I,1,mean)
# }
# rownames(res)<-c('(20,20)','(50,50)','(100,100)','(500,500)','(20,50)','(20,100)','(50,100)','(50,500)','(100,500)')  
# colnames(res)<-c('count5','permu-count5','double-permu-couunt5','fixed-permu-count5')
res

## -----------------------------------------------------------------------------
# s2<-1.5
# clusterExport(cl,"s2",envir = environment())
# res1<-matrix(rep(0,36),ncol = 4)
# for (i in 1:9) {
#   n1<-n[i,1]
#   n2<-n[i,2]
#   clusterExport(cl,"n1",envir = environment())
#   clusterExport(cl,"n2",envir = environment())
#   I<-parSapply(cl,1:1000,FUN= function(o){
#     X <- rnorm(n1,mu1,s1)
#     #X<- rexp(n1,rate = s1)
#     #Y<- rexp(n2,rate = s2)
#     Y <- rnorm(n2,mu2,s2)
#     c(maxout(X,Y),count5test(X,Y),count5test1(X,Y),maxout20(X,Y))
#   })
#   res1[i,] <- 1-apply(I,1,mean)
# }
# stopCluster(cl)
# rownames(res1)<-c('(20,20)','(50,50)','(100,100)','(500,500)','(20,50)','(20,100)','(50,100)','(50,500)','(100,500)')  
# colnames(res1)<-c('count5','permu-count5','double-permu-couunt5','fixed-permu-count5')
res1

## -----------------------------------------------------------------------------
# dCov <- function(x, y) {
#   x <- as.matrix(x); y <- as.matrix(y)
#   n <- nrow(x); m <- nrow(y)
#   if (n != m || n < 2) stop("Sample sizes must agree")
#   if (! (all(is.finite(c(x, y)))))
#     stop("Data contains missing or infinite values")
#   Akl <- function(x) {
#     d <- as.matrix(dist(x))
#     m <- rowMeans(d); M <- mean(d)
#     a <- sweep(d, 1, m); b <- sweep(a, 2, m)
#     b + M
#   }
#   A <- Akl(x); B <- Akl(y)
#   sqrt(mean(A * B))
# }
# ndCov2 <- function(z, ix, dims) {
#   #dims contains dimensions of x and y
#   p <- dims[1]
#   q <- dims[2]
#   d <- p + q
#   x <- z[ , 1:p] #leave x as is
#   y <- z[ix, -(1:p)] #permute rows of y
#   return(nrow(z) * dCov(x, y)^2)
# }
# 
# library(boot)
# library(Ball)
# nn<-c(20,30,50,100,200)
# cl <- makeCluster(8)
# clusterExport(cl,"dCov",envir = environment())
# clusterExport(cl,"bcov.test",envir = environment())
# clusterExport(cl,"ndCov2",envir = environment())
# clusterExport(cl,"boot",envir = environment())
# ress<-matrix(rep(0,20),ncol = 4)
# for (i in 1:5) {
#   n<- nn[i]
#   clusterExport(cl,"n",envir = environment())
#   res2<- parSapply(cl,1:1000,FUN= function(i){
#     set.seed(i*123)
#     X <- cbind(rnorm(n),rnorm(n))
#     e <- cbind(rnorm(n),rnorm(n))
#     Y1 <- X/4+e
#     Y2 <- X*e/4
#     boot.obj <- boot(data = cbind(Y1,X), statistic = ndCov2, R = 99,
#                      sim = "permutation", dims = c(2, 2))
#     tb <- c(boot.obj$t0, boot.obj$t)
#     p.cor <- mean(tb>=tb[1])
#     boot.obj2 <- boot(data = cbind(X,Y2), statistic = ndCov2, R = 99,
#                      sim = "permutation", dims = c(2, 2))
#     tb2 <- c(boot.obj2$t0, boot.obj2$t)
#     p.cor2 <- mean(tb2>=tb2[1])
#     p.ball <- bcov.test(X,Y1)$p.value
#     p.ball2 <- bcov.test(X,Y2)$p.value
#     #p.ball <- bcov.test(Y1,Y2)$p.value
#     as.numeric(c(p.cor<0.05,p.ball<0.05,p.cor2<0.05,p.ball2<0.05))
#     #as.numeric(c(100*dCov(Y2,Y1)>qchisq(0.95,4),p.ball<0.05))
#   })
#   ress[i,] <- apply(res2,1,mean)
# }
# 
# stopCluster(cl)
# par(mfrow=c(1,2))
plot(nn,ress[,1],type = "b",ylab = "power",xlab = "sample size",main = "Model 1")
lines(nn,ress[,2],type = "b",col = "red")

plot(nn,ress[,3],type = "b",ylab = "power",xlab = "sample size",main = "Model 2")
lines(nn,ress[,4],type = "b",col = "red")

