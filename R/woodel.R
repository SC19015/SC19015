#' @title Log-likelihood ratio using sequential linearization method
#' @description Computation on Log-likelihood ratio using sequential linearization method 
#' @param X data set, matrix or vector, if it is matrix, each row represents one sample.
#' @param theta the parameter needed to evaluated at.
#' @param kerh the kernel of the U-statistic,2-dimension only.
#' @return Log-likelihood ratio using sequential linearization method
#' @examples
#' \dontrun{
#' a <- rchisq(100,1)
#' kerh<-function(x,y) max(x,y)/2
#' woodel(X=a,theta = 0.83, kerh = kerh)
#' }
#' @export

woodel<-function(X,theta,kerh){ 
  X <- as.matrix(X)
  n<-dim(X)[1]
  u.theta<-function(p){
    t<-0
    for (i in 1: (n-1)){
      for (j in (i+1):n){
        t<-t+p[i]*p[j]*kerh(X[i,],X[j,])
      }
    }
    t/(n*(n-1)/2)*n^2 
  } ######### u-statistics
  d.u.theta<-function(p,i){
    t<-0
    for (j in (1:n)[-i]){
      t<-t+p[j]*kerh(X[i,],X[j,])
    }
    2*t/(n-1)*n
  }  #### derivate of u-statistics
  m<-20
  p0<-rep(1/n,n)
  p1<-rep(0,n)
  s<-1
  while (m> 0 & sum((p0-p1)^2)>1/n*0.001&s>sum((p0-p1)^2)| m>15& sum((p0-p1)^2)>1/n*0.001) {
    s <- sum((p0-p1)^2)
    p1<-p0
    Y<-sapply(1:n, FUN = function(a) d.u.theta(p0,a))
    theta1<-theta-u.theta(p0)+sum(p0*Y)
    res.el<-emplik(Y,theta1)
    p0<-res.el$wts
    m<-m-1
  }
  res.el<-emplik(Y,theta1)
  res.el$logelr
}