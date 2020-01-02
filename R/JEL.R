#' @title Jackknife Empirical log-likelihood ratio 
#' @description Computation on Jackknife Empirical log-likelihood ratio 
#' @param X data set, matrix or vector, if it is matrix, each row represents one sample.
#' @param theta the parameter needed to evaluated at.
#' @param kerh the kernel of the U-statistic,2-dimension only.
#' @return The Jackknife Empirical log-likelihood ratio
#' @examples
#' \dontrun{
#' a <- rchisq(100,1)
#' kerh<-function(x,y) max(x,y)/2
#' JEL(X=a,theta = 0.83, kerh = kerh)
#' }
#' @export
JEL<-function(X,theta,kerh){
  X <- as.matrix(X)
  n <- dim(X)[1]
  u.theta<-function(x){
    t<-0
    for (i in 1: (n-1)){
      for (j in (i+1):n){
        t<-t+kerh(x[i,],x[j,])
      }
    }
    t/(n*(n-1)/2)
  }  # compute U-statistic
  u<-u.theta(X)
  us.theta<-function(i){
    t<-0
    for (j in (1:n)[-i]) t<-t+kerh(X[i,],X[j,])
    t
  }
  V<-sapply(1:n, FUN = function(i) (u*n*(n-1)/2-us.theta(i))/((n-1)*(n-2)/2))
  V<-n*u-(n-1)*V
  res.el<-emplik(V,theta)
  res.el$logelr
}