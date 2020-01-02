## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning = FALSE,message = FALSE)

## -----------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)

## -----------------------------------------------------------------------------
formu.la <- lapply(formulas,FUN = function(formu) lm(formu,data = mtcars))
summary(formu.la)
formu.loop <- list()
for (i in 1:4)  formu.loop[[i]] <- lm(formulas[[i]],data = mtcars)
summary(formu.loop)


## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE)
mtcars[rows, ]
})

## -----------------------------------------------------------------------------
boot.loop <- list()
summary(boot.loop)
for (i in 1:4)  boot.loop[[i]] <- lm(mpg ~ disp,data = bootstraps[[i]])
boot.la <- lapply(bootstraps,FUN = function(x) lm(mpg ~ disp,data = x))
summary(boot.la)
lapply(bootstraps,lm,formula= mpg ~ disp)

## -----------------------------------------------------------------------------
  rsq <- function(mod) summary(mod)$r.squared

## -----------------------------------------------------------------------------
  lapply(formu.la,rsq)
  lapply(boot.la,rsq)

## -----------------------------------------------------------------------------
trials <- replicate(100,t.test(rpois(10, 10), rpois(7, 10)),simplify = FALSE)

## -----------------------------------------------------------------------------
sapply(1:100,FUN = function(o) t.test(rpois(10, 10), rpois(7, 10))$p.value)
sapply(trials, '[[',3)

## -----------------------------------------------------------------------------
  library(parallel)
  cl <- makeCluster(4)
  parSapply(cl,1:100,FUN=function(o){
    x<- rnorm(100)
    mean(x)
  })
  stopCluster(cl)

