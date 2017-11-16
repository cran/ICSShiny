MCD <- function(x,...){
  mcd = CovMcd(x, ...)
  return(list(location = mcd@center, scatter = mcd@cov))
}

TM <- function(x,...){
  tm = tM(x, ...)
  return(list(location = tm$mu, scatter = tm$V))
}

HRMEST <- function(x,...){
  HR.Mest(x,...)
}

FuncICS<-function(X.data, S1 = MeanCov, S2 = Mean3Cov4, S1args = list(), S2args = list(), S1name, S2name ) {

  RES <- (ics2(X.data, S1 = S1, S2 = S2, S1args = S1args, S2args = S2args))
  RES@S1name <- S1name
  RES@S2name <- S2name
  RES
  
}
