#global script for ICSShiny
if(inherits(x, "data.frame")){
  X<-x
  nameData<-nameData
  var.names <- colnames(X)[sapply(X, is.numeric)]
  var.names.quali <- names(which(!(sapply(X, is.numeric))))
  S1<-S1
  S2<-S2
  S1args<-S1args
  S2args<-S2args
  S1name <- S1name
  S2name <- S2name
  data.ics<-ics2(X[,var.names], S1, S2, S1args, S2args)
  seed<-seed 
  n <- nrow(X)
  #data.ics.dist<-rep(0, n)
  data.ics.comp<- 1:ncol(X[,var.names])
  #data.ics.cluster<-rep(0, n)
  data.ics.outlier<-rep(0, n)
  varMode <- 1
  
  alpha <- ifelse((S1name == "MCD" & length(S1args$alpha)>0) | (S2name == "MCD" & length(S2args$alpha)>0),
                  unique(S1args$alpha, S2args$alpha), 0.5)
  df <- ifelse((S1name == "tM" & length(S1args$alpha)>0) | (S2name == "tM" & length(S2args$alpha)>0),
               unique(S1args$df, S2args$df), 1)
  maxiter <- ifelse((S1name == "HRMEST" & length(S1args$alpha)>0) | (S2name == "HRMEST" & length(S2args$alpha)>0),
               unique(S1args$df, S2args$df), 100)
  varChoice <- var.names
  varChoice.input <- NULL
  labelChoice <- "Observation"
  categoricalChoice <- "No categories"
  initialValueFirst <- NULL
  initialValueLast <- NULL
  SimuBeg <- NULL
  SimuEnd <- NULL
  simu.bool <- 0
  
  result<-c("")
  level<-0.05
  iteration<-100
  indexSimuComp<-0:ncol(X)
  levelCompSimu <- 0.05
  
  colorIndex<-rep(0, n)
  pchIndex<-rep(0, n)
  labelIndex<-rep(0, n)
  labelBrushedIndex<-rep(0, n)
  
  cutOff.out<-0
  labelIndex.out<-rep(0, n)
  labelBrushedIndex.out<-rep(0, n)
  bool.out<-FALSE
  cutOffMode.out<-0
  dist.out<-rep(0, n)
  
  existingClusters<- c(0)
  
  saveDirectory<-getwd()
  textSummary<-list()
  
  kernelIndexvalue <- 1
  bandwidth <- NULL
  
  componentXAxis <- 1
  componentYAxis <- NULL
  
  labelICvsIC <- 1
  
  cluster <- 1
  labelOutlier <-1
  nbIterationCutOff <- 100
  levelCutOff <- 0.025
  
  rejectionRate <- 0
  rejectionNumber <- 0
}

if(inherits(x, "icsshiny")){
  X <- x$X
  nameData <- x$nameData
  var.names <- x$var.names
  var.names.quali <- x$var.names.quali
  data.ics <- x$data.ics
  S1 <- x$S1
  S2 <- x$S2
  S1args <- x$S1args
  S2args <- x$S2args
  S1name <- x$S1name
  S2name <- x$S2name
  seed <- x$seed
  n <- x$n
  
  
  varMode <- x$varMode
  alpha <- ifelse(is.null(x$alpha), 0.5, x$alpha)
  df <-ifelse(is.null(x$df), 1, x$df) 
  maxiter <- ifelse(is.null(x$maxiter), 100, x$maxiter)  
  varChoice.input <- x$varChoice.input
  varChoice <- x$varChoice
  labelChoice <- x$labelChoice
  categoricalChoice <-x$categoricalChoice
  
  initialValueFirst <- x$initialValueFirst
  initialValueLast <- x$initialValueLast
  
  
  result<-x$result
  level<-x$level
  iteration<-x$iteration
  indexSimuComp<-x$indexSimuComp
  levelCompSimu <- x$levelCompSimu
  
  colorIndex<-x$colorIndex
  pchIndex<-x$pchIndex
  labelIndex<-x$labelIndex
  labelBrushedIndex<-x$labelBrushedIndex
  
  cutOff.out<-x$cutOff.out
  labelIndex.out<-x$labelIndex.out
  labelBrushedIndex.out<-x$labelBrushedIndex.out
  bool.out<-x$bool.out
  cutOffMode.out<-x$cutOffMode.out
  dist.out<-x$dist.out
  #data.ics.dist <- x$data.ics.dist
  data.ics.comp <- x$data.ics.comp
  #data.ics.cluster <- x$data.ics.cluster
  data.ics.outlier <- x$data.ics.outlier
  
  existingClusters <- x$existingClusters
  
  saveDirectory<-x$saveDirectory
  textSummary<-x$textSummary
  
  kernelIndexvalue <- x$kernelIndexvalue
  bandwidth <- x$bandwidth
  
  SimuBeg <- x$SimuBeg
  SimuEnd <- x$SimuEnd
  simu.bool <- x$simu.bool
 
  componentXAxis <- x$componentXAxis
  componentYAxis <- x$componentYAxis
  
  labelICvsIC <- x$labelICvsIC
  
  cluster <- x$cluster
  labelOutlier <- x$labelOutlier
  
  nbIterationCutOff <- x$nbIterationCutOff
  levelCutOff <- x$levelCutOff
  rejectionRate <- x$rejectionRate 
  rejectionNumber <- x$rejectionNumber
}


compt.change <-0
input.First.change <- 0
input.Last.change <- 0