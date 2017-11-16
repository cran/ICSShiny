ICSShiny <- function(x, S1= MeanCov, S2=Mean3Cov4, S1args=list(), S2args=list(), seed=NULL){
    
    #   Assign the parameters of the function to the global environment 
    G <- .GlobalEnv
    assign("x", x, envir=G)
    nom=sys.calls()[[1]]
    nameJDD=nom[2]
    assign("nameData",nameJDD, envir=G)
    assign("S1", S1, envir=G)
    assign("S2", S2, envir=G)
    assign("S1args", S1args, envir=G)
    assign("S2args", S2args, envir=G)
    assign("seed", seed, envir=G)
    S1name <- deparse(substitute(S1))
    S2name <- deparse(substitute(S2))
    assign("S1name", S1name, envir=G)
    assign("S2name", S2name, envir=G)

    # Launch the function only if the inputed data is of type data.frame
    if (!(inherits(x, "data.frame") | inherits(x, "icsshiny"))){
      stop(gettext('data must receive an argument of type data.frame or  the results of the ICSshiny function'))
    }
    if(is.data.frame(x)==TRUE){
      #Launch the function only if there are at least two numerical variable
      if(ncol(x[sapply(x, is.numeric)])<=1)
        stop(gettext('data argument must contain at least 2 numerical variables'))
    }
    
    
    res <- runApp(system.file("ICSShiny",package="ICSShiny"),launch.browser = TRUE)
    class(res) <- "icsshiny"
    #We return result to get back the global variables of the ICSShiny function in the workspace
    return(invisible(res))
}

print.icsshiny <-  function(x, ...){
  cat("Summary of operations\n")
  
  cat("\n")
  if (!is.null( x$nameData)){
    cat(paste0("The data file is: ", x$nameData,"\n"))
    cat(paste0("It contains ",x$n," observations and ", length(x$var.names), " numerical variables.\n"))
  }else{
    cat(paste0("The data file contains ",x$n," observations and ", length(x$var.names), " numerical variables.\n"))
  }
  cat("\n")
  
  
  
  if(x$varMode == 0)
  {
    cat("All the numerical variables were kept in the analysis.\n")
    cat(paste0("So ", ncol(x$X[,x$var.names]), 
               " variables are taken into account in ICS.\n"))
  }
  else if(x$varMode == 1)
  {
    cat("The following numerical variables were kept in the analysis:\n")
    cat(x$varChoice)
    cat(paste0("\nSo ", length(x$varChoice), 
               " variables are taken into account in ICS.\n"))
  }
  else if(x$varMode == 2)
  {
    cat("The following numerical variables were excluded from the analysis:\n")
    cat(x$var.names[!(x$var.names%in%x$varChoice)])
    cat(paste0("\nSo ", length(x$varChoice), 
               " variables are taken into account in ICS.\n"))
  }
  cat("\n")
  
  cat("The scatter matrices in ICS are:\n")
  cat(paste0("S1 = ", x$S1name, "\n"))
  if ( !is.null(names( x$S1args))){
    cat("with the following parameters: ")
    cat(paste(names( x$S1args), unlist( x$S1args), sep = " = "))
  }
  cat(paste0("\nS2 = ",  x$S2name, "\n"))
  if ( !is.null(names( x$S2args))){
    cat("with the following parameters: ")
    cat(paste(names( x$S2args), unlist( x$S2args), sep = " = "))
  }
  
  cat("\n")
  
  cat(paste0("\nThe observations are labelled using the variable: ",  x$labelChoice,".\n"))
  
  if( x$categoricalChoice != "No categories")
  {
    cat(paste0("The categories are defined by the variable ",
               x$categoricalChoice, "\n"))
  }
  
  cat("\n")
  
  cat("Looking at the screeplot of the generalized kurtosis associated with the invariant components or/and at the suggestions of the normality tests, the following components are selected:\n")
  cat( x$data.ics.comp)
  
  cat("\n")
  if( x$simu.bool==1)
  {
    cat("\nLooking at the Monte Carlo simulations for the selection of the invariant components.\n")
    cat(paste0("The simulation contain ",x$iteration, 
               " iterations at the level ",x$levelCompSimu, ".\n"))
    cat("The suggested index are:\n")
    cat( x$indexSimuComp)
    cat("\n")
  }
  
  cat("\n")
  
  if(x$bool.out == FALSE)
  {
    cat("No outliers were tagged in the data.\n")
  }
  else if(x$bool.out == TRUE)
  {
    if(x$cutOffMode.out == 1)
    {
      cat("In order to identify outliers, a cut-off was defined via Monte Carlo simulations.\n")
      cat(paste0("The simulations contain ", x$nbIterationCutOff, 
                 " iterations and are at the level ", x$levelCutOff, ".\n")) 
      cat(paste0("The cut-off value is ", round(x$cutOff.out, 3), ".\n")) 
    } 
    else
    {
      cat("In order to identify outliers, a cut-off was defined via a percentage rate.\n")
      cat(paste0("The rejection rate was setted up to ", x$rejectionRate, 
                 "% which corresponds to ", x$rejectionNumber, " outliers. \n")) 
      cat(paste0("The cut-off value is ", round(x$cutOff.out, 3), ".\n"))
    }
  }
}
