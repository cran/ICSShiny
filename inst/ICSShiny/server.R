function(input, output, session) {
  
  ##############################################################################################################
  #                                              REACTIVE VALUES                                               #
  ##############################################################################################################
  
  
  #                                 REACTIVE VALUES 1 Store the data (datvals)                                 #
  
  ### Declaration of the reactive value datvals ###
  
  #We need several type of datvals along our analysis: 
  #- X: the current dataset considering the choice of variables
  #- data.ics: the ICS2 object of the data considering the choice of variables and of scatter matrices
  #The value are initialized to data with only numeric values. 
  datvals<- reactiveValues()
  datvals$data<-X[,varChoice]
  datvals$data.ics<-data.ics
  datvals$S1<-S1
  datvals$S2<-S2
  datvals$S1args<-S1args
  datvals$S2args<-S2args
  datvals$S1.init<-S1
  datvals$S2.init<-S2
  datvals$S1name.init<-S1name
  datvals$S2name.init<-S2name
  datvals$S1args.init<-S1args
  datvals$S2args.init<-S2args
  datvals$varChoice<-varChoice
  datvals$varMode<-varMode
  datvals$errorText<-"error"
  
  #                            REACTIVE VALUES 2 Observe change for compSimuResults                            #
  
  ### Declaration of the reactive value simucompvals ###
  
  #As explained in the Output tab2, Display 2.3, we need to store some values to actualise the result of 
  #compSimuResults when needed 
  simucompvals<- reactiveValues()
  simucompvals$level <- levelCompSimu
  simucompvals$bool<-simu.bool
  simucompvals$result<-result
  simucompvals$iteration<-iteration
  simucompvals$data<-X[,var.names]
  simucompvals$data.ics<-data.ics
  
  
  ### Declaration of the reactive value reac ###
  
  #We will need these reactive value in order to create a delay in the computation of ics2, if not, it would be 
  #compute for each new values when a user quickly goes through the numeric input and it would take a lot of time
  #especially if the data is big. 
  reacvals<- reactiveValues()
  reacvals$alpha<-isolate(input$alpha)
  reacvals$degreeFreedom<-isolate(input$degreeFreedom)
  reacvals$recompute<-TRUE # only needed for the timer (to know if it has to change the reactive values alpha and degreeFreedom)
  
  observe({
    input$alpha
    input$degreeFreedom
    reacvals$recompute <- FALSE
  })
  
  observe({
    #wait 1s
    invalidateLater(1000, session)
    input$alpha
    input$degreeFreedom

    # to reset the timer to 0
    if (isolate(reacvals$recompute)) {
      reacvals$alpha <- input$alpha
      reacvals$degreeFreedom <- input$degreeFreedom
    } else {
      isolate(reacvals$recompute <- TRUE)
    }
  })
  
  ### Updating the datvals ###
  
  #The value datvals$data must change when the number of selected variable changes
  observeEvent(input$applyVar, {
    
    if(is.null(input$varChoice))
    {
        datvals$data<-X[,var.names]
        datvals$varChoice<-var.names
        datvals$varMode<-0
    }
    else
    {
      if(input$varSelectMode == "1")
      {
        datvals$varChoice<-input$varChoice
        datvals$varMode<-1
        datvals$data<-X[, datvals$varChoice]
      }
      else if(input$varSelectMode == "2")
      {
        var <- !(var.names %in% input$varChoice)
        datvals$varChoice<- var.names[var]
        datvals$varMode<-2
        datvals$data<-X[, datvals$varChoice]
        
      }
    }
      
  })
  
  observeEvent(input$labelChoice, {
    

      if(input$labelChoice !="Observation" )
      {
        Label<- X[,match(input$labelChoice, names(X))]
      }  
      
    })
  
  observeEvent(input$categoricalChoice, {
    
    if(input$categoricalChoice !="No categories")
    {
      Category<- X[,match(input$categoricalChoice, names(X))]
    } 
    
  })
 
  #The value of datvals$data.ics must change in numerous occasion: when datvals$data changes, when the pair of
  #scatter matrice changes, when the parameters of MCD and tM changes
  
  observeEvent({datvals$data
    input$scatterChoice1
    input$scatterChoice2
    datvals$S1
    datvals$S2
    reacvals$alpha
    input$maxiter
    reacvals$degreeFreedom}, {
      req(datvals$data)
      validate(
        need(ncol(as.matrix(datvals$data))>1, "error")
      )
      req(input$scatterChoice1)
      req(input$scatterChoice2)
      validate(
        need(input$scatterChoice1!=input$scatterChoice2 || input$scatterChoice1=="Personnalized", "error" )
      )
      
      req(reacvals$alpha)
      validate(
        need(reacvals$alpha >= 0.5 && reacvals$alpha <= 1, "errorAlpha")
      )
      req(reacvals$degreeFreedom)
      validate(
        need(reacvals$degreeFreedom >= 1 && reacvals$degreeFreedom <= 10, "errorDf")
      )
      req(input$maxiter)
      
      if(input$scatterChoice1 == "Personnalized")
      {
        datvals$S1<-datvals$S1.init
      }
      else 
      {
        datvals$S1<-eval(parse(text = input$scatterChoice1))
      }
      
      if(input$scatterChoice1 == "MCD")
      {
        datvals$S1args<-list(alpha = reacvals$alpha)
      }
      else if(input$scatterChoice1 == "tM")
      {
        datvals$S1args<-list(df = reacvals$degreeFreedom)
      }
      else if(input$scatterChoice1 == "HRMEST")
      {
        datvals$S1args<-list(maxiter = input$maxiter)
      }
      else if(input$scatterChoice1 == "Personnalized")
      {
        datvals$S1args<-datvals$S1args.init
      }
      else
      {
        datvals$S1args<-list()
      }
      
      if(input$scatterChoice2 == "Personnalized")
      {
        datvals$S2<-datvals$S2.init
      }
      else 
      {
        datvals$S2<-eval(parse(text = input$scatterChoice2))
      }
      
      if(input$scatterChoice2 == "MCD")
      {
        datvals$S2args<-list(alpha = reacvals$alpha)
      }
      else if(input$scatterChoice2 == "tM")
      {
        datvals$S2args<-list(df = reacvals$degreeFreedom)
      }
      else if(input$scatterChoice2 == "HRMEST")
      {
        datvals$S2args<-list(maxiter = input$maxiter)
      }
      else if(input$scatterChoice2 == "Personnalized")
      {
        datvals$S2args<-datvals$S2args.init
      }
      else
      {
        datvals$S2args<-list()
      }
      
      set.seed(seed) 
      S1<<-datvals$S1
      S2<<-datvals$S2
      S1args<<-datvals$S1args
      S2args<<-datvals$S2args
      S1name <<-  input$scatterChoice1
      S2name <<- input$scatterChoice2
      
      datvals$data.ics = simsalapar::tryCatch.W.E(ICSShiny:::FuncICS(datvals$data, S1 = S1, S2 = S2,  S1args = S1args,  S2args = S2args,
                                              S1name = input$scatterChoice1, S2name = input$scatterChoice2))
      
      datvals$errorText<-"error"
      if(length(datvals$data.ics$warning)==2)
      {
        datvals$data.ics = datvals$data.ics$warning
      } 
      else{datvals$data.ics = datvals$data.ics$value}
      
      if(is.list(datvals$data.ics))
      {
        datvals$errorText<-gettext(print(datvals$data.ics))
      }
      
      data.ics<<-datvals$data.ics
      compt.change <<- compt.change +1
      
      
      updateNumericInput(session,"levelCompNorm", 
                         label= "", 
                         value = level,
                         step = 0.01,
                         min=0,
                         max=1)
      updateNumericInput(session,"levelCompSimu", 
                         label= "", 
                         value = levelCompSimu,
                         step = 0.01,
                         min=0,
                         max=1) 
      
    })
  
   
  #                            REACTIVE VALUES 3 Observe change for the ICvsIC plot                            #
  
  ### Declaration of the reactive value icvsicvals ###
  
  #We need several reactive value to store vector of observation. These vectors will indicate if a given 
  #observation should be colored, shaped, or labelled
  icvsicvals<- reactiveValues()
  icvsicvals$colorIndex<-colorIndex
  icvsicvals$pchIndex<-pchIndex
  icvsicvals$labelIndex<-labelIndex
  icvsicvals$labelBrushedIndex<-labelBrushedIndex
  
  
  #                            REACTIVE VALUES 4 Observe change for the Outlier plot                            #
  
  ### Declaration of the reactive value outliervals ###
  
  #We need several reactive value to store vector of observation. These vectors will indicate if a given 
  #observation should be colored, shaped, or labelled
  outliervals<- reactiveValues()
  outliervals$index<-data.ics.comp
  outliervals$cutOff<-cutOff.out
  outliervals$labelIndex<-labelIndex.out
  outliervals$labelBrushedIndex<-labelBrushedIndex.out
  outliervals$bool<-bool.out
  outliervals$cutOffMode<-cutOffMode.out
  outliervals$dist<-dist.out
  
  
  
  #                            REACTIVE VALUES 5 Data for description and comparison                          #
  
  ### Declaration of the reactive value descvals ###
  
  descvals<- reactiveValues()
  descvals$dataRef<-X[,var.names]
  descvals$dataCom<-X[,var.names]
  descvals$existingClusters<-existingClusters
  
  ### Declaration of the reactive value savevals ###
  
  savevals<- reactiveValues()
  savevals$saveDirectory<-saveDirectory
  savevals$datasave<-X
  savevals$textSummary<-textSummary
  
  
  #                                             RESET ALL SETTINGS                                             #

  observeEvent(input$reset, {
    if(datvals$S1name.init != datvals$S2name.init  &datvals$S1name.init %in% c("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST"))
    {
      updateSelectInput(session, "scatterChoice1", label = "", 
                  choices = list("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST"),
                  selected = datvals$S1name.init)
    }
    else{
      updateSelectInput(session, "scatterChoice1", label = "", 
                  choices = list("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST", 
                                 "Personnalized"),
                  selected = "Personnalized")
    }
    
    if(datvals$S1name.init != datvals$S2name.init  & datvals$S2name.init %in% c("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST"))
    {
      updateSelectInput(session, "scatterChoice2", label = "", 
                        choices = list("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST"),
                        selected = datvals$S2name.init)
    }
    else 
    {
      updateSelectInput(session, "scatterChoice2", label = "", 
                        choices = list("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST", 
                                       "Personnalized"),
                        selected = "Personnalized")
    }
    
    updateCheckboxInput(session, "parametersMCD", label="Parametrize MCD", value= FALSE)
    updateCheckboxInput(session, "parameterstM", label="Parametrize tM", value= FALSE)
    
    updateNumericInput(session, "alpha", value=0.5,
                      min=0, max=1, step=0.05)
    
    updateNumericInput(session, "degreeFreedom", value=1,
                       min=1, max=10, step=1)
    
    updateSelectizeInput(session, "varChoice", label="",
                       choices=var.names)
    
    datvals$data<-X[,var.names]
    datvals$varChoice<-var.names
    datvals$varMode<-0
    
    updateSelectInput(session, "labelChoice", "", 
                  choices = c("Observation", 
                              var.names.quali), selected="Observation")
    updateSelectInput(session, "categoricalChoice", "", 
                      choices = c("No categories", 
                                  var.names.quali), selected="No categories")
    
    updateRadioButtons(session,"varSelectMode", "", 
                 choices = list("To include in the analysis" = 1, 
                                "To exclude from the analysis" = 2),
                 selected = 1)
    
    updateNumericInput(session,"levelCompNorm", 
                 label= "", 
                 value = level,
                 step = 0.01,
                 min=0,
                 max=1)
    updateNumericInput(session,"levelCompSimu", 
                 label= "", 
                 value = levelCompSimu,
                 step = 0.01,
                 min=0,
                 max=1) 


  })
  
  
  ##############################################################################################################
  #                                                OUTPUT TAB1                                                 #
  ##############################################################################################################
  
  #                                   TOOL 1.1 Scatter matrices selection                                      #
  
  #We first select the variable which have less than 25 values, then we take all of these variables which are not
  #numeric
  output$categoricalChoiceUI<- renderUI({
    
    datatemp<-X[, sapply(X, function(col) length(unique(col))) <= 25, drop = FALSE]
    
    if(ncol(datatemp) == 0)
    {
      selectInput("categoricalChoice", "", 
                  choices = c("No categories")) 
    }
    else
    {
      listnames<-colnames(datatemp[!(colnames(datatemp) %in% 
                                       colnames(datatemp[,sapply(datatemp, 
                                                                 is.numeric)]))])
      selectInput("categoricalChoice", "", 
                  choices = c("No categories",
                              listnames), selected = categoricalChoice)
    }
    
  }) 
  
  
  #                               DISPLAY 1.1 Initial matrix of scatter matrices                              #
  
  #Plot an object of class ics2 from {ICS} is enough to display automatically the three first and the three last
  #invariant components. Hence for a first overview it is enough to plot datvals$data.ics
  output$scatterPlotSetup <- renderPlot({ 
    req(datvals$data) 
    validate(
      need(ncol(as.matrix(datvals$data))>1, "Please select at least two components.")
    )
    req(datvals$data.ics) 
    validate(
      need(!(is.list(datvals$data.ics)), gettext(datvals$errorText))
    )
   
    plot(datvals$data.ics)
  }) 
  
  
  ##############################################################################################################
  #                                                OUTPUT TAB2                                                 #
  ##############################################################################################################
  
  #                                     TOOL 2.1 Choice of the components                                      #
  
  ### Construction of the two sliders ###
  
  #First, we have to construct the two slider, they must be bounded by the number of components and their
  #initial value must be defined by the result of an agotisno normality test 
  output$sliderFirstTab2UI <- renderUI({
      req(datvals$data.ics)
      initialValueFirst<<- ifelse(is.null(initialValueFirst) | compt.change>1, max(comp.norm.test(datvals$data.ics, test = "agostino.test", type = "smallprop", level = 0.05, 
                                             adjust = TRUE)$index),initialValueFirst)
   
    
    sliderInput("sliderFirstTab2", "",
                min = 0, max = ncol(datvals$data), value = initialValueFirst, step=1)
  })
  
  #For the second, we have to inverse the scores of datvals$data.ics because we want to test the last components
  output$sliderLastTab2UI <- renderUI({
    
    data.ics.rev<-datvals$data.ics
    data.ics.rev@Scores<-rev(data.ics.rev@Scores)
    
    initialValueLast<<-  ifelse(is.null(initialValueLast)| compt.change>1,max(comp.norm.test(data.ics.rev, test = "agostino.test", type = "smallprop", level = 0.05, 
                                     adjust = TRUE)$index), initialValueLast)
  
    sliderInput("sliderLastTab2", "",
                min = 0, max = ncol(datvals$data), value = initialValueLast, step=1)
  })
  
  
  ### Update the value of the slider ###
  
  #Then we have to update the value of the slider in tab2 if the twin slider in tab4 is modified.
  observeEvent(input$sliderFirstTab4, {
    
    updateSliderInput(session, "sliderFirstTab2", value=input$sliderFirstTab4,
                      min=0, max=ncol(datvals$data), step=1)
    
    
  })
  
  observeEvent(input$sliderLastTab4, {
    
    updateSliderInput(session, "sliderLastTab2", value=input$sliderLastTab4,
                      min=0, max=ncol(datvals$data), step=1)
    
    
  })
  
  #Update global value comp and dist
  observeEvent({input$sliderFirstTab2
    input$sliderLastTab2
    datvals$data.ics}, {
    req(datvals$data.ics)
    req(datvals$data)
    validate(
      need(ncol(as.matrix(datvals$data))>1,"error")
    )
    
    validate(
      need(!is.null(input$sliderFirstTab2), "error" )
    )
    validate(
      need(!is.null(input$sliderLastTab2), "error" )
    )
  
    nbComp<-1:ncol(datvals$data)
    initialValueFirst<<-input$sliderFirstTab2
    initialValueLast<<-input$sliderLastTab2
    index <- append(nbComp[0:initialValueFirst], nbComp[-(1:(ncol(datvals$data)-initialValueLast))])
    if (length(index)==0) index<-NULL
    outliervals$index = unique(index)
    outliervals$dist <- ics.distances(datvals$data.ics, index=outliervals$index)
    outliervals$cutOff <- ifelse(input.First.change>1|input.Last.change>1, 0, cutOff.out)
  })
  
  
    #                                  DISPLAY 2.1 Screeplot of the components                                   #
  
  #We simply use the function screeplot in the package {rrcov} to draw the kurtosis of each component. 
  output$screeplot <- renderPlot({
    screeplot(datvals$data.ics, cex.lab=1.2, cex.axis=1.2, cex.names=1.2, cex.main=1.2,
              main="",xlab="Components", ylab="Generalized kurtosis")
  })
  
  #We add a summary of ICS2 which remind the pair of scatter matrices used and give the exact value of the 
  #kurtosis
  
  output$summaryICS <- renderPrint({
    summary(datvals$data.ics)
  })
  
  #We save this plot
  observeEvent(input$downloadScreeplotPNG, {
    
    dir<-choose.dir(saveDirectory)
    fileName <- sprintf("\\Screeplot_%s.png", gsub(":", ",", date()))

    if(!(is.na(dir)))
    {
      file<-paste0(dir, fileName)
      png(filename=file)
      screeplot(datvals$data.ics, cex.lab=1.2, cex.axis=1.2, cex.names=1.2, cex.main=1.2,
                main="",xlab="Components", ylab="Generalized kurtosis")
      dev.off()
    }
  })  
  
  
  #                                DISPLAY 2.2 Kernel density of the components                                #
  
  ### Which component to draw the kernel density of ? ###
  
  #The numeric input is bounded by the number of component.
  output$kernelIndexUI <- renderUI({
    numericInput("kernelIndex", 
                 label= "", 
                 value = kernelIndexvalue,
                 step = 1,
                 min=1,
                 max=ncol(datvals$data))
  })
  
  
  ### Which bandwidth to send to the kernel plot ? ###
  
  #The default value of the bandwidth is defined by the rule of thumb of Silverman (2012): 
  # h= (1.06*s.e*n)^(-1/5)
  output$bandwidthUI <- renderUI({
    numericInput("bandwidth",  
                 label= "", 
                 value = ifelse(is.null(bandwidth),round((1.06*sd(ics.components(datvals$data.ics)[,input$kernelIndex])
                                *nrow(datvals$data))**(-1/5), 3),bandwidth), 
                 step = 0.01, 
                 min=0,  
                 max=100)  
  }) 
  
  
  ### The kernel density ###
  
  #We use density in {stats}, it's a gaussian kernel 
  output$kernel <- renderPlot({
    
    validate(
      need(is.integer(input$kernelIndex) &&input$kernelIndex > 0 && input$kernelIndex <= ncol(datvals$data), 
           "Please select a valid component")
    )
    
    validate(
      need(input$bandwidth > 0 && input$bandwidth <= 100, 
           "The bandwidth value needs to be included in ]0 ; 100] ")
    )
    
    data.component<-ics.components(datvals$data.ics)
    
    plot(density(data.component[,input$kernelIndex], bw=input$bandwidth), las=1, main="")
    rug(data.component[,input$kernelIndex], ticksize=0.06, side=1, lwd=0.5, col = 1)
  })
  #We save this plot
  observeEvent(input$downloadPlotKernelPNG, {
    
    dir<-choose.dir(default = saveDirectory)
    fileName <- sprintf("\\PlotKernel_%s.png", gsub(":", ",", date()))
    
    if(!(is.na(dir)))
    {
      file<-paste0(dir, fileName)
      png(filename=file)
      
      data.component<-ics.components(datvals$data.ics)
      
      plot(density(data.component[,input$kernelIndex], bw=input$bandwidth), las=1, main="")
      rug(data.component[,input$kernelIndex], ticksize=0.06, side=1, lwd=0.5, col = 1)
      
      dev.off()
    }
  })  
  
  #                                 DISPLAY 2.3 Results of the normality tests                                 #
  
  ### The suggestion of component choice according to normality test ###
  
  #We search for how many components are not normal at the beginning and the end of the data, to do that, we use
  #five different normal test: agostino, anscombe, bonett, jarque et shapiro, then we return the results
  output$normalityTestResults <- renderPrint({
    
    validate(
      need(input$levelCompNorm > 0 && input$levelCompNorm <= 1, 
           "The level of the test must be included in ]0 ; 1]")
    )
    
    data.ics<-datvals$data.ics
    data.ics.rev<-data.ics
    data.ics.rev@Scores<-rev(data.ics@Scores)
    
    AgBeg<-max(comp.norm.test(data.ics, test = "agostino.test", type = "smallprop", 
                              level = input$levelCompNorm, adjust = TRUE)$index)
    AgEnd<-max(comp.norm.test(data.ics.rev, test = "agostino.test", type = "smallprop", 
                              level = input$levelCompNorm, adjust = TRUE)$index)
    AnBeg<-max(comp.norm.test(data.ics, test = "anscombe.test", type = "smallprop", 
                              level = input$levelCompNorm, adjust = TRUE)$index)
    AnEnd<-max(comp.norm.test(data.ics.rev, test = "anscombe.test", type = "smallprop", 
                              level = input$levelCompNorm, adjust = TRUE)$index)
    BoBeg<-max(comp.norm.test(data.ics, test = "bonett.test", type = "smallprop", 
                              level = input$levelCompNorm, adjust = TRUE)$index)
    BoEnd<-max(comp.norm.test(data.ics.rev, test = "bonett.test", type = "smallprop", 
                              level = input$levelCompNorm, adjust = TRUE)$index)
    JaBeg<-max(comp.norm.test(data.ics, test = "jarque.test", type = "smallprop", 
                              level = input$levelCompNorm, adjust = TRUE)$index)
    JaEnd<-max(comp.norm.test(data.ics.rev, test = "jarque.test", type = "smallprop", 
                              level = input$levelCompNorm, adjust = TRUE)$index)
    ShBeg<-max(comp.norm.test(data.ics, test = "shapiro.test", type = "smallprop", 
                              level = input$levelCompNorm, adjust = TRUE)$index)
    ShEnd<-max(comp.norm.test(data.ics.rev, test = "shapiro.test", type = "smallprop", 
                              level = input$levelCompNorm, adjust = TRUE)$index)
    
    nbComp<-1:ncol(datvals$data)
    index=append(nbComp[0:AgBeg], nbComp[-(1:(ncol(datvals$data)-AgEnd))])
    index<-unique(index[index > 0]) 
    data.ics.comp.Agostino<<-index 
    
    nbComp<-1:ncol(datvals$data)
    index=append(nbComp[0:AnBeg], nbComp[-(1:(ncol(datvals$data)-AnEnd))])
    index<-unique(index[index > 0]) 
    data.ics.comp.Anscombe<<-index 
    
    nbComp<-1:ncol(datvals$data)
    index=append(nbComp[0:BoBeg], nbComp[-(1:(ncol(datvals$data)-BoEnd))])
    index<-unique(index[index > 0]) 
    data.ics.comp.Bonett<<-index 
    
    nbComp<-1:ncol(datvals$data)
    index=append(nbComp[0:JaBeg], nbComp[-(1:(ncol(datvals$data)-JaEnd))])
    index<-unique(index[index > 0]) 
    data.ics.comp.Jarque<<-index 
    
    nbComp<-1:ncol(datvals$data)
    index=append(nbComp[0:ShBeg], nbComp[-(1:(ncol(datvals$data)-ShEnd))])
    index<-unique(index[index > 0]) 
    data.ics.comp.Shapiro<<-index 
    
    
    writeLines(c("D'Agostino test", "\n", "Number of components to keep, starting from the highest kurtosis: ", AgBeg,
                 "\n", "Number of components to keep, starting from the lowest kurtosis: ", AgEnd, "\n", "\n",
                 "Anscombe test", "\n", "Number of components to keep, starting from the highest kurtosis: ", AnBeg,
                 "\n", "Number of components to keep, starting from the lowest kurtosis: ", AnEnd, "\n", "\n",
                 "Bonett test", "\n", "Number of components to keep, starting from the highest kurtosis: ", BoBeg,
                 "\n", "Number of components to keep, starting from the lowest kurtosis: ", BoEnd, "\n", "\n",
                 "Jarque test", "\n", "Number of components to keep, starting from the highest kurtosis: ", JaBeg,
                 "\n", "Number of components to keep, starting from the lowest kurtosis: ", JaEnd, "\n", "\n",
                 "Shapiro test", "\n", "Number of components to keep, starting from the highest kurtosis: ", ShBeg,
                 "\n", "Number of components to keep, starting from the lowest kurtosis: ", ShEnd, "\n", "\n"), sep="")
  })
  
  
  ### The suggestion of component choice according to a simulation ###
  
  #We print the result according to a boolean value, because, if not we would have an error when it has yet to be
  #computed. Morever, if not, the result would stay even when the data.ics is modified
  output$compSimuResults <- renderPrint({
    req( simucompvals$bool)
    validate(
      need(input$levelCompSimu > 0 && input$levelCompSimu <= 1, 
           "The level of the test must be included in ]0 ; 1]")
    )
    
    validate(
      need(input$nbIterationCompSimu > 0 && is.integer(input$nbIterationCompSimu), 
           "The number of iteration must be an integer strickly greater than 0")
    )
   
    if (simucompvals$bool==1)
    {
      writeLines(simucompvals$result, sep="")
    }
    else
    {
      writeLines("Click on the 'Launch the test' button")
    }
    
  }) 
  observe({
    
    req(input$levelCompSimu)
    req(input$nbIterationCompSimu)
    req(simucompvals$data.ics)
    req(datvals$data.ics)
    req(simucompvals$data)
    req(datvals$data)
    
    if (simucompvals$level != input$levelCompSimu) 
    {
      simucompvals$bool<-0
    }
    if (simucompvals$iteration != input$nbIterationCompSimu) 
    {
      simucompvals$bool<-0
    }
    if (identical(simucompvals$data, datvals$data)== FALSE) 
    {
      simucompvals$bool<-0
    }
    if (identical(simucompvals$data.ics, datvals$data.ics)== FALSE) 
    {
      simucompvals$bool<-0
    }
    
  })
  
  
  
  #We observe the action of launching the test. When it occurs, the results are computed, and the boolean value 
  #become true for the result to be print.
  #We also save the state of the data, the data.ics, the number of iteration and the level test, because the 
  #result has to disapear if one of them change. 
  observeEvent(input$launchCompSimu, {
    
    req(input$levelCompSimu)
    req(input$nbIterationCompSimu)
    
    validate(
      need(input$levelCompSimu > 0 && input$levelCompSimu <= 1, 
           "The level of the test must be included in ]0 ; 1]")
    )
    
    validate(
      need(input$nbIterationCompSimu > 0 && is.integer(input$nbIterationCompSimu), 
           "The number of iteration must be an integer strickly greater than 0")
    )
    simucompvals$bool<<-1
    simucompvals$level<-input$levelCompSimu
    simucompvals$iteration<-input$nbIterationCompSimu
    simucompvals$indexSimuComp<-indexSimuComp
    simucompvals$data<-datvals$data
    simucompvals$data.ics<-datvals$data.ics
    
    data.ics.rev<-datvals$data.ics
    data.ics.rev@Scores<-rev(datvals$data.ics@Scores)
    
    set.seed(seed)
    SimuBeg<<-ifelse(is.null(SimuBeg) | input$launchCompSimu > 0, max(comp.simu.test(datvals$data.ics, m=input$nbIterationCompSimu, type = "smallprop", 
                                level = input$levelCompSimu, adjust = TRUE)$index),  SimuBeg)
    set.seed(seed)
    SimuEnd<<-ifelse(is.null(SimuEnd) | input$launchCompSimu > 0, max(comp.simu.test(data.ics.rev, m=input$nbIterationCompSimu, type = "smallprop", 
                                level = input$levelCompSimu, adjust = TRUE)$index), SimuEnd)
    
    simucompvals$indexSimuComp<-append(0:SimuBeg, (ncol(datvals$data)+1-SimuEnd):(ncol(datvals$data)+1))
    simucompvals$indexSimuComp<-simucompvals$indexSimuComp[simucompvals$indexSimuComp>0]
    simucompvals$indexSimuComp<-simucompvals$indexSimuComp[simucompvals$indexSimuComp<=ncol(datvals$data)]
    simucompvals$indexSimuComp<-unique(simucompvals$indexSimuComp)
    
    nbComp<-1:ncol(datvals$data)
    index=append(nbComp[0:SimuBeg], nbComp[-(1:(ncol(datvals$data)-SimuEnd))])
    index<-unique(index[index > 0]) 
    data.ics.comp.Simulation<<-index 
    
    simucompvals$result<-c(input$nbIterationCompSimu, " simulations, level = ", input$levelCompSimu, "\n", 
                           "Number of components to keep, starting from the highest kurtosis: ", SimuBeg,
                           "\n", "Number of components to keep, starting from the lowest kurtosis: ", SimuEnd)
  })

  
  ##############################################################################################################
  #                                                OUTPUT TAB3                                                 #
  ##############################################################################################################
  
  #                      TOOL 3.1.1 Select the components to plot against each other                           #
  
  ### Construction of two numeric input ###
  
  #They must be bounded by the number of components
  output$componentXAxisUI <- renderUI({
    numericInput("componentXAxis", 
                 label= "", 
                 value = componentXAxis,
                 step = 1,
                 min=1,
                 max=ncol(datvals$data))
  })
  
  output$componentYAxisUI <- renderUI({
    numericInput("componentYAxis", 
                 label= "", 
                 value = min(ncol(as.matrix(datvals$data)), componentYAxis),
                 step = 1,
                 min=1,
                 max=ncol(datvals$data))
  })
  
  
  #                              DISPLAY 3.1.1 Plot two IC against each other                               #
  
  ### Construction of the plot ###
  
  output$plotICvsIC <- renderPlot({
    
    #We plot the two components chosen against each other
    #Col and Pch are determined by the previous actions of the user
    plot(x = ics.components(datvals$data.ics)[,input$componentXAxis], 
         y = ics.components(datvals$data.ics)[,input$componentYAxis], 
         col=icvsicvals$colorIndex+1, pch=icvsicvals$pchIndex,
         xlab=paste0("IC.",input$componentXAxis), ylab=paste0("IC.",input$componentYAxis),
         cex.lab=1.5)
    
    #We checked if each observation is supposed to be labelled or not. 
    #If yes, we put the label at the correct position, if not we put it very far, where it won't be seen
    #If the label is observation, the label used is rownames(data), if not, it's whenever he chose. 
    if(input$labelChoice == "Observation")
    {
      text(x = ifelse(icvsicvals$labelIndex > 0, 
                      ics.components(datvals$data.ics)[,input$componentXAxis], -1000000), 
           y = ifelse(icvsicvals$labelIndex > 0, 
                      ics.components(datvals$data.ics)[,input$componentYAxis], -1000000),
           labels = rownames(X), pos=3)
    }
    else if(input$labelChoice != "Observation")
    {
      id<-match(input$labelChoice, colnames(X))
      text(x = ifelse(icvsicvals$labelIndex > 0, 
                      ics.components(datvals$data.ics)[,input$componentXAxis], -1000000), 
           y = ifelse(icvsicvals$labelIndex > 0, 
                      ics.components(datvals$data.ics)[,input$componentYAxis], -1000000),
           labels = X[,id], pos=3)
    }
  })
  
  #We save this plot
  observeEvent(input$downloadPlotICvsICPNG, {
    
    dir<-choose.dir(default = saveDirectory)
    fileName <- sprintf("\\PlotICvsIC_%s.png", gsub(":", ",", date()))
    
    if(!(is.na(dir)))
    {
      file<-paste0(dir, fileName)
      png(filename=file)
      plot(x = ics.components(datvals$data.ics)[,input$componentXAxis], 
           y = ics.components(datvals$data.ics)[,input$componentYAxis], 
           col=icvsicvals$colorIndex+1, pch=icvsicvals$pchIndex,
           xlab=paste0("IC.",input$componentXAxis), ylab=paste0("IC.",input$componentYAxis),
           cex.lab=1.5)
      
      if(input$labelChoice == "Observation")
      {
        text(x = ifelse(icvsicvals$labelIndex > 0, 
                        ics.components(datvals$data.ics)[,input$componentXAxis], -1000000), 
             y = ifelse(icvsicvals$labelIndex > 0, 
                        ics.components(datvals$data.ics)[,input$componentYAxis], -1000000),
             labels = rownames(X), pos=3)
      }
      else if(input$labelChoice != "Observation")
      {
        id<-match(input$labelChoice, colnames(X))
        text(x = ifelse(icvsicvals$labelIndex > 0, 
                        ics.components(datvals$data.ics)[,input$componentXAxis], -1000000), 
             y = ifelse(icvsicvals$labelIndex > 0, 
                        ics.components(datvals$data.ics)[,input$componentYAxis], -1000000),
             labels = X[,id], pos=3)
      }
      dev.off()
    }
  })  
  
  
  #                            Modification of DISPLAY 3.1.1 with the TOOLS 3.1                               #
  
  ### Change in label 
  
  #Toapply the label we use two different vector of reactive value, one which contain the true labellized value,
  #and a second which contain what should be contain if the input is "brush". Without this method, the label 
  #define by brushing could not be erased. 
  observeEvent(
    {input$labelICvsIC
      icvsicvals$labelBrushedIndex}, {
        if(input$labelICvsIC == "1")
        { 
          icvsicvals$labelIndex<-rep(0, n) 
          icvsicvals$labelBrushedIndex<-rep(0, n)
        }
        else if(input$labelICvsIC == "2")
        {
          icvsicvals$labelIndex<-rep(1, n) 
          icvsicvals$labelBrushedIndex<-rep(0, n)
        }
        else if(input$labelICvsIC == "3")
        {
          icvsicvals$labelIndex<-icvsicvals$labelBrushedIndex
        }
      })
  
  #If brush is chosen and applied, we give the value 1 to the points that are brushed. 
  observeEvent(input$applyLabelICvsIC, {
    icvsicvals$labelBrushedIndex=replace(icvsicvals$labelBrushedIndex,
                                         brushedPoints(ics.components(datvals$data.ics), 
                                                       input$brushICvsIC, 
                                                       xvar = paste0("IC.",input$componentXAxis), 
                                                       yvar = paste0("IC.",input$componentYAxis), 
                                                       allRows = T)$selected_,1)
  })
  
  
  ### Change in categorical variable used for groups 
  
  #We use categoricalChoice definied in Tab1 to distinguish group on the plot using pch to alter their shape.
  #pch is the reason why the maximum number of levels is 25. 
  observeEvent(input$categoricalChoice, { 
    
    if(input$categoricalChoice != "No categories")
    {
      cat<-colnames(X) %in% input$categoricalChoice
      icvsicvals$pchIndex<-match(X[,cat], levels(as.factor(X[,cat]))) 
    }
    else
    {
      icvsicvals$pchIndex<-rep(1, nrow(datvals$data))
    }

    
  })
  
  ### Change in color and definition of clusters 
  
  #Once again we used a vector that we modify the index according to the cluster assigned to the brushed points
  observeEvent(input$colorCluster, {
    if(input$cluster=="1")
    {
      icvsicvals$colorIndex=replace(icvsicvals$colorIndex,
                                    brushedPoints(ics.components(datvals$data.ics), 
                                                  input$brushICvsIC, 
                                                  xvar = paste0("IC.",input$componentXAxis), 
                                                  yvar = paste0("IC.",input$componentYAxis), 
                                                  allRows = T)$selected_,1)
    }
    else if(input$cluster=="2")
    {
      icvsicvals$colorIndex=replace(icvsicvals$colorIndex,
                                    brushedPoints(ics.components(datvals$data.ics), 
                                                  input$brushICvsIC, 
                                                  xvar = paste0("IC.",input$componentXAxis), 
                                                  yvar = paste0("IC.",input$componentYAxis), 
                                                  allRows = T)$selected_,2)
    }
    else if(input$cluster=="3")
    {
      icvsicvals$colorIndex=replace(icvsicvals$colorIndex,
                                    brushedPoints(ics.components(datvals$data.ics), 
                                                  input$brushICvsIC, 
                                                  xvar = paste0("IC.",input$componentXAxis), 
                                                  yvar = paste0("IC.",input$componentYAxis), 
                                                  allRows = T)$selected_,3)
    }
    else if(input$cluster=="4")
    {
      icvsicvals$colorIndex=replace(icvsicvals$colorIndex,
                                    brushedPoints(ics.components(datvals$data.ics), 
                                                  input$brushICvsIC, 
                                                  xvar = paste0("IC.",input$componentXAxis), 
                                                  yvar = paste0("IC.",input$componentYAxis), 
                                                  allRows = T)$selected_,4)
    }
    else if(input$cluster=="5")
    {
      icvsicvals$colorIndex=replace(icvsicvals$colorIndex,
                                    brushedPoints(ics.components(datvals$data.ics), 
                                                  input$brushICvsIC, 
                                                  xvar = paste0("IC.",input$componentXAxis), 
                                                  yvar = paste0("IC.",input$componentYAxis), 
                                                  allRows = T)$selected_,5)
    }
    else if(input$cluster=="6")
    {
      icvsicvals$colorIndex=replace(icvsicvals$colorIndex,
                                    brushedPoints(ics.components(datvals$data.ics), 
                                                  input$brushICvsIC, 
                                                  xvar = paste0("IC.",input$componentXAxis), 
                                                  yvar = paste0("IC.",input$componentYAxis), 
                                                  allRows = T)$selected_,6)
    }
    else if(input$cluster=="7")
    {
      icvsicvals$colorIndex=replace(icvsicvals$colorIndex,
                                    brushedPoints(ics.components(datvals$data.ics), 
                                                  input$brushICvsIC, 
                                                  xvar = paste0("IC.",input$componentXAxis), 
                                                  yvar = paste0("IC.",input$componentYAxis), 
                                                  allRows = T)$selected_,7)
    }
    else if(input$cluster=="0")
    {
      icvsicvals$colorIndex=replace(icvsicvals$colorIndex,
                                    brushedPoints(ics.components(datvals$data.ics), 
                                                  input$brushICvsIC, 
                                                  xvar = paste0("IC.",input$componentXAxis), 
                                                  yvar = paste0("IC.",input$componentYAxis), 
                                                  allRows = T)$selected_,0)
    }
    
    #data.ics.cluster<<-icvsicvals$colorIndex
  })
  
  
  #                            Modification of DISPLAY 3.2.1 with the TOOLS 3.2                               #
  
  ### The selection sliders ###
  
  #Here we needs two sided selection sliders as the users may want intermediary components or component on both
  #end of the components
  #The sliders needs to be bounded by the number of components
  output$sliderFirstTab3.2UI <- renderUI({
    sliderInput("sliderFirstTab3.2", "",
                min = 0, max = ncol(datvals$data), value = c(0, input$sliderFirstTab2), step=1)
  })
  
  output$sliderLastTab3.2UI <- renderUI({
    sliderInput("sliderLastTab3.2", "",
                min = 0, max = ncol(datvals$data), value = c(0,input$sliderLastTab2), step=1)
  })
  
  ### The ICS list selection ###
  
  #We need to create a list of a list of ICS according to the inputs of the sliders 
  #We use our own define function.
  output$listChoiceICSUI <- renderUI({ 

    
    if(!is.null(input$sliderFirstTab3.2))
    {
      IndICS<-append(input$sliderFirstTab3.2[1]:input$sliderFirstTab3.2[2], 
                     ((input$sliderLastTab3.2[2]-(ncol(datvals$data)+1))*-1):
                       ((input$sliderLastTab3.2[1]-(ncol(datvals$data)+1))*-1))
      IndICS<-unique(IndICS[IndICS <= ncol(datvals$data)])
      IndICS<-IndICS[IndICS >0]
      
      Funclist<- function(Ind)
      {
        ListIC<-list()
        Ind.init<-Ind
        
        for(i in 1:length(Ind)/6)
        {
          if(length(Ind)>6)
          { 
            ListIC<-c(ListIC, paste0("ICS: ", substr(paste0(",", Ind[1:6], collapse =""), 2, 
                                                     nchar(paste0(",", Ind[1:6], collapse ="")))))
            Ind<-Ind[-(1:6)]
          }  
          else if(length(Ind) <= 6 && length(Ind)>1) 
          {
            ListIC<-c(ListIC, paste0("ICS: ", substr(paste0(",", Ind[1:length(Ind)], collapse =""), 2, 
                                                     nchar(paste0(",", Ind[1:length(Ind)], collapse ="")))))
            Ind<-Ind[-(1:length(Ind))]
            
            break
          }
          else if(length(Ind) == 1)
          {
            if(length(Ind.init) < 6)
            {
              ListIC<-c(ListIC, paste0("ICS: ", substr(paste0(",", Ind.init[1:length(Ind.init)], collapse =""), 2, 
                                                       nchar(paste0(",", Ind.init[1:length(Ind.init)], collapse ="")))))
            }
            else if(length(Ind.init) >= 6)
            {
              ListIC<-c(ListIC, paste0("ICS: ", substr(paste0(",", Ind.init[(length(Ind.init)-5):length(Ind.init)],
                                                              collapse =""), 2, 
                                                       nchar(paste0(",", Ind.init[1:length(Ind.init)], collapse ="")))))
            }
            Ind<-Ind[-(1:length(Ind))]
            
            break
          }
        }
        return(ListIC)
      }
      
      ListICs<-Funclist(IndICS)
      selectInput("listChoiceICS", "", 
                  choices = ListICs)
    }
    
  })
  
  
  ### The matrix of scatter matrices ###
  
  #There is actually a lot of cases to deal with. No more than 6 plots, plots which are not chosen should not
  #be plot, and when there is only one plot, its needs to plot one more component before and after. 
  output$matrixScatter <- renderPlot({ 
    req(input$listChoiceICS)
    
    chain<-substr(input$listChoiceICS[[1]], 6, nchar(input$listChoiceICS[[1]]))
    index<-as.numeric(strsplit(chain, ",")[[1]])
    validate(
      need(length(index)>1, "Please select at least two components")
    )
      plot(datvals$data.ics, index=index, 
           col=icvsicvals$colorIndex+1, pch=icvsicvals$pchIndex)


  })
  
  
  ##############################################################################################################
  #                                                OUTPUT TAB4                                                 #
  ##############################################################################################################
  
  #                                     TOOL 4.1 Choice of the components                                      #
  
  
  ### Construction of the two sliders ###
  
  #First, we have to construct the two slider, they must be bounded by the number of components and their
  #initial value must be defined by the result of an agotisno normality test 
  output$sliderFirstTab4UI <- renderUI({
    sliderInput("sliderFirstTab4", "",
                min = 0, max = ncol(datvals$data), value = initialValueFirst, step=1)
  })
  
  #For the second, we have to inverse the scores of datvals$data.ics because we want to test the last components
  output$sliderLastTab4UI <- renderUI({
    
     sliderInput("sliderLastTab4", "",
                min = 0, max = ncol(datvals$data), value = initialValueLast, step=1)
  })
  
  
  ### Update the value of the slider ###
  
  #Then we have to update the value of the slider if the twin slider in tab2 is modified.
  #We also modify the value of tab3.2, because the two needs to be changed in the same place to avoid conflict
  observeEvent(input$sliderFirstTab2, {
    input.First.change <<- input.First.change+1
    updateSliderInput(session, "sliderFirstTab4", value=input$sliderFirstTab2,
                      min=0, max=ncol(datvals$data), step=1)
    updateSliderInput(session, "sliderFirstTab3.2", value=c(0,input$sliderFirstTab2),
                      min=0, max=ncol(datvals$data), step=1)
    
    
  })
  
  observeEvent(input$sliderLastTab2, {
    input.Last.change <<- input.Last.change+1
    updateSliderInput(session, "sliderLastTab4", value=input$sliderLastTab2,
                      min=0, max=ncol(datvals$data), step=1)
    updateSliderInput(session, "sliderLastTab3.2", value=c(0,input$sliderLastTab2),
                      min=0, max=ncol(datvals$data), step=1)
    
    
  })
  
  
  #This sliders define and reinitialize the index and the cutoff reactive values
  observeEvent({datvals$data
    datvals$data.ics
    input$sliderFirstTab4 
    input$sliderLastTab4}, {
      req(datvals$data)
      validate(
        need(ncol(as.matrix(datvals$data))>1,"error")
      )
      req(input$sliderFirstTab4)
      req(input$sliderLaststTab4 )
      validate(
        need(!is.null(input$sliderFirstTab4), "error" )
      )
      validate(
        need(!is.null(input$sliderLastTab4), "error" )
      )
      
      
      initialValueFirst <<- input$sliderFirstTab4 
      initialValueLast <<- input$sliderLasstTab4 
      nbComp<-1:ncol(datvals$data)
      index=append(nbComp[0:initialValueFirst], nbComp[-(1:(ncol(datvals$data)-initialValueLast))])
      if (length(index)==0) index<-NULL
      outliervals$index <- unique(index)
      outliervals$dist <- ics.distances(datvals$data.ics, index=outliervals$index)
      outliervals$cutOff <- ifelse(input.First.change>1|input.Last.change>1, 0, cutOff.out)
    })
  
  #                                     TOOL 4.2 Label the observations                                      #
  
  
  #To apply the label we use two different vector of reactive value, one which contain the true labellized value,
  #and a second which contain what should be contain if the input is "brush". Without this method, the label 
  #define by brushing could not be erased. 
  observeEvent(
    {input$labelOutlier
      outliervals$cutOff
      outliervals$labelBrushedIndex}, {
        
        if(input$labelOutlier == "1")
        { 
          outliervals$labelIndex<-rep(0, n) 
          outliervals$labelBrushedIndex<-rep(0, n)
        }
        else if(input$labelOutlier == "2")
        {
          outliervals$labelIndex<-rep(1, n) 
          outliervals$labelBrushedIndex<-rep(0, n)
        }
        else if(input$labelOutlier == "3" & sum(labelIndex.out)==0)
        {
          outliervals$labelIndex<-ifelse( outliervals$dist > outliervals$cutOff, 1, 0) 
          outliervals$labelBrushedIndex<-rep(0, n)
          
        }
        else if(input$labelOutlier == "4")
        {
          outliervals$labelIndex<-outliervals$labelBrushedIndex
        }
        
        if(input$labelOutlier == "3")
        {
          data.ics.outlier<<-outliervals$labelIndex
        }
        else
        {
          data.ics.outlier<<-rep(0, n)
        }
        labelIndex.out <<- rep(0,n)
        
      })
  
  #If brush is chosen and applied, we give the value 1 to the points that are brushed. 
  observeEvent(input$applyLabelOutlier, {
    req(outliervals$index)
    validate(
      need(!is.null(outliervals$index), "Please select at least one component.")
    )
    datat<-as.data.frame(cbind(1:n, outliervals$dist))
    colnames(datat)<-c("x", "y")
    
    outliervals$labelBrushedIndex=replace(outliervals$labelBrushedIndex,
                                          brushedPoints(datat, 
                                                        input$brushOutlier, 
                                                        xvar = "x", 
                                                        yvar="y",
                                                        allRows = T)$selected_,1)
  })
  
  #                                     TOOL 4.3 Simulation of a cut-off                                      #
  
  #Simulate a cut-off point if the user wishes it
  #The simulation is done by dist.simu.test in {ICSOutlier}, it might takes some times
  #The user can choose the number of iterations and the level of the test
  observeEvent(input$cutOff, {
    
    validate(
      need(input$levelCutOff > 0 && input$levelCutOff <= 1, 
           "The level of the test must be included in ]0 ; 1]")
    )
    
    validate(
      need(input$nbIterationCutOff > 0 && is.integer(input$nbIterationCutOff), 
           "The number of iteration must be an integer strickly greater than 0")
    )
    
    set.seed(seed)
    outliervals$cutOff = ifelse((cutOff.out==0 | nbIterationCutOff!=input$nbIterationCutOff | levelCutOff!= input$levelCutOff), 
                                   dist.simu.test(datvals$data.ics, index=outliervals$index,
                                        m=input$nbIterationCutOff, level=input$levelCutOff),cutOff.out)
    outliervals$bool <- TRUE
    outliervals$cutOffMode<-1
  })
  
  
  #                                     TOOL 4.4 Cut-off: rejection rate                                      #
  
  #Simulate a rejection rate if the user wishes it
  #The rejection is implemented through a quantile 
  #The user can choose the percentage of reject
  observeEvent(input$rejectOutlierRate, {
    req(input$rejectionRate)
    validate(
      need(input$rejectionRate > 0 && input$rejectionRate <= 100, "errorRejectionRate")
    )
    
    outliervals$cutOff =  quantile(outliervals$dist, 
                                  probs=1-0.01*input$rejectionRate)
    outliervals$bool <- TRUE 
    outliervals$cutOffMode<-2
    
    updateNumericInput(session, "rejectionNumber", label = "", 
                       value = ceiling(input$rejectionRate*0.01*n),
                       step = 1,
                       min= 0,
                       max=n)
    
  })
  
  observeEvent(input$rejectOutlierNumber, {
    req(input$rejectionNumber)
    validate(
      need(input$rejectionNumber > 0 && input$rejectionNumber <= nrow(datvals$data), "errorRejectionRate")
    )
    
    rejectionRate<-input$rejectionNumber/n
    
    if(rejectionRate == 1)
    {
      rejectionRate == 0.9999999
    }
    
    outliervals$cutOff = quantile(outliervals$dist, 
                                  probs=1-rejectionRate)
    outliervals$bool <- TRUE 
    outliervals$cutOffMode<-2
    
    updateNumericInput(session, "rejectionRate", label = "", 
                       value = round(input$rejectionNumber*100/n, 2),
                       step = 1,
                       min= 0,
                       max=100)
  })
  
  
  observeEvent(datvals$data.ics, {
    if (compt.change>1){
      outliervals$bool <- FALSE
      outliervals$cutOff<-0
      outliervals$dist <- 0
      initialValueFirst <<- NULL
      initialValueLast <<- NULL
    }
  })
  
  #                                     DISPLAY 4.1 Outlier detection plot                                   #
  
  #This plot allows to identify potential outliers.
  #It consists in plotting the ics.distance of each observation given the selected component
  #it should be adjusted by all the tool implemented. 
  output$plotOutlier <- renderPlot({
    
    req(input$sliderFirstTab4)
    validate(
      need(input$sliderFirstTab4+input$sliderLastTab4>0, "Please select at least one component.")
    )
    
    if(!is.null(input$sliderFirstTab4))
    {
    
      #To avoid error if no components are chosen
      if(input$sliderFirstTab4+input$sliderLastTab4 != 0)
      {
        
        #We compute a new Malahanobis distance and cut-off point according to the components we have selected
        #outliervals$dist<-ics.distances(datvals$data.ics, index=outliervals$index)
        #If no cutoff was asked, we put a given value such that it won't appears at a random place on the graph
        if(outliervals$cutOff==0)
        {outliervals$cutOff<- max(outliervals$dist)+0.1}
        
        #We graphically distinguish point which are over the cut-off point
        colPoint<-ifelse(outliervals$dist > outliervals$cutOff, 2, grey(0.5))
        pchPoint<-ifelse(outliervals$dist > outliervals$cutOff, 16, 4)
        
        #We plot the distance plot and we have the cut-off line if needed
        plot(outliervals$dist, cex.lab=1, cex.axis=1, 
             ylim=c(0, max(outliervals$dist, outliervals$cutOff)+0.2),
             xlab="Observations", ylab="ICS distance with respect to the selected ICs",
             col=colPoint, pch=pchPoint,
             cex.lab=1.2)
        if(outliervals$cutOff != max(outliervals$dist)+0.1) 
        {
          abline(h=outliervals$cutOff) 
        }
        
        #We also add label if needed
        if(input$labelChoice == "Observation")
        {
          text(x = ifelse(outliervals$labelIndex > 0, outliervals$dist, -1000000), 
               labels = rownames(X), pos=3)
        }
        else if(input$labelChoice != "Observation")
        {
          id<-match(input$labelChoice, colnames(X))
          text(x = ifelse(outliervals$labelIndex > 0, outliervals$dist, -1000000), 
               labels = X[,id], pos=3)
        }
      }
    }
  })
  
  
  #We save this plot
  observeEvent(input$downloadPlotOutlierPNG, {
    
    dir<-choose.dir(saveDirectory)
    fileName <- sprintf("\\PlotOutlier_%s.png", gsub(":", ",", date()))
    
    if(!(is.na(dir)))
    {
      file<-paste0(dir, fileName)
      png(filename=file)
      
      #We graphically distinguish point which are over the cut-off point
      colPoint<-ifelse(outliervals$dist > outliervals$cutOff, 2, grey(0.5))
      pchPoint<-ifelse(outliervals$dist > outliervals$cutOff, 16, 4)
      
      #We plot the distance plot and we have the cut-off line if needed
      plot(outliervals$dist, cex.lab=1, cex.axis=1, 
           ylim=c(0, max(outliervals$dist, outliervals$cutOff)+0.2),
           xlab="Observations", ylab="ICS distance with respect to the selected ICs",
           col=colPoint, pch=pchPoint,
           cex.lab=1.2)
      if(outliervals$cutOff != max(outliervals$dist)+0.1) 
      { abline(h=outliervals$cutOff) }
      
      #We also add label if needed
      if(input$labelChoice == "Observation")
      {
        text(x = ifelse(outliervals$labelIndex > 0, outliervals$dist, -1000000), 
             labels = rownames(X), pos=3)
      }
      else if(input$labelChoice != "Observation")
      {
        id<-match(input$labelChoice, colnames(X))
        text(x = ifelse(outliervals$labelIndex > 0, outliervals$dist, -1000000), 
             labels = X[,id], pos=3)
      }
      
      dev.off()
    }
  })  

  
  ##############################################################################################################
  #                                                OUTPUT TAB5                                                 #
  ##############################################################################################################
  
  
  #                                    TOOL 5.1 and 2: Change the data                                         #
  
  #Adjust to the change in variable
  observeEvent(input$applyVar, {
    descvals$dataRef<-datvals$data
    descvals$dataCom<-datvals$data
  })
  
  #Change the reference data
  
  output$dataToDescribeUI<- renderUI ({ 
    
    datalist<- list("All observations", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4",
                    "Cluster 5", "Cluster 6", "Cluster 7")
    
    index<-c(1, unique(icvsicvals$colorIndex+1))  
    index<-unique(index)
    index<-sort(index)
    
    datalist<-datalist[index]
    
    if(outliervals$cutOff !=0 && outliervals$cutOff != max(outliervals$dist)+0.1)
    {
      datalist<-append(datalist, "Outliers")
    }
    
    
    selectInput("dataToDescribe", label = "", 
                choices = datalist, selected = "All observations", selectize = FALSE)
    
  })
  
  output$comparisonWithUI<- renderUI ({
    
    datalist<- list("All observations", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4",
                    "Cluster 5", "Cluster 6", "Cluster 7")
    
    index<-c(1, unique(icvsicvals$colorIndex+1))
    index<-unique(index)
    index<-sort(index)
    
    datalist<-datalist[index]
    
    if(outliervals$cutOff !=0 && outliervals$cutOff != max(outliervals$dist)+0.1)
    {
      datalist<-append(datalist, "Outliers")
    }
    
    
    selectInput("comparisonWith", label = "", 
                choices = datalist, selected = "All observations", selectize = FALSE) 
    
  })
  
  
  observeEvent(input$dataToDescribe, {
    if(input$dataToDescribe=="All observations")
    {
      descvals$dataRef<-datvals$data
    }
    else if(input$dataToDescribe=="Cluster 1")
    {
      descvals$dataRef<-datvals$data[which(icvsicvals$colorIndex==1), ]
    }
    else if(input$dataToDescribe=="Cluster 2")
    {
      descvals$dataRef<-datvals$data[which(icvsicvals$colorIndex==2), ]
    }
    else if(input$dataToDescribe=="Cluster 3")
    {
      descvals$dataRef<-datvals$data[which(icvsicvals$colorIndex==3), ]
    }
    else if(input$dataToDescribe=="Cluster 4")
    {
      descvals$dataRef<-datvals$data[which(icvsicvals$colorIndex==4), ]
    }
    else if(input$dataToDescribe=="Cluster 5")
    {
      descvals$dataRef<-datvals$data[which(icvsicvals$colorIndex==5), ]
    }
    else if(input$dataToDescribe=="Cluster 6")
    {
      descvals$dataRef<-datvals$data[which(icvsicvals$colorIndex==6), ]
    }
    else if(input$dataToDescribe=="Cluster 7")
    {
      descvals$dataRef<-datvals$data[which(icvsicvals$colorIndex==7), ]
    }
    else if(input$dataToDescribe=="Outliers")
    {
      
      descvals$dataRef<-datvals$data[which(outliervals$dist > outliervals$cutOff), ]
    } 
  })
  
  #Same with comparison
  observeEvent(input$comparisonWith, {
    if(input$comparisonWith=="All observations")
    {
      descvals$dataCom<-datvals$data
    }
    else if(input$comparisonWith=="Cluster 1")
    {
      descvals$dataCom<-datvals$data[which(icvsicvals$colorIndex==1), ]
    }
    else if(input$comparisonWith=="Cluster 2")
    {
      descvals$dataCom<-datvals$data[which(icvsicvals$colorIndex==2), ]
    }
    else if(input$comparisonWith=="Cluster 3")
    {
      descvals$dataCom<-datvals$data[which(icvsicvals$colorIndex==3), ]
    }
    else if(input$comparisonWith=="Cluster 4")
    {
      descvals$dataCom<-datvals$data[which(icvsicvals$colorIndex==4), ]
    }
    else if(input$comparisonWith=="Cluster 5")
    {
      descvals$dataCom<-datvals$data[which(icvsicvals$colorIndex==5), ]
    }
    else if(input$comparisonWith=="Cluster 6")
    {
      descvals$dataCom<-datvals$data[which(icvsicvals$colorIndex==6), ]
    }
    else if(input$comparisonWith=="Cluster 7")
    {
      descvals$dataCom<-datvals$data[which(icvsicvals$colorIndex==7), ]
    }
    else if(input$comparisonWith=="Outliers")
    {
      descvals$dataCom<-datvals$data[which(outliervals$dist > outliervals$cutOff), ]
    } 
  })
  
  #                                           DISPLAY 5.1: Summaries                                          #
  
  #Simply a summary
  output$summaryStat <- renderPrint({
    summary(descvals$dataRef)
  })
  
  output$compareStat <- renderPrint({
    if(input$dataToDescribe != input$comparisonWith && input$compare == TRUE && input$comparisonWith != "")
    {
      summary(descvals$dataCom)
    }
  })
  
  
  #                                         TOOL 5.2: Variable choice                                        #
  
  #Select the variable of which plot the boxplot and the histogram
  #It is restricted to numeric chosen variable
  output$varDescribeSelectUI <- renderUI({
    selectInput('varDescribeSelect', '', colnames(datvals$data), selectize=TRUE)
  })
  
  #                                           DISPLAY 5.2: Box plots                                         #
  
  #Box plot of the selected variable on selected dataset
  output$boxplot <- renderPlot({
    
    if(input$dataToDescribe != input$comparisonWith && input$compare == TRUE && input$comparisonWith != "")
    {
      Id<-which(colnames(descvals$dataRef)==input$varDescribeSelect)
      boxplot(descvals$dataRef[,Id], descvals$dataCom[,Id])
    }
    else
    {
      Id<-which(colnames(descvals$dataRef)==input$varDescribeSelect)
      boxplot(descvals$dataRef[,Id])
    }
  })
  
  #                                        DISPLAY 5.3: Density plots                                       #
  
  #Box plot of the selected variable on selected dataset
  output$densityPlot <- renderPlot({
    
    Id<-which(colnames(descvals$dataRef)==input$varDescribeSelect)
    
    if(input$dataToDescribe != input$comparisonWith && input$compare == TRUE && input$comparisonWith != "")
    {
      densityRef<-density(descvals$dataRef[,Id])
      densityCom<-density(descvals$dataCom[,Id])
      
      plot(densityRef,
           xlim=c(min(min(descvals$dataRef[,Id]), min(descvals$dataCom[,Id])),
                  max(max(descvals$dataRef[,Id]), max(descvals$dataCom[,Id]))),
           ylim=c(0, max(max(densityRef$y), max(densityCom$y))), 
           col=2,
           main="")
      lines(densityCom, col=4)
      legend("topright", lwd=2, col=c("red", "blue"), 
             legend=c("Reference data", "Compared data"))
      rug(descvals$dataRef[,Id], ticksize=0.06, side=1, lwd=0.5, col = 2)
      rug(descvals$dataCom[,Id], ticksize=0.03, side=1, lwd=0.5, col = 4)
    }
    else
    {
      densityRef<-density(descvals$dataRef[,Id])
      
      plot(x=densityRef,
           xlim=c(min(descvals$dataRef[,Id]), max(descvals$dataRef[,Id])),
           ylim=c(0, max(densityRef$y)), 
           col=1,
           main="")
      rug(descvals$dataRef[,Id], ticksize=0.03, side=1, lwd=0.5)
    } 
  })
  
  #                                           DISPLAY 5.4: Histograms                                        #
  
  #Histogram of the selected variable on selected dataset
  output$histogram <- renderPlot({
    Id<-which(colnames(descvals$dataRef)==input$varDescribeSelect)
    hist(descvals$dataRef[,Id], main="", xlab="", 
         breaks = 10) 
  })
  
  output$histogramCompare <- renderPlot({
    if(input$dataToDescribe != input$comparisonWith && input$compare == TRUE && input$comparisonWith != "")
    {
      Id<-which(colnames(descvals$dataRef)==input$varDescribeSelect)
      hist(descvals$dataCom[,Id], main="", xlab="",
           breaks = 10)
    }
  })
  
  
  ##############################################################################################################
  #                                                OUTPUT TAB6                                                 #
  ##############################################################################################################
  
  #                                       DISPLAY 6.1:  The data tables                                        #
  
  output$datasetUI<- renderUI ({
    
    datalist<- list("All observations", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4",
                    "Cluster 5", "Cluster 6", "Cluster 7")
    
    index<-c(1, unique(icvsicvals$colorIndex+1))
    index<-unique(index)
    index<-sort(index)
    
    datalist<-datalist[index]
    
    if(outliervals$cutOff !=0 && outliervals$cutOff != max(outliervals$dist)+0.1)
    {
      datalist<-append(datalist, "Outliers")
    }
    
    
    selectInput("dataset", label = "", 
                choices = datalist, selected = "All observations", selectize = FALSE)
    
  })
  
  #Can select the wished part of the data(whole, cluster, outliers)
  output$dataTab <- DT::renderDataTable({
    data.save <- datvals$data
    if(input$labelChoice !="Observation")
    {
      Label<- X[,match(input$labelChoice, names(X))]
      data.save <- cbind(Label, data.save)
    }
    
    if(input$categoricalChoice !="No categories")
    {
      Category<- X[,match(input$categoricalChoice, names(X))]
      data.save <- cbind(Category, data.save)
    }
    
    if(input$dataset=="All observations")
    {
      DT::datatable(data.save)
    }
    else if("Cluster" %in% strsplit(input$dataset, " ")[[1]][1])
    {
      ind.clus <- strsplit(input$dataset, " ")[[1]][2]
      DT::datatable(data.save[which(icvsicvals$colorIndex==1), ])
    }
    else
    {
      DT::datatable(data.save[which(outliervals$dist > outliervals$cutOff), ]) 
    }
  }) 
  
  ##############################################################################################################
  #                                                OUTPUT TAB7                                                 #
  ##############################################################################################################
  
  #                                      TOOL 7.1:  Save the data table                                        #
  
  #Save the data into the chosen file
  observeEvent(input$saveData, {
    
    datasave<- savevals$datasave
    outputDir <- choose.dir(default = saveDirectory)
    
    # Create a unique file name
    fileName <- sprintf("ICS shiny_%s.csv", gsub(":", ",", date()))
    
    # Write the file to the local system
    write.csv(
      x = datasave,
      file = file.path(outputDir, fileName), 
      row.names = FALSE, quote = TRUE
    )
  }) 
  
  #                                TOOL 7.2:  Save the summary of operations                                    #
  
  #Save the data into the chosen file
  observeEvent(input$saveSummary, {
    req(simucompvals$bool==1)
    outputDir <- choose.dir(default = saveDirectory)
    fileName <- sprintf("ICS shiny summary_%s.txt", gsub(":", ",", date()))
    
	if (!is.null(nameData)){
       write(paste(
      paste0("The data file is: ", nameData),
      paste0("It contains ",n," observations and ", length(var.names), " numerical variables."),
      "",
      sep="\n"),
      file = file.path(outputDir, fileName))    
	}else{
	 write(paste(
      paste0("The data file contains ",n," observations and ", length(var.names), " numerical variables."),
      "",
      sep="\n"),
      file = file.path(outputDir, fileName))
    }
	
    if(datvals$varMode == 0)
    {
      write("All the numerical variables were kept in the analysis.", 
            file=file.path(outputDir, fileName), append=TRUE)
      write(paste0("So ", ncol(X[,var.names]), 
                   " variables are taken into account in ICS."), file = file.path(outputDir, fileName), append=TRUE)
    }
    else if(datvals$varMode == 1)
    {
      write("The following numerical variables were kept in the analysis:", 
            file=file.path(outputDir, fileName), append=TRUE)
      write(datvals$varChoice, file=file.path(outputDir, fileName), append=TRUE)
      write(paste0("So ", length(datvals$varChoice), 
                   " variables are taken into account in ICS."), file=file.path(outputDir, fileName), append=TRUE)
    }
    else if(datvals$varMode == 2)
    {
      write("The following numerical variables were excluded from the analysis:", 
            file=file.path(outputDir, fileName), append=TRUE)
      write(var.names[!(var.names%in%datvals$varChoice)], file=file.path(outputDir, fileName), append=TRUE)
      write(paste0("So ", length(datvals$varChoice), 
                   " variables are taken into account in ICS."), file=file.path(outputDir, fileName), append=TRUE)
    }
    write("", file = file.path(outputDir, fileName), append=TRUE)
    
    write("The scatter matrices in ICS are:", file = file.path(outputDir, fileName), append=TRUE)
    write(paste0("S1 = ", input$scatterChoice1, " with the following parameters: "), 
          file =file.path(outputDir, fileName), append=TRUE)
    write(names(datvals$S1args), file=file.path(outputDir, fileName), append=TRUE)
    write(unlist(datvals$S1args), file=file.path(outputDir, fileName), append=TRUE)
    write(paste0("S2 = ", input$scatterChoice2, " with the following parameters: "), 
          file =file.path(outputDir, fileName), append=TRUE)
    write(names(datvals$S2args), file=file.path(outputDir, fileName), append=TRUE)
    write(unlist(datvals$S2args), file=file.path(outputDir, fileName), append=TRUE)
    
    write("", file=file.path(outputDir, fileName), append=TRUE)
    
    write(paste0("The observations are labelled using the variable: ", input$labelChoice), 
          file=file.path(outputDir, fileName), append=TRUE)
    
    if(input$categoricalChoice != "No categories")
    {
      write(paste0("The categories are defined by the variable ",
                   input$categoricalChoice), file=file.path(outputDir, fileName), append=TRUE)
    }
    
    write("", file=file.path(outputDir, fileName), append=TRUE)
    
    write(paste("Looking at the screeplot of the generalized kurtosis associated with the invariant components",
                "or/and at the suggestions of the normality tests, the following components are selected:", 
                sep = "\n"), file=file.path(outputDir, fileName), append=TRUE)
    write(outliervals$index, file=file.path(outputDir, fileName), append=TRUE)
    
    if(simucompvals$bool==1)
    {
      write(paste("Moreover, invariant components are selected via Monte Carlo simulations.",
                  paste0("The simulation contained ",input$nbIterationCompSimu, 
                         " iterations and was of level ",input$levelCompSimu),
                  "The suggested index was:",
                  sep= "\n"), file=file.path(outputDir, fileName), append=TRUE)
      write(simucompvals$indexSimuComp, file=file.path(outputDir, fileName), append=TRUE)
    }
    
    write("", file=file.path(outputDir, fileName), append=TRUE)
    
    if(outliervals$bool == FALSE)
    {
      write("No outliers were tagged in the data", file=file.path(outputDir, fileName), append=TRUE)
    }
    else if(outliervals$bool == TRUE)
    {
      if(outliervals$cutOffMode == 1)
      {
        write(paste0("In order to identify outliers, a cut-off was defined via Monte Carlo simulations.",
                     " The simulations contained ", input$nbIterationCutOff, 
                     " iterations and are at the level ", input$levelCutOff), 
              file=file.path(outputDir, fileName), append=TRUE)
        write(paste0("The cut-off value is ", round(outliervals$cutOff, 3)), 
              file=file.path(outputDir, fileName), append=TRUE) 
      } 
      else
      {
        write(paste0("In order to identify outliers, a cut-off was defined via a percentage rate", 
                     "The rejection rate was setted up to ", input$rejectionRate, 
                     "% which corresponds to ", input$rejectionNumber, " outliers"), 
              file=file.path(outputDir, fileName), append=TRUE)
        write(paste0("The cut-off value is ", round(outliervals$cutOff, 3)), 
              file=file.path(outputDir, fileName), append=TRUE)
      }
    }
    
  }) 
  #                                 DISPLAY 7.1:  The data table of the components                               #
  
  #Save the data into the chosen file
  output$dataComponent <- DT::renderDataTable({
    
    if (is.null(outliervals$index)){
      datasave<-cbind(ics.components(datvals$data.ics), Distances = outliervals$dist)
    }else{
      datasave<-cbind(ics.components(datvals$data.ics)[outliervals$index], Distances = outliervals$dist)
    }
    
    if(sum(icvsicvals$colorIndex) != 0)
    {
      Cluster<-icvsicvals$colorIndex
      datasave<-cbind(datasave, Cluster)
    }

    
    if(outliervals$cutOff !=0 && outliervals$cutOff != max(outliervals$dist)+0.1)
    {
      Outlier<- as.integer(outliervals$cutOff < outliervals$dist)
      datasave<-cbind(datasave, Outlier)
    }
    
    if(input$labelChoice !="Observation")
    {
      Label<- X[,match(input$labelChoice, names(X))]
      datasave<-cbind(Label, datasave)
    }  
    
    if(input$categoricalChoice !="No categories")
    {
      Category<- X[,match(input$categoricalChoice, names(X))]
      datasave<-cbind(Category, datasave)
    } 
    
    savevals$datasave<-datasave
    DT::datatable(datasave)
  }) 
  
  #                                 DISPLAY 7.2:  The summary of operations                               #
  
   output$summaryOperations <- renderPrint({ 
 req(simucompvals$bool)
    cat("\n")
    if (!is.null( nameData)){
      cat(paste0("The data file is: ", nameData,"\n"))
      cat(paste0("It contains ",n," observations and ", length(var.names), " numerical variables.\n"))
    }else{
      cat(paste0("The data file contains ",n," observations and ", length(var.names), " numerical variables.\n"))
    }
    cat("\n")
    
    
    if(datvals$varMode == 0)
    {
      cat("All the numerical variables were kept in the analysis.\n")
      cat(paste0("So ", ncol(X[,var.names]), 
                   " variables are taken into account in ICS.\n"))
    }
    else if(datvals$varMode == 1)
    {
	  cat("The following numerical variables were kept in the analysis:\n")
      cat(datvals$varChoice)
      cat(paste0("\nSo ", length(datvals$varChoice), 
                   " variables are taken into account in ICS.\n"))
    }
    else if(datvals$varMode == 2)
    {
	  cat("The following numerical variables were excluded from the analysis:\n")
      cat(var.names[!(var.names%in%datvals$varChoice)])
      cat(paste0("\nSo ", length(datvals$varChoice), 
                   " variables are taken into account in ICS.\n"))
    }
	cat("\n")
    
    cat("The scatter matrices in ICS are:\n")
    cat(paste0("S1 = ", input$scatterChoice1, "\n"))
    if ( !is.null(names( datvals$S1args))){
    cat("with the following parameters: ")
    cat(paste(names( datvals$S1args), unlist( datvals$S1args), sep = " = "))
    }
    cat(paste0("\nS2 = ",  input$scatterChoice2, "\n"))
    if ( !is.null(names( datvals$S2args))){
    cat("with the following parameters: ")
    cat(paste(names( datvals$S2args), unlist( datvals$S2args), sep = " = "))
    }
    
    cat("\n")
    
    cat(paste0("\nThe observations are labelled using the variable: ",  input$labelChoice,".\n"))

    
    if(input$categoricalChoice != "No categories")
    {
	  cat(paste0("The categories are defined by the variable ",
                   input$categoricalChoice, "\n"))
    }
	  cat("\n")
    
    cat("Looking at the screeplot of the generalized kurtosis associated with the invariant components or/and\nat the suggestions of the normality tests, the following components are selected:\n")
    cat(outliervals$index)
    
    cat("\n")
    if( simucompvals$bool==1)
    {
      cat("\nLooking at the Monte Carlo simulations for the selection of the invariant components.\n")
      cat(paste0("The simulation contain ",input$nbIterationCompSimu, 
                   " iterations at the level ",input$levelCompSimu, ".\n"))
      cat("The suggested index are:\n")
      cat(simucompvals$indexSimuComp)
      cat("\n")
    }
    
    cat("\n")
    
    if(outliervals$bool == FALSE)
    {
      cat("No outliers were tagged in the data.")
    }
    else if(outliervals$bool == TRUE)
    {
      if(outliervals$cutOffMode == 1)
      {
		cat("In order to identify outliers, a cut-off was defined via Monte Carlo simulations.\n")
        cat(paste0("The simulations contain ", input$nbIterationCutOff, 
                     " iterations and are at the level ", input$levelCutOff, ".\n")) 
        cat(paste0("The cut-off value is ", round(outliervals$cutOff, 3), ".\n")) 
      } 
      else
      {
		cat("In order to identify outliers, a cut-off was defined via a percentage rate.\n")
        cat(paste0("The rejection rate was setted up to ", input$rejectionRate, 
                     "% which corresponds to ", input$rejectionNumber, " outliers. \n")) 
        cat(paste0("The cut-off value is ", round(outliervals$cutOff, 3), ".\n"))
      }
    }
  })
  
  
  ##############################################################################################################
  #                                                  OPTIONS                                                   #
  ##############################################################################################################
  
  #This option allows for an output to be computed directly and not just when it is first seen in the application
  #We apply it to widgets, because it does not slow down the application too much and it avoid an error, when
  #the plot has yet to receive the input. 
  #To that we add the dataTableOutput, because the user will be able to check if everything is as he inputed.
  outputOptions(output, "categoricalChoiceUI", suspendWhenHidden = FALSE) 
  outputOptions(output, "sliderFirstTab2UI", suspendWhenHidden = FALSE) 
  outputOptions(output, "sliderLastTab2UI", suspendWhenHidden = FALSE)
  outputOptions(output, "kernelIndexUI", suspendWhenHidden = FALSE) 
  outputOptions(output, "bandwidthUI", suspendWhenHidden = FALSE)
  outputOptions(output, "componentXAxisUI", suspendWhenHidden = FALSE) 
  outputOptions(output, "componentYAxisUI", suspendWhenHidden = FALSE)
  outputOptions(output, "sliderFirstTab3.2UI", suspendWhenHidden = FALSE)
  outputOptions(output, "sliderLastTab3.2UI", suspendWhenHidden = FALSE)
  #outputOptions(output, "listChoiceICSUI", suspendWhenHidden = FALSE) 
  outputOptions(output, "sliderFirstTab4UI", suspendWhenHidden = FALSE) 
  outputOptions(output, "sliderLastTab4UI", suspendWhenHidden = FALSE) 
  outputOptions(output, "varDescribeSelectUI", suspendWhenHidden = FALSE)
  outputOptions(output, "datasetUI", suspendWhenHidden = FALSE)
  
  
  ##############################################################################################################
  #                                                QUIT BUTTONS                                                #
  ##############################################################################################################
  
  observeEvent({
    input$quitTab1}, {
      stopApp(returnValue=valeuretour())
    })
  
  observeEvent({
    input$quitTab2}, {
      stopApp(returnValue=valeuretour())
    })
  
  observeEvent({
    input$quitTab3.1}, {
      stopApp(returnValue=valeuretour())
    })
  
  observeEvent({
    input$quitTab3.2}, {
      stopApp(returnValue=valeuretour())
    })
  
  observeEvent({
    input$quitTab4}, {
      stopApp(returnValue=valeuretour())
    })
  
  observeEvent({
    input$quitTab5}, {
      stopApp(returnValue=valeuretour())
    })
  
  observeEvent({
    input$quitTab6}, {
      stopApp(returnValue=valeuretour())
    })
  
  observeEvent({
    input$quitTab7}, {
      stopApp(returnValue=valeuretour())
    })
  
  #Use for closing the application when the browser is closed. 
  session$onSessionEnded(function() {
    stopApp(returnValue=valeuretour())
  })
  valeuretour=function(){
    res.ics=list()
    res.ics$nameData <- nameData
    res.ics$X <- X
    res.ics$var.names <- var.names
    res.ics$var.names.quali <- var.names.quali
    res.ics$data.ics<-data.ics
    res.ics$S1<-S1
    res.ics$S2<-S2
    res.ics$S1name <- S1name
    res.ics$S2name <- S2name
    res.ics$S1args<-S1args
    res.ics$S2args<-S2args
    res.ics$seed<-seed 
    res.ics$n <- nrow(X)
    #res.ics$data.ics.dist<-outliervals$dist
    res.ics$data.ics.comp<-outliervals$index
    #res.ics$data.ics.cluster<-data.ics.cluster
    res.ics$data.ics.outlier<-data.ics.outlier
    res.ics$varMode <- datvals$varMode
    res.ics$varChoice <- datvals$varChoice
    res.ics$alpha <-  reacvals$alpha
    res.ics$df <- input$degreeFreedom
    res.ics$maxiter <- input$maxiter
    res.ics$varChoice.input <- input$varChoice
    res.ics$labelChoice <- input$labelChoice
    res.ics$categoricalChoice <- input$categoricalChoice
    res.ics$initialValueFirst <- initialValueFirst
    res.ics$initialValueLast <- initialValueLast
    
    res.ics$result <- simucompvals$result
    res.ics$level<- input$levelCompNorm
    res.ics$iteration <- input$nbIterationCompSimu
    res.ics$indexSimuComp <- simucompvals$indexSimuComp
    res.ics$levelCompSimu <- input$levelCompSimu
    
    res.ics$colorIndex <- icvsicvals$colorIndex
    res.ics$pchIndex <- icvsicvals$pchIndex
    res.ics$labelIndex <- icvsicvals$labelIndex
    res.ics$labelBrushedIndex <- icvsicvals$labelBrushedIndex
    
     res.ics$cutOff.out<-outliervals$cutOff
     res.ics$labelIndex.out<-outliervals$labelIndex
     res.ics$labelBrushedIndex.out<-outliervals$labelBrushedIndex
     res.ics$bool.out<-outliervals$bool
     res.ics$cutOffMode.out<-outliervals$cutOffMode
     res.ics$dist.out<-outliervals$dist
     
     res.ics$existingClusters <- descvals$existingClusters
     
     res.ics$saveDirectory <- savevals$saveDirectory
     res.ics$textSummary <- savevals$textSummary
    
     res.ics$kernelIndexvalue <- input$kernelIndex
     res.ics$bandwidth <- input$bandwidth
     
     
     res.ics$SimuBeg <- SimuBeg
     res.ics$SimuEnd <- SimuEnd 
     res.ics$simu.bool <- simucompvals$bool

     res.ics$componentXAxis <- input$componentXAxis
     res.ics$componentYAxis <- input$componentYAxis
     
     res.ics$labelICvsIC <- input$labelICvsIC
     
     res.ics$cluster <- input$cluster
     res.ics$labelOutlier <- input$labelOutlier
     
     res.ics$nbIterationCutOff <- input$nbIterationCutOff
     res.ics$levelCutOff <- input$levelCutOff
     res.ics$rejectionNumber <- input$rejectionNumber
     res.ics$rejectionRate <- input$rejectionRate
     
    class(res.ics) <- "icsshiny"
    #rm(alpha, envir = .GlobalEnv)
    return(res.ics)
  }
  
}#End of server 
