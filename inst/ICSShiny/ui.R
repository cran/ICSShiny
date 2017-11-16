#ui script for ICSShiny

fluidPage(
  
  titlePanel("ICS Shiny"),
  
  tabsetPanel(
    
    #############################################################################################################
    #                                                TAB PANEL 1                                                #
    #############################################################################################################
    
    #This first panel display a overlook of the matrix of scatter matrices to let the user choose the parameters
    #that best characterize the data.
    tabPanel("Parameter choice", 
             
             sidebarLayout(
               
               #The side bar will contain the parameter tools.
               sidebarPanel(width=3,
                            
                            #                          TOOL 1.1 Scatter matrices selection                      #
                            
                            #If S1 and/or S2 are not inputed by the user, he can choose between Cov-Cov4, tM-Cov,
                            #MCD-Cov and MCD-tM
                            #However, if S1 and S2 are both specified, they are the first choice proposed to the
                            #user attention althrough he can still change.
                            h4("Select the scatter matrices"),
                            fluidRow(
                              column(6, 
                                     h6("Select the first scatter matrix"),
                                     if (S1name!=S2name & (S1name%in%c("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST")))
                                      {
                                         selectInput("scatterChoice1", label = "", 
                                                     choices = list("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST"),
                                                     selected = S1name)
                                      }
                                      else 
                                      {
                                         selectInput("scatterChoice1", label = "", 
                                                     choices = list("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST", 
                                                                    "Personnalized"),
                                                     selected = "Personnalized")
                                      }
                                    
                              ),
                              column(6, 
                                     h6("Select the second scatter matrix"),
                                     if (S2name!=S1name & (S2name%in%c("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST")))
                                      {
                                         selectInput("scatterChoice2", label = "", 
                                                     choices = list("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST"),
                                                     selected = S2name)
                                      }
                                      else
                                      {
                                         selectInput("scatterChoice2", label = "", 
                                                     choices = list("MeanCov", "Mean3Cov4", "MCD", "tM", "HRMEST", 
                                                                    "Personnalized"),
                                                     selected = "Personnalized")
                                       }
                                     
                                     )),
                            conditionalPanel(
                              condition = 'input.scatterChoice1 == input.scatterChoice2 &&
                              input.scatterChoice1 != "Personnalized"',
                              p("*Please select different scatter matrices",
                                style = "font-family: 'times'; font-si16pt; color : red")
                            ),
                            
                            
                            #                    TOOL 1.2 Adjust the scatter matrice parameters                #
                            
                            conditionalPanel(
                              condition = 'input.scatterChoice1 == "MCD" || input.scatterChoice2 == "MCD"',
                              
                              fluidRow(
                                checkboxInput("parametersMCD", label = "Parametrize MCD",
                                              value = ifelse(alpha==0.5, FALSE, TRUE))),
                              
                              #The following conditional panel is displayed only if the user has ticked the 
                              #"Parametrize MCD" checkbox
                              #It allows the user to change the parameter alpha of the function CovMcd {rrcov}
                              
                              conditionalPanel(
                                condition = "input.parametersMCD == true",
                                fluidRow(
                                  column(6, align="center", 
                                         br(),
                                         h5("Select an alpha value between 0.5 and 1")),
                                  column(6, align="center", 
                                         numericInput("alpha", 
                                                      label= "", 
                                                      value = alpha,
                                                      step = 0.05,
                                                      min=0.5,
                                                      max=1)))
                                
                              )
                            ), 
                            
                            conditionalPanel(
                              condition = 'input.scatterChoice1 == "tM" || input.scatterChoice2 == "tM"',
                              
                              fluidRow(
                                checkboxInput("parameterstM", label = "Parametrize tM", 
                                              value = ifelse(df==1,FALSE,TRUE))),
                              
                              #The following conditional panel is displayed only if the user has ticked the 
                              #"Parametrize tM" checkbox
                              #It allows the user to change the parameter df of the function tM {ICS}
                              
                              conditionalPanel(
                                condition = "input.parameterstM == true",
                                fluidRow(
                                  column(6, align="center", 
                                         br(),
                                         h5("Select a degree of freedom between 1 and 10")),
                                  column(6, align="center", 
                                         numericInput("degreeFreedom", 
                                                      label= "", 
                                                      value = df,
                                                      step = 1,
                                                      min=1,
                                                      max=10)))
                              )
                            ),
                            
                            conditionalPanel(
                              condition ='input.scatterChoice1 == "HRMEST" || input.scatterChoice2 == "HRMEST"',
                              
                              fluidRow(
                                checkboxInput("parametersHRMest", label = "Parametrize HRMEST", 
                                              value = ifelse(maxiter==100, FALSE, TRUE))),
                              
                              #The following conditional panel is displayed only if the user has ticked the 
                              #"Parametrize HRMEST" checkbox
                              #It allows the user to change the parameter alpha of the function CovMcd {rrcov}
                              
                              conditionalPanel(
                                condition = "input.parametersHRMest == true",
                                fluidRow(
                                  column(6, align="center", 
                                         br(),
                                         h5("Select the maximum number of iteration")),
                                  column(6, align="center", 
                                         numericInput("maxiter", 
                                                      label= "", 
                                                      value = maxiter,
                                                      step = 1,
                                                      min=1,
                                                      max=10000)))
                                
                              )
                            ), 
                            
                            
                            #                 TOOL 1.3 Choose the variable to keep in the analysis              #
                            
                            #According to the size of the dataset, and the number of wished variable, it may
                            #sometimes be more usefull to select variable or to exclude variable.
                            fluidRow(
                              h4("Select the variables"),
                              radioButtons("varSelectMode", "", 
                                           choices = list("To include in the analysis" = 1, 
                                                          "To exclude from the analysis" = 2),
                                           selected = varMode)),
                            
                            #Multiple variables can be chosen at one, only numeric variable are proposed
                            #Selectize allows the user to pick nothing, in this case all variables are kept. 
                            fluidRow(
                              selectizeInput("varChoice", "", 
                                             choices=var.names, multiple = TRUE, selected=varChoice.input)),
                            #Only when the user have chosen he should click on the following button to apply
                            #the selection. It avoid unnecesary calculations and the program would crash if the 
                            #user were to input the first variable since it needs at least two variable to work.
                            fluidRow(
                              column(12, align="center", 
                                     actionButton("applyVar", "Apply the choice"))),
                            
                            
                            #         TOOL 1.4 Choose the variable to labels the observations in plots         #
                            
                            #These will be used latter on to label the observations. The choice doesn't include
                            #variable of data which are numeric. 
                            fluidRow(
                              h4("Select the variable to label the observations"),
                              selectInput("labelChoice", "", 
                                          choices = c("Observation", 
                                                      var.names.quali),
                                          selected = labelChoice)),
                            
                            
                            #      TOOL 1.5 Choose a categorical variable to distinguish groups on plots       #
                            
                            #Since this widget needs a two-step-computation to limit the number of levels to 25
                            #it will be constructed in the server part. 
                            fluidRow(
                              h4("Distinguish observations according to a categorical variable"),
                              uiOutput("categoricalChoiceUI")),
                            
                            
                            #                         TOOL 1.6 Reset all parameters                           #
                            
                            #Althrough it appears identcally on all the page, it needs its own widget to avoid 
                            #conflict with observeEvent 
                            fluidRow(
                              column(12, align="center", 
                                     br(),
                                     actionButton("reset", "Reset the settings"))),
                            
                            
                            #                         TOOL 1.7 Close the application                          #
                            
                            #Althrough it appears identcally on all the page, it needs its own widget to avoid 
                            #conflict with observeEvent 
                            fluidRow(
                              column(12, align="center", 
                                     br(),
                                     actionButton("quitTab1", "Close the session")))
                            
                            
               ),
               
               #The main panel will contain the matrix of scatter matrices reactive to the chosen parameters.
               mainPanel(
                 
                 fluidRow(
                   plotOutput("scatterPlotSetup", width = "100%", height = "850px"))
               )
             )
    ),#End of Tab1
    
    
    #############################################################################################################
    #                                                TAB PANEL 2                                                #
    #############################################################################################################
    
    #The second tab panel gives information on each components to the user, to help him identify which components
    #are relevant.
    tabPanel("Component selection", 
             
             sidebarLayout(
               
               #The side bar will allows the user to select the number of components he think relevant according
               #to the information on this page, this choice will have consequence for outlier detection.
               sidebarPanel(width=3,
                            
                            #                        TOOL 2.1 Choice of the components                        #
                            
                            #These sliders are constructed in server because the length needs to be adjust as
                            #the number of kept variable changes. 
                            #The first slider selects a number of first components, the second slider selects a 
                            #number of last components. This is consistent with ICS theory. 
                            #This sliders are linked with the ones in Tab 4, as such modifying one slider will
                            #modify the twin slider.
                            fluidRow(
                              column(12,
                                     h4("Number of components to keep, starting from the highest kurtosis"),
                                     uiOutput("sliderFirstTab2UI"))),
                            fluidRow(
                              column(12,
                                     h4("Number of components to keep, starting from the lowest kurtosis"),
                                     uiOutput("sliderLastTab2UI"))),
                            p("*The default values are given by a D'Agostino normality test of level 5%",
                              style = "font-family: 'times'; font-si16pt"),
                            p("Please feel free to select the components you deem relevant considering the
                              screeplot and the automatic selection of components",
                              style = "font-family: 'times'; font-si16pt"),
                            
                            
                            #                         TOOL 2.2 Close the application                          #
                            
                            #Althrough it appears identcally on all the page, it needs its own widget to avoid 
                            #conflict with observeEvent 
                            fluidRow(
                              column(12, align="center", 
                                     br(),
                                     actionButton("quitTab2", "Close the session")))
                            
                            
               ),
               
               #This main panel is actually displayed in three tabs.
               mainPanel(
                 
                 tabsetPanel(
                   
                   
                   #                         DISPLAY 2.1 Screeplot of the components                          #
                   
                   #This display the screeplot of the components, allowing to choose which are interesting at
                   #the beginning or the end according to the size of the drop in kurtosis. 
                   #This is followed by a short summary of the ICS object, with the precise values of kurtosis
                   tabPanel("Screeplot",
                            fluidRow(
                              column(12,
                                     h3("Screeplot of the components"),
                                     plotOutput("screeplot"))),

                            fluidRow(
                              column(12, align="center", 
                                actionButton("downloadScreeplotPNG", "Download as png")
                              )),
                   
                            fluidRow(
                              column(12,
                                     h3("Summary of ICS2"),
                                     verbatimTextOutput("summaryICS")))
                   ),
                   
                   
                   #                      DISPLAY 2.2 Kernel density of the components                        #
                   
                   #This displays the kernel density of each selected components. 
                   #The component can be selected through a numeric input, which is constructed in server because
                   #its maximum depends on the choice of variable. 
                   #The bandwidth is selected through a numeric input, which is constructed in server because the
                   #default value od the bandwidth is different for each component selected by kernelIndex
                   tabPanel("Kernel density",
                            h3("Kernel density of the selected component"),
                            fluidRow(
                              column(6,
                                     h4("Select a component"),
                                     uiOutput("kernelIndexUI")),
                              column(6,
                                     h4("Select the bandwidth"),
                                     uiOutput("bandwidthUI"))),
                            
                            fluidRow(
                              plotOutput("kernel", width = "100%", height = "600px")),
                            fluidRow(
                              column(12, align="center", 
                                     actionButton("downloadPlotKernelPNG", "Download as png")
                              ))
                   ),
                   
                   
                   #                       DISPLAY 2.3 Results of the normality tests                         #
                   
                   #This displays the results of five differents normality test to help the user choose 
                   #relevant components. 
                   #A result can also be obtain by simulation. It is only computed if the user asks for it because
                   #it takes a lot of times. 
                   tabPanel("Automatic selection of components",
                            
                            h3("Components selected through different normality tests"),
                            
                            fluidRow(
                              column(6,
                                     h4("Select the level of the normality tests")
                              ),
                              column(6,
                                     numericInput("levelCompNorm", 
                                                  label= "", 
                                                  value = level,
                                                  step = 0.01,
                                                  min=0,
                                                  max=1))),
                            
                            fluidRow(
                              verbatimTextOutput("normalityTestResults")),
                            
                            h3("Parallel analysis: Threshold from Monte Carlo simulations"),
                            
                            #The simulation made by comp.simu.test {ICSOutlier} is parametrize according to 
                            #"m" the number of components, and "level" the level of the test. Default values
                            #are the same as in the function. 
                            #The test needs to be launched to avoid heavy unnecessary computations. 
                            fluidRow(
                              column(4,
                                     h4("Select the number of simulations"),
                                     numericInput("nbIterationCompSimu", 
                                                  label= "", 
                                                  value = iteration,
                                                  step = 100,
                                                  min=0,
                                                  max=1000000)),
                              column(4,
                                     h4("Select the level of the test"),
                                     numericInput("levelCompSimu", 
                                                  label= "", 
                                                  value = levelCompSimu,
                                                  step = 0.01,
                                                  min=0,
                                                  max=1)), 
                              column(4,
                                     br(), br(), br(),
                                     actionButton("launchCompSimu", "Launch the test")
                              )),
                            p("*It can take quite a long time according to the number of simulations",
                              style = "font-family: 'times'; font-si16pt"), 
                            fluidRow(
                              verbatimTextOutput("compSimuResults"))
                   )
                   
                 )
               )
               )
    ), #End of Tab2
    
    
    #############################################################################################################
    #                                                TAB PANEL 3                                                #
    #############################################################################################################
    
    #The third tab panel allows the user to search for the presence of cluster in the data and to define these 
    #clusters
    tabPanel("Matrix scatterplot of invariant components",
             
             #This third tab panel allows to plot the distance of two invariants components against each other
             #to allows the user to construct self-defined clusters.
             #A second sub-tab allows to visualize several scatter plot at once to help the user choose the
             #best component to construct groups on. 
            
               
               #                               CONTENT 3.1 Observation selection                               #
               
               #This sub-tab panel allows to plot the distance of two invariants components against each other
               #for the user to construct self-defined clusters.
               
                        
                        sidebarLayout(
                          
                          #The side bar gives several options for the user to defines his clusters. 
                          sidebarPanel(width=3,
                              conditionalPanel(condition = "input.tabs1==1",       
                                       
                                       #       TOOL 3.1.1 Select the components to plot against each other     #
                                       
                                       #The numeric inputs are constructed in server because it needs to be 
                                       #bounded by the number of selected variable
                                       fluidRow(
                                         column(6,
                                                h4("Select a component for the x-axis"),
                                                uiOutput("componentXAxisUI")
                                         ),
                                         column(6,
                                                h4("Select a component for the y-axis"),
                                                uiOutput("componentYAxisUI"))),
                                       
                                       
                                       #             TOOL 3.1.2 Apply a cluster on the brushed points          #
                                       
                                       #The user first choose a cluster amoung a list, then click to apply it 
                                       #to the points brushed on the plot
                                       fluidRow(
                                         column(9,
                                                selectInput("cluster", label = h4("Choose a cluster"),
                                                            choices = list("Cluster 1 (Red)" = 1, 
                                                                           "Cluster 2 (Green)" = 2,
                                                                           "Cluster 3 (Blue)" = 3,
                                                                           "Cluster 4 (Cyan)" = 4,
                                                                           "Cluster 5 (Magenta)" = 5,
                                                                           "Cluster 6 (Yellow)" = 6,
                                                                           "Cluster 7 (Grey)" = 7,
                                                                           "Unclustered (Black)" = 0),
                                                            selected = cluster))),
                                       fluidRow(
                                         column(6,
                                                actionButton("colorCluster", "Apply to the selection"))),
                                       
                                       
                                       #        TOOL 3.1.3 Select the observations to which apply a label      #
                                       
                                       #The user can choose to put a label on no observations, all observations
                                       #or uppon brushing some points 
                                       fluidRow(
                                         column(6,
                                                selectInput("labelICvsIC", 
                                                            label = h4("Observations to label"),
                                                            choices = list("No observations" = 1, 
                                                                           "All observations" = 2,
                                                                           "Brushed observations" = 3),
                                                            selected = labelICvsIC))),
                                       
                                       #Appears only if needed (brushed observation is chosen)
                                       conditionalPanel(
                                         condition = "input.labelICvsIC == 3",
                                         fluidRow(
                                           column(6,
                                                  actionButton("applyLabelICvsIC", "Apply to the selection")))
                                       ),
                                       
                                       
                                       
                                       #                    TOOL 3.1.4 Close the application                   #
                                       
                                       #Althrough it appears identcally on all the page, it needs its own widget
                                       #to avoid conflict with observeEvent 
                                       fluidRow(
                                         column(12, align="center",
                                                br(),
                                                actionButton("quitTab3.1", "Close the session")))
                                       
                          ),
                          conditionalPanel(condition = "input.tabs1==2",
                                           #                TOOL 3.2.1 Choose the ICs to display                 #
                                           
                                           #Althrough it appears identcally on all the page, it needs its own widget
                                           #to avoid conflict with observeEvent
                                           #Their are constructed in the server, because their are bounded by the 
                                           #number of variable. 
                                           fluidRow(
                                             h4("Components to keep, starting from the highest kurtosis"),
                                             uiOutput("sliderFirstTab3.2UI")),
                                           fluidRow(
                                             h4("Components to keep, starting from the lowest kurtosis"),
                                             uiOutput("sliderLastTab3.2UI")),
                                           
                                           
                                           #                    TOOL 3.2.2 Close the application                   #
                                           
                                           #Althrough it appears identcally on all the page, it needs its own widget
                                           #to avoid conflict with observeEvent 
                                           fluidRow(
                                             column(12, align="center",
                                                    br(),
                                                    actionButton("quitTab3.2", "Close the session")))
                          )),         
                          
                        
    
                mainPanel(
                  tabsetPanel(id="tabs1",
                    #The first main panel will contain the IC against IC plot 
                      tabPanel("Observation selection", value=1,
                               fluidRow(
                                 column(12,
                                        plotOutput("plotICvsIC", 
                                                   brush = "brushICvsIC", width = "100%", height = "800px"))),
                               
                               fluidRow(
                                 column(12, align="center", 
                                        actionButton("downloadPlotICvsICPNG", "Download as png")
                                 ))), 
                    
                    #                        CONTENT 3.1 Scatter plots visualization                            #
                    
                    #This second sub-tab allows to visualize several scatter plot at once to help the user choose the
                    #best component to construct groups on.
                      tabPanel("Matrix scatterplot", value=2,      
                          #The second main panel display no more than 6 scatter plots at the time, the list of 
                          #scatter displayed is contructed throught listICS and can be chosen. 
                            fluidRow(
                              column(3,
                                     br(),
                                     h4("Choose the ICs to display")),
                              column(6,
                                     uiOutput("listChoiceICSUI"))),
                            
                            fluidRow(
                              column(12,
                                     plotOutput("matrixScatter", width = "100%", height = "800px")))
                          )
                        )
               )
             )
    ), #End of Tab3
    
    
    #############################################################################################################
    #                                                TAB PANEL 4                                                #
    #############################################################################################################
    
    #The fourth tab panel makes use of the package {ICSOutlier} to try to identify outlying values in the data. 
    tabPanel("Outlier identification",
             
             sidebarLayout(
               
               #The sidebar panel contains the choice of the relevant components, whihc will have an impact on 
               #the ICS distance graph.
               #The outliers can be detected through the simulation of a cutoff or through the definition of 
               #a reject rate 
               #It also allows to label some observations
               sidebarPanel(width=3,
                            
                            
                            #                        TOOL 4.1 Choice of the components                        #
                            
                            #These sliders are constructed in server because the length needs to be adjust as
                            #the number of kept variable changes. 
                            #The first slider selects a number of first components, the second slider selects a 
                            #number of last components. This is consistent with ICS theory. 
                            #This sliders are linked with the ones in Tab 2, as such modifying one slider will
                            #modify the twin slider.
                            fluidRow(
                              column(12,
                                     h4("Number of components to keep, starting from the highest kurtosis"),
                                     uiOutput("sliderFirstTab4UI"))),
                            fluidRow(
                              column(12,
                                     h4("Number of components to keep, starting from the lowest kurtosis"),
                                     uiOutput("sliderLastTab4UI"))),
                            
                            
                            #              TOOL 4.2 Select the observations to which apply a label           #
                            
                            #The user can choose to put a label on no observations, all observations, outliers
                            #or uppon brushing some points 
                            fluidRow(
                              h4("To which observations apply a label"),
                              column(6,
                                     selectInput("labelOutlier", 
                                                 label = "",
                                                 choices = list("No observations" = 1, 
                                                                "All observations" = 2,
                                                                "Outliers" = 3,
                                                                "Brushed observations" = 4),
                                                 selected = labelOutlier))),
                            
                            #Appears only if needed (brushed observation is chosen)
                            conditionalPanel(
                              condition = "input.labelOutlier == 4",
                              fluidRow(
                                column(6,
                                       actionButton("applyLabelOutlier", "Apply to the selection")))
                            ),
                            
                            
                            #           TOOL 4.3 Simulate a cut-off with dist.simu.test {ICSOutlier}          #
                            
                            #This command initiate a simulation to define a cut-off on the ICS distance plot
                            #With default parameters, this may take some time. 
                            #The user may specify the number of iterations and the level of the test. 
                            #This needs to be launch to avoid heavy unnecessary computations
                            fluidRow(
                              column(12, align="center",
                                     br(),
                                     h4("Cut-off from Monte Carlo simulations"))),
                            
                            fluidRow(
                              column(7, align="center",
                                     numericInput("nbIterationCutOff", 
                                                  label= "Number of simulations", 
                                                  value = nbIterationCutOff,
                                                  step = 100,
                                                  min=0,
                                                  max=100000)),
                              column(5, align="center",
                                     numericInput("levelCutOff", 
                                                  label= "Level of the test", 
                                                  value = levelCutOff,
                                                  step = 0.01,
                                                  min= 0.001,
                                                  max=1))),
                            
                            fluidRow(
                              column(12, align="center",
                                     actionButton("cutOff", "Simulate a Cut-off"))),
                            
                            
                            #               TOOL 4.4 Simulate a cut-off with a rejection rate              #
                            
                            #This command put a cut-off on the plot such that a given percentage of the 
                            #observations are taken as outlying.
                            #The user may specify the rejection rate.
                            #This needs to be launch to avoid unnecessary computations
                            fluidRow(
                              column(12, align="center",
                                     br(),
                                     h4("Tag a given percentage or number of observations"))),
                            
                            fluidRow(
                              column(6,
                                     numericInput("rejectionRate", 
                                                  label= "", 
                                                  value = rejectionRate,
                                                  step = 1,
                                                  min= 0,
                                                  max=100)
                              ),
                              column(6,
                                     numericInput("rejectionNumber", 
                                                  label= "", 
                                                  value = rejectionNumber,
                                                  step = 1,
                                                  min= 0,
                                                  max=n))),
                            
                            fluidRow(
                              column(6, align="center",
                                     actionButton("rejectOutlierRate", "Tag a given %")),
                            
                              column(6, align="center",
                                     actionButton("rejectOutlierNumber", "Tag a given number"))),
                            
                            
                            #                           TOOL 4.5 Close the application                        #
                            
                            #Althrough it appears identcally on all the page, it needs its own widget
                            #to avoid conflict with observeEvent 
                            fluidRow(
                              column(12, align="center",
                                     br(),
                                     actionButton("quitTab4", "Close the session")))
               ), 
               
               #This main panel display the ICS distance plot which allows to detect outliers 
               mainPanel(
                 
                 fluidRow(
                   column(12,
                          h3("Outlier identification"),
                          plotOutput("plotOutlier", 
                                     brush = "brushOutlier", width = "100%", height = "800px"))),
                 
                 fluidRow(
                   column(12, align="center", 
                          actionButton("downloadPlotOutlierPNG", "Download as png")
                   ))

             ))
    ), #End of Tab4
    
    
    #############################################################################################################
    #                                                TAB PANEL 5                                                #
    #############################################################################################################
    
    #This fifth tab panel gives some descriptive satatistics on the variables of the data. 
    #It also allows to compare the dataset, clusters and outliers between outliers to get some idea on the 
    #variable which are responsible for this classification 
    tabPanel("Descriptive statistics",
             
             sidebarLayout(
               
               #The sidebar here contains only the quit button.
               sidebarPanel(width=3,
                            
                            
                            #                           TOOL 5.1 Close the application                        #
                            
                            #Althrough it appears identcally on all the page, it needs its own widget
                            #to avoid conflict with observeEvent 
                            fluidRow(
                              column(12, align="center",
                                     br(),
                                     actionButton("quitTab5", "Close the session")))
               ),
               
               
               #The main panel may either describe one variable, either compare two variables. 
               mainPanel(
                 
                 
                 #                          TOOL 5.2 Choice of the data to describe                           #
                 
                 #By default, it describes only one data, this data can be the whole dataset, or a cluster or 
                 #even outlying values. 
                 #The user may also define a second data, and, if it is different from the first and that the 
                 #user ticks the compare checkbox, then it will display the same descriptive statistics for both 
                 #datas.
                 fluidRow(
                   column(6,
                          h4("Select the data to display"),
                          uiOutput("dataToDescribeUI")),
                   column(6,
                          h4("Compare with:"),
                          column(5,
                                 uiOutput("comparisonWithUI")),
                          column(5, 
                                 br(),
                                 checkboxInput("compare", label = "Enable comparison", value = FALSE)))), 
                 
                 
                 #                            Display 5.1 the data(s) to describe                             #
                 
                 #This selection of variable is created in server because it depends on the variable selected in
                 #the first step. It can only be numeric variables. 
                 fluidRow(
                   column(12,
                          h4("Select a variable"),
                          uiOutput("varDescribeSelectUI")
                   )),
                 
                 #The object boxplot is constructed differently if compare is ticked or not.
                 fluidRow(
                   column(6,
                          plotOutput("boxplot")
                   ),
                   #Density plot
                   column(6,
                          plotOutput("densityPlot"))),
                 
                 #A second histogram will appear if compare is ticked.
                 fluidRow(
                   column(6,
                          plotOutput("histogram")
                   ),
                   column(6,
                          plotOutput("histogramCompare"))),
                 
                 #It always describes two data, however, when the second data doesn't exists it does't appears
                 #without generating errors
                 #this data can be the whole dataset, or a cluster or even
                 #outlying values. 
                 #Are given:
                 #- summary statistics (min, Q1, mean, median, Q3, Q4)
                 #- a boxplot
                 #- an histogram
                 fluidRow(
                   column(12,
                          h4("Descriptive statistics"),
                          verbatimTextOutput("summaryStat"),
                          verbatimTextOutput("compareStat")
                   ))
               )
             )
    ), #End of Tab5
    
    
    #############################################################################################################
    #                                                TAB PANEL 6                                                #
    #############################################################################################################
    
    #This sixth tab panel shows the datatable of the chosen data, either the whole dataset, either a cluster or
    #the outliers. 
    tabPanel("Data Table",
             
             sidebarLayout(
               
               #The sidebar here contains only the quit button.
               sidebarPanel(width=3,
                            
                            #                           TOOL 6.1 Close the application                        #
                            
                            #Althrough it appears identcally on all the page, it needs its own widget
                            #to avoid conflict with observeEvent 
                            fluidRow(
                              column(12, align="center",
                                     br(),
                                     actionButton("quitTab6", "Close the session")))
               ),
               
               #The main panel permits to choose the wished data and to display it. 
               mainPanel(
                 fluidRow(
                   column(6,
                          h4("Select the data to display"),
                          uiOutput("datasetUI"))),
                 
                 DT::dataTableOutput('dataTab')))
             
             
    ), #End of Tab6
    
    
    #############################################################################################################
    #                                                TAB PANEL 7                                                #
    #############################################################################################################
    
    #This sixth tab panel shows the datatable of the chosen data, either the whole dataset, either a cluster or
    #the outliers. 
    tabPanel("Save",
             
             sidebarLayout(
               
               #The sidebar here contains only the quit button.
               sidebarPanel(width=3,
                            
                            
                            #                           TOOL 7.1 Save the data set                            #
                              fluidRow(
                                column(12, align="center",
                                       br(),
                                       actionButton("saveData", "Save the data"))),
                            
                            
                            #                      TOOL 7.2 Save the summary of operations                      #
                            
                              fluidRow(
                                column(12, align="center",
                                       br(),
                                       actionButton("saveSummary", "Save the summary of operations"))),
                            
                            
                            #                           TOOL 7.3 Close the application                        #
                            
                            #Althrough it appears identcally on all the page, it needs its own widget
                            #to avoid conflict with observeEvent 
                            fluidRow(
                              column(12, align="center",
                                     br(),
                                     actionButton("quitTab7", "Close the session")))
               ),
               
               #The main panel permits to choose the wished data and to display it. 
               mainPanel(
                 tabsetPanel(
                   tabPanel("Data table of the components",
                            br(), br(),
                            DT::dataTableOutput('dataComponent')
                   ), 
                   tabPanel("Summary of operations",
                            br(), br(),
                            verbatimTextOutput('summaryOperations')
                   )
                 )

                 ))
             
             
    ) #End of Tab7
    
    
    
    #############################################################################################################
    
  )#End of tabPanel
)#End of fluidPage