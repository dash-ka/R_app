library(shiny)
library(ggplot2)
library(mlr)
library(dplyr)
library(bslib)
library(skimr)
library(shinythemes)
thematic::thematic_shiny() 
library(parallel)
library(parallelMap)
library(dbplyr)
library(tidyr)
data<- read.csv("C://Users/dashb/Downloads/train.csv")
titanic <- tibble(data) %>%                
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>% 
  filter(Embarked!= "" ) %>% 
  drop_na() %>% 
  mutate_at( c(1,2,3,5,6,8), .funs=list(factor)) %>% 
  rename(Gender=Sex, Passenger_Class=Pclass, Relatives_aboard=Parch) %>% 
  mutate_at("Age", .funs=list(as.integer))


ui <- navbarPage("Titanic: ",
                 
         tabPanel("Machine Learning from Disaster",
                  tags$head(
                    tags$script(src = "https://platform.twitter.com/widgets.js", charset = "utf-8"),
                    tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Mono", rel = "stylesheet"),
                    tags$style(HTML('
      * {
        font-family: Roboto Mono;
        font-size: 100%;
      }
      #sidebar {
         background-color: #fff;
         border: 0px;
      }
      .rt-th {
        display: none;
      }
      .rt-noData {
        display: none;
      }
      .rt-pagination-nav {
        float: left;
        width: 100%;
      }
    '))),
         fluidRow( 
           column(titlePanel("Titanic: Machine Learning from Disaster"),
                  
                  br(),
                  
                  p("The sinking of the Titanic is one of the most infamous shipwrecks in history.
                  While there was some element of luck involved in surviving, it seems some groups of people were more likely to survive than others.
                  Through this application, it is intended to develop a learning environment starting from the passenger data to build a predictive model that answers the question: 'what sorts of people were more likely to survive?'.
                  ",style="color:black;background-color:#80ced6;padding:15px;border-radius:10px"
                    ),
                  
                  br(),
                  
                  p("The dataset used in this application contains information on the survival status, sex, age, passenger class and other four variables of 712 passengers in the Titanic disaster of 1912..",
                    style="color:black;background-color:#d5f4e6;padding:15px;border-radius:10px"
                    ),
                  
                  width=5
                  ),
           
          column(6, br(),br(), br(), tags$img(src="titanic.jpg", width="650px",height="400px"),br()
                )
           ),
                  
           hr(),
         
           tags$style(".fa-database {color:#E87722}"),
           h3(p("Dataset ",icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
         
           fluidRow(column(width =1), 
                    column(dataTableOutput("tit"),
                    width = 10)
                    ),
                  
           hr(),
           p(em("Developed by"),br("Shlyk Darya"),style="text-align:center; font-family: times")
         ),
         
         
                 
         tabPanel("Data",
                  
                 fluidPage(
                   
                   tabsetPanel(tabPanel("Explore",
                                        column(4, 
                                     titlePanel("Data summary"),
                                     br(), 
                                     p("The very first basic exploration is to see the data yourself. Examine the structure of your dataset: variable names and variable type. 
                                       In this 'getting to know your data' section your can also visualize the data. Take a look and see what might be a thing that makes the passenger survive from Titanic incident. ",br(),
                                     br(), br(),
                                      style="color:black;background-color:#d5f4e6;padding:15px;border-radius:10px"),br(),br(),
                                     HTML(paste0(
                                       "<p>P.S. The dataset was cleaned up to remove the missing values.</p>")), br(),
                                     radioButtons("radio", "Choose a display mode ...", choices = list("structure"=1, "summary"=2), selected=1)
                                     ),
                                     column(7,
                                            br(),br(),br(),br(),
                                            verbatimTextOutput("summary_data"),
                                            br(),
                                            br(),
                                            HTML(paste0(
                                              "<p>The variables present in the dataset are listed below:</p>",
                                              "<p></p>",
                                              "<ul><li>Survived: Survival ( 0 = No;1 = Yes)</li>
                                              <li>Passenger_Class: Ticket Class (1 = 1st, 2= 2nd; 3= 3rd) </li>
                                              <li>Gender: Gender (Male or Female)</li>
                                              <li>Age: Passenger Age</li>
                                              <li>SibSp: Nos of sibling and/or spouses aboard</li>
                                              <li>Relatives_aboard: Number of parents and/or children aboard</li>
                                              <li>Fare: Fare price (British Pound)</li>
                                              <li>Embarked: Port of embarkation (C = Cherbourg: Q = Queenstown: S= Southampton)</li></ul>"
                                              
                                            )))), 
              
                               
                               tabPanel("Visualize",
                                        column(titlePanel("Data Visualization for Titanic Data"),
                                        br(),
                                        p("In this section you can play with graphical tools and plot the distribution of different features of passengers of the ship. ",
                                          style="color:black;background-color:#d5f4e6;padding:15px;border-radius:10px"
                                        ),br(),
                                        HTML(paste0(
                                          "<p>Use the options below to explore the data and find out the explanatory power of different features on the probability of surviaval of the passenger.</p>","<p> Happy plotting! \U0001F57A</p>"
                                        )),
                                        br(),br(),
                                        selectInput('x', 'Select a variable to display', names(select(titanic,c(1,2,3,5,6,8))), selected = "Passenger_Class"),
                                        br(),
                                        selectInput('fill', 'Select a grouping variable', names(select(titanic, c(1,2,3,5,6,8))), selected = "Survived"),
                                        br(),
                                        width=5),
                                        
                                        column(7,br(), br(), br(),plotOutput("Barplot"))
                                        )
                               )
                   )
                 ),
         
                 tabPanel("Building a tree", 
                          br(),
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3("Defining the hyperparameter space for tuning",style="color:black"),
                              br(),
                              
       
                              sliderInput("minsplit",p("Select a range of values for the min n. of cases to split a node"),
                                          value=c(5, 20),
                                          min=1,
                                          max=30,
                                          step=1),
                              br(),
                              sliderInput("minbucket",p("Select a range of values for tmin n. of cases in a leaf"),
                                          value=c(3, 10),
                                          min=1,
                                          max=30,
                                          step=1),
                              br(),
                              sliderInput("cp",p("Select a range of values for complexity parameter"),
                                          value=c(0.01, 0.1),
                                          min=0,
                                          max=0.5,
                                          step=0.01),
                              br(),
                              sliderInput("depth",p("Select a range of values for max depth of the tree"),
                                          value=c(3, 10),
                                          min=1,
                                          max=20,
                                          step=1),
                              br(), 
                              h3("Defining the random search", style="color:black"),
                              br(),
                              sliderInput("maxiter",p("choose the number of iterations of random pick"),
                                          value=200,
                                          min=10,
                                          max=300,
                                          step=10),
                              br(), 
                              sliderInput("folds",p("choose the number of folds fot the ordinary CV"),
                                          value=5,
                                          min=1,
                                          max=21,
                                          step=2)
                              ),
                            
                            mainPanel(fluidRow(plotOutput("tree")))
                            ),
                          ),
         
         
                 tabPanel("Step 3"))
                 
      
server<- function(input, output) {
  var_x <- reactive({
    attach(titanic)
    switch (input$x,
            "Survived" = Survived,
            "Passenger_Class" = Passenger_Class,
            "Gender" = Gender, 
            "Age" = Age,
            "SibSp" = SibSp,
            "Relatives_aboard" = Relatives_aboard,
            "Fare" = Fare,
            "Embarked" = Embarked)
  })
  
  
  var_fill <- reactive({
    attach(titanic)
    switch (input$fill,
            "Survived" = Survived,
            "Passenger_Class" = Passenger_Class,
            "Gender" = Gender,
            "SibSp" = SibSp,
            "Relatives_aboard" = Relatives_aboard,
            "Embarked" = Embarked)
  })
  
  
  output$plot1 <- renderPlot({
    
    ggplot(NULL,aes(as.double(var_x())))+geom_histogram(bins=nclass.Sturges(as.double(var_x())),
                                                        fill="seagreen1",aes(y=..count..),lwd=0.8)+
      labs(title = paste(input$x, " histogram"),x=input$x,y="Frequency")+
      theme(plot.title = element_text(color="#80ced6", size=15, face="bold",hjust=0.5),
            axis.title.x = element_text(color="#80ced6", size=13, face="bold"),
            axis.title.y = element_text(color="#80ced6", size=13, face="bold"))
  })
  
  
  output$Barplot <- renderPlot({  
    ggplot(NULL,aes(x=var_x(),fill=var_fill()))+
      geom_bar(position="dodge")+
      labs(title = paste(input$x, " Barplot"), x=input$x, fill=input$fill)
  })    
 
  

  output$tree <- renderPlot({ task <-makeClassifTask(data=titanic, target = "Survived")
  tree <-makeLearner("classif.rpart")
  
  #Defining the hyperparameter space for tuning
  treeParamSpace <- makeParamSet(
    makeIntegerParam("minsplit", lower = input$minsplit[[1]], upper = input$minsplit[[2]]), #min n. of cases needed to split a node
    makeIntegerParam("minbucket", lower = 1, upper = 30), #min n. of cases in a leaf
    makeNumericParam("cp", lower = 0.01, upper = 0.15), #complexity parameter
    makeIntegerParam("maxdepth", lower = 3, upper = 15)) #max depth of the tree
  randSearch <- makeTuneControlRandom(maxit = input$maxiter) #user :choose the number of iterations of random
  cvForTuning <- makeResampleDesc("CV", iters = input$folds)
  
  cores = detectCores()
  parallelStartSocket(cores-1)
  tunedTreePars <- tuneParams(tree, task = task,
                              resampling = cvForTuning,
                              par.set = treeParamSpace,
                              control = randSearch)
  parallelStop()
  perf <-tunedTreePars
  tunedTree <- setHyperPars(tree, par.vals = tunedTreePars$x)
  tunedTreeModel <- train(tunedTree, task)
  treeModelData <- getLearnerModel(tunedTreeModel)
  visTree(treeModelData, fallenLeaves = TRUE, height = "600px", edgesFontSize = 15, 
          colorVar = c("#deeaee","#eea29a","#80ced6", "#ffef96","#f7786b", "#618685", "#b2b2b2"), 
          colorY = c( "#b8a9c9", "orange"), colorEdges=c("#86af49"), shapeY = "box", nodesFontSize = 30, 
          legend = F) %>%
    visNodes(font=list("size"=40,"face"="Garamond", "color"="#36486b", "align"="center") ) %>% 
    visEdges(font =list("face"="Garamond"))
  })
  
  output$tit<- renderDataTable({titanic}, options = list(pageLength = 10))
  
  output$summary_data <- renderPrint({   
    if (input$radio == 1) {str(titanic)}
    if (input$radio == 2) {skim(titanic)}
    })
  
}

shinyApp(ui=ui, server = server)