#install.packages("shinyWidgets")
#install.packages("mlr")
#install.packages("parallelMap")
#install.packages("shinythemes")
#install.packages("bslib")
#install.packages("visNetwork")
#install.packages("parallel")
#install.packages("skimr")
#install.packages("thematic")
#install.packages("sparkline")
library(sparkline)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(ggplot2)
library(mlr)
library(bslib)
library(skimr)
thematic::thematic_shiny() 
library(parallel)
library(parallelMap)
library(visNetwork)
library(dplyr)
library(tidyr)

data<- read.csv("www/train.csv")
titanic <- tibble(data) %>%                
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>% 
  filter(Embarked!= "" ) %>% 
  drop_na() %>% 
  mutate_at( c(1,2,3,5,6,8), .funs=list(factor)) %>% 
  rename(Gender=Sex, Passenger_Class=Pclass, Relatives_aboard=Parch) %>% 
  mutate_at("Age", .funs=list(as.integer)) %>% 
  mutate("Survived" = factor(Survived,labels=c("No","Yes")))


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
           column(width=1),
           column(titlePanel("Titanic: Machine Learning from Disaster"),
                  br(),
                  p("The sinking of the Titanic is one of the most infamous shipwrecks in history.
                  While there was some element of luck involved in surviving, it seems some groups of people were more likely to survive than others.
                  Through this application, it is intended to develop a learning environment to build a predictive model that answers the question: 'what sorts of people were more likely to survive?'.
                  ",style="color:black;background-color:#E9967A;padding:15px;border-radius:10px"
                    ),
                  br(),
                  p("The dataset used in this application contains information on the survival status, sex, age, passenger class and other four variables of 712 passengers in the Titanic disaster of 1912.",
                    style="color:black;background-color:#d5f4e6;padding:15px;border-radius:10px"),
                  width=4),
           
          column(6,
                 br(),
                 br(),
                 br(),
                 tags$img(src="titanic.jpg", width="650px",height="400px"),
                 br())),
                  
        hr(),
         
        tags$style(".fa-database {color:#E87722}"),
        h3(p("Dataset ",icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
         
        fluidRow(column(width =1), 
                    column(dataTableOutput("tit"),
                    width = 10)),
                  
        hr(),
        p(em("Developed by"),br("Shlyk Darya"),style="text-align:center; font-family: times")),
         
        tabPanel("Data",
                 fluidPage(
                   tabsetPanel(
                     tabPanel("Explore",
                              
                              tags$style(HTML(".tabbable > .nav > li[class=active]> a {background-color: #E9967A; color:white}
                                        
                                        ")),
                               
                              column(4, 
                                     titlePanel("Data summary"),
                                     br(), 
                                     p("The very first basic exploration is to see the data yourself. Examine the structure of your dataset: variable names and variable type. 
                                       In this 'getting to know your data' section your can also visualize the data. Take a look and see what might be a thing that makes the passenger survive from Titanic incident. ",
                                       style="color:black;background-color:#80ced6;padding:15px;border-radius:10px"),
                                     br(),
                                     HTML(paste0(
                                       "<p>P.S. The dataset was cleaned up to remove the missing values.</p>")), 
                                     br(),
                                     br(),
                                     br(),
                                     radioButtons("radio", "Choose a display mode ...", choices = list("structure"=1, "summary"=2), selected=1)),
                              
                              column(7, 
                                    tags$head(tags$style("#summary_data{background-color:#E0FFFF}")),
                                    fluidRow( 
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             verbatimTextOutput("summary_data")
                                            ),
                                    fluidRow(
                                            br(),
                                            br(),
                                            HTML(paste0(
                                              "<p>The variables present in the dataset are listed below:</p>",
                                              "<p></p>",
                                              "<ul><li>Survived: Survival status ( 0 = No;1 = Yes)</li>
                                              <li>Passenger_Class: Ticket Class (1 = 1st, 2= 2nd; 3= 3rd) </li>
                                              <li>Gender: Gender of the passenger (Male or Female)</li>
                                              <li>Age: Passenger Age</li>
                                              <li>SibSp: Number of sibling and/or spouses aboard</li>
                                              <li>Relatives_aboard: Number of parents and/or children aboard</li>
                                              <li>Fare: Fare price (British Pound)</li>
                                              <li>Embarked: Port of embarkation (C = Cherbourg: Q = Queenstown: S= Southampton)</li></ul>"
                                              
                                            ))))), 
              
                               
                               tabPanel("Visualize",
                                        column(titlePanel("Data Visualization for Titanic Data"),
                                        br(),
                                        p("In this section you can play with graphical tools and plot the distribution of different features of the passengers on the ship. ",
                                          style="color:black;background-color:#80ced6;padding:15px;border-radius:10px"
                                        ),
                                        br(),
                                        HTML(paste0(
                                          "<p>Use the options below to explore the data and study the relationship between passengers' characteristics.</p>","<p> Happy plotting! \U0001F57A</p>"
                                        )),
                                        br(),
                                        br(),
                                        selectInput('x', 'Select a variable to display', names(select(titanic,c(1,2,3,5,6,8))), selected = "Passenger_Class"),
                                        br(),
                                        selectInput('fill', 'Select a grouping variable', names(select(titanic, c(1,2,3,5,6,8))), selected = "Survived"),
                                        br(),
                                        width=5),
                                        
                                        column(
                                          br(),
                                          br(),
                                          br(),
                                          plotOutput("Barplot"),
                                          width = 7))))),
         
                 tabPanel("Model", 
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              h2("Build your decision tree!"),
                              br(),
                              p("Now let's build a decision tree in order to predict what kind of the passengers were more likely to survive the disaster.
                              Tune the algorithm's hyperparameters and train your model using the
                              cross-validated hyperparameter combination."),
                              br(),
                              br(),
                              h4("STEP 1: Defining the hyperparameter space for tuning."),
                              br(),
                              h4("STEP 2: Defining the cross-validation strategy for tuning."),
                              br(), 
                              h4("STEP 3: Tune & Train!")),
                             
                            mainPanel(
                              fluidRow(
                                setSliderColor(rep("#80ced6",6), 1:6),
                                column(4, h3("STEP 1",style="color:black;text-align:center"),
                                      br(),
                                       helpText("The minimum number of observations that must exist in a node in order for a split to be attempted."),
                                       sliderInput("minsplit","",
                                                                  value=c(5, 20),
                                                                  min=1,
                                                                  max=30,
                                                                  step=1),
                                       br(),
                                                      
                                       helpText("The minimum number of observations in any terminal leaf."),
                                       sliderInput("minbucket","",
                                                                  value=c(3, 10),
                                                                  min=1,
                                                                  max=30,
                                                                  step=1),
                                       br(),
                                       helpText("Complexity parameter."),
                                       sliderInput("cp","",
                                                                  value=c(0.01, 0.1),
                                                                  min=0,
                                                                  max=0.5,
                                                                  step=0.01),
                                       br(),
                                       helpText("Set the maximum depth of any node of the final tree."),
                                       sliderInput("depth","",value=c(3, 10),
                                                                  min=1,
                                                                  max=20,
                                                                  step=1),
                                       br()),
                                
                                 column(4,h3("STEP 2", style="color:black;text-align:center"),
                                        br(),
                                        helpText("Select the number of iterations for the random selection of a hyperparameter combination."),
                                        sliderInput("maxiter","",
                                                                  value=200,
                                                                  min=10,
                                                                  max=300,
                                                                  step=10),
                                        br(), 
                                        helpText("Choose the number of folds for the ordinary CV."),
                                        sliderInput("folds","",
                                                                  value=5,
                                                                  min=1,
                                                                  max=20,
                                                                  step=1)),
                                               
                                 column(4,
                                        fluidRow(
                                                 h3("STEP 3", style="color:black;text-align:center"),br(),
                                                 p("Start the tuning process using the parallelization to speed things up. Once tuned you can use these hyperparameters to train your final model!"),
                                                 tags$head(
                                                 tags$style(HTML(".btn {
                                                                               display:block;
                                                                               height: 70px;
                                                                               width: 140px;
                                                                               border-radius: 10%;
                                                                               border: 1px solid green;
                                                                               align-item: center

                                                                               }
                                                                 "))),
                                                 br(),
                                                
                                                 column(1),
                                                 column(10,actionButton("click", "Train!",style="color:black; background-color: #80ced6;;border-color: #80ced6")) ),
                                                 br(),
                                                 br(),
                                        
                                                 fluidRow(column(11,textOutput("params"),
                                                                 br(),
                                                                 tags$head(tags$style("#result{background-color: #E0FFFF}")),
                                                                 verbatimTextOutput("result")), 
                                                          column(1))))))),
                          
         
         
                 tabPanel(icon("leaf",lib = "font-awesome"),
                          fluidRow(br(),
                                   br(),
                                   br(),
                                   column(width=1),
                                   column(h2("Plotting the decision tree", style="color:black;text-align:center"),
                                          br(),
                                          br(),
                                          br(),
                                          p("Now we are ready to make a prediction! ", style="color:black;text-align:center"),
                                          p("Play with the plot of your decision tree. Just point at any node with your mouse to get more insight.", style="color:black;text-align:center",icon("mouse-pointer",lib = "font-awesome")),
                                          br(),
                                          p(" The splitting criterion is shown for each node. Each leaf shows the predicted class, the proportion of survived passengers in that leaf, 
                                             and the proportion of all passengers classified by that leaf. ", style="color:black;text-align:center"),
                                          br(),
                                          width = 4),
                                   
                                   column(textOutput("lack"),
                                          br(),
                                          visNetworkOutput("tree"), width=6)
                                  )))


                 
      
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
  
  
  
  output$Barplot <- renderPlot({  
    ggplot(NULL,aes(x=var_x(),fill=var_fill()))+
      geom_bar(position="dodge")+
      labs( x=input$x, y= "Frequency",fill=input$fill)+
      theme(axis.title.x = element_text(color="#80ced6", size=13, face="bold"),
            axis.title.y = element_text(color="#80ced6", size=13, face="bold"),
            legend.title = element_text(color="#80ced6", size=13, face="bold"))
  })    
 
  task <-makeClassifTask(data=titanic, target = "Survived")
  tree <-makeLearner("classif.rpart")
  
 
  #Defining the hyperparameter space for tuning
  param<- reactive({
  treeParamSpace <- makeParamSet(
    makeIntegerParam("minsplit", lower = input$minsplit[[1]], upper = input$minsplit[[2]]), #min n. of cases needed to split a node
    makeIntegerParam("minbucket", lower = 1, upper = 30), #min n. of cases in a leaf
    makeNumericParam("cp", lower = 0.01, upper = 0.15), #complexity parameter
    makeIntegerParam("maxdepth", lower = 3, upper = 15))
  })
  
  randSearch <- reactive({makeTuneControlRandom(maxit = input$maxiter)}) #user :choose the number of iterations of random
  cvForTuning <- reactive({makeResampleDesc("CV", iters = input$folds)})#max depth of the tree
  
  tuned_params<-eventReactive(input$click, {tuning()})
  
  model <-eventReactive(input$click, {training()})
  
  tuning<- reactive({cores = detectCores()
           parallelStartSocket(cores-1)
           tunedTreePars <- tuneParams(tree, task = task,
                              resampling = cvForTuning(),
                              par.set = param(),
                              control = randSearch())
           parallelStop()
           tunedTreePars$x
  })
  
   training<-reactive({ 
           tunedTree <- setHyperPars(tree, par.vals = tuning() )
           tunedTreeModel <- train(tunedTree, task)
           getLearnerModel(tunedTreeModel)})
           
  output$result <-renderPrint({tuned_params()})
  
  rv1 <-reactiveValues(text = "Click the button and wait a second to see the tuning results...")
  observeEvent(input$click,{rv1$text <-"The tuned hyperparameters are reported below:"} )
  output$params <-renderText({rv1$text})
  
  
  rv2 <-reactiveValues(text = " ***  You should tune hyperparameters and train your model first in order to visualize the decision tree! ***")
  observeEvent(input$click,{rv2$text <-""} )
  output$lack <- renderText({rv2$text})
  
  output$tree <- renderVisNetwork({
    visTree(model(), fallenLeaves = TRUE, height = "600px", edgesFontSize = 15, 
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
