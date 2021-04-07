#install.packages("mlr")
library(shiny)
library(ggplot2)
library(mlr)
library(dplyr)
irisKNN <- data_frame(iris)
ui<- fluidPage(

  titlePanel("KNN"),
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
    '))
  ),
  
  sidebarLayout(
    
    sidebarPanel("Build your first kNN model",
                 
      helpText("You would like to use the kNN algorithm to train a model that can
      predict which of these classes a new flower will belong to, so that classification can be
      improved. This is a three-class classification problem."),
      br(),
      
      numericInput("par", 
                   h3("Set k parameter for K-NN classifier"), 
                   value = 3), br(),
      
      tableOutput("k_param")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                h3("Summary statistics Iris Dataset"),
                br(),
                dataTableOutput("summary_iris")
                ), 
        tabPanel("Descriptive_stats", 
                 fluidRow(
                   
                   column(6, h5("Scatterplot Sepal Length vs Sepal Width"),
                          plotOutput("plot_Sepal")),
                   
                   column(6, h5("Scatterplot Petal Length vs Petal Width"),
                          plotOutput("plot_Petal")))),
        tabPanel("CrossValidation")
      
      )
    )
  )
)
server<- function(input, output) {
  
  output$summary_iris<- renderDataTable({summary(irisKNN)})
  
  output$plot_Sepal <-renderPlot({
    ggplot(irisKNN, aes(Sepal.Length, Sepal.Width, col = Species)) +
      geom_point() +
      theme_bw()
    })
   
 output$plot_Petal <-renderPlot({ggplot(irisKNN, aes(Petal.Length, Petal.Width, col = Species)) +
     geom_point() +
     theme_bw()
   })
  
 k_model <- reactive({
   irisTask <- makeClassifTask(data = irisKNN, target = "Species")
   knn <- makeLearner("classif.knn",
                    par.vals = list("k" = input$par))
   knnModel <- train(knn, irisTask)
   knnPred <- predict(knnModel, newdata = irisKNN)
   round(performance(knnPred, measures = list(mmce)), 2)
   })
 
  output$k_param <- renderText({
    print(paste("The misclassification error for the selected model is equal to : ", k_model()))
  })
}

shinyApp(ui=ui, server = server)


