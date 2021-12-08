library(shiny)
library(readr)
library(ggplot2)
library(gridExtra)
library(e1071)
library(RSQLite)
library(DBI)
library(jsonlite)
library(GGally)

iris.true <- iris[,5]
iris.train <- iris[,-5] # no cheating

# Naive Bayes
nb.classifier <- naiveBayes(iris.train, iris.true)
nb.clusters <- factor(as.integer(predict(nb.classifier,iris.train)))

# SVM
svm.classifier <- svm(iris.train, iris.true)
svm.clusters <- factor(as.integer(predict(svm.classifier,iris.train)))

iris.result <- cbind(iris.train, nb.clusters, svm.clusters)

#Modules
cluster <- function(data1) {}


# Ui
ui <- fluidPage(
  sidebarPanel(
    fileInput("file1", "Choose CSV File", accept = ".csv"),
    checkboxInput("header", "Header", TRUE),
    radioButtons("radio", label = "Chose a method",
                 choices = c("SVM" = 1, "Regression" = 2),selected = 1),
    selectInput("svm.x", label = "X Variable", choices=colnames(iris.train)),
    selectInput("svm.y", label = "Y Variable", choices=colnames(iris.train))
  ),
  mainPanel(
    textOutput("contents"),
    plotOutput("plot1")
  )
)


# Server
server <- function(input, output) {
  output$contents <- renderPrint({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "txt", "Please upload a csv file"))
    data <- read.csv(file$datapath, header = input$header)
    head(data)
    con <- dbConnect(RSQLite:: SQLite(), "/db")
    dbWriteTable(con, "Data_iris", data)
    dbDisconnect(con)
    paste("File uploaded successfully")
    
  })
  output$plot1 <- renderPlot(
    if(input$file1 == FALSE){paste("Choose file")}
    else{
    if(input$radio == 1) {
      con <- dbConnect(RSQLite:: SQLite(), "/db")
      data <- dbReadTable(con, "Data_iris")
      data$variety <- factor(data$variety)
      model <- svm (variety ~., data = data)
      summary (model)
      ggplot(iris.result,aes_string(x=input$svm.x, y=input$svm.y,color='svm.clusters')) + geom_point()
    }
    else {
      con <- dbConnect(RSQLite:: SQLite(), "/db")
      table <- dbReadTable(con, "Data_iris")
      table$variety <- factor(table$variety)
      levels(table$variety)
      sum(is.na(table))
      table<-table[1:100,]
      set.seed(100)
      samp<-sample(1:100,80)
      ir_test<-table[samp,]
      ir_ctrl<-table[-samp,]
      ggpairs(ir_test)
      #y<-ir_test$variety
      #x<-ir_test$sepal.length
      #glfit<-glm(y~x, family = 'binomial')
      #summary(glfit)
      #newdata<- data.frame(x=ir_ctrl$Sepal.length)
      #predicted_val<-predict(glfit, newdata, type="response")
      #prediction<-data.frame(ir_ctrl$sepal.length, ir_ctrl$variety,predicted_val)
      #prediction
      
      qplot(prediction[,1], round(prediction[,3]), col=prediction[,2], xlab = 'Sepal Length', ylab = 'Prediction using Logistic Reg.')
      
    }
    }
      )
  
}



# Run app
shinyApp(ui = ui, server = server)