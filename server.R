#
# Final Project
#
# Vivi Feathers
# 12/01/2023

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(caret)
library(ggplot2)
library(shinyjs)

# read in RA data
data_whole <- read_csv("ra_data.csv")
data_whole$HighBloodPressure <- as.factor(data_whole$HighBloodPressure) 
data_whole$Smoke <- as.factor(data_whole$Smoke)
data_whole$Bronchitis <- as.factor(data_whole$Bronchitis) 
data_whole$Melanoma <- as.factor(data_whole$Melanoma)
data_whole$Remission <- as.factor(data_whole$Remission) 
data_whole$Narcotic <- as.factor(data_whole$Narcotic)
data_whole$PainMed <- as.factor(data_whole$PainMed) 
data_whole$DmardMed <- as.factor(data_whole$DmardMed)
data_whole$Age <- as.numeric(data_whole$Age)
data_whole$PainScale <- as.numeric(data_whole$PainScale)
data_whole$PatientGlobal <- as.numeric(data_whole$PatientGlobal)
data_whole$FatigueScale <- as.numeric(data_whole$FatigueScale)
data_whole$MDGlobal <- as.numeric(data_whole$MDGlobal)
data_whole$MdhaqScore <- as.numeric(data_whole$MdhaqScore)
data_whole$BMI <- as.numeric(data_whole$BMI)

server <- function(input, output, session) {
  
######################## EDA ###########################
  
  # change the filter slide value based on different numeric variable
  observe({updateSliderInput(session, inputId = "filter", 
                             min = min(select(data_whole,input$select1)),
                             max = max(select(data_whole,input$select1)),
                             value =c(min(select(data_whole,input$select1)), max(select(data_whole,input$select1)) ))})
  
  #get new data based on the selected numeric variable and filter rows
  getData <- reactive({
    newData <- data_whole %>% 
               select(!!sym(input$select1), Remission) %>%
               filter(!!sym(input$select1) >= input$filter[1] & !!sym(input$select1) <= input$filter[2])
  })
  
  #summary table
  output$table <- renderTable({
    #get new data 
    newData <- getData()
    
    if(input$RB1=="Mean"){
      newData %>% group_by(Remission) %>%
        summarise(
          Avg = round(mean(!!sym(input$select1)), 2),
          Sd = round(sd(!!sym(input$select1)), 2))
    }else if(input$RB1=="Median"){
      newData %>% group_by(Remission) %>%
        summarise(
          Median = median(!!sym(input$select1)),
          IQR = round(IQR(!!sym(input$select1)), 2))
    }
  })
  
  #Distribution Plot 
  output$dataPlot <- renderPlot({
    #get new data 
    newData <- getData()
    
    if(input$RB2=="Histogram" & input$Bin){
    h <- ggplot(newData, aes(x = !!sym(input$select1), fill = Remission))
    h + geom_histogram(alpha=0.5, position = input$RB3, bins = input$Bin) + labs(x = input$select1, title = paste0("Histogram of ", input$select1, " Distribution Across Remission Groups"))
    }else if (input$RB2=="Density"){
    d <- ggplot(newData, aes(x = !!sym(input$select1), fill = Remission))
    d + geom_density(alpha=0.5, position = input$RB3, adjust = input$Adjust) + labs(x = input$select1, title = paste0("Density Plot of ", input$select1, " Distribution Across Remission Groups"))
    }
})
  
  #get new data based on the selected categorical variable
  getData1 <- reactive({
    newData1 <- data_whole %>% select(!!sym(input$select2), Remission)
  })

  #bar chart 
  output$dataPlot1 <- renderPlot({
    #get new data 
    newData1 <- getData1()
    b <- ggplot(newData1, aes(x = !!sym(input$select2), fill = Remission))
    b + geom_bar(alpha=0.5, position = 'dodge') + labs(x = input$select2, title = paste0("Bar Chart of ", input$select2, " Count Across Remission Groups"))
  })
  
  #get new data based on the selected numeric and categorical variable
  getData2 <- reactive({
    newData2 <- data_whole %>% select(!!sym(input$select1), !!sym(input$select2))
  })
  #box plot
  output$dataPlot2 <- renderPlot({
    #get new data 
    newData2 <- getData2()
    x <- ggplot(newData2, aes(y = !!sym(input$select1), x = !!sym(input$select2), fill = !!sym(input$select2)))
    x + geom_boxplot(alpha=0.5, adjust = 0.5) + labs(y = input$select1, title = paste0("Box Plot of ", input$select1, " Distribution Across ", input$select2," Groups"))
  })
  
  #get new data based on the selected numeric variables
  getData3 <- reactive({
    newData3 <- data_whole %>% select(!!sym(input$select1), !!sym(input$select3), Remission)
  })
  #scatter plot
  output$dataPlot3 <- renderPlot({
    #get new data 
    newData3 <- getData3()
    s <- ggplot(newData3, aes(x = !!sym(input$select1), y = !!sym(input$select3)))
    s + geom_point(aes(col = Remission), alpha = 0.6, size = 1, position = "jitter") + labs(x = input$select1, y = input$select3, title = paste0("Scatter Plot of ", input$select1, " and ", input$select3," Across Remission Groups"))
  })
  
######################## Modeling ###########################
  # set a formula for logistic regression
  output$formular <- renderUI({
    withMathJax(
      helpText('
               $$\\log {(}{\\frac {p}{1-p}}{)}=\\beta _{0}+\\beta x$$'))
  })
  
  #change random forest slide bar number based on how many predictors are chosen
  observe({updateSliderInput(session, inputId = "mtry", 
                             max = length(input$predictor),
                             value =c(1, length(input$predictor)))})
  
  #fit model button setting
  button <- eventReactive(input$fitmodel,{
  
  #get new data based on the selected variables
  data_model <- select(data_whole, c(input$predictor, "Remission"))
  
  #split data
  set.seed(20)
  index <- createDataPartition(data_model$Remission, p = input$split, list = FALSE)
  train <- data_model[index, ]
  test <- data_model[-index, ]
  
  #fit logistic regression
  log <- train(Remission ~ .,
                    data=train, 
                    method = "glm", 
                    family = "binomial",
                    preProcess = c("center", "scale")
              )
  #regression coefficient
  logs <- summary(log)
  
  #test logistic regression on testing set
  logt <- confusionMatrix(data=test$Remission, reference = predict(log, newdata = test))
  
  #fit random forest
  rf <- train(Remission ~ .,
                    data=train, 
                    method = "rf", 
                    ntree = 500,
                    preProcess = c("center", "scale"),
                    tuneGrid = data.frame(mtry = c(input$mtry[1]:input$mtry[2])),
                    trControl = trainControl(method = "cv", number = input$cv),
                    importance=TRUE
              )
  
  #random forest variable importance
  rfi <- varImp(rf, scale=TRUE)[[1]]
  rfi$Variable <-row.names(rfi) 
  rfi <- reshape2::melt(rfi)
  rfi <- rename(rfi, Remission = variable)
  
  #test random forest on testing set
  rft <- confusionMatrix(data=test$Remission, reference = predict(rf, newdata = test))
  
  #compare 2 confusion matrix and automatically return the better model
  l <- data.frame(cbind(logt[[3]][1], model = "Logistc Regression"))
  r <- data.frame(cbind(rft[[3]][1], model = "Random Forest"))
  two <- rbind(l, r)
  final <- two[which.max(as.numeric(two$V1)), ]
  
  #return everything 
  list(log, logs, logt, rf, rfi, rft, final, two)
  })
  
  #create logistic regression summary
  output$logacu <- renderPrint({
    button()[[1]]
  })
  
  #regression coefficient
  output$logsum <- renderPrint({
    button()[[2]]
  })
  
  #test logistic regression on testing set
  output$logtest <- renderPrint({
    button()[[3]]
  })  
  
  #create random forest summary
  output$rfacu <- renderPrint({
    button()[[4]]
  })
  
  #create random forest importance Plot
  output$rfplot <- renderPlot({
    i <- ggplot(button()[[5]], aes(value, Variable, col = Remission))
    i + geom_point() + facet_wrap( ~ Remission)
  })
  
  #test random forest on testing set
  output$rftest <- renderPrint({
    button()[[6]]
  })  
  
  #compare 2 confusion matrix and automatically return the better model
  
  output$best <- renderText({
    paste("After comparing both models’ accuracy on the test set, the ", 
          button()[[7]]$model, " model has better accuracy. Thus, the ", button()[[7]]$model, " model is the better model for predicting RA remission.")
    
  })  
  
  #test
  output$t1 <- renderPrint({
    button()[[7]]
  })  
  
  output$t2 <- renderPrint({
    button()[[8]]
  })  
  
  #set up the action button tab
  observeEvent(input$fitmodel, {
    updateTabItems(session, "tabs", selected = "model")
  })
    
}
