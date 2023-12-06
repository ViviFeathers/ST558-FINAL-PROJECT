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
  
  
  #get new data based on the selected numeric variable
  getData <- reactive({
    newData <- data_whole %>% select(!!sym(input$select1), Remission)
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
  
  #Histogram 
  output$dataPlot <- renderPlot({
    #get new data 
    newData <- getData()
    h <- ggplot(newData, aes(x = !!sym(input$select1), fill = Remission))
    h + geom_histogram(alpha=0.5, position = 'dodge', bins = input$Bin) + labs(x = input$select1, title = paste0("Histogram of ", input$select1, " Distribution Across Remission Groups"))
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
  
}
