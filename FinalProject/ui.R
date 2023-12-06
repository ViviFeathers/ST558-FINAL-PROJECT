#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(caret)
library(ggplot2)

data_whole <- read_csv("ra_data.csv")
data_whole$HighBloodPressure <- as.factor(data_whole$HighBloodPressure) 
data_whole$Smoke <- as.factor(data_whole$Smoke)
data_whole$Bronchitis <- as.factor(data_whole$Bronchitis) 
data_whole$Melanoma <- as.factor(data_whole$Melanoma)
data_whole$Remission <- as.factor(data_whole$Remission) 
data_whole$Narcotic <- as.factor(data_whole$Narcotic)
data_whole$PainMed <- as.factor(data_whole$PainMed) 
data_whole$DmardMed <- as.factor(data_whole$DmardMed)

# read in RA data

ui <- dashboardPage(skin = "black",
  
  dashboardHeader(title = "Rheumatoid Arthritis Disease Remission Prediction"),
  
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    # First tab
    menuItem("About",tabName = "About",icon = icon("info-sign", lib = "glyphicon")),
    
    # Second tab
    menuItem("Data Exploration", tabName = "DataExploration",icon = icon("th-list", lib = "glyphicon"),
             selectizeInput(
               inputId ="select1",
               "Select a Numeric Variable",
               choices = c("Age", "PainScale", "FatigueScale", "PatientGlobal", "MDGlobal", "MdhaqScore", "BMI"), selected = "Age"
             ),
             
             #numeric summary
             radioButtons(
               "RB1",
               label="Type of Summary",
               choices = list(
                 "Mean",
                 "Median")
             ),
             
             menuSubItem("Summary Table", tabName = "Summary",icon = icon("play", lib = "glyphicon")),
             
             #plots
             sliderInput(inputId = "Bin", "Bin Number on the Histogram",
                         min = 10, max = 30, value = 21, step = 1),
             menuSubItem("Histogram", tabName = "Plot",icon = icon("play", lib = "glyphicon")),
             
             #categorical summary
             selectizeInput(
               inputId ="select2",
               "Select a Categorical Variable",
               choices = c("HighBloodPressure", "Smoke", "Bronchitis", "Melanoma", "Narcotic", "PainMed", "DmardMed"), selected = "HighBloodPressure"
             ),
             
             #bar chart
             menuSubItem("Bar Chart", tabName = "barPlot",icon = icon("play", lib = "glyphicon")),
             
             #box plot
             h5("Investigate the Selected Numeric"),
             h5("Variable Distribution in Each Group"),
             h5("of the Selected Categorical Variable"),
             
             menuSubItem("Box Chart", tabName = "boxPlot",icon = icon("play", lib = "glyphicon"))
            )
    
    
    
    
    
    )),
    
  dashboardBody(
      #About
    tabItems(
      tabItem(tabName = "About",
              h3("Purpose: To predict Rheumatoid Arthritis(RA) remission based on patients' disease activities, RA treatments, physical condition and other diagnostic diseases."),
              br(),
              h3("Data Source: Harvard Medical School Rheumatoid Arthritis Database"),
              h4("Data consist of 1581 observations and 14 predictors. The outcome is Remission with vaule 0 as no remission, and 1 means remission."),
              h4("14 predictors are listed as below:"),
              h4("Age: numeric, patient's age"),
              h4("HighBloodPressure: Binary, 1 = Yes, 0 = No"),
              br(),
              h3("App functions:"),
              h4("1. Exploratory data analysis of the dataset"),
              h4("2. Compare the performance of three models in predicting whether or not the patients in the dataset have diabetes or not."),
              h4("3. Predict the probability of a patient have remission by user input values"),
              h4("4. Save the data used in modeling as a csv file.")
             ),
      
      tabItem(tabName = "Summary",
              h2("Summary Table of Numeric Variable"),
              fluidRow(
                       box(width = 5,  
                       solidHeader = TRUE, collapsible = FALSE,
                       tableOutput("table")))),
      
      tabItem(tabName = "Plot",
              h2("Histogram Plot for Numeric Variable"),
              fluidRow(
                box(width = 8,  
                    solidHeader = TRUE, collapsible = FALSE,
                    plotOutput("dataPlot")))),
      
      tabItem(tabName = "barPlot",
              h2("Bar Chart for Categorical Variable"),
              fluidRow(
                box(width = 8,  
                    solidHeader = TRUE, collapsible = FALSE,
                    plotOutput("dataPlot1")))),
      
      tabItem(tabName = "boxPlot",
              h2("Box Plot of the Selected Numeric Variable Distribution Across Groups from the Selected Categorical Variable"),
              fluidRow(
                box(width = 8,  
                    solidHeader = TRUE, collapsible = FALSE,
                    plotOutput("dataPlot2"))))
      
      
))
)
