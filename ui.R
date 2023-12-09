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

ui <- dashboardPage(skin = "black",
  
  dashboardHeader(title = "Rheumatoid Arthritis Disease Remission Prediction"),
  
  ## Sidebar content
  dashboardSidebar(useShinyjs(),
    sidebarMenu(id = "tabs", 
    # First tab, about
    menuItem("About",tabName = "About",icon = icon("info-sign", lib = "glyphicon")),
    
    # Second tab, EDA
    menuItem("Data Exploration", tabName = "DataExploration",icon = icon("th-list", lib = "glyphicon"),
             selectizeInput(
               inputId ="select1",
               "Select a Numeric Variable",
               choices = c("Age", "PainScale", "FatigueScale", "PatientGlobal", "MDGlobal", "MdhaqScore", "BMI"), selected = "Age"
             ),
             
             # add a filter 
             sliderInput(inputId = "filter",
                         label = "Filter the Values",
                         min = 1,
                         max = 5,
                         value = c(1,5)),
             
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
             radioButtons(
               "RB2",
               label="Type of Plot",
               choices = list(
                 "Histogram",
                 "Density")
             ),
             
             conditionalPanel(condition = "input.RB2 == 'Histogram'",
                              sliderInput(inputId = "Bin", "Bin Number on the Histogram",
                         min = 10, max = 30, value = 20, step = 1)),
             
             conditionalPanel(condition = "input.RB2 == 'Density'",
                              sliderInput(inputId = "Adjust", "Adjust Value for the Density Plot",
                                          min = 0.1, max = 1, value = 0.5, step = 0.1)),
             
             radioButtons(
               "RB3",
               label="Plot Postion",
               choices = list(
                 "stack",
                 "dodge")
             ),
             
             menuSubItem("Distribution", tabName = "Plot",icon = icon("play", lib = "glyphicon")),
             
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
             
             menuSubItem("Box Chart", tabName = "boxPlot",icon = icon("play", lib = "glyphicon")),
             
             #correlation
             selectizeInput(
               inputId ="select3",
               "Select A Second Numeric Variable and Investigate the Correlation With the Previous Selected one Across Remission Groups",
               choices = c("Age", "PainScale", "FatigueScale", "PatientGlobal", "MDGlobal", "MdhaqScore", "BMI"), selected = "PainScale"
             ),
             
             menuSubItem("Scatter Plot", tabName = "scatterPlot",icon = icon("play", lib = "glyphicon"))
          
            ),
    
            # Third tab, modeling 
    menuItem("Modeling",tabName = "Modeling",icon = icon("signal", lib = "glyphicon"),
             
             #modeling info
             menuSubItem("Model Information", tabName = "modelinfo",icon = icon("play", lib = "glyphicon")),
             
             #model setting
             menuSubItem("Model Statistics", tabName = "modelstat",icon = icon("play", lib = "glyphicon")),
            
             #model statistics
            hidden(menuSubItem("Model", tabName = "model")),
            
             #prediction
            menuSubItem("Prediction", tabName = "pred",icon = icon("play", lib = "glyphicon"))
            
            #prediction out
            #hidden(menuSubItem("Prediction Output", tabName = "out"))
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
      #EDA
      tabItem(tabName = "Summary",
              h2("Summary Table of Numeric Variable"),
              fluidRow(
                       box(width = 5,
                       solidHeader = TRUE, collapsible = FALSE,
                       tableOutput("table")))),
      
      tabItem(tabName = "Plot",
              h2("Distribution Plot for Numeric Variable"),
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
                    plotOutput("dataPlot2")))),
      
      tabItem(tabName = "scatterPlot",
              h2("Scatter Plot of the Selected Numeric Variables"),
              fluidRow(
                box(width = 10,
                    solidHeader = TRUE, collapsible = FALSE,
                    plotOutput("dataPlot3")))),
      #Modeling
      
      #modeling info
      tabItem(tabName = "modelinfo", 
              h2("Modeling Information:"),
              h3("Logistic Regression"),
              h4("The first model we are fitting is logistic regression, which is a generalized linear model that models the probability of an event by calculating the log-odds for the event based on the linear combination of one or more independent variables. The most common logistic regression has a single binary variable as the response, usually the two values are coded as 0 and 1."),
              br(),
              h4("In logistic regression, the dependent variable is a logit, which is the natural log of the odds and assumed to be linearly related to X as the formula below:"),
              br(),
              uiOutput('formular'),
              h4("Logist regression is much easier to set up and train than other machine learning models, it does not have many assumptions about the data it deals with and it is one of the most efficient algorithms when the different outcomes or distinctions represented by the data are linearly separable."),
              br(),
              h4("The major limitation of Logistic Regression is the assumption of linearity between the dependent variable and the independent variables. Also, logistic Regression requires average or no multicollinearity between independent variables. So be careful and refer to the scatter plots before choosing predictors!"),
              br(),
              h3("Random Forest"),
              h4("The second model we will fit is a tree based model called random forest. This model is made up of multiple decision trees which are non-parametric supervised learning method and used for classification and regression. The goal of decision tree is to create a model that predicts the value of a target variable by splitting the predictor into regions with different predictions for each region. A random forest utilizes the “bootstrap” method to takes repeatedly sampling with replacement, fits multiple decision trees with a random subset of predictors, then returns the average result from all the decision trees."),
              br(),
              h4("Random forests are generally more accurate than individual classification trees because there is always a scope for over fitting caused by the presence of variance in classification trees, while random forests combine multiple trees and prevent over fitting. Random forests also average the predicted results from classification trees and gives a more accurate and precise prediction."),
              br(),
              h4("The training time of a random forest is more than other models due to its complexity, also it is difficult to interpret the relationships within data in a random forest.")
              ),
      
      
      #model setting
      tabItem(tabName = "modelstat",
              h3("Model Setting"),
          checkboxGroupInput(inputId ="predictor", label = "Which Predictors do you want to include into the model?",
                         choices = c("Age", "PainScale", "FatigueScale", "PatientGlobal", "MDGlobal", "MdhaqScore", "BMI",
                                     "HighBloodPressure", "Smoke", "Bronchitis", "Melanoma", "Narcotic", "PainMed", "DmardMed")),

          sliderInput(inputId = "split",
                  label = "How big do you want the training set to be?",
                  min = 0.6,
                  max = 0.9,
                  value = 0.8,
                  step = 0.1),

          sliderInput(inputId = "mtry",
                  label = "How do you want to set the tuning parameter for random forest model?",
                  min = 1,
                  max = 14,
                  value = c(1,14),
                  step = 1),

          sliderInput(inputId = "cv",
                  label = "How many folds do you want for cross-validation?",
                  min = 2,
                  max = 10,
                  value = 5,
                  step = 1),

          actionButton("fitmodel",h5("Fit Models")),
          
          h4("Note: It takes a while to run the models!")
      ),
      
      
      #model stat
      tabItem(tabName = "model",
              h3("Model Statistics"),
              fluidRow(
                box(title = "Logistic  Regression Accuracy on Training Set",  width = 8,
                    solidHeader = TRUE, collapsible = FALSE,
                    verbatimTextOutput("logacu")),
                box(title = "Logistic  Regression Coefficients",  width = 8,
                    solidHeader = TRUE, collapsible = FALSE,
                    verbatimTextOutput("logsum")),
                box(title = "Logistic  Regression Accuracy on Test Set",  width = 8,
                    solidHeader = TRUE, collapsible = FALSE,
                    verbatimTextOutput("logtest")),
                box(title = "Random Forest Accuracy on Training Set",  width = 8,
                    solidHeader = TRUE, collapsible = FALSE,
                    verbatimTextOutput("rfacu")),
                box(title = "Random Forest Importance Plot",  width = 8,
                    solidHeader = TRUE, collapsible = FALSE,
                    plotOutput("rfplot")),
                box(title = "Random Forest Accuracy on Test Set",  width = 8,
                    solidHeader = TRUE, collapsible = FALSE,
                    verbatimTextOutput("rftest")),
                box(title = "Better Model",  width = 8,
                    solidHeader = TRUE, collapsible = FALSE,
                    span(textOutput("best"), style="color:red"))
                      )
              ),
              
      #prediction
      tabItem(tabName = "pred",
              h2("Input Values and Predict Remission Using Both Logistic Regression and Random Forest Model"),
              br(),
              h4("Numeric Variables"),
              
              sliderInput("Age", label = "Age", value = 18, min = 18, max = 100, step = 1),
              sliderInput("PainScale", label = "PainScale", value = 0, min = 0, max = 100, step = 25),
              sliderInput("FatigueScale", label = "FatigueScale", value = 0, min = 0, max = 100, step = 25),
              sliderInput("PatientGlobal", label = "PatientGlobal", value = 0, min = 0, max = 100, step = 25),
              sliderInput("MDGlobal", label = "MDGlobal", value = 0, min = 0, max = 100, step = 25),
              sliderInput("MdhaqScore", label = "MdhaqScore", value = 0, min = 0, max = 3, step = 0.1),
              sliderInput("BMI", label = "BMI", value = 10, min = 10, max = 60, step = 0.1),
              
              br(),
              h4("Binary Variables"),
            
              radioButtons("HighBloodPressure", label = "HighBloodPressure", choices = list(0,1)),
              radioButtons("Smoke", label = "Smoke", choices = list(0,1)),
              radioButtons("Bronchitis", label = "Bronchitis", choices = list(0,1)),
              radioButtons("Melanoma", label = "Melanoma", choices = list(0,1)),
              radioButtons("Narcotic", label = "Narcotic", choices = list(0,1)),
              radioButtons("PainMed", label = "PainMed", choices = list(0,1)),
              radioButtons("DmardMed", label = "DmardMed", choices = list(0,1)),
              
              actionButton("predgo",h5("PREDICT")),
             )
      
      # tabItem(tabName = "out",        
      #         h3("Probability of Remission")
              
              # fluidRow(box(title = "",  width = 10,
              #              solidHeader = TRUE, collapsible = FALSE,
              #              tableOutput("predication"))
      #        )
              


))
)
