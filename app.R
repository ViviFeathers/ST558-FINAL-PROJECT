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

ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "RA Remission Prediction"),
                    
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
                                                          menuSubItem("Prediction", tabName = "pred",icon = icon("play", lib = "glyphicon")),
                                                          
                                                          #prediction out
                                                          hidden(menuSubItem("Prediction Output", tabName = "out"))
                                                 )
                                     )),
                    
                    
                    dashboardBody(
                      #About
                      tabItems(
                        tabItem(tabName = "About",
                                h2("Rheumatoid Arthritis(RA) Remission Prediction"),
                                h3("Purpose: To predict Rheumatoid Arthritis(RA) remission based on patients' RA disease activities, RA treatments, physical condition and other diagnostic diseases."),
                                img(src="rar.png",height=50,width=50),
                                h3("Data Source: The Brigham and Women’s Hospital Rheumatoid Arthritis Sequential Study"),
                                tags$a(href="https://www.brassstudy.org/", "BRASS"),
                                h4("Our data set has 1581 observations and 14 predictors, the outcome is 'Remission' with vaule 0 as no remission, and 1 means remission."),
                                h4("Foureen predictors are listed as below:"),
                                h5("Age: numeric, patient's age, from 18 to 100 by 1"),
                                h5("PainScale: numeric, pain scale, from 0 to 100 by 25"),
                                h5("FatigueScale: numeric, fatigue scale, from 0 to 100 by 25"),
                                h5("PatientGlobal: numeric, patient global score, from 0 to 100 by 25"),
                                h5("MDGlobal: numeric, global score from patient's physician, from 0 to 100 by 25"),
                                h5("MdhaqScore: numeric, MDHAQ score, from 0 to 3 by 0.1"),
                                h5("BMI: numeric, body mass index, from 10 to 60 by 0.1"),
                                h5("HighBloodPressure: binary, 1 = Have, 0 = Not have"),
                                h5("Smoke: binary, 1 = Ever smoked, 0 = Never smoked"),
                                h5("Bronchitis: binary, 1 = Have, 0 = Not have"),
                                h5("Melanoma: binary, 1 = Have, 0 = Not have"),
                                h5("Narcotic: binary, 1 = Taking, 0 = Not taking"),
                                h5("PainMed: binary, 1 = Taking, 0 = Not taking"),
                                h5("DmardMed: binary, 1 = Taking, 0 = Not taking"),
                                br(),
                                h3("App Introduction:"),
                                h4("Tab 1. About:  Infromation about the data set and purpose of this APP"),
                                h4("Tab 2.Data Explorary Analysis:  we can investigate the data distribution by creating summary tables, contingency tables, histogram, density plot and bar chart  We can also learn the relationship of those predictors with each other by generating box plots and scatter plots."),
                                h4("Tab 3. Modeling:  We explore modeling by fitting a logistic regression model and a random forest model."),
                                h5("The first subtab has the introduction about these two models."),
                                h5("The second subtab is used for model setting where you can choose variables, tuning parameter and cross-validation fold number, you can also decide how to split the training and test set."),
                                h5("The last subtab is for remission prediction, you can manually set input values for each predictor, run the models you fit in the previous tab and return the probability of Remissions.")
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
                                
                                sliderInput(inputId = "split",
                                            label = "How big do you want the training set to be?",
                                            min = 0.6,
                                            max = 0.9,
                                            value = 0.8,
                                            step = 0.1),
                                
                                h4("Logistic Regression"),
                                
                                checkboxGroupInput(inputId ="predictor", label = "Which Predictors do you want to include into the Logistic Regression model?",
                                                   choices = c("Age", "PainScale", "FatigueScale", "PatientGlobal", "MDGlobal", "MdhaqScore", "BMI",
                                                               "HighBloodPressure", "Smoke", "Bronchitis", "Melanoma", "Narcotic", "PainMed", "DmardMed")),
                                
                                h4("Random Forest"),
                                
                                checkboxGroupInput(inputId ="predictor1", label = "Which Predictors do you want to include into the Random Forest model?",
                                                   choices = c("Age", "PainScale", "FatigueScale", "PatientGlobal", "MDGlobal", "MdhaqScore", "BMI",
                                                               "HighBloodPressure", "Smoke", "Bronchitis", "Melanoma", "Narcotic", "PainMed", "DmardMed")),
                                
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
                                
                                h4("Note: It takes a while to run & rerun the models!")
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
                                
                                radioButtons("HighBloodPressure", label = "HighBloodPressure", choices = list(0, 1)),
                                radioButtons("Smoke", label = "Smoke", choices = list(0, 1)),
                                radioButtons("Bronchitis", label = "Bronchitis", choices = list(0, 1)),
                                radioButtons("Melanoma", label = "Melanoma", choices = list(0, 1)),
                                radioButtons("Narcotic", label = "Narcotic", choices = list(0, 1)),
                                radioButtons("PainMed", label = "PainMed", choices = list(0, 1)),
                                radioButtons("DmardMed", label = "DmardMed", choices = list(0, 1)),
                                
                                actionButton("predgo",h5("PREDICT")),
                                
                                h4("Note: It takes a while if re-fit models & rerun the predictions!")
                        ),
                        
                        tabItem(tabName = "out",        
                                h3("Probability of Remission"),
                                
                                fluidRow(
                                  box(title = "Remission Probablity Prediction From Logistic Regression",  width = 10,
                                      solidHeader = TRUE, collapsible = FALSE,
                                      tableOutput("logpred")),
                                  box(title = "Remission Probablity Prediction From Random Forest",  width = 10,
                                      solidHeader = TRUE, collapsible = FALSE,
                                      tableOutput("rfpred"))
                                )
                        )
                        
                        
                      ))
)

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
  
  ######################## Model Statistic ###########################
  # set a formula for logistic regression
  output$formular <- renderUI({
    withMathJax(
      helpText('
               $$\\log {(}{\\frac {p}{1-p}}{)}=\\beta _{0}+\\beta x$$'))
  })
  
  #change random forest slide bar number based on how many predictors are chosen
  observe({updateSliderInput(session, inputId = "mtry", 
                             max = length(input$predictor1),
                             value =c(1, length(input$predictor)))})
  
  #fit model button setting
  button <- eventReactive(input$fitmodel,{
    
    #split data
    set.seed(20)
    index <- createDataPartition(data_whole$Remission, p = input$split, list = FALSE)
    train <- data_whole[index, ]
    test <- data_whole[-index, ]
    
    #get new data for logistic regression based on the selected variables
    train1 <- select(train, c(input$predictor, "Remission"))
    test1 <- select(test, c(input$predictor, "Remission"))
    
    #get new data for random forest based on the selected variables
    train2 <- select(train, c(input$predictor1, "Remission"))
    test2 <- select(test, c(input$predictor1, "Remission"))
    
    #fit logistic regression
    log <- train(Remission ~ .,
                 data=train1, 
                 method = "glm", 
                 family = "binomial",
                 preProcess = c("center", "scale")
    )
    #regression coefficient
    logs <- summary(log)
    
    #test logistic regression on testing set
    logt <- confusionMatrix(data=test1$Remission, reference = predict(log, newdata = test1))
    
    #fit random forest
    rf <- train(Remission ~ .,
                data=train2, 
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
    rft <- confusionMatrix(data=test2$Remission, reference = predict(rf, newdata = test2))
    
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
  
  #set up the action button tab
  observeEvent(input$fitmodel, {
    updateTabItems(session, "tabs", selected = "model")
  })
  
  ######################## Model Prediction ###########################
  
  button1 <- eventReactive(input$predgo,{
    #create a new data set according to the input values  
    pred_data <- data.frame(cbind(input$Age, input$PainScale, input$FatigueScale, input$PatientGlobal, input$MDGlobal, input$MdhaqScore, input$BMI, 
                                  input$HighBloodPressure, input$Smoke, input$Bronchitis, input$Melanoma, input$Narcotic, input$PainMed, input$DmardMed))
    colnames(pred_data)<-c("Age", "PainScale", "FatigueScale", "PatientGlobal", "MDGlobal", "MdhaqScore", "BMI",
                           "HighBloodPressure", "Smoke", "Bronchitis", "Melanoma", "Narcotic", "PainMed", "DmardMed")
    
    pred_data$Age <- as.numeric(pred_data$Age)
    pred_data$PainScale <- as.numeric(pred_data$PainScale)
    pred_data$PatientGlobal <- as.numeric(pred_data$PatientGlobal)
    pred_data$FatigueScale <- as.numeric(pred_data$FatigueScale)
    pred_data$MDGlobal <- as.numeric(pred_data$MDGlobal)
    pred_data$MdhaqScore <- as.numeric(pred_data$MdhaqScore)
    pred_data$BMI <- as.numeric(pred_data$BMI)
    pred_data$HighBloodPressure <- as.factor(pred_data$HighBloodPressure) 
    pred_data$Smoke <- as.factor(pred_data$Smoke)
    pred_data$Bronchitis <- as.factor(pred_data$Bronchitis) 
    pred_data$Melanoma <- as.factor(pred_data$Melanoma)
    pred_data$Narcotic <- as.factor(pred_data$Narcotic)
    pred_data$PainMed <- as.factor(pred_data$PainMed) 
    pred_data$DmardMed <- as.factor(pred_data$DmardMed)
    
    #return everything 
    pred_data
  })
  
  #output the prediction value for from both the logistic regression and random forest model
  output$logpred <- renderTable({
    predict(button()[[1]], newdata = button1(), type = "prob")
  })
  
  output$rfpred <- renderTable({
    predict(button()[[4]], newdata = button1(), type = "prob")
  })  
  
  #set up the action button tab
  observeEvent(input$predgo, {
    updateTabItems(session, "tabs", selected = "out")
  })
  
}

shinyApp(ui, server)
