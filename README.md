# Rheumatoid Arthritis(RA) Remission Prediction

## About this APP
The purpose of this APP is to predict Rheumatoid Arthritis(RA) remission based on patients' RA disease activities, RA treatments, physical condition and other diagnostic diseases.

The data source used by this APP is from The Brigham and Women’s Hospital Rheumatoid Arthritis Sequential Study, it has 1581 observations and 14 predictors, the outcome is 'Remission' with vaule 0 as no remission, and 1 means remission.

There are 3 tabs in the APP:

Tab 1.  About:  Infromation about the data set and purpose of this APP.  
Tab 2.  Data Explorary Analysis:  we can investigate the data distribution by creating summary tables, contingency tables, histogram, density plot and bar chart  We can also learn the relationship of those predictors with 
        each other by generating box plots and scatter plots.  
Tab 3.  Modeling:  We explore modeling by fitting a logistic regression model and a random forest model.  
        The first subtab has the introduction about these two models.  
        The second subtab is used for model setting where you can choose variables, tuning parameter and cross-validation fold number, you can also decide how to split the training and test set.  
        The last subtab is for remission prediction, you can manually set input values for each predictor, run the models you fit in the previous tab and return the probability of Remissions.  

## Packages 
`shiny`
`shinydashboard`
`shinyWidgets`
`tidyverse`
`caret`
`ggplot2`
`shinyjs`

## Package Installing
install.pacakge("shiny")
install.pacakge("shinydashboard")
install.pacakge("shinyWidgets")
install.pacakge("tidyverse")
install.pacakge("caret")
install.pacakge("ggplot2")
install.pacakge("shinyjs")

## Run the APP
The code I used to run my APP from repo is as below:
`shiny::runGitHub('ST558-FINAL-PROJECT','ViviFeathers', ref="main")`
