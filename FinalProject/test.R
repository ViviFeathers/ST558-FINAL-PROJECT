library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(caret)
library(ggplot2)
library(shinyjs)


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

set.seed(20)
index <- createDataPartition(data_whole$Remission, p = 0.7, list = FALSE)
train <- data_whole[index, ]
test <- data_whole[-index, ]

rf <- train(Remission ~ .,
            data=train, 
            method = "rf", 
            ntree = 500,
            preProcess = c("center", "scale"),
            tuneGrid = data.frame(mtry = c(5:6)),
            trControl = trainControl(method = "cv", number = 5),
            importance=TRUE
)
log <- train(Remission ~ .,
             data=train, 
             method = "glm", 
             family = "binomial",
             preProcess = c("center", "scale")
)

logt <- confusionMatrix(data=test$Remission, reference = predict(log, newdata = test))
rft <- confusionMatrix(data=test$Remission, reference = predict(rf, newdata = test))

c <- data.frame(cbind(logt[[3]][1], model = "Logistc"))
c

b <- data.frame(cbind(rft[[3]][1], model = "Random"))
b
a <- rbind(c, b)
a
final_best <- a[which.min(as.numeric(a$V1)),]
final_best
