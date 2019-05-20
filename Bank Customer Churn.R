

#########################################################################################################

# Churn Analysis 

#########################################################################################################

# Load Libraries

library("dplyr")
library("tidytext")
library("ggplot2")
library("caTools")
library("ROCR")
library("magrittr")
library("forecast")
library("nnet")

getwd()
#Import Data
setwd("C:\\Data Science A-Z\\Part 3 Data Prep\\Logistic Regression")
data1 <- read.csv("P12-Churn-Modelling.csv", header = TRUE)
# changing character variables to factors
data1 <- data1 %>% mutate_if(is.character, as.factor)
attach(data1)
# looking for missing values
colSums(is.na(data1))
summary(data1)

# removing RowNumber,customerID,Surname; doesn't add any value to the model
data1$RowNumber <- NULL
data1$CustomerId <- NULL
data1$Surname <-NULL

str(data1)
head(data1)

# fitting the model

model <- glm(Exited~.,data1,family = "binomial")
summary(model)
# making predictions
res <- predict(model,data1,type="response")
table(Actualvalue=data1$Exited,Predictedvalue=res>0.5)

#Removing Geography from model

model1 <- glm(Exited~(CreditScore) +(Age)+(Tenure)+(Gender)+Balance+NumOfProducts+HasCrCard+EstimatedSalary+IsActiveMember ,data1,family = "binomial")
summary(model1)
res <- predict(model1,data1,type="response")
table(Actualvalue=data1$Exited,Predictedvalue=res>0.5)

model2 <- glm(Exited~(CreditScore) +(Age)+(Tenure)+(Gender)+Balance+NumOfProducts+EstimatedSalary+IsActiveMember ,data1,family = "binomial")
summary(model2)
res <- predict(model2,data1,type="response")
table(Actualvalue=data1$Exited,Predictedvalue=res>0.5)

model3 <- glm(Exited~(CreditScore) +(Age)+(Tenure)+(Gender)+Balance+NumOfProducts+IsActiveMember ,data1,family = "binomial")
summary(model3)
res <- predict(model3,data1,type="response")
table(Actualvalue=data1$Exited,Predictedvalue=res>0.5)

model4 <- glm(Exited~(CreditScore) +(Age)+(Gender)+Balance+NumOfProducts+IsActiveMember ,data1,family = "binomial")
summary(model4)
res <- predict(model4,data1,type="response")
table(Actualvalue=data1$Exited,Predictedvalue=res>0.5)

#SEMI FINAL MODEL

model5 <- glm(Exited~(CreditScore) +(Age)+(Gender)+Tenure+Geography+Balance+NumOfProducts+IsActiveMember ,data1,family = "binomial")
summary(model5)
res <- predict(model5,data1,type="response")
table(Actualvalue=data1$Exited,Predictedvalue=res>0.5)

#VAriable Transform

log_BL<-log(data1$Balance+1)

#Final Model
model6 <- glm(Exited~(CreditScore) +(Age)+(Gender)+Tenure+Geography+log_BL+NumOfProducts+IsActiveMember ,data1,family = "binomial")
summary(model6)
res <- predict(model6,data1,type="response")
table(Actualvalue=data1$Exited,Predictedvalue=res>0.5)

############################################################

# Performance Measure

#############################################################


#CIs using profiled log-lkelihood
confint(model6)

#CIs using standard errors
confint.default(model6)

#Put the coefficents and CI in a format onto a useful Scale.

exp(confint(model6))

# odds ratios only
exp(coef(model6))

# odds ratios and 95% CI
exp(cbind(OR = coef(model6),confint(model6)))

#Model Testing Using Test Data

setwd("C:\\Data Science A-Z\\Part 3 Data Prep\\Logistic Regression")
Testdata <- read.csv("P12-Churn-Modelling-Test-Data.csv", header = TRUE)
# changing character variables to factors
Testdata <- Testdata %>% mutate_if(is.character, as.factor)
attach(Testdata)
# looking for missing values
colSums(is.na(Testdata))
summary(Testdata)

# removing RowNumber,customerID,Surname; doesn't add any value to the model
Testdata$RowNumber <- NULL
Testdata$CustomerId <- NULL
Testdata$Surname <-NULL

log_BL<-log(Testdata$Balance+1)

Testmodel <- glm(Exited~(CreditScore) +(Age)+(Gender)+Tenure+Geography+log_BL+NumOfProducts+IsActiveMember ,Testdata,family = "binomial")
summary(Testmodel)
res <- predict(Testmodel,Testdata,type="response")
table(Actualvalue=Testdata$Exited,Predictedvalue=res>0.5)
