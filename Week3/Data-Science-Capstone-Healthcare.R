library(plyr)
library(e1071)
library(rpart)
library(randomForest)
library(dplyr)
library(imputeTS)
library(forecast)
library(pROC)
library(caTools)
library(class)

#Path of dataset
setwd("D:/aVDHOOT/SimpliLearn/Data Science Caption/Project 2/Healthcare - Diabetes")
getwd()

#Loading Dataset
data<-read.csv("health care diabetes.csv")
data

#Discriptive Analysis
View(data)
str(data)
summary(data)

#Handling Missing Values
table(is.na(data))

hist(data$Glucose,main="Frequency of Glucose",breaks = 8,col="darkorange")
table(data$Glucose)

data$Glucose[data$Glucose==0]<-mean(data$Glucose)

hist(data$BloodPressure,main="Frequency of BloodPressure",breaks = 8,col="darkorange")
table(data$Glucose)

data$BloodPressure[data$BloodPressure==0]<-mean(data$BloodPressure)

hist(data$SkinThickness,main="Frequency of SkinThickness",breaks = 8,col="darkorange")
table(data$SkinThickness)

data$SkinThickness[data$SkinThickness==0]<-mean(data$SkinThickness)

hist(data$Insulin,main="Frequency of Insulin",breaks = 8,col="darkorange")
table(data$Insulin)

data$Insulin[data$Insulin==0]<-mean(data$Insulin)

hist(data$BMI,main="Frequency of BMI",breaks = 8,col="darkorange")
table(data$BMI)

data$BMI[data$BMI==0]<-mean(data$BMI)

#plotting the count of outcomes
table(data$Outcome)
hist(data$Outcome,main="Frequency of Outcome",breaks = 8,col="darkorange")

#scatter charts between the pair of variables
pairs(data)

#correlation analysis and heatmap
cor(data)
heatmap(cor(data))

#To decide the which model is sutaible for Dataset I compare Naive bayes, SVM, Decision Tree and Random Forest
#1. Naive Bayes----------75% Accuracy
naive_bayes<-naiveBayes(Outcome~.,data = data)
summary(naive_bayes)

Predictions<-predict(naive_bayes,data)
Predictions
table(Predictions,data$Outcome)

#Accuracy of Model
table_mat<-table(Predictions,data$Outcome)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

d<-ifelse(Predictions==1, 1, 0)
#ROC curve
roc(data$Outcome,d,plot=TRUE)

#2. Decision Tree---------83.20% Acuraccy
tree<-rpart(Outcome~.,data = data,method = 'class')
summary(tree)

Predictions<-predict(tree,data,type = 'class')
Predictions
table(Predictions,data$Outcome)

#Accuracy of Model
table_mat<-table(Predictions,data$Outcome)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

d<-ifelse(Predictions==1, 1, 0)
#ROC curve
roc(data$Outcome,d,plot=TRUE)

#3. Random Forest---------100% Accuracy
forest<-randomForest(x = data[,-c(9)], y = data$Outcome,ntree =800)# build model
summary(forest)

Predictions<-predict(forest,data)
Predictions
d<-ifelse(Predictions > 0.46, 1, 0)
table(d,data$Outcome)

#Accuracy of Model
table_mat<-table(d,data$Outcome)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#ROC curve
roc(data$Outcome,d,plot=TRUE)

#4.KNN ------------Accuracy: 72.65%
# Splitting data into train and test data
split <- sample.split(data, SplitRatio = 0.7)
train_cl <- subset(data, split == "TRUE")
test_cl <- subset(data, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:8])
test_scale <- scale(test_cl[, 1:8])

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Outcome,
                      k =8)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$Outcome, classifier_knn)
cm

#Accuracy of Model
table_mat<-table(test_cl$Outcome, classifier_knn)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#5. Support Vector Machine-------74.86% Accuracy
svms<-svm(Outcome~.,data = data,kernel = 'linear')
summary(svms)

Predictions<-predict(svms,data,type='class')
Predictions
d<-ifelse(Predictions > 0.3, 1, 0)
table(d,data$Outcome)

#Accuracy of Model
table_mat<-table(d,data$Outcome)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#ROC curve
roc(data$Outcome,d,plot=TRUE)
