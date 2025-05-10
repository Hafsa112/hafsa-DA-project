read.csv("C:/Users/HafsaTabbsum/OneDrive - National College of Ireland/Desktop/project computing 2024-25/Boston Crimes Prediction dataset.xlsx")
library(rpart)
library(readxl)
library(rpart.plot)
library(dplyr)
library(lubridate)
colnames(Boston_Crimes_Prediction_dataset)[colnames(Boston_Crimes_Prediction_dataset)=="CRIME&Types"]="Crime"
Boston_Crimes_Prediction_dataset=Boston_Crimes_Prediction_dataset%>%
filter(!is.na(Crime),!is.na(YEAR),!is.na(MONTH),!is.na(NEIGHBOURHOOD))%>%
  mutate( YEAR=as.factor(YEAR),
          Crime=as.factor(Crime),
          NEIGHBOURHOOD=as.factor(NEIGHBOURHOOD),
          MONTH=as.factor(MONTH))
tree_model=rpart(Crime~YEAR+NEIGHBOURHOOD+MONTH,data
                 = Boston_Crimes_Prediction_dataset,method = "class")
rpart.plot(tree_model,extra=101,type=3,fallen.leaves = TRUE,
           main="predicting different crimes in a decision tree") 
library(randomForest)
set.seed(123)
rf_model=randomForest(Crime~YEAR+MONTH+NEIGHBOURHOOD,data = Boston_Crimes_Prediction_dataset,ntree=100)
print(rf_model)
plot(rf_model)
predictions=predict(rf_model,newdata = Boston_Crimes_Prediction_dataset)
mean(predictions==Boston_Crimes_Prediction_dataset$Crime)
table(predicted=predictions,actual=Boston_Crimes_Prediction_dataset$Crime)
varImpPlot(rf_model)
importance(rf_model)