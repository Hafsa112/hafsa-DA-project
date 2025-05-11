read.csv("C:/Users/HafsaTabbsum/OneDrive - National College of Ireland/Desktop/project computing 2024-25/Boston Crimes Prediction dataset.xlsx")
library(dplyr)
library(ggplot2)
library(readxl)
library(nnet)
library(forecast)
library(lubridate)
colnames(Boston_Crimes_Prediction_dataset)[colnames(Boston_Crimes_Prediction_dataset)=="CRIME&Types"]="Crime"
colnames(Boston_Crimes_Prediction_dataset)[colnames(Boston_Crimes_Prediction_dataset)=="MONTH"]="Month"
colnames(Boston_Crimes_Prediction_dataset)[colnames(Boston_Crimes_Prediction_dataset)=="YEAR"]="Year"
colnames(Boston_Crimes_Prediction_dataset)[colnames(Boston_Crimes_Prediction_dataset)=="NEIGHBOURHOOD"]="Neighbourhood"
print(colnames(Boston_Crimes_Prediction_dataset))
Boston_Crimes_Prediction_dataset = Boston_Crimes_Prediction_dataset %>%
  mutate(
    Crime = trimws(as.character(Crime)),
    Neighbourhood = trimws(as.character(Neighbourhood)),
    Year = as.integer(trimws(as.character(Year))),
    Month= as.integer(trimws(as.character(Month)))
  ) %>%
  filter(Crime != "",Neighbourhood != "",!is.na(Year),!is.na(Month))
nrow(Boston_Crimes_Prediction_dataset)

  Crime_model=multinom(Crime~Month+Year,data = Boston_Crimes_Prediction_dataset)
summary(Crime_model)
predicted_crimes=predict(Crime_model,newdata=Boston_Crimes_Prediction_dataset)
table(predicted=predicted_crimes,actual=Boston_Crimes_Prediction_dataset$Crime)
mean(predicted_crimes==Boston_Crimes_Prediction_dataset$Crime)
Boston_Crimes_Prediction_dataset$predicted=predicted_crimes
ggplot(Boston_Crimes_Prediction_dataset,aes(x=predicted,fill = predicted))+
  geom_bar()+
  theme_light()+
  labs(title = "amount of predicted Crime Type",x="Crime Type predicted",y="count")+
  theme(axis.text.x= element_text(angle = 40,hjust = 1))