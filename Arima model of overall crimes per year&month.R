read.csv("C:/Users/HafsaTabbsum/OneDrive - National College of Ireland/Desktop/project computing 2024-25/Boston Crimes Prediction dataset.xlsx")
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)
library(forecast)
colnames(Boston_Crimes_Prediction_dataset)[colnames(Boston_Crimes_Prediction_dataset)=="CRIME&Types"]="Crime"
Boston_Crimes_Prediction_dataset=Boston_Crimes_Prediction_dataset%>%
  filter(!is.na(YEAR),!is.na(MONTH))%>%
  mutate(
    MONTH=as.integer(MONTH),
          YEAR=as.integer(YEAR),
          Date=make_date(YEAR,MONTH,1))
min(Boston_Crimes_Prediction_dataset$YEAR)
min(Boston_Crimes_Prediction_dataset$MONTH)
summary(Boston_Crimes_Prediction_dataset$YEAR)
summary(Boston_Crimes_Prediction_dataset$MONTH)
monthly_crimes =Boston_Crimes_Prediction_dataset%>%
  group_by(Date)%>%
  summarise(Crime_Count=n())%>%
  arrange(Date)
Crime_ts=ts(monthly_crimes$Crime_Count,frequency = 12,start = c(min(Boston_Crimes_Prediction_dataset$YEAR),
                                                               min (Boston_Crimes_Prediction_dataset$MONTH)))
autoplot(Crime_ts)+
  labs(title = "total crimes monthly in USA Boston", y="Crime Numbers", x="time")
            crime_arima=auto.arima(Crime_ts)
            summary(crime_arima)
   future_forecast=forecast(crime_arima,h=12)   
   autoplot(future_forecast)+
     labs(title = "further 12 months crimes predicted",y="crimes predicted")
   forecast(crime_arima,h=24)
   autoplot(future_forecast)+
     labs(title="futher 2 year crimes predicted",y="crimes predicted")
   
   
            