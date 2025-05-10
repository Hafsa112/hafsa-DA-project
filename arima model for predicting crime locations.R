read.csv("C:/Users/HafsaTabbsum/OneDrive - National College of Ireland/Desktop/project computing 2024-25/Boston Crimes Prediction dataset.xlsx")
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)
library(forecast)
colnames(Boston_Crimes_Prediction_dataset)[colnames(Boston_Crimes_Prediction_dataset)=="CRIME&Types"]="Crime"
Boston_Crimes_Prediction_dataset=Boston_Crimes_Prediction_dataset%>%
  filter(!is.na(Crime),!is.na(YEAR),!is.na(MONTH),!is.na(NEIGHBOURHOOD))%>%
  mutate(
    YEAR=as.integer(YEAR),
    MONTH=as.integer(MONTH),
    Date=as.Date(paste(YEAR,MONTH,1,sep="-")),
  NEIGHBOURHOOD=as.factor(NEIGHBOURHOOD))
#crime location area of Roxbury
  selected_Neighbourhood="Roxbury"
  NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
    filter(NEIGHBOURHOOD=="Roxbury")%>%
    group_by(Date)%>%
    summarise(crime_count=n())%>%
    arrange(Date)
  Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                  month(min(NEIGHBOURHOOD_data$Date))))
  forecast=forecast(fit,h=12)
    fit=auto.arima(Crime_ts)
    autoplot(forecast)+
      ggtitle(paste("predicted further 12 month crimes in ","Roxbury"))+
      ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24)
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in ","Roxbury"))+
        ylab("Total Crimes")
      xlab("Timeline")
      #crime location area of Dorchester 
   selected_Neighbourhood="Dorchester"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="Dorchester")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24)
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in" ,"Dorchester"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12)
       fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in" ,"Dorchester"))+
        ylab("Total Crimes")
      xlab("Timeline")
     
      #crime location area of Beacon Hill
      selected_Neighbourhood="Beacon Hill"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="Beacon Hill")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
                              
      forecast=forecast(fit,h=12)                                                                                        
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in ","Beacon Hill"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24)
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in" ,"Beacon Hill"))+
        ylab("Total Crimes")
      xlab("Timeline")
      #crime location area of East Boston
      selected_Neighbourhood="East Boston"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="East Boston")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
     forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," East Boston"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in "," East Boston"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      #crime area of South End 
      selected_Neighbourhood="South End"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="South End")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," South End"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in "," South End"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      #crime location of South Boston
      selected_Neighbourhood="South Boston"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="South Boston")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," South Boston"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year Crimes in "," South Boston"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      #crime location of Brighton
      selected_Neighbourhood="Brighton"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="Brighton")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," Brighton"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in "," Brighton"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      #crime location of West End 
      selected_Neighbourhood="West End"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="West End")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," West End"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 Year crimes in "," West End"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      #crime location of Mattapan
      selected_Neighbourhood="Mattapan"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="Mattapan")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," Mattapan"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 Year crimes in "," Mattapan"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      #crime location of Back Bay
      selected_Neighbourhood="Back Bay"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="Back Bay")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," Back Bay"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in "," Back Bay"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      #crime location of North End
      selected_Neighbourhood="North End"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="North End")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," North End"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in "," North End"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      #crime location of Roslindale
      selected_Neighbourhood="Roslindale"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="Roslindale")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," Roslindale"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in "," Roslindale"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      #crime location of chinatown
      selected_Neighbourhood="China TOwn"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="Chinatown")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," China Town"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in "," China Town"))+
        ylab("Total Crimes")
      xlab("Timeline")
      #crime location of Down Town
      selected_Neighbourhood="Down Town"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="Downtown")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," Down Town"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in "," Down Town"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      #crime location of Hyde park
      selected_Neighbourhood="Hyde Park"
      NEIGHBOURHOOD_data=Boston_Crimes_Prediction_dataset%>%
        filter(NEIGHBOURHOOD=="Hyde Park")%>%
        group_by(Date)%>%
        summarise(crime_count=n())%>%
        arrange(Date)
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 12,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=12) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 12 month crimes in "," Hyde Park"))+
        ylab("Total Crimes")
      xlab("Timeline")
      Crime_ts=ts(NEIGHBOURHOOD_data$crime_count,frequency = 24,start = c(year(min(NEIGHBOURHOOD_data$Date)),
                                                                          month(min(NEIGHBOURHOOD_data$Date))))
      forecast=forecast(fit,h=24) 
      fit=auto.arima(Crime_ts)
      autoplot(forecast)+
        ggtitle(paste("predicted further 2 year crimes in "," Hyde Park"))+
        ylab("Total Crimes")
      xlab("Timeline")
      
      
                                                                  
  
  
  