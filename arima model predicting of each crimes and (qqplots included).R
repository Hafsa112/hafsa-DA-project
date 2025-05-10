read.csv("C:/Users/HafsaTabbsum/OneDrive - National College of Ireland/Desktop/project computing 2024-25/Boston Crimes Prediction dataset.xlsx")
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)
library(forecast)
library(stringr)
Boston_Crimes_Prediction_dataset=Boston_Crimes_Prediction_dataset%>%
  rename(Crime=`CRIME&Types`)%>%
  mutate( Crime=str_trim(str_to_upper(Crime)),
          NEIGHBOURHOOD=str_trim(str_to_upper(NEIGHBOURHOOD)),
          MONTH=as.integer(MONTH),
          YEAR=as.integer(YEAR),
          Date=make_date(YEAR,MONTH,1))
#crimes of murder and death
Murder_Crimes=c("DEATH INVESTIGATION","NON-NEGLIGENT MANSLAUGHTER","SUDDEN DEATH")
 murder_data=Boston_Crimes_Prediction_dataset%>%
filter(Crime%in%Murder_Crimes)%>%
  mutate(crime_group="MURDER AND DEATH")
monthly_murder=murder_data%>%
  group_by(Date)%>%
  summarise(crime_count=n())%>%
  arrange(Date)
start_year=year(min(monthly_murder$Date))
start_month=month(min(monthly_murder$Date))
murder_ts=ts(monthly_murder$crime_count,frequency = 12,start = c(start_year,start_month))
murder_forecast=forecast(murder_model,h=12)
murder_model=auto.arima(murder_ts)
autoplot(murder_forecast)+
  labs(title = "predicted crimes of murder&death of further 12 months",y="count of crime",x="time")
murder_forecast=forecast(murder_model,h=24)
autoplot(murder_forecast)+
  labs(title = "predicted crimes of murder&death of further 2 Years",y="count of crime",x="time")

#QQPLOTS of murder and death
murder_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%Murder_Crimes)
qqnorm(table(murder_data$Crime),main = "crimes of murder and death QQPLOT")
qqline(table(murder_data$Crime),col="red",lwd=1)

#crimes of sexual assaults
Sexual_Crimes=c("FAILURE TO REGISTER AS A SEX OFFENDER","SEXUAL OFFENSE-OTHER","SEX OFFENSE-RAPE-FORCIBLE&SODOMY",
"SEXUAL ASSAULT INVESTIGATION","SEXUAL ASSAULT KIT COLLECTED","SEXUAL ASSAULT-FONDLING")
Sexual_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%Sexual_Crimes)%>%
  mutate(crime_group="SEX OFFENSE AND ASSAULT")
monthly_Sexual=Sexual_data%>%
  group_by(Date)%>%
  summarise(crime_count=n())%>%
  arrange(Date)
start_year=year(min(monthly_Sexual$Date))
start_month=month(min(monthly_Sexual$Date))
Sexual_ts=ts(monthly_Sexual$crime_count,frequency = 12,start = c(start_year,start_month))
Sexual_forecast=forecast(Sexual_model,h=12)
Sexual_model=auto.arima(Sexual_ts)
autoplot(Sexual_forecast)+
  labs(title = "predicted crimes of Sexual Assaults of further 12 months",y="count of crime",x="time")
Sexual_forecast=forecast(Sexual_model,h=24)
autoplot(Sexual_forecast)+
  labs(title = "predicted crimes of sexual Assaults of further 2 Years",y="count of crime",x="time")
#QQPLOT OF SEXUAL ASSAULTS
Sexual_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%Sexual_Crimes)
qqnorm(table(Sexual_data$Crime),main = "crimes of sexual Assault QQPLOT")
qqline(table(Sexual_data$Crime),col="red",lwd=1)

#crimes of Fraud
Fraud_Crimes=c("CREDIT CARD/ATM FRAUD","FALSE/PRETENSE SCHEME","WELFARE FRAUD",
"WIRE FRAUD",	"IDENTITY FRAUD")
Fraud_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%Fraud_Crimes)%>%
  mutate(crime_group="Fraud")
monthly_fraud=Fraud_data%>%
  group_by(Date)%>%
  summarise(crime_count=n())%>%
  arrange(Date)
start_year=year(min(monthly_fraud$Date))
start_month=month(min(monthly_fraud$Date))
Fraud_ts=ts(monthly_fraud$crime_count,frequency = 12,start = c(start_year,start_month))
Fraud_model=auto.arima(Fraud_ts)
Fraud_forecast=forecast(Fraud_model,h=12)
autoplot(Fraud_forecast)+
  labs(title = "predicted crimes of different Frauds of further 12 months",y="count of crime",x="time")
theme_light()
fraud_forecast=forecast(Fraud_model,h=24)
autoplot(fraud_forecast)+
  labs(title = "predicted crimes of different Frauds of further 2 Years",y="count of crime",x="time")
#QQ plot of Fraud
Fraud_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%Fraud_Crimes)
qqnorm(table(Fraud_data$Crime),main = "crimes of Fraud QQPLOT")
qqline(table(Fraud_data$Crime),col="red",lwd=1)

#crimes of larceny theft&burglary
larceny_Crimes=c("	
PURSE SNATCH-NOFORCE","	
SHOPLIFTING","THEFT FROM BUILDING","LARCENY ALL OTHERS","COMMERCIAL BURGLARY",
                 "RESEDENTIAL BURGLARY","ROBBERY","POSSESSION OF BURGLARIOUS TOOLS", 
                 "AUTO THEFT","LEASED/RENTED VEHICLE","MOTORCYCLE/SCOOTER",
                 "AUTO THEFT RECOVERY","BICYCLE","MOTOR VEHCILE PARTS AND ACCESSORIES",
                 "TOWED MV")
larceny_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%larceny_Crimes)%>%
  mutate(crime_group="larceny")
monthly_larceny=larceny_data%>%
  group_by(Date)%>%
  summarise(crime_count=n())%>%
  arrange(Date)
start_year=year(min(monthly_larceny$Date))
start_month=month(min(monthly_larceny$Date))
larceny_ts=ts(monthly_larceny$crime_count,frequency = 12,start = c(start_year,start_month))
larceny_model=auto.arima(larceny_ts)
larceny_forecast=forecast(larceny_model,h=12)
autoplot(larceny_forecast)+
  labs(title = "predicted crimes of larceny Theft and Burglary of further 12 months",y="count of crime",x="time")
theme_light()
larceny_forecast=forecast(larceny_model,h=24)
autoplot(larceny_forecast)+
  labs(title = "predicted crimes of larceny Theft and Burgulary of further 2 Years",y="count of crime",x="time")
#qqplot of larceny Theft&Burgulary
larceny_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%larceny_Crimes)
qqnorm(table(larceny_data$Crime),main = "crimes of Larceny Theft&Burgulary QQPLOT")
qqline(table(larceny_data$Crime),col="red",lwd=1)

#crimes of MV(motor vehicles) accidents
  MV_accident_Crimes=c("PERSONAL INJURY","PROPERTY DAMAGE","INVOLVING BICYCLE-INJURY",
  "INVOLVING BICYCLE-NO INJURY","INVOLVING-PEDESTRIAN NO INJURY","OTHER",
"INVOLVING-PEDESTRIAN INJURY","TOWED MV","PERSONAL INJURY","POLICE VEHICLE",
"M/V PLATES-LOST","VANDALISM","M/V ACCIDENT OTHER CITY VEHICLE")
MV_accident_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%MV_accident_Crimes)%>%
  mutate(crime_group="MV accidents")
monthly_MV_accident=MV_accident_data%>%
  group_by(Date)%>%
  summarise(crime_count=n())%>%
  arrange(Date)
start_year=year(min(monthly_MV_accident$Date))
start_month=month(min(monthly_MV_accident$Date))
MV_accident_ts=ts(monthly_MV_accident$crime_count,frequency = 12,start = c(start_year,start_month))
MV_accident_forecast=forecast(MV_accident_model,h=12)
MV_accident_model=auto.arima(MV_accident_ts)
autoplot(MV_accident_forecast)+
  labs(title = "predicted crimes of Accidents relating to Motor Vehicles of further 12 Months",y="count of crime",x="time")
MV_accident_forecast=forecast(MV_accident_model,h=24)
autoplot(MV_accident_forecast)+
  labs(title = "predicted crimes of Accidents related to Motor Vehicles of further 2 Years",y="count of crime",x="time")
#QQplots of M/V Accidents
MV_accident_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%MV_accident_Crimes)
qqnorm(table(MV_accident_data$Crime),main = "crimes of MV Accidents QQPLOT")
qqline(table(MV_accident_data$Crime),col="blue",lwd=1)

#crimes of violations
Violation_Crimes=c("ASSEMBLY OR GATHERING VIOLATIONS","HARBOUR INIDENT/VIOLATION",
                   "LICENSE PREMISE VIOLATION","LIQUOR LAW VIOLATION","RESTRAINING ORDERS VIOLATION(NO ARREST)",
                   "VIOLATION OF AUTO LAW","CITY ORDINANCE-VIOLATION","WEAPON VIOLATION-CARRY/POCESSING/SALE/TRAFFIC",
                   "RESTRAINING ORDERS VIOLATION( WITH  ARREST")
Violation_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%Violation_Crimes)%>%
  mutate(crime_group="VIOLATIONS")
monthly_Violation=MV_accident_data%>%
  group_by(Date)%>%
  summarise(crime_count=n())%>%
  arrange(Date)
start_year=year(min(monthly_Violation$Date))
start_month=month(min(monthly_Violation$Date))
Violation_ts=ts(monthly_Violation$crime_count,frequency = 12,start = c(start_year,start_month))
Violation_model=auto.arima(Violation_ts)
Violation_forecast=forecast(Violation_model,h=12)
autoplot(Violation_forecast)+
  labs(title = "predicted crimes of VIOLATIONS of further 12 Months",y="count of crime",x="time")
Violation_forecast=forecast(Violation_model,h=24)
autoplot(Violation_forecast)+
  labs(title = "predicted crimes of VIOLATIONS of further 2 Years",y="count of crime",x="time")
#QQ plots of Violations
Violation_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%Violation_Crimes)
  qqnorm(table(Violation_data$Crime),main = "crimes of Violations QQPLOT")
qqline(table(Violation_data$Crime),col="red",lwd=1)

#crimes of assault&drugs
Assault_Drug_Crimes=c("AGGRAVATED","SIMPLE","POSSESSION OF DRUG PARAPANALIA",
             "DRUGS-POSSESION/SALE/MANUFACTURING/USE","VERBAL DISPUTE","CHILD ENDANGERGMENT",
             "KIDNAPPING/CUSTODIAL KIDNAPPING/ABDUCTION")
Assault_Drug_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%Assault_Drug_Crimes)%>%
  mutate(crime_group="Assault&Drugs")
monthly_Assault_Drug=Assault_Drug_data%>%
  group_by(Date)%>%
  summarise(crime_count=n())%>%
  arrange(Date)
start_year=year(min(monthly_Assault_Drug$Date))
start_month=month(min(monthly_Assault_Drug$Date))
Assault_Drug_ts=ts(monthly_Assault_Drug$crime_count,frequency = 12,start = c(start_year,start_month))
Assault_Drug_model=auto.arima(Assault_Drug_ts)
Assault_Drug_forecast=forecast(Assault_Drug_model,h=12)
autoplot(Assault_Drug_forecast)+
  labs(title = "predicted crimes of Assault and Drugs of further 12 Months",y="count of crime",x="time")
Assault_Drug_forecast=forecast(Assault_Drug_model,h=24)
autoplot(Assault_Drug_forecast)+
  labs(title = "predicted crimes of Assault and Drugs of further 2 Years",y="count of crime",x="time")
#QQ plot of Assualt&Drugs
Assault_Drug_data=Boston_Crimes_Prediction_dataset%>%
  filter(Crime%in%Assault_Drug_Crimes)
qqnorm(table(Assault_Drug_data$Crime),main = "crimes of Assaults and drugs QQPLOT")
qqline(table(Assault_Drug_data$Crime),col="red",lwd=1)
#normal overall QQ plots of dataset
Boston_Crimes_Prediction_dataset=Boston_Crimes_Prediction_dataset%>%
  qqnorm(table(Boston_Crimes_Prediction_dataset$Crime),main = "overall crimes QQPLOT")
qqline(table(Boston_Crimes_Prediction_dataset$Crime),col="red",lwd=1)

qqnorm(table(Boston_Crimes_Prediction_dataset$Crime),main = "year with crimes QQPLOT")
qqline(table(Boston_Crimes_Prediction_dataset$YEAR),col="blue",lwd=1)

qqnorm(table(Boston_Crimes_Prediction_dataset$Crime),main = "month with crimes QQPLOT")
qqline(table(Boston_Crimes_Prediction_dataset$MONTH),col="red",lwd=1)

qqnorm(table(Boston_Crimes_Prediction_dataset$Crime),main = " neighbourhood with crimes QQPLOT")
qqline(table(Boston_Crimes_Prediction_dataset$NEIGHBOURHOOD),col="red",lwd=1)

qqnorm(table(Boston_Crimes_Prediction_dataset$YEAR),main = "year column crimes QQPLOT")
qqline(table(Boston_Crimes_Prediction_dataset$YEAR),col="red",lwd=1)

qqnorm(table(Boston_Crimes_Prediction_dataset$MONTH),main = "month column crime QQPLOT")
qqline(table(Boston_Crimes_Prediction_dataset$MONTH),col="blue",lwd=1)

qqnorm(table(Boston_Crimes_Prediction_dataset$NEIGHBOURHOOD),main = "neighbourhood column crimes QQPLOT")
qqline(table(Boston_Crimes_Prediction_dataset$NEIGHBOURHOOD),col="red",lwd=1)











