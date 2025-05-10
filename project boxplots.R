library(tidyverse)
library(ggplot2)
library(caret)  
library(MASS)
data <- read.csv("C:/Users/HafsaTabbsum/OneDrive - National College of Ireland/Desktop/Boston crimes dataset.csv")
view("Boston crimes dataset")
ggplot(Boston_crimes_dataset,aes(x=Year...2,
                                  y=`CRIME&Types...1`))+
  geom_point()
ggplot(Boston_crimes_dataset,aes(x=Year...2,
                                 y=`CRIME&Types...1`))+
geom_boxplot()+
  ggtitle("yearly Fraud crimes Boxplot")
xlab=("year")
ylab("crimes")
geom_smooth()

ggplot(Boston_crimes_dataset,aes(x=Month...3,
                                  y=`CRIME&Types...1`))+
  geom_point()
ggplot(Boston_crimes_dataset,aes(x=Month...3,
                                 y=`CRIME&Types...1`))+
geom_boxplot()+
  ggtitle("monthly Fraud crimes Boxplot")
xlab=("month")
ylab("crimes")
geom_smooth()

ggplot(Boston_crimes_dataset,aes(x=Year...7,
                                  y=`CRIME&Types...5`))+
  geom_point()
ggplot(Boston_crimes_dataset,aes(x=Year...7,
                                 y=`CRIME&Types...5`))+
geom_boxplot()+
  ggtitle( "Yearly Sexual Assault crimes Boxplot" )
xlab=("year")
ylab("crimes")
geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Month...8,
                                    y=`CRIME&Types...5`))+
  geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Month...8,
                                   y=`CRIME&Types...5`))+
  geom_boxplot()+
    ggtitle( "monthly Sexual Assault crimes Boxplot" )
  xlab=("month")
  ylba=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Year...13,
                                    y=`CRIME&Types...11`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Year...13,
                                   y=`CRIME&Types...11`))+
    geom_boxplot()+
    ggtitle(" yearly Death & Murder crimes Boxplot")
  xlab=("year")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Month...14,
                                    y=`CRIME&Types...11`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Month...14,
                                   y=`CRIME&Types...11`))+
    geom_boxplot()+
    ggtitle("monthly Death and Murder crimes Boxplot")
  xlab=("year")+
    ylab=("crime")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Year...18,
                                    y=`CRIME&Types...17`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Year...18,
                                   y=`CRIME&Types...17`))+
    geom_boxplot()+
    ggtitle("Auto Theft yearly crimes Boxplot")
  xlab=("year")+
    ylab=("crimes")
  
  ggplot(Boston_crimes_dataset,aes(x=Month...19,
                                    y=`CRIME&Types...17`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Month...19,
                                   y=`CRIME&Types...17`))+
    geom_boxplot()+
    ggtitle("Auto Theft Monthly crimes Boxplot")
  xlab=("year")+
    ylab=("crimes")
  
  ggplot(Boston_crimes_dataset,aes(x=Year...24, 
                                    y=`CRIME&Types...22`))+
  geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Year...24, 
                                   y=`CRIME&Types...22`))+
  geom_boxplot()+
    ggtitle("larceny Theft& Burglary yearly Crimes Boxplot")
    xlab=("crimes")+
  ylab=("year")
    geom_smooth()
    
  ggplot(Boston_crimes_dataset,aes(x=Month...25,
                                    y=`CRIME&Types...22`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Month...25,
                                   y=`CRIME&Types...22`))+
    geom_boxplot()+
    ggtitle("larceny Theft & Burglary monthly Crimes Boxplot")
  xlab=("crimes")+
    ylab=("month")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Year...29,
                                    y=`CRIME&Types...28`))+
  geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Year...29,
                                   y=`CRIME&Types...28`))+
  geom_boxplot()+
    ggtitle("M/V Accident yearly crimes Boxplot")
    xlab=("crimes")+
    ylab=("year")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Month...30,
                                    y=`CRIME&Types...28`))+
  geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Month...30,
                                   y=`CRIME&Types...28`))+
  geom_boxplot()+
    ggtitle(" M/v Accident monthly crimes Boxplot")
  xlab=("month")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Year...33,
                                    y=`CRIME&Types...32`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Year...33,
                                   y=`CRIME&Types...32`))+
    geom_boxplot()+
    ggtitle("violation crimes yearly Boxplot")
  xlab=("year")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Month...34,
                                    y=`CRIME&Types...32`))+
  geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Month...34,
                                   y=`CRIME&Types...32`))+
  geom_boxplot()+
    ggtitle("violation crimes monthly Boxplot")
  xlab=("month")+
    ylab=("crimes")
  geom_smooth() 
  
  ggplot(Boston_crimes_dataset,aes(x=Year...37,
                                    y=`CRIME&Types...36`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Year...37,
                                   y=`CRIME&Types...36`))+
    geom_boxplot()+
    ggtitle("Assault & Drug crimes yearly Boxplot")
  xlab=("year")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Month...38,
                                    y=`CRIME&Types...36`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Month...38,
                                   y=`CRIME&Types...36`))+
  geom_boxplot()+
    ggtitle("Assault & Drugs monthly crimes Boxplot")
  xlab=("month")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...4,
                                    y=`CRIME&Types...1`))+
  geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...4,
                                   y=`CRIME&Types...1`))+
  geom_boxplot()
  ggtitle("location of Fraud crimes Boxplot")
  xlab=("location")+
    ylab=("crimes")
  geom_smooth()
   
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...9,
                                   y=`CRIME&Types...5`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...9,
                                   y=`CRIME&Types...5`))+
  geom_boxplot()
  ggtitle("location of Sex offense & Assault crimes Boxplot")
  xlab=("location")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...15,
                                   y=`CRIME&Types...11`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...15,
                                   y=`CRIME&Types...11`))+
  geom_boxplot()
  ggtitle("location of Murder & Death crimes Boxplot")
  xlab=("location")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...20,
                                   y=`CRIME&Types...17`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...20,
                                   y=`CRIME&Types...17`))+
  geom_boxplot()
  ggtitle("location of Auto Theft crimes Boxplot")
  xlab=("location")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...26,
                                   y=`CRIME&Types...22`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...26,
                                   y=`CRIME&Types...22`))+
  geom_boxplot()
  ggtitle("location of Larceny Theft & Burglary crimes Boxplot")
  xlab=("location")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...31,
                                   y= `CRIME&Types...28`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...31,
                                   y= `CRIME&Types...28`))+
  geom_boxplot()
  ggtitle("location of M/V Accident  crimes Boxplot")
  xlab=("location")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...35,
                                   y=`CRIME&Types...32`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...35,
                                   y=`CRIME&Types...32`))+
  geom_boxplot()
  ggtitle("location of Violation crimes Boxplot")
  xlab=("location")+
    ylab=("crimes")
  geom_smooth()
  
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...39,
                                   y=`CRIME&Types...36`))+
    geom_point()
  ggplot(Boston_crimes_dataset,aes(x=Neighbourhood...39,
                                   y=`CRIME&Types...36`))+
  geom_boxplot()
  ggtitle("location of Assault & Drug crimes Boxplot")
  xlab=("location")+
    ylab=("crimes")
  geom_smooth()
  
  
  
  
  
  
   


