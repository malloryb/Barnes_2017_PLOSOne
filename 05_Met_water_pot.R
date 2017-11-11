#05_Met_water_pot
#Purpose: Process meterological data and prepare water potential timeseries
#Written by: Mallory Barnes
#June 2017

library(plyr)

Plot_data <- read.csv("C:/Users/rsstudent/Dropbox (Personal)/Drought_Expt_2016/all_data_3_6_2017.csv")
str(Plot_data)
#Cleanup data file:
#Format Dates properly: 
Plot_data$Date <- as.Date(Plot_data$Date.x, "%m/%d/%Y")
#Delete extraneous columns: 
Plot_data <- subset(Plot_data, select=-c(X.2, X.3, X.1, X, Plant_ID.x, Date.x, Date.y))
str(Plot_data)

#Plot_data$VPD_scale <- rescale
#Remove 5-20 Observation as it's the only one that day: 
Plot_data <- Plot_data[-c(38),]

Climate_data <- read.csv("C:/Users/Mallory/Dropbox/Summer_2016_Drought_Experiment/Cleaned_Table.csv")
head(Climate_data)
str(Climate_data)
#Custom function for pulling x number of characters from the right of a string: from 
#http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

Climate_data$TOD <- substrRight(as.character(Climate_data$TIMESTAMP), 5)
Climate_data$Date <- as.Date(substr(as.character(Climate_data$TIMESTAMP),1, 9), "%m/%d/%Y")
str(Climate_data)
#Get values for results section------------------------------------
#Get average maximum and minimum daily temperature
Climate_data_study <- subset(Climate_data, Date >= as.Date("2016-05-24", "%Y-%m-%d") & Date <= as.Date("2016-07-07", "%Y-%m-%d"))
str(Climate_data_study)
tempbydate <- ddply(Climate_data_study, .(Date), summarise, maxtemp=max(AirTC_Avg), mintemp=min(AirTC_Avg), precip=sum(Rain_mm_Tot))
mean(tempbydate$maxtemp)
mean(tempbydate$mintemp)
sum(tempbydate$precip)

#Need sum of precip, and average daytime temp (06:00 to 18:00 hours)
#Sum precip by date; 
str(precip)
precip <- ddply(Climate_data, .(Date), summarise, Precip = sum(Rain_mm_Tot))
#To get daytime temp: 
daytime_temp <-function(x){
  x$hr <-as.numeric(substr(x$TOD, 1,2))
  print(x$hr)
  daytime <- x[ which( x$hr > 06 & x$hr < 18) , ]
  print(daytime)
  return(ddply(daytime, .(Date), summarise, Temp=mean(AirTC_Avg)))
} 
temp <- daytime_temp(Climate_data)
#To get VPD (Vapor pressure deficit) in kPA
#Doing midday VPD to assess atmospheric demand: from 10:00am 2:00pm (10:00 to 14:00 hrs)
get_VPD <- function(x){
  x$hr <-as.numeric(substr(x$TOD, 1,2))
  temp <- x$AirTC_Avg
  RH <- x$RH
  SVP <- 610.7*10^(7.5*temp/(237.3+temp))
  x$VPD <- (((100 - RH)/100)*SVP)/1000
  print(x$VPD)
  midday <- x[ which( x$hr > 10 & x$hr < 14) , ]
  return(ddply(midday, .(Date), summarise, VPD=mean(VPD)))
}

VPD <- get_VPD(Climate_data)
plot_climate <- merge(merge(temp, VPD, by="Date"), precip)

#Get VPD/temp/Vcmax/Jmax/Water_Potential values for results section---------------
str(plot_climate)
summary(plot_climate)
sd(plot_climate$Temp)
sd(plot_climate$VPD)

plot_climate
sum(plot_climate$Precip)

summary(Plot_data)
sd(Plot_data$Vcmax)
sd(Plot_data$Jmax)
sd(Plot_data$Water_Pot)

