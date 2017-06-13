#06_Figure_1
#Purpose: Create Manuscript Figure 1 (Timeseries of water potential, climate, and Vcmax/Jmax)
#Written by: Mallory Barnes
#June 2017

library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(grid)
library(tidyr)
library(reshape2)
library(gtable)
library(signal)
library(plsropt)
library(pls)
library(gridExtra)
library(outliers)

#Figure 1------------------------------------------------------------
Plot_data <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data_3_6_2017.csv")
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
#Figure 1: Multipanel stacked figure; climate/physiology
#Climate data:
#1) Dual-axis with VPD and precip
#2) VPD Timeseires
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




#Now for the climate data: Want a free axis graph with precip and temp and then another with VPD (bars)
read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/plot_climate.csv")
str(plot_climate)

par(mar = c(5,8,2,5))
par(new=T)
rect(as.Date("2016-06-02", "%Y-%m-%d"), 0, as.Date("2016-06-07", "%Y-%m-%d"), 50, col = "lightblue")
rect(as.Date("2016-06-19", "%Y-%m-%d"), 0, as.Date("2016-06-20", "%Y-%m-%d"), 50, col = "lightblue")

par(new=T)
with(plot_climate, plot(Date, Temp, type="l", col="firebrick", 
             ylab="Temp (C)",
             ylim=c(20,40)))

par(new = T)
with(plot_climate, plot(Date, VPD, type="l", col="mediumvioletred", axes=F, xlab=NA, ylab=NA, cex=1)) 
axis(side = 4)
mtext(side = 4, line = 1.5, "VPD (KPa)")

par(new=T)
with(plot_climate, barplot(Precip, col="blue", axes=F, xlab=NA, ylab=NA, ylim=c(0,40)))
axis(side=2, ylim=c(0, 40), lwd=1, line=3)
mtext(side=2, line=4, "Precip (mm)")


#For the boxplots: 

#Physiological data
#Water Potential Data 
str(Plot_data)


w <- ggplot(Plot_data, aes(x = Date, y = Water_Pot, 
                   group = (Date))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Water Potential (MPa)")+
  geom_boxplot(width=1)+
  geom_point(size=1, position = position_jitter(width = 0.2))+theme(axis.title.x=element_blank(),
                                                 axis.text.x=element_blank(),
                                                 axis.ticks.x=element_blank())


v <- ggplot(Plot_data, aes(x = Date, y = Vcmax, 
                      group = (Date))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Vcmax")+
  geom_boxplot(width=1)+
  geom_point(size=1, position = position_jitter(width = 0.2))+theme(axis.title.x=element_blank(),
                                                            axis.text.x=element_blank(),
                                                            axis.ticks.x=element_blank())

j <- ggplot(Plot_data, aes(x = Date, y = Jmax, 
                      group = (Date))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Jmax")+
  geom_boxplot(width=1)+
  geom_point(size=1, position = position_jitter(width = 0.2))


grid.newpage()
grid.arrange(w,v,j, ncol=1, heights=c(2,3,5))

grid.draw(rbind(ggplotGrob(w2), ggplotGrob(v2), ggplotGrob(j2), size = "last"))





#draw it!
grid.arrange(p, p2, w2, v2, j2, ncol=1, heights=c(2,2,3,5,5.5))
grid.arrange(vpd, w2, v2, j2, ncol=1, heights=c(3,3,5.5, 5.5))
grid.arrange(p1, w2, v2, j2, ncol=1, heights=c(3,3,5.5, 5.5))

grid.draw(rbind(ggplotGrob(vpd), ggplotGrob(p2), ggplotGrob(p1), size = "last"))
grid.draw(rbind(ggplotGrob(vpd), ggplotGrob(g), size = "last"))
grid.draw(rbind(ggplotGrob(vpd), ggplotGrob(p2), ggplotGrob(p0), ggplotGrob(j2), ggplotGrob(v2), ggplotGrob(w2), size = "last"))
grid.draw(rbind(ggplotGrob(vpd), ggplotGrob(p2), ggplotGrob(p0), ggplotGrob(jl2), ggplotGrob(vl2), ggplotGrob(wl2), size = "last"))
  
