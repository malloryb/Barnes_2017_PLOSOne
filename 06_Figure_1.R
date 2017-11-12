#06_Figure_1
#Purpose: Create Manuscript Figure 1 (Timeseries of water potential, climate, and Vcmax/Jmax)
#Written by: Mallory Barnes
#June 2017
#Input: All cleaned data -> physiology, water potential, and climate data 
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
library(gridBase)

#This figure is very complicated and utilizes both Base R and ggplot graphics to get the 
#Desired appearance
grid.newpage()
#Figure 1------------------------------------------------------------
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

#Figure 1: Multipanel stacked figure; climate/physiology
#Now for the climate data: Want a free axis graph with precip and temp and then another with VPD (bars)
plot_climate <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Barnes_PLOSOne_Poplar/plot_climate.csv")
plot_climate$Date <- as.Date(plot_climate$Date)
plot_climate <- subset(plot_climate, Date > "2016-05-23")


dev.off()


w <- ggplot(Plot_data, aes(x = Date, y = Water_Pot, 
                   group = (Date))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),  axis.title.y= element_text(hjust=0.5, size=14), axis.text.y = element_text(colour="grey20",size=8, hjust=0.2))+
  ylab(expression(paste(psi[pd])))+
  geom_boxplot(width=1)+
  theme(plot.margin= unit(c(0.05,0.30,0.05,0.10), "inches"))+
  geom_point(size=1, position = position_jitter(width = 0.2))+theme(axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank(),
                                                                    panel.background = element_blank())+
        theme(axis.line.x = element_line(color="black", size = 0.5),
              axis.line.y = element_line(color="black", size = 0.5))


v <- ggplot(Plot_data, aes(x = Date, y = Vcmax, 
                      group = (Date))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0),  axis.title.y= element_text(hjust=0.3, size=12), axis.text.y = element_text(colour="grey20",size=10))+
  ylab("Vcmax")+
  geom_boxplot(width=1)+
  theme(plot.margin= unit(c(0.05,0.30,0.05,0.15), "inches"))+
  geom_point(size=1, position = position_jitter(width = 0.2))+theme(axis.title.x=element_blank(),
                                                            axis.text.x=element_blank(),
                                                            axis.ticks.x=element_blank(),
                                                            panel.background = element_blank())+
        theme(axis.line.x = element_line(color="black", size = 0.5),
              axis.line.y = element_line(color="black", size = 0.5))

j <- ggplot(Plot_data, aes(x = Date, y = Jmax, 
                      group = (Date))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),axis.title.y= element_text(hjust=0.5, size=12), axis.text.y = element_text(colour="grey20",size=10))+
  ylab("Jmax")+
  #ylab(bquote('Jmax ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))+
  geom_boxplot(width=1)+
  theme(plot.margin= unit(c(0.05,0.30,0.05,0.15), "inches"))+
  geom_point(size=1, position = position_jitter(width = 0.2))+ theme(panel.background=element_blank())+
        theme(axis.line.x = element_line(color="black", size = 0.5),
              axis.line.y = element_line(color="black", size = 0.5))
#graphics.off()
plot.new()
grid.newpage()
dev.new(width=8, height=12)
pushViewport(viewport(layout=grid.layout(4,1, heights=unit(c(3,3,3,4.5), c("cm", "cm", "cm", "cm")))))

pushViewport(viewport(layout.pos.row=2))
print(w, newpage=FALSE)
popViewport()


pushViewport(viewport(layout.pos.row=3))
print(v, newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=4))
print(j, newpage=FALSE)
popViewport()


pushViewport(viewport(layout.pos.row=1))
par(fig=gridFIG(), new=TRUE)
par(mai = c(0.2,0.7,0.05,0.5), tck=0.04, mgp=c(1.5,0,0))
par(bty="n")
par(new=T)

with(plot_climate, plot(Date, Temp, axes=FALSE, type="l", ylab='',cex.axis=0.75, col="orangered", ylim=c(20,40)), xlab=NA)
axis.Date(side = 3, plot_climate$Date, format = "%b %d", pos=42, cex.axis=0.75)
axis(2, cex.axis=0.75)
mtext(side=2, cex=0.86, line=0.8, "Temp (C)")

rect(as.Date("2016-06-02", "%Y-%m-%d"), 0, as.Date("2016-06-07", "%Y-%m-%d"), 40, col = "lightblue")
rect(as.Date("2016-06-19", "%Y-%m-%d"), 0, as.Date("2016-06-20", "%Y-%m-%d"), 40, col = "lightblue")
par(new=T)
with(plot_climate, plot(Date, Temp, type="l", ylab='', axes=FALSE, col="orangered", ylim=c(20,40)), xlab=NA, ylab=NA)
par(new = T)
with(plot_climate, plot(Date, VPD, type="l", col="darkorchid2", axes=F, xlab=NA, ylab=NA)) 
axis(side = 4, cex.axis=0.75)
mtext(side = 4, cex=0.86, cex.lab=0.5, line = 1.2, "VPD (KPa)", cex.axis=0.75)
par(new=T)
with(plot_climate, barplot(Precip, col="blue", axes=F, ylab='', xlab=NA, ylim=c(0,40)))
axis(side=2, ylim=c(0, 40), lwd=1, line=1.8, cex.axis=0.75)
mtext(side=2, line=2.4, cex=0.86, "Precip (mm)")


#tiff("Fig_1_tryit.TIFF")
#pushViewport(viewport())
#popViewport()
dev.print(tiff, 'figure_1_again.TIFF', width=5, height=6, unit="in", res=300)
#Then manually resize in paint.net :-/ 
dev.off()

#ggsave("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Barnes_PLOSOne_Poplar/Revised_9_2017/Submitted_Docs/Fig1_3.TIFF", dpi=400)

