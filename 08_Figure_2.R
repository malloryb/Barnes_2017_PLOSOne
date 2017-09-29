#08_Figure_2
#Purpose: Create Manuscript Figure 2 (PLSR estiamtes of Vcmax and Jmax: Predicted vs. observed)
#Written by: Mallory Barnes
#June 2017
#Input: Hyperspectral Long, Jmax PLSR results (stressed vs. nonstressed), Vcmax results (stressed vs. nonstresed)
#Output: Figure 3

library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)

#Input
hyperspectral_long <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/hyperspec_long.csv")
SR_Jmax <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_test_stressed_period/5comps(n=73)_test/selectivityRatio.csv")
SR_Vcmax <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_test_stressed_Period/2comps(n=73)_test/selectivityRatio.csv")

#Maniuplate reflectance to get mean, max and min for each wavelength
hyperspect_stats <- ddply(hyperspectral_long, .(wavelength), summarise, mean_ref=mean(reflectance, na.rm=TRUE), max_ref=max(reflectance,na.rm=TRUE), min_ref=min(reflectance,na.rm=TRUE))

#Most important wavelengths for each? 
SR_Jmax[which.max(SR_Jmax$x),]
SR_Vcmax[which.max(SR_Vcmax$x),]

#Figure 2: 
str(SR_Jmax)
SR_Jmax$wavelength <- SR_Jmax$X+500
SR_plotJ <- ggplot(SR_Jmax, aes(y=x, x=wavelength))+
  geom_line(colour="darkslategray3")+
  xlim(500,2400)+
  ylim(0,3)+
  scale_x_continuous(breaks=seq(500, 2400, 200), limits=c(500,2400))+
  geom_smooth(method="loess",span=0.1, color="red")+
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("c) Jmax - tested on variable water potential") +
  xlab("wavelength (nm)")+
  ylab("Selectivity Ratio")+
  theme(plot.margin=unit(c(-0.2,0.5,0,0.5), "cm"))+
  theme(plot.title = element_text(margin = margin(c(10, 10, -10, 0))))+
  guides(fill=FALSE)

str(SR_Vcmax)
SR_Vcmax$wavelength <- SR_Vcmax$X+500
SR_plotV <- ggplot(SR_Vcmax, aes(y=x, x=wavelength))+
  geom_line(colour="darkslategray3")+
  xlim(500,2400)+
  scale_x_continuous(breaks=seq(500, 2400, 200), limits=c(500,2400))+
  ylim(0,7)+
  geom_smooth(method="loess",span=0.1, color="red")+
  theme(axis.text.x=element_blank(), axis.text.y = element_text(size=10), axis.title.x=element_blank(), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("b) Vcmax - tested on variable water potential") +
  xlab("wavelength (nm)")+
  ylab("Selectivity Ratio")+
  theme(plot.title = element_text(margin = margin(c(10, 10, -10, 0))))+
  guides(fill=FALSE)+
  theme(plot.margin=unit(c(-0.2,0.5,-0.1,0.5), "cm"))

Refs <- ggplot(hyperspectral_long, aes(y=reflectance, x=wavelength, group=uniqueID))+ geom_line(alpha=0.2)+
  #scale_x_continuous(breaks=seq(500, 2400, 200))+
  xlim(500,2400)+
  scale_x_continuous(breaks=seq(500, 2400, 200), limits=c(500,2400))+
  geom_line(data=hyperspect_stats, aes(x=wavelength, y=mean_ref, group=1, fill=1), size=1, colour="red")  +
  geom_line(data=hyperspect_stats, aes(x=wavelength, y=max_ref, group=1, fill=1), size=1, colour="blue")  +
  geom_line(data=hyperspect_stats, aes(x=wavelength, y=min_ref, group=1, fill=1), size=1, colour="green")  +
  theme(legend.position="none")+ ggtitle("Mean Reflectance")+
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title.x=element_blank(), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.margin=unit(c(0.2,0.6,0,0.5), "cm"))+
  ggtitle("a) Leaf Spectra") +
  xlab("wavelength (nm)")+
  ylab("reflectance")+
  theme(plot.title = element_text(margin = margin(c(10, 10, -10, 0))))


grid.newpage()
grid.arrange(Refs, SR_plotV, SR_plotJ, heights=c(6,3,3.7))
g <- arrangeGrob(Refs, SR_plotV, SR_plotJ, heights=c(6,3,3.7))
ggsave(file="C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Barnes_PLOSOne_Poplar/Revised_9_2017/Submitted_Docs/Figure_2.TIFF", g)

