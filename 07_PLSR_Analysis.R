#07_PLSR_Analysis
#Purpose: Perform PLSR analysis in 3 different ways (increasing statistical rigor)
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

#Figure 3
PLSR_formatted <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/hyperspec_for_PLSR_formatted.csv")
#now average all reflectances by 'uniqueID' - this averages all 9 observaitons
str(PLSR_formatted)
mean_no_neg <- function(x){
  return(mean(x[x>0]))
  
}

hyperspectral_mean <- ddply(PLSR_formatted, .(uniqueID), colwise(mean_no_neg))
hyperspectral_med <- ddply(PLSR_formatted, .(uniqueID), numcolwise(median))
str(hyperspectral_mean)
str(hyperspectral)
hyperspectral_mean <- hyperspectral_mean[ -c(2:7)]
#colnames(hyperspectral)[1] <- "uniqueID"
#remove columns: x, observation, filename
#Load file with other data
data_aci_etc <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data_3_6_2017.csv")
str(data_aci_etc)
merged_hyperspec <- merge(data_aci_etc, hyperspectral_mean, by="uniqueID")
str(merged_hyperspec)
#wanna keep date and genotype columns for potential easy subsetting I think
merged_hyperspec <- merged_hyperspec[ -c(2:6, 9:14, 16:19, 21:32)]
str(merged_hyperspec)
write.csv(merged_hyperspec, "C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_4_8.csv")

#Figure 3---------------------------------------------
#Figure 3 -  new PLSR figure
indices_formatted <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/hyperspec_for_PLSR_formatted.csv")
#now average all reflectances by 'uniqueID'
str(indices_formatted)
hyperspectral <- ddply(indices_formatted, .(uniqueID), colwise(mean_no_neg))
str(hyperspectral_mean)
hyperspectral <- hyperspectral_mean[ -c(2:7)]
#colnames(hyperspectral)[1] <- "uniqueID"
#remove columns: x, observation, filename
#Load file with other data
data_aci_etc <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/Merged_data_to_analyze_3_6_2017.csv")
str(data_aci_etc)
merged_hyperspec <- merge(data_aci_etc, hyperspectral_mean, by="uniqueID")
str(merged_hyperspec)
#wanna keep date and genotype columns for potential easy subsetting I think
head(merged_hyperspec)
#Columns to keep: uniqueID, Vcmax, Jmax, Water_Pot, Genotype, Date.x
#Their positions are: column 1,6,13,16,21 
merged_hyperspec <- merged_hyperspec[ -c(2:4, 7:10, 12:15, 17:20, 22:24)]
str(merged_hyperspec)
write.csv(merged_hyperspec, "C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_4_8_2017.csv")
#Prepping data for PLSR
poplar_names <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_4_8_2017.csv")
str(poplar_names)
poplar_names<-poplar_names[!(poplar_names$uniqueID=="e04-2016-05-20"),]
write.csv(poplar_names, "C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_4_8_2017.csv")
poplar_names <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_4_8_2017.csv")
poplar_names$Date.x <- as.Date(poplar_names$Date.x, format="%m/%d/%Y")
#Figure 3a&3b: Vcmax &Jmax- randomly order samples; train with 80% and test with 20% of data
#get rid of 'x' column, Water_Pot, and Genotype for now 
poplar_names <- poplar_names[ -c(1:2, 6:8)]
#change col names (all say 'x' right now(?))
names(poplar_names) <- gsub("X", "", names(poplar_names))
str(poplar_names)
#rownames of all data
poplar_all <- poplar_names[,-1]
rownames(poplar_all) <- poplar_names[,1]
#Rowname should be 'UniqueID'
rownames(poplar_all)
#check column names
colnames(poplar_all)[1:5]
#last five column names
colnames(poplar_all)[(ncol(poplar_all)-4):ncol(poplar_all)]
#extract x variables (450 nm - 2500 nm)
x <-extdat(poplar_all, start=450,end=2500)
poplar<-data.frame(poplar_all[,1:2], NIR=I(as.matrix(x)))
#extracting range of 700 nm to 1098 nm
poplar$NIR<-extdat(poplar$NIR,start=450,end=2500)
#preprocessing: standard normal variate
poplar$NIR <- snv(poplar$NIR)
#Savitky-Golay second derivative
poplar$NIR <- matsgolay(poplar$NIR, p=2, n=11, m=2)
#Auto-scaling
poplar$NIR <- scale(poplar$NIR, center = TRUE, scale = TRUE)
#Shuffle Rowwise: 
poplar_rand <- poplar[sample(nrow(poplar)),]
#Divide data set into training and test set
datTrain <- poplar_rand[-c(69:86),]
datTest <- poplar_rand[69:86,]
str(datTrain)
str(datTest)
#Now for the PLSR!
result3a <- plsrPlot(Vcmax ~ NIR, data = datTrain, testdata = datTest,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = TRUE)

result3aall <- plsrPlot(Vcmax ~ NIR, data = poplar_rand,
                        ncomp = "auto", maxcomp = 10,
                        validation = "CV", segment.type ="interleaved",
                        output = TRUE)

result3ball <- plsrPlot(Jmax ~ NIR, data = poplar_rand,
                        ncomp = "auto", maxcomp = 10,
                        validation = "CV", segment.type ="interleaved",
                        output = TRUE)

result3b <- plsrPlot(Jmax ~ NIR, data = datTrain, testdata = datTest,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = TRUE)

function

#result$validation
#result.all <- plsrauto(Vcmax ~ NIR, data = datTrain, testdata = datTest, xrange = list(c(350, 2500)))
#str(result.all)

#Figure 3c&d: Vcmax & Jmax - train on high (not stressed) water potential and test on stressed (low water potential)
#When ordering by water potential (descending)
poplar_namescd <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_4_8_2017.csv")
poplar_namescd$Date.x <- as.Date(poplar_namescd$Date.x, format="%m/%d/%Y")
poplar_namescd <- poplar_namescd[order(poplar_namescd$Date.x),] 
str(poplar_namescd)
#Figure 3a&3b: Vcmax &Jmax- randomly order samples; train with 80% and test with 20% of data
#get rid of 'x' column, Water_Pot, and Genotype for now 
poplar_namescd <- poplar_namescd[ -c(1:2, 6:8)]
#change col names (all say 'x' right now(?))
names(poplar_namescd) <- gsub("X", "", names(poplar_namescd))
str(poplar_namescd)
#rownames of all data
poplar_allcd <- poplar_namescd[,-1]
rownames(poplar_allcd) <- poplar_namescd[,1]
rownames(poplar_allcd)
#check column names
colnames(poplar_allcd)[1:5]
#last five column names
colnames(poplar_allcd)[(ncol(poplar_allcd)-4):ncol(poplar_allcd)]
#extract x variables (350 nm - 2500 nm)
xcd <-extdat(poplar_allcd, start=450,end=2500)
poplarcd<-data.frame(poplar_allcd[,1:2], NIR=I(as.matrix(xcd)))
#extracting range of 700 nm to 1098 nm
poplarcd$NIR<-extdat(poplarcd$NIR,start=450,end=2500)
#preprocessing: standard normal variate
poplarcd$NIR <- snv(poplarcd$NIR)
#Savitky-Golay second derivative
poplarcd$NIR <- matsgolay(poplarcd$NIR, p=2, n=11, m=2)
#Auto-scaling
poplarcd$NIR <- scale(poplarcd$NIR, center = TRUE, scale = TRUE)
#Divide data set into training and test set
datTraincd <- poplarcd[-c(39:51),]
datTestcd <- poplarcd[39:51,]

result3c <- plsrPlot(Vcmax ~ NIR, data = datTraincd, testdata = datTestcd,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = FALSE)

result3d <- plsrPlot(Jmax ~ NIR, data = datTraincd, testdata = datTestcd,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = FALSE)

#Shuffle Rowwise: 
#poplar_rand <- poplar[sample(nrow(poplar)),]

str(datTrain)
str(datTest)

#This is for only obs on June 23rd and 24th which are observations 40-52 (out of 87)
#This is for only obs on June 23rd and 24th for GT 52-276
datTrain <- poplarcd[-c(40:52),]
datTest <- poplarcd[40:52,]

result <- plsrPlot(Vcmax ~ NIR, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = TRUE)

result <- plsrPlot(Jmax ~ NIR, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = TRUE)


#Figure 4----------------------
Corrs <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/Corr_to_plot_3_7_2017.csv")
str(Corrs)

c1 <- ggplot(Corrs, aes(y=Vcmax_R_squared, x=reorder(Index, -Vcmax_R_squared)))+
  ylim(0,0.8)+
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x=element_blank())+
  ylab("Vcmax R-squared")

c2 <- ggplot(Corrs, aes(y=Jmax_R_squared, x=reorder(Index, -Jmax_R_squared)))+
  ylim(0,0.8)+
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Indices")+
  ylab("Jmax R-squared")

multiplot(c1,c2)

#Plots--------------------------------------------------------------
#Figure2a
Vcmax_80_20 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_80_20_fig/2comps(n=68)_test/fittedvalue_test.csv")
Vcmax_80_20_SR <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_80_20_fig/2comps(n=68)_test/selectivityRatio.csv")
Vcmax_80_20_VIP <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_80_20_fig/2comps(n=68)_test/vip.csv")
Vcmax_80_20_stats <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_80_20_fig/2comps(n=68)_test/stats.csv")
str(Vcmax_80_20_VIP)
str(Vcmax_80_20_SR)
r2 <- round((Vcmax_80_20_stats$R.test)^2, 2)
lb1 <- paste("R^2 == ", r2)

VP01 <- ggplot(Vcmax_80_20, aes(x=y.test, y=y.2.comps)) + 
  geom_point(aes(colour="red"), size=4) + 
  geom_point(shape = 1,size = 4,colour = "black")+
  geom_abline(intercept=0, slope=1)+
  theme_bw()+
  xlab("Measured Vcmax")+
  ylab("Predicted Vcmax")+
  guides(colour=FALSE)+
  annotate("text", x=110, y=50, label=lb1, parse=TRUE, size=7)+
  ggtitle("Vcmax - representative 80/20")

VP02 <- ggplot(Vcmax_80_20_VIP, aes(x=X, y=Comp.1))+ geom_line(alpha=0.5, color="salmon")+
  geom_smooth(method="loess", span=0.05, color="deepskyblue3")+
  xlab("wavelength (nm)")+
  ylab("VIP")+
  ggtitle("Var.Importance in Projection")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())


VP03 <- ggplot(Vcmax_80_20_SR, aes(x=X, y=x))+ geom_line(alpha=0.5, color="deepskyblue")+
  geom_smooth(method="loess", span=0.05, color="firebrick1")+
  xlab("wavelength (nm)")+
  ylab("SR")+
  ggtitle("Selectivity Ratio")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())


grid.arrange(VP01, arrangeGrob(VP02,VP03, ncol=1), widths=c(2.3/4, 1.7/4), ncol=2)

