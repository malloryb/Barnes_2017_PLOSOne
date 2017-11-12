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
#Also remove any negative values 
mean_no_neg <- function(x){
  return(mean(x[x>0]))
  
}

hyperspectral_mean <- ddply(PLSR_formatted, .(uniqueID), colwise(mean_no_neg))
str(hyperspectral_mean)
hyperspectral_mean <- hyperspectral_mean[ -c(2:7)]
#remove columns: x, observation, filename
#Load file with other data
data_aci_etc <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data_3_6_2017.csv")
str(data_aci_etc)
merged_hyperspec <- merge(data_aci_etc, hyperspectral_mean, by="uniqueID")
str(merged_hyperspec)
#keep date and genotype columns for potential easy subsetting 
merged_hyperspec <- merged_hyperspec[ -c(2:6, 9:14, 16:19, 21:32)]
str(merged_hyperspec)
write.csv(merged_hyperspec, "C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_4_8.csv")

#PLSR Analysis---------------------------------------------
#Figure 3 -  PLSR analysis for figure 3
#Prepping data for PLSR
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
poplar<-data.frame(poplar_all[,1:2], SPC=I(as.matrix(x)))
#extracting range of 450 nm to 2500 nm
poplar$SPC<-extdat(poplar$SPC,start=450,end=2500)
#preprocessing: standard normal variate
poplar$SPC <- snv(poplar$SPC)
#Savitky-Golay second derivative
poplar$SPC <- matsgolay(poplar$SPC, p=2, n=11, m=2)
#Auto-scaling
poplar$SPC <- scale(poplar$SPC, center = TRUE, scale = TRUE)
#Shuffle Rowwise: 
poplar_rand <- poplar[sample(nrow(poplar)),]
#Divide data set into training and test set
datTrain <- poplar_rand[-c(69:86),]
datTest <- poplar_rand[69:86,]
str(datTrain)
str(datTest)
#Now for the PLSR!
Plot8020V <- plsrPlot(Vcmax ~ SPC, data = datTrain, testdata = datTest,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = TRUE)

PlotLOOV <- plsrPlot(Vcmax ~ SPC, data = poplar_rand,
                        ncomp = "auto", maxcomp = 10,
                        validation = "CV", segment.type ="interleaved",
                        output = TRUE)

PlotLOOJ <- plsrPlot(Jmax ~ SPC, data = poplar_rand,
                        ncomp = "auto", maxcomp = 10,
                        validation = "CV", segment.type ="interleaved",
                        output = TRUE)

Plot8020J <- plsrPlot(Jmax ~ SPC, data = datTrain, testdata = datTest,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = TRUE)
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
#extract x variables (450 nm - 2500 nm)
xcd <-extdat(poplar_allcd, start=450,end=2500)
poplarcd<-data.frame(poplar_allcd[,1:2], SPC=I(as.matrix(xcd)))
#extracting range of 450 nm to 2500 nm
poplarcd$SPC<-extdat(poplarcd$SPC,start=450,end=2500)
#preprocessing: standard normal variate
poplarcd$SPC <- snv(poplarcd$SPC)
#Savitky-Golay second derivative
poplarcd$SPC <- matsgolay(poplarcd$SPC, p=2, n=11, m=2)
#Auto-scaling
poplarcd$SPC <- scale(poplarcd$SPC, center = TRUE, scale = TRUE)
#Divide data set into training and test set
datTraincd <- poplarcd[-c(39:51),]
datTestcd <- poplarcd[39:51,]

result3c <- plsrPlot(Vcmax ~ SPC, data = datTraincd, testdata = datTestcd,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = FALSE)

result3d <- plsrPlot(Jmax ~ SPC, data = datTraincd, testdata = datTestcd,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = FALSE)

str(datTrain)
str(datTest)

#This is for only obs on June 23rd and 24th which are observations 40-52 (out of 87)
#This is for only obs on June 23rd and 24th for GT 52-276
datTrain <- poplarcd[-c(40:52),]
datTest <- poplarcd[40:52,]

resultV <- plsrPlot(Vcmax ~ SPC, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = TRUE)

resultJ <- plsrPlot(Jmax ~ SPC, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = TRUE)



