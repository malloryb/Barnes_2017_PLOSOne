#01_Licor QA/QC
#Purpose: QA/QC Licor files for Vcmax/Jmax estimation using PeCAN package
#Creator: Mallory Barnes
#June 2017



##### Install Pecan: Only need to do once #####
#See PeCAN documentation at: https://github.com/PecanProject/pecan/blob/master/modules/photosynthesis/vignettes/ResponseCurves.Rmd
#install PEcAn.photosynthesis as a stand alone
#need Rtools installed
#need rjags installed in R and also JAGS (stand alone application) installed on your computer
#To download JAGS: http://www.sourceforge.net/projects/mcmc-jags/files

if (!require("PEcAn.photosynthesis",character.only = TRUE))
{
  library(devtools)
  install_github("PecanProject/pecan/modules/photosynthesis") 
}

#Load Packages----------------------------
#Grab the following packages before you start
library(devtools)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(PEcAn.photosynthesis)

#QA/QC Licor Files-----------------------
## Get list of LI-COR 6400 file names (I put all ASCII files in 1 folder)
setwd("C:/Users/rsstudent/Dropbox/")
path_files <- "Summer_2016_Drought_Experiment/Licor_data/"
filenames <- dir(path_files)

fileswithpath=paste0(path_files, filenames)

## Load files to a list
master = lapply(fileswithpath, read.Licor)

## Code performs interactive QA/QC checks on the loaded LI-COR data 

#Test on 1 file: licor QC on a file - you click on outliers to remove them
master[[1]] <- Licor.QC(master[[1]])

#same process for all files - this makes the first image pop up. 
for(i in 1:length(master)){
  master[[i]] = Licor.QC(master[[i]])
}

#after the QC process combine the files into one data frame
#Method suggested in pecan documentation was giving unequal columns error so I used "bind_rows" from dplyr instead: 

dat <- bind_rows(master)

#If you want both the "bad" and "good" observations in a data file, write out .csv file before the following step: 
#e.g. write.csv(dat, "QC_bad_and_good.csv")

## if QC was done, remove both unchecked points and those that fail QC
if("QC" %in% colnames(dat)){
  dat = dat[-which(dat$QC < 1),]  
} else{
  QC = rep(1,nrow(dat))
  dat = cbind(dat,QC)
}

#write "dat" to a .csv file: 
write.csv(dat, "QC_licorfiles.csv")


