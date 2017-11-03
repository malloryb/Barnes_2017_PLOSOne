#O3_Hyperspectral_Processing
#Purpose: Process and quality check hyperspectral reflectance data
#Written by: Mallory Barnes
#June 2017


#Author: Mallory Barnes
#Date: 09/5/2016 
#Purpose: Quality check Hyperspectral Files for any obvious outliers 
#Input: Hyperspectral  Reflectances in ASCII format
#Output: Quality Checked Hyperspectral Reflectances 

library(ggplot2)
library(hyperSpec)
library(plyr)
library(pavo)
#hyperSpec manual can be found here: https://cran.r-project.org/web/packages/hyperSpec/vignettes/introduction.pdf

#Set working directory to location of hyperspectral ASD Files
getwd()
#for my personal laptop
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
#for my ARS computer
setwd(dir = "C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")

#Read in file, used "col.names" argument to rename columns properly. Will need to figure out
#how to do this with a list of ASCII files
test <- read.table("ASCII_Reflectance/b2popmlb_A01_leaf_5-20-201600000.asd.txt", col.names=c("wavelength", "reflectance"))
str(test)

#Create Column with filename and then delete the first row of data frame
test$filename <- (test$reflectance[1:1])
test = test[-1,]

#Change factors to numeric
test$wavelength <- as.numeric(levels(test$wavelength))[test$wavelength]
test$reflectance <-as.numeric(levels(test$reflectance))[test$reflectance]

str(test)
head(test)

#Simple Plot of Test
qplot(test$wavelength, test$reflectance)

#Test Calculate Indices: all indices are in a position that's their wavelength - 349 (because wavelengths start at 350), so wavelength 860 is in position
#511. Still wanted to test/double check this. 

filename <- substr(test[1,3], 1,31)
ID <-  substr(test[1,3], 10,11)
date <- (substr(test[1,3], 18,26))
observation <- (substr(test[1,3], 30,31))
PRI <- ((test[182,2]-test[221,2])/(test[182,2]+test[221,2]))
NDVI <- ((test[511,2]-test[341,2])/(test[511,2]+test[341,2]))
NDWI <- ((test[511,2]-test[891,2])/(test[511,2]+test[891,2]))

#Calcluating full width half max----------------------------------------------
names(test)[names(test) == 'wavelength'] <- 'wl'
red <- test[271:321,]
NIR <- test[492:527,]
r1 <- as.data.frame(peakshape(red, select = 2, plot=FALSE))





test_df <- cbind(filename, ID, date, observation, PRI, NDVI, NDWI)

test[182,2]
test[221,2]
test[511,2]
test[341,2]
test[323,]
test[201,]
test[359,]
test[385,]
test[398,]
test[366,]
test[377,]
#Lapply to calculate for all hyperspec files -----------------------------------------------------------

#Define functions to calculate vegetation indices from hyperspectral file
calc_PRI = function(w_531, w_570){((w_531-w_570)/(w_531+w_570))}
calc_NDVI = function(w_860, w_690){((w_860-w_690)/(w_860+w_690))}
calc_NDWI = function(w_860, w_1240){((w_860-w_1240)/(w_860+w_1240))}
calc_Datt4 = function(w_672, w_550, w_708){(w_672)/(w_550*w_708)}
calc_Vogelmann2 = function(w_734, w_747, w_715, w_726){(w_734-w_747)-(w_715+w_726)}
calc_Maccioni=function(w_780, w_710, w_680){(w_780-w_710)/(w_780-w_680)}
calc_Double_Difference=function(w_749, w_720, w_701, w_672){(w_749-w_720)-(w_701-w_672)}
calc_Vogelmann1=function(w_740, w_720){(w_740/w_720)}
calc_mSR705=function(w_750, w_705, w_445){(w_750-w_445)/(w_705-w_445)}
calc_mNDVI705=function(w_750, w_705, w_445){(w_750-w_705)/(w_750+w_705-2*w_445)}
calc_SR3=function(w_750, w_550){(w_750/w_550)}
calc_SR4=function(w_700, w_670){(w_700/w_670)}
calc_SR1=function(w_750, w_700){(w_750/w_700)}
calc_Gitelson=function(w_700){(1/w_700)}
calc_SR2=function(w_752, w_690){(w_752/w_690)}
calc_SIPI=function(w_800, w_445, w_680){(w_800-w_445)/(w_800-w_680)}
calc_mNDVI=function(w_800, w_680, w_445){(w_800-w_680)/(w_800+w_680+-2*w_445)}
calc_mSRCHL=function(w_800, w_445, w_680){(w_800-w_445)/(w_680-w_445)}
calc_SRcarter=function(w_760, w_695){(w_760/w_695)}


#create list of text files (files must be in working directory); 'pattern' is case-sensitive
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
textfiles = list.files("ASCII_Reflectance/", pattern = "*.txt")

#txtfiles_subset is to test out the lapply
textfiles_subset = textfiles[1:5]


setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/")

#Lapply "calc_indices functinon" - takes a couple minutes over whole list 
indices_tmp <- lapply(textfiles, calc_indices)
#Bind rows together - this part takes awhile
indices <- do.call(rbind, indices_tmp)
#str still looks really funky but 'head' looks normal
str(indices)
indices[200:235,]
head(indices)
#Going to format different variables now (numeric, date, etc. etc. for analysis)
indices$PRI <- as.numeric(as.character(indices$PRI))
indices$NDVI <- as.numeric(as.character(indices$NDVI))
indices$NDWI <- as.numeric(as.character(indices$NDWI))
indices$Datt4<- as.numeric(as.character(indices$Datt4))
indices$Vogelmann2<- as.numeric(as.character(indices$Vogelmann2))
indices$Maccioni<- as.numeric(as.character(indices$Maccioni))
indices$Double_Difference <-as.numeric(as.character(indices$Double_Difference))
indices$Vogelmann1 <- as.numeric(as.character(indices$Vogelmann1))
indices$mSR705 <- as.numeric(as.character(indices$mSR705))
indices$mNDVI705 <- as.numeric(as.character(indices$mNDVI705))
indices$SR3 <- as.numeric(as.character(indices$SR3))
indices$SR4 <- as.numeric(as.character(indices$SR4))
indices$SR1 <- as.numeric(as.character(indices$SR1))
indices$Gitelson <- as.numeric(as.character(indices$Gitelson))
indices$SR2 <- as.numeric(as.character(indices$SR2))
indices$SIPI <- as.numeric(as.character(indices$SIPI))
indices$mNDVI <- as.numeric(as.character(indices$mNDVI))
indices$mSRCHL <- as.numeric(as.character(indices$mSRCHL))
indices$NDVI_mod <- as.numeric(as.character(indices$NDVI_mod))
indices$SRcarter <- as.numeric(as.character(indices$SRcarter))



indices$date <- as.Date(indices$date, format="%m-%d-%Y")
str(indices)

write.csv(indices,"C:/Users/Mallory/Dropbox/Drought_Expt_2016/Processed_Hyperspec_Files_4_25_2017.csv")

calc_indices <- function(x){
  tmp = read.table(x,  col.names=c("wavelength", "reflectance"))
  tmp$filename <- basename(x)
  tmp = tmp[-1,]
  tmp$wavelength <- as.numeric(levels(tmp$wavelength))[tmp$wavelength]
  tmp$reflectance <-as.numeric(levels(tmp$reflectance))[tmp$reflectance]
  filename <- substr(tmp[1,3], 1,40)
  ID <-  substr(tmp[1,3], 10,12)
  date <- (substr(tmp[1,3], 19,27))
  observation =(substr(tmp[1,3], 31,32))
  
  mod_red=tmp[96:102, 2]
  mod_NIR=tmp[120:123, 2]
  w_445=tmp[96,2]
  w_531=tmp[182,2]
  w_550=tmp[201,2]
  w_570=tmp[221,2]
  w_670=tmp[321,2]
  w_672=tmp[323,2]
  w_680=tmp[331,2]
  w_690=tmp[341,2]
  w_695=tmp[346,2]
  w_700=tmp[351,2]
  w_701=tmp[352,2]
  w_705=tmp[356,2]
  w_708=tmp[359,2]
  w_710=tmp[361,2]
  w_715=tmp[366,2]        
  w_720=tmp[371,2]
  w_726=tmp[377,2]
  w_734=tmp[385,2]        
  w_740=tmp[391,2]
  w_747=tmp[398,2]        
  w_749=tmp[400,2]
  w_750=tmp[401,2]
  w_752=tmp[403,2]
  w_760=tmp[411,2]
  w_780=tmp[431,2]
  w_800=tmp[451,2]
  w_860=tmp[511,2]
  w_1240=tmp[891,2]
  
  NDVI_mod <- FWHM(tmp)
  PRI = calc_PRI(w_531, w_570)
  NDVI = calc_NDVI(w_860, w_690)
  NDWI = calc_NDWI(w_860, w_1240)
  Datt4 = calc_Datt4(w_672, w_550, w_708)
  Vogelmann2 = calc_Vogelmann2(w_734, w_747, w_715, w_726)
  Maccioni=calc_Maccioni(w_780, w_710, w_680)
  Double_Difference=calc_Double_Difference(w_749, w_720, w_701, w_672)
  Vogelmann1=calc_Vogelmann1(w_740, w_720)
  mSR705=calc_mSR705(w_750, w_705, w_445)
  mNDVI705=calc_mNDVI705(w_750, w_705, w_445)
  SR3=calc_SR3(w_750, w_550)
  SR4=calc_SR4(w_700, w_670)
  SR1=calc_SR1(w_750, w_700)
  Gitelson=calc_Gitelson(w_700)
  SR2=calc_SR2(w_752, w_690)
  SRcarter=calc_SRcarter(w_760, w_695)
  SIPI=calc_SIPI(w_800, w_445, w_680)
  mNDVI=calc_mNDVI(w_800, w_680, w_445)
  mSRCHL=calc_mSRCHL(w_800, w_445, w_680)
  indices=as.data.frame(cbind(filename, ID, date, observation, PRI, NDVI, NDWI, Datt4, Vogelmann2, Maccioni, SRcarter, Double_Difference, Vogelmann1, mSR705, mNDVI705, SR3, SR4, SR1, Gitelson, SR2, SIPI, mNDVI, mSRCHL, NDVI_mod))
  return(indices)
}


# define vectors to store results (mean sortable silt values)
results = data.frame(txtfiles_subset, indices)
print(results)



#Function to calculate mean over FWHM range - uses 'peakshape' function from 'pavo" package
#to get wavelength at full width half max and the range around it 
#Where x is a dataframe
FWHM = function(x){
  names(x)[names(x) == 'wavelength'] <- 'wl'
  redrange <- x[271:321,]
  NIRrange <- x[492:527,]
  r1 <- as.data.frame(peakshape(redrange, select = 2, plot=FALSE))
  top <- r1$H1 + r1$HWHM.r
  bottom <- r1$H1 - r1$HWHM.l
  redvalue <-mean(subset(x, wl < top & wl > bottom)$reflectance)
  NIR1 <- as.data.frame(peakshape(NIRrange, select = 2, plot=FALSE))
  top1 <- NIR1$H1 + NIR1$HWHM.r
  bottom1 <- NIR1$H1 - NIR1$HWHM.l
  NIRvalue <-mean(subset(x, wl < top1 & wl > bottom1)$reflectance)
  NDVI_modis <- (NIRvalue-redvalue)/(NIRvalue + redvalue)
  return(NDVI_modis)
  
}

FWHM(test)