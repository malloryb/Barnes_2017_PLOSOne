#11_Figure_S1_Table_S2
#Purpose: Create Manuscript Figure S1 and get data for Table S2 (Bootstrapping analysis of Vcmax/Jmax 
#estimates based on proportion training vs. testing data)
#Written by: Mallory Barnes
#June 2017

#Purpose: create R-squared varaiance figure using plsropt package
#Figure shows variance around R-squared (10 different random samples) given different proportions
#of training vs. testing data. 
#Input: .csv file containing full leaf specta in wide format
#Output: ggplot figure: shows spread of R-squared values for various proportions of testing vs. training 
#data for this dataset


library(pls)
library(ggplot2)
library(plyr)
library(devtools)
library(signal)
#If plsropt is not installed: use command 'install_github("uwadaira/plsropt", dependencies = TRUE)'
library(plsropt)
library(gdata)
library(progress)

#Figure S1
#Load wide format hyperspectral data
poplar_names <- read.csv("C:/Users/rsstudent/Dropbox/Drought_Expt_2016/poplar_allwavelengths_3_17_2017.csv")
str(poplar_names)
#Format date properly
poplar_names$Date.x <- as.Date(poplar_names$Date.x, format="%m/%d/%Y")

#When subsetting by genotype: 
#poplar_names <- subset(poplar_names, Genotype=="R-270")
#or
#poplar_names <-subset(poplar_names, Genotype=="52-276")

#When ordering by water potential (descending)
#poplar_names <- poplar_names[order(-poplar_names$Water_Pot),] 

#When ordering by date (descending)
#poplar_names <- poplar_names[order(poplar_names$date.y),] 
#poplar_names$date.y

#get rid of 'x' column, Water_Pot, and Genotype for now 
poplar_names <- poplar_names[ -c(1:2, 6:8)]
#change col names (all say 'x' right now(?))
names(poplar_names) <- gsub("X", "", names(poplar_names))
str(poplar_names)
#get rownames of all data: 'unique_ID" containing plant ID and date
poplar_all <- poplar_names[,-1]
rownames(poplar_all) <- poplar_names[,1]
rownames(poplar_all)

#check column names
colnames(poplar_all)[1:5]
#check last five column names
colnames(poplar_all)[(ncol(poplar_all)-4):ncol(poplar_all)]

#extract x variables (350 nm - 2500 nm)
x <-extdat(poplar_all, start=350,end=2500)
poplar<-data.frame(poplar_all[,1:2], NIR=I(as.matrix(x)))
#extracting range of 700 nm to 1098 nm
poplar$NIR<-extdat(poplar$NIR,start=350,end=2500)
#preprocessing: standard normal variate
poplar$NIR <- snv(poplar$NIR)
#Savitky-Golay second derivative
poplar$NIR <- matsgolay(poplar$NIR, p=2, n=11, m=2)
#Auto-scaling
poplar$NIR <- scale(poplar$NIR, center = TRUE, scale = TRUE)
#Divide data set into training and test set
#For non-randomzed purposes
#datTrain <- poplar[-c(1:18),]
#datTest <- poplar[19:28,]

#This is for only obs on June 23rd and 24th
#datTrain <- poplar[-c(25:33),]
#datTest <- poplar[25:33,]

#This is for only obs on June 23rd and 24th for GT 52-276
#datTrain <- poplar[-c(13:17),]
#datTest <- poplar[13:17,]

str(poplar)
#Shuffle Rowwise: 
poplar_rand <- poplar[sample(nrow(poplar)),]
#Break into training and testing set based on 'cutoff'
datTest <- poplar[-c(46:56),]
datTrain <- poplar[c(46:56),]
cutoff = round(0.3*nrow(poplar_rand))

datTrain <- poplar_rand[1:cutoff,]
datTest <- poplar_rand[-(1:cutoff),]
#don't need this anymore
#datTrain <- poplar_rand[-c(41:56),]
#datTest <- poplar_rand[41:56,]
str(datTrain)
str(datTest)

#Plot of Vcmax
resultV <- plsrPlot(Vcmax ~ NIR, data = datTrain, testdata = datTest,
                    ncomp = "auto", maxcomp = 10,
                    validation = "CV", segment.type ="interleaved",
                    output = FALSE, return.stats=TRUE)


resultJ <- plsrPlot(Jmax ~ NIR, data = datTrain, testdata = datTest,
                    ncomp = "auto", maxcomp = 10,
                    validation = "CV", segment.type ="interleaved",
                    output = FALSE, return.stats=TRUE)

round(resultV$R.test^2 ,3)
round(resultJ$R.test^2, 3)

#These functions are used in the 'repeat R squared function': it pulls the R-squared for the 
#Correlation between the actual vs. predicted data (training vs. test) from the plsrPlot function
#and defines a 'cutoff' value (used to divide the proportion of testing and training data). 
#R_squared_V is for Vcmax and R_squared_J is for Jmax

R_squared_V <- function(x, prop_train){
  x_rand <- x[sample(nrow(x)),]
  cutoff = round(prop_train*nrow(x_rand))
  datTrain <- x_rand[1:cutoff,]
  datTest <- x_rand[-(1:cutoff),]
  resultV <- plsrPlot(Vcmax ~ NIR, data = datTrain, testdata = datTest,
                      ncomp = "auto", maxcomp = 10,
                      validation = "CV", segment.type ="interleaved",
                      output = FALSE, return.stats=TRUE, plot=FALSE)
  return(round(resultV$R.test^2 ,3))
  
}


R_squared_J <- function(x, prop_train){
  x_rand <- x[sample(nrow(x)),]
  cutoff = round(prop_train*nrow(x_rand))
  datTrain <- x_rand[1:cutoff,]
  datTest <- x_rand[-(1:cutoff),]
  resultJ <- plsrPlot(Jmax ~ NIR, data = datTrain, testdata = datTest,
                      ncomp = "auto", maxcomp = 10,
                      validation = "CV", segment.type ="interleaved",
                      output = FALSE, return.stats=TRUE, plot=FALSE)
  return(round(resultJ$R.test^2 ,3))
  
}

R_squared_V_2 <- function(x, prop_train){
  datTest <- poplar[c(70:86),]
  datTrain <- poplar[-c(70:86),]
  x_rand <- datTrain[sample(nrow(datTrain)),]
  cutoff = round(prop_train*nrow(x_rand))
  datTrain <- x_rand[1:cutoff,]
  #datTest <- x_rand[-(1:cutoff),]
  resultV <- plsrPlot(Vcmax ~ NIR, data = datTrain, testdata = datTest,
                      ncomp = "auto", maxcomp = 10,
                      validation = "CV", segment.type ="interleaved",
                      output = FALSE, return.stats=TRUE, plot=FALSE)
  return(round(resultV$R.test^2 ,3))
  
}

R_squared_J_2 <- function(x, prop_train){
  datTest <- poplar[c(70:86),]
  datTrain <- poplar[-c(70:86),]
  x_rand <- datTrain[sample(nrow(datTrain)),]
  cutoff = round(prop_train*nrow(x_rand))
  datTrain <- x_rand[1:cutoff,]
  #datTest <- x_rand[-(1:cutoff),]
  resultJ <- plsrPlot(Jmax ~ NIR, data = datTrain, testdata = datTest,
                      ncomp = "auto", maxcomp = 10,
                      validation = "CV", segment.type ="interleaved",
                      output = FALSE, return.stats=TRUE, plot=FALSE)
  return(round(resultJ$R.test^2 ,3))
  
}

#This function pulls n random samples for dfferent proportions of training vs. testing data and
#combines them all into one data frame. 

Repeat_R_V <- function(x, n){
  Prop_0.3 <- do.call(rbind, rlply(n, R_squared_V(x,0.3), .progress = "text"))
  Prop_0.4 <- do.call(rbind, rlply(n, R_squared_V(x,0.4), .progress = "text"))
  Prop_0.5 <- do.call(rbind, rlply(n, R_squared_V(x,0.5), .progress = "text"))
  Prop_0.6 <- do.call(rbind, rlply(n, R_squared_V(x,0.6), .progress = "text"))
  Prop_0.7 <- do.call(rbind, rlply(n, R_squared_V(x,0.7), .progress = "text"))
  Prop_0.8 <- do.call(rbind, rlply(n, R_squared_V(x,0.8), .progress = "text"))
  Prop_0.9 <- do.call(rbind, rlply(n, R_squared_V(x,0.9), .progress = "text"))
  return(all_prop <- combine(Prop_0.3, Prop_0.4, Prop_0.5, Prop_0.6, Prop_0.7, Prop_0.8, Prop_0.9))
}


Repeat_R_J <- function(x, n){
  Prop_0.3 <- do.call(rbind, rlply(n, R_squared_J(x,0.3), .progress = "text"))
  Prop_0.4 <- do.call(rbind, rlply(n, R_squared_J(x,0.4), .progress = "text"))
  Prop_0.5 <- do.call(rbind, rlply(n, R_squared_J(x,0.5), .progress = "text"))
  Prop_0.6 <- do.call(rbind, rlply(n, R_squared_J(x,0.6), .progress = "text"))
  Prop_0.7 <- do.call(rbind, rlply(n, R_squared_J(x,0.7), .progress = "text"))
  Prop_0.8 <- do.call(rbind, rlply(n, R_squared_J(x,0.8), .progress = "text"))
  Prop_0.9 <- do.call(rbind, rlply(n, R_squared_J(x,0.9), .progress = "text"))
  return(all_prop <- combine(Prop_0.3, Prop_0.4, Prop_0.5, Prop_0.6, Prop_0.7, Prop_0.8, Prop_0.9))
}

all_prop_V <- Repeat_R_V(poplar,100)
all_prop_J <- Repeat_R_J(poplar,100)
all_prop_V$source <- revalue(all_prop_V$source, c("Prop_0.3"="30%", "Prop_0.4"="40%", "Prop_0.5"="50%", "Prop_0.6"="60%", "Prop_0.7"="70%", "Prop_0.8"="80%", "Prop_0.9"="90%"))
all_prop_J$source <- revalue(all_prop_J$source, c("Prop_0.3"="30%", "Prop_0.4"="40%", "Prop_0.5"="50%", "Prop_0.6"="60%", "Prop_0.7"="70%", "Prop_0.8"="80%", "Prop_0.9"="90%"))

Vcmax_r <- ggplot(all_prop_V, aes(y=data, x=source)) + 
  geom_point(size=2, alpha=0.2)+
  
  theme_bw(base_size=12)+labs(title = "Vcmax", y="R-squared", x="Proportion Training Data (%)")

Jmax_r <- ggplot(all_prop_J, aes(y=data, x=source)) + 
  geom_point(size=2, alpha=0.2)+
  theme_bw(base_size=12)+labs(title = "Jmax", y="R-squared", x="Proportion Training Data (%)")

multiplot(Vcmax_r, Jmax_r, cols=2)

#Doing the same thing with the _2 function: need to randomize the data first but the rest should be the same
#devtools::source_gist("gist.github.com/mrdwab/6424112", filename = "stratified.R")
#stratified(df, "color", 3)
#?stratified
#poplar_rand_V <- stratified(poplar, "Vcmax", size=0.25)
#poplar_rand_J <- poplar[strata(nrow(poplar)),]

poplar_rand <- poplar[sample(nrow(poplar)),]
poplar_rand$Vcmax
poplar_rand$Jmax

poplar %>% 
  group_by(Vcmax) %>% 
  sample_n(2)

Repeat_R_V_2 <- function(x, n){
  Prop_0.3 <- do.call(rbind, rlply(n, R_squared_V_2(x,0.3), .progress = "text"))
  Prop_0.4 <- do.call(rbind, rlply(n, R_squared_V_2(x,0.4), .progress = "text"))
  Prop_0.5 <- do.call(rbind, rlply(n, R_squared_V_2(x,0.5), .progress = "text"))
  Prop_0.6 <- do.call(rbind, rlply(n, R_squared_V_2(x,0.6), .progress = "text"))
  Prop_0.7 <- do.call(rbind, rlply(n, R_squared_V_2(x,0.7), .progress = "text"))
  Prop_0.8 <- do.call(rbind, rlply(n, R_squared_V_2(x,0.8), .progress = "text"))
  Prop_0.9 <- do.call(rbind, rlply(n, R_squared_V_2(x,0.9), .progress = "text"))
  return(all_prop <- combine(Prop_0.3, Prop_0.4, Prop_0.5, Prop_0.6, Prop_0.7, Prop_0.8, Prop_0.9))
}


Repeat_R_J_2 <- function(x, n){
  Prop_0.3 <- do.call(rbind, rlply(n, R_squared_J_2(x,0.3), .progress = "text"))
  Prop_0.4 <- do.call(rbind, rlply(n, R_squared_J_2(x,0.4), .progress = "text"))
  Prop_0.5 <- do.call(rbind, rlply(n, R_squared_J_2(x,0.5), .progress = "text"))
  Prop_0.6 <- do.call(rbind, rlply(n, R_squared_J_2(x,0.6), .progress = "text"))
  Prop_0.7 <- do.call(rbind, rlply(n, R_squared_J_2(x,0.7), .progress = "text"))
  Prop_0.8 <- do.call(rbind, rlply(n, R_squared_J_2(x,0.8), .progress = "text"))
  Prop_0.9 <- do.call(rbind, rlply(n, R_squared_J_2(x,0.9), .progress = "text"))
  return(all_prop <- combine(Prop_0.3, Prop_0.4, Prop_0.5, Prop_0.6, Prop_0.7, Prop_0.8, Prop_0.9))
}


all_prop_V_2 <- Repeat_R_V_2(poplar_rand,100)
all_prop_J_2 <- Repeat_R_J_2(poplar_rand,100)
all_prop_V_2$source <- revalue(all_prop_V_2$source, c("Prop_0.3"="30%", "Prop_0.4"="40%", "Prop_0.5"="50%", "Prop_0.6"="60%", "Prop_0.7"="70%", "Prop_0.8"="80%", "Prop_0.9"="90%"))
all_prop_J_2$source <- revalue(all_prop_J_2$source, c("Prop_0.3"="30%", "Prop_0.4"="40%", "Prop_0.5"="50%", "Prop_0.6"="60%", "Prop_0.7"="70%", "Prop_0.8"="80%", "Prop_0.9"="90%"))

Vcmax_r_2 <- ggplot(all_prop_V_2, aes(y=data, x=source)) + 
  geom_point(size=2, alpha=0.2)+
  ylim(0,0.8)+
  theme_bw(base_size=12)+labs(title = "Vcmax", y="R-squared", x="Proportion Training Data (%)")

Jmax_r_2 <- ggplot(all_prop_J_2, aes(y=data, x=source)) + 
  geom_point(size=2, alpha=0.2)+
  ylim(0,0.8)+
  theme_bw(base_size=12)+labs(title = "Jmax", y="R-squared", x="Proportion Training Data (%)")

multiplot(Vcmax_r_2, Jmax_r_2, cols=2)

#Information for Table S1

ddply(all_prop_V_2, .(source), summarize, mean=mean(data), median=median(data))
ddply(all_prop_J_2, .(source), summarize, mean=mean(data), median=median(data))