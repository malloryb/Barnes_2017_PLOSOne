#02_Licor A/Ci
#Purpose: Fitting A/Ci curves using Plant Ecophys package and creating summary df of Vcmax/Jmax values
#Written by: Mallory Barnes
#June 2017


######  Install PlantEcoPhys: only need to do this once ######
# Plantecophys - An R Package for Analysing and Modelling Leaf Gas Exchange Data
# Remko A. Duursma
# Paper describing the package: http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0143346
# Source code and descriptions: https://bitbucket.org/remkoduursma/plantecophys

install_bitbucket("remkoduursma/plantecophys")

#Load packages and fit A/Ci curves-----------

#Necessary Pacakges: 
library(devtools)
library(PEcAn.photosynthesis)
library(plantecophys)

#Load up saved datafile with QC-ed licor files
dat <- read.csv("C:/Users/rsstudent/Dropbox (Personal)/QC_licorfiles.csv")

#Format dat$fname as factor
dat$fname <- as.factor(dat$fname)

#Fit curves to all A/Ci files - takes a bit of time (~3-5 minutes)
fits <- fitacis(dat, "fname")

#exploring curves------------------
#Plot Vcmax by Jmax
with(coef(fits), plot(Vcmax, Jmax))

#Extract 1 curve: 
fits[[1]]
plot(fits[[1]])

#Plot all curves separately
plot(fits)

#Plot all curves in `1 plot
plot(fits, how="oneplot")

#Can summarize elements using sapply
rmses <- sapply(fits, "[[", "RMSE")

#Plot worst fitting curve
plot(fits[[which.max(rmses)]])

#Get Vcmax_Jmax and write out---------------------
#This is what we want for analysis: 
vcmax_jmax <- coef(fits)

#Parse filname to get Plant ID and Date using substr
vcmax_jmax$fname <- as.character(vcmax_jmax$fname)
str(vcmax_jmax)
write.csv(vcmax_jmax, "vcmax_jmax_estimates.csv")

#Review points with large SEs 
to_review1 <- subset(vcmax_jmax, Jmax_SE >5)
to_review2 <- subset(vcmax_jmax, Vcmax_SE >5)
