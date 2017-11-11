#O3_Hyperspectral_Processing
#Purpose: Prep hyperspectral data for PLSR regression
#Written by: Mallory Barnes
#June 2017

library(reshape2)

# UniqueID Vcmax Jmax Water_Pot  300 301 302 303 etc. etc. (data must be in wide format)
#for my personal laptop
setwd(dir = "C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")

#Read in file, used "col.names" argument to rename columns properly. Will need to figure out
#how to do this with a list of ASCII files
#Testing reshape with a single file 'test'
test <- read.table("C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/b2popmlb_A01_leaf_5-20-201600000.asd.txt", col.names=c("wavelength", "reflectance"))
str(test)
test$filename <- basename("C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/b2popmlb_A01_leaf_5-20-201600000.asd.txt")
test_wide <- reshape(test, idvar="filename", timevar="wavelength", direction="wide")



#Lapply to calculate for all hyperspec files -----------------------------------------------------------

#create list of text files (files must be in working directory); 'pattern' is case-sensitive
setwd(dir = "C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
textfiles = list.files("ASCII_Reflectance/", pattern = "*.txt")
#txtfiles_subset is to test out the lapply
textfiles_subset = textfiles[1:5]
textfiles_subset
setwd(dir = "C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/")

#Function to format in wide format
format_PLSR <- function(x){
  tmp = read.table(x,  col.names=c("wavelength", "reflectance"))
  tmp$filename <- basename(x)
  tmp = tmp[-1,]
  tmp$wavelength <- as.numeric(levels(tmp$wavelength))[tmp$wavelength]
  tmp$reflectance <-as.numeric(levels(tmp$reflectance))[tmp$reflectance]
  filename <- substr(tmp[1,3], 1,40)
  print(filename)
  ID <-  substr(tmp[1,3], 10,12)
  date <- (substr(tmp[1,3], 19,27))
  observation =(substr(tmp[1,3], 31,32))
  reflectances <- reshape(tmp, idvar="filename", timevar="wavelength", direction="wide")
  indices=as.data.frame(cbind(ID, date, observation, reflectances))
  return(indices)
}

#Lapply "calc_indices functinon" over subset of files: subset of 15 files takes 30 seconds
indices_tmp <- lapply(textfiles_subset, format_PLSR)
#Lapply "calc_indices functinon" over full list of files: takes awhile - 34 minutes for all files
indices_tmp <- lapply(textfiles, format_PLSR)
#Bind rows together: this can also take awhile - about 15 minutes for all files 
indices <- do.call(rbind, indices_tmp)
write.csv(indices, "C:/Users/rsstudent/Dropbox/Drought_Expt_2016/hyperspec_for_PLSR.csv")
#str still looks really funky
indices[200:235,]
head(indices)
#Need to: 
#1) Format all reflectances as numeric
#2) Format date "as.date"
#3) create "unique_ID" column
#4) Get rid of extraneous filename column
#1) Format all reflectances as numeric: columns [,7:2157] - opening from .csv solves this problem!
indices_formatted <- (read.csv("C:/Users/rsstudent/Dropbox/Drought_Expt_2016/hyperspec_for_PLSR.csv"))
str(indices_formatted)
#2)
indices_formatted$date <-as.Date(indices_formatted$date, format="%m-%d-%Y")
#3) create "unique_ID" column
indices_formatted$ID <- as.character(tolower(indices_formatted$ID))
indices_formatted$uniqueID <- paste(indices_formatted$ID, indices_formatted$date, sep='-') 
str(indices_formatted)
indices_formatted$uniqueID
#Not an issuea anymore: #4) Get rid of extraneous filename column 'filename.1'
#indices_formatted <- subset(indices_formatted, select=-c(filename.1))

#5) Get rid of "reflectance" in column because we know that's what it is
names(indices_formatted) <- gsub("reflectance.", "", names(indices_formatted))
#No longer an issue: Get rid of 'wavelength' column too
#indices_formatted <- subset(indices_formatted, select=-c(Wavelength))
str(indices_formatted)
write.csv(indices_formatted, "C:/Users/rsstudent/Dropbox/Drought_Expt_2016/hyperspec_for_PLSR_formatted.csv")

#now average all reflectances by 'uniqueID'
str(indices_formatted)
#This is where we average the 9 spectra together 
hyperspectral <- ddply(indices_formatted, .(uniqueID), colwise(mean))
str(hyperspectral)
hyperspectral <- hyperspectral[ -c(2:3, 5:6)]
#colnames(hyperspectral)[1] <- "uniqueID"
#remove columns: x, observation, filename

#Load file with other data
data_aci_etc <- read.csv("C:/Users/rsstudent/Dropbox/Drought_Expt_2016/All_with_more_licor_vars.csv")
str(data_aci_etc)
merged_hyperspec <- merge(data_aci_etc, hyperspectral, by="uniqueID")
str(merged_hyperspec)
#wanna keep date and genotype columns for potential easy subsetting I think
merged_hyperspec <- merged_hyperspec[ -c(2:6, 9:14, 16:19, 21:32)]
str(merged_hyperspec)
write.csv(merged_hyperspec, "C:/Users/rsstudent/Dropbox/Drought_Expt_2016/poplar_allwavelengths.csv")

