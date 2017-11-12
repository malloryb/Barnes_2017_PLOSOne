#10_Figure_4_Table_S1
#Purpose: Create Manuscript Figure 4 (Hyperspectral indices - correlation with Vcmax & Jmax) & info for table S1
#Written by: Mallory Barnes
#June 2017

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Figure 4----------------------
#Corrs <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/Corr_to_plot_4_25_2017.csv")
graphics.off()
Corrs <- read.csv("C:/Users/rsstudent/Dropbox/Paper_2/Corr_to_plot_9_25_2017.csv")
str(Corrs)
Corrs$Sum_Corrs <- Corrs$Vcmax_R_squared + Corrs$Jmax_R_squared
str(Corrs)

c1 <- ggplot(Corrs, aes(y=Vcmax_R_squared, x=reorder(Index, order)))+
  ylim(0,0.8)+
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x=element_blank())+
  ylab("Vcmax R-squared")+
  annotate("text", x = 19:20, y = 0.1, label = c("ns", "ns"))+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())



c2 <- ggplot(Corrs, aes(y=Jmax_R_squared, x=reorder(Index, order)))+
  ylim(0,0.8)+
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Indices")+
  ylab("Jmax R-squared")+
  annotate("text", x = 18:20, y = 0.1, label = c("ns", "ns", "ns"))+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


tiff(filename = "Figure_4_done.tiff", width=13, height=11, unit="cm", res = 300)
multiplot(c1,c2)
dev.off()

#Information for Table 1 
#hyperspectral data - create "unique id" (same as in 'merged')and then average by unique ID
#Then will be able to merge with A/Ci and Climate files
#'uniqueID' takes form: a18-2016-06-08 
merged <- read.csv("C:/Users/rsstudent/Dropbox/Drought_Expt_2016/Merged_data_to_analyze_3_6_2017.csv")
hyperspec <- read.csv("C:/Users/rsstudent/Dropbox/Drought_Expt_2016/Processed_Hyperspec_Files_4_25_2017.csv")
hyperspec$uniqueID <- paste(tolower(hyperspec$ID), hyperspec$date, sep='-')
str(hyperspec)
#want means of VIs by uniqueID - use ddply
hyperspec_avg <- ddply(hyperspec,~uniqueID,summarise, SRcarter=mean(SRcarter), NDVI=mean(NDVI), NDVI_mod=mean(NDVI_mod, na.rm=TRUE), PRI=mean(PRI), NDWI=mean(NDWI), Datt4=mean(Datt4), Vogelmann2=mean(Vogelmann2), Maccioni=mean(Maccioni), Double_Difference=mean(Double_Difference), Vogelmann1=mean(Vogelmann1), mSR705=mean(mSR705), SR3=mean(SR3), SR4=mean(SR4), SR1=mean(SR1), Gitelson=mean(Gitelson), SR2=mean(SR2), SIPI=mean(SIPI), mNDVI=mean(mNDVI), mSRCHL=mean(mSRCHL))
#merge hyperspec and a/ci data
anti_join(merged, hyperspec_avg, by="uniqueID")
hyperspec_avg$uniqueID
all_data <- merge(hyperspec_avg, merged, by="uniqueID")
str(all_data)
#clean up "all_data"
#delete X.2, X.1, and X columns (what even are these?)
all_data <- subset(all_data, select=-c(X,X.1))
#giant correlation matrix to clipboard for quick glance
#Saved as 'corr matrix' in the Paper_2 folder in dropbox

check_corrs <- function(xx, ii)
{
  CORV=cor(xx$Vcmax, xx[[ii]], use="pairwise.complete.obs")
  SIGV=cor.test(xx$Vcmax,xx[[ii]], use="pairwise.complete.obs")$p.value
  CORJ=cor(xx$Jmax, xx[[ii]], use="pairwise.complete.obs")
  SIGJ=cor.test(xx$Jmax,xx[[ii]], use="pairwise.complete.obs")$p.value
  RSQV=(CORV*CORV)
  RSQJ=(CORJ*CORJ)
  return(data.frame(RSQV, SIGV, RSQJ, SIGJ))
  
}
str(all_data)

check_corrs(all_data, "SR1")
check_corrs(all_data, "NDWI")
check_corrs(all_data, "SIPI")

check_corrs(all_data, "PRI")
check_corrs(all_data, "Gitelson")
check_corrs(all_data, "mNDVI")

#etc...