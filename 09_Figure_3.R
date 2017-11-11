#09_Figure_3
#Purpose: Create Manuscript Figure 3 (Hyperspectral data + Selectivity ratio)
#Written by: Mallory Barnes
#June 2017
grid.newpage()
plot.new()
dev.off()
#Vcmax
file1 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_LOO/2comps(n=86)_train/fittedvalue.csv")
tests1 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result//Vcmax_LOO/2comps(n=86)_train/stats.csv")
rsquared1 <- tests1$R.val^2
lb1 <- paste("R^2 == ", round(rsquared1,2))
RMSE1 <- paste("RMSE==", round(RMSE(file1$yhat.cal, file1$yhat.val),1))
str(file1)
plot1_resize <- ggplot(file1, aes(y=yhat.cal, x=yhat.val))+
  geom_point(aes(fill="tomato2"), shape=21, colour="black", size=2)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("Measured Vcmax")+
  ylab("Predicted Vcmax")+
  xlim(35, 150)+
  ylim(35, 150)+
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
  annotate("text", label = lb1, parse=TRUE, x = 120, y = 60, size = 4, colour = "Black")+
  annotate("text", label = RMSE1, parse=TRUE, x = 120, y = 45, size = 4, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("a) Vcmax - leave-one-out") + 
  theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))+
  guides(fill=FALSE)

#Jmax
file2 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_LOO/4comps(n=86)_train/fittedvalue.csv")
tests2 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_LOO/4comps(n=86)_train/stats.csv")
rsquared2 <- tests2$R.val^2
lb2 <- paste("R^2 == ", round(rsquared2,2))
RMSE2 <- paste("RMSE==", round(RMSE(file2$yhat.cal, file2$yhat.val),1))
str(file2)
plot2_resize <- ggplot(file2, aes(y=yhat.cal, x=yhat.val))+
  geom_point(aes(fill="tomato2"), shape=21, colour="black", size=2)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("Measured Jmax")+
  ylab("Predicted Jmax")+
  xlim(60, 275)+
  ylim(60, 275)+
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
  annotate("text", label = lb2, parse=TRUE, x = 225, y = 110, size = 4, colour = "Black")+
  annotate("text", label = RMSE2, parse=TRUE, x = 225, y = 80, size = 4, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("b) Jmax - leave-one-out") + 
  theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))+
  guides(fill=FALSE)


#80-20
#Vcmax
file3 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_80_20_fig/2comps(n=68)_test/fittedvalue_test.csv")
tests3 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_80_20_fig/2comps(n=68)_test/stats.csv")
rsquared3 <- tests3$R.test^2
lb3 <- paste("R^2 == ", round(rsquared3,2))
RMSE3 <- paste("RMSE==", round(RMSE(file3$y.2.comps, file3$y.test),1))
plot3_resize <- ggplot(file3, aes(y=y.test, x=y.2.comps))+
  geom_point(aes(fill="tomato2"), shape=21, colour="black", size=3)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("Measured Vcmax")+
  ylab("Predicted Vcmax")+
  xlim(35, 150)+
  ylim(35, 150)+
   theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
  annotate("text", label = lb3, parse=TRUE, x = 120, y = 60, size = 4, colour = "Black")+
  annotate("text", label = RMSE3, parse=TRUE, x = 120, y = 45, size = 4, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("c) Vcmax - representative 80/20") +
  theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))+
  guides(fill=FALSE)


#Jmax
file4 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_80_20_fig/4comps(n=68)_test/fittedvalue_test.csv")
tests4 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_80_20_fig/4comps(n=68)_test/stats.csv")
rsquared4 <- tests4$R.test^2
lb4 <- paste("R^2 == ", round(rsquared4,2))
RMSE4 <- paste("RMSE==", round(RMSE(file4$y.4.comps, file4$y.test),1))

plot4_resize <- ggplot(file4, aes(y=y.test, x=y.4.comps))+
  geom_point(aes(fill="tomato2"), shape=21, colour="black", size=3)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("Measured Jmax")+
  ylab("Predicted Jmax")+
  xlim(60, 275)+
  ylim(60, 275)+
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
  annotate("text", label = lb4, parse=TRUE, x = 225, y = 110, size = 4, colour = "Black")+
  annotate("text", label = RMSE4, parse=TRUE, x = 225, y = 80, size = 4, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("d) Jmax - representative 80/20") + 
  theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))+
  guides(fill=FALSE)

#Hardest
#Vcmax
file5 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_test_stressed_Period/2comps(n=73)_test/fittedvalue_test.csv")
tests5 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_test_stressed_Period/2comps(n=73)_test/stats.csv")
rsquared5 <- tests5$R.test^2
lb5 <- paste("R^2 == ", round(rsquared5,2))
RMSE5 <- paste("RMSE==", round(RMSE(file5$y.2.comps, file5$y.test),1))

plot5_resize <- ggplot(file5, aes(y=y.test, x=y.2.comps))+
  geom_point(aes(fill="tomato2"), shape=21, colour="black", size=3)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("Measured Vcmax")+
  ylab("Predicted Vcmax")+
  xlim(35, 150)+
  ylim(35, 150)+
  
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
  annotate("text", label = lb5, parse=TRUE, x = 120, y = 60, size = 4, colour = "Black")+
  annotate("text", label = RMSE5, parse=TRUE, x = 120, y = 45, size = 4, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("e) Vcmax - tested on variable water potential") +
  theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))+
  guides(fill=FALSE)


#Jmax
file6 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_test_stressed_Period/5comps(n=73)_test/fittedvalue_test.csv")
tests6 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_test_stressed_Period/5comps(n=73)_test/stats.csv")
rsquared6 <- tests6$R.test^2
lb6 <- paste("R^2 == ", round(rsquared6,2))
RMSE6 <- paste("RMSE==", round(RMSE(file6$y.5.comps, file6$y.test),1))


plot6_resize <- ggplot(file6, aes(y=y.test, x=y.5.comps))+
  geom_point(aes(fill="tomato2"), shape=21, colour="black", size=3)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("Measured Jmax")+
  ylab("Predicted Jmax")+
  xlim(60, 275)+
  ylim(60, 275)+
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
  annotate("text", label = lb6, parse=TRUE, x = 225, y = 110, size = 4, colour = "Black")+
  annotate("text", label = RMSE5, parse=TRUE, x = 225, y = 80, size = 4, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("f) Jmax - tested on variable water potential") +
  theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))+
  guides(fill=FALSE)

tiff(filename = "Figure_3_done.tiff", width=13, height=13, unit="cm", res = 300)
grid.arrange(plot1_resize,plot2_resize,plot3_resize,plot4_resize,plot5_resize,plot6_resize, ncol=2)
dev.print(tiff, 'figure_3_again.TIFF', width=8, height=6, unit="in", res=300)

dev.off()
plot1_resize
plot2_resize
plot3_resize
plot4_resize
plot5_resize
plot6_resize
