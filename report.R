#installing packages
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
clean_screen <- read.csv("tbl_final_screen_data_ENG_clean.csv", header=T)
#filtering the category of the chemical groups
GCMS<-subset(clean_screen, MEAS_DETERMINAND_CODE ==4994)
# checking the class of each group using lappy
#lapply(GCMS,class)
#lapply(LCMS,class)
#lapply(ICP,class)

#chemical frequency and arrange in a data frame for GCMS group
#GCMS$Det <- factor(GCMS$Det)
GCMS$Det <- as.vector(GCMS$Det)
table(unlist(GCMS$Det))
GCMS_chem <- as.data.frame(table(unlist(GCMS$Det)))
colnames(GCMS_chem)<- c("CHEMICAL", "FREQUENCY")
GCMS_chem <- GCMS_chem[ order(GCMS_chem$FREQUENCY, decreasing=TRUE), ]
#write the GCMS chemical back to csv file
write.csv(GCMS_chem,"H:/R programming/GWORK/GCMS_chem.csv")
GCMS_p<-ggplot(GCMS_chem, aes(x=CHEMICAL, y=FREQUENCY)) + 
  geom_bar(stat="identity") +
  xlab("Chemical") + 
  ylab("Frequency") +
  theme_bw(base_size=8) +  
  ggtitle("Graph showing frequency of all chemicals in GCMS")
print(GCMS_p)
#

GCMS_chem2 <- head(GCMS_chem,21)
# removing no target based from the table
GCMS_chem2<-GCMS_chem2[-c(4),]
GCMS_chem2<- GCMS_chem2[ order(GCMS_chem2$CHEMICAL),]

ggplot(GCMS_chem2, aes(x = CHEMICAL, y = FREQUENCY)) + 
  geom_bar(stat="identity", fill = "blue") + coord_flip()+
  ggtitle("Top 20 chemicals in GCMS with there frequency")
#--------------
TRIAL <- data.frame(CHEMICAL = rownames(GCMS_chem2), GCMS_chem2, row.names = NULL)
TRIAL <- TRIAL[order(TRIAL$FREQUENCY),]
TRIAL$CHEMICAL.1 <- factor(TRIAL$CHEMICAL.1, levels = TRIAL$CHEMICAL.1[order(TRIAL$FREQUENCY)])
#TRIAL$CHEMICAL

ggplot(TRIAL, aes(x = CHEMICAL.1, y = FREQUENCY)) + geom_bar(stat = "identity", fill = "green") + 
  coord_flip()+ggtitle("frequency of top 20 chemicals in GCMS")

#box plot to find the concentration of GCMS chemical
ggplot(data= TRIAL, aes(x = CHEMICAL.1, y = FREQUENCY)) + geom_boxplot(colour = "red")+
  coord_flip() + 
  stat_summary(fun.y = mean, geom="point", shape =18, size =5, show_guide = FALSE, position=position_dodge(width=0.7, height = 0))+
  ggtitle("boxplot frequency of top 20 chemicals in GCMS")

# least 10 chemcals in GCMS
GCMS_chem3 <- tail(GCMS_chem,11)
# removing The chromatogram shows a series of phthalates and phthalic acids
GCMS_chem3<-GCMS_chem3[-c(2),]
ggplot(GCMS_chem3, aes(x = CHEMICAL, y = FREQUENCY)) + 
  geom_bar(stat="identity", fill = "pink") + coord_flip()+ggtitle("least 10 chemicals in GCMS")
# boxplot frequency
ggplot(GCMS_chem3, aes(x = CHEMICAL, y = FREQUENCY)) + 
  geom_boxplot()

# finding the mean, median, first qurter, thrid quarter of the gcms chemical
mean(na.omit(GCMS_chem2$FREQUENCY))
median(na.omit(GCMS_chem2$FREQUENCY))
summary(GCMS_chem2$FREQUENCY)
summary(GCMS_chem2)
quantile(GCMS_chem2$FREQUENCY)
min(GCMS_chem2$FREQUENCY)
max(GCMS_chem2$FREQUENCY)
sd(GCMS_chem2$FREQUENCY)
# means per chemical
gcms <- as.data.table(GCMS_chem2)
gcms[,list(Mean = rowMeans(.SD)), by = CHEMICAL]
#summary(GCMS2)
#sort(table(GCMS$Region), decreasing=TRUE)
# merging file with the region
#rg<-select(GCMS, Region)
#rgemeg<- merge(rg,GCMS_chem,  all = TRUE)
#rg_grp <- group_by(rg, GCMS_chem)
#rg_summ <- summarise(rg_grp,  freq = n())
#rgplt<- ggplot(rg_summ, aes(x=Region, y =freq, fill = "Det")) + geom_bar(stat ="identity") 
#print(rgplt)
#===========================================
# gcms by sample date and month
#trunm<- select(GCMS, Det, SAMP_SAMPLE_DATE,SAMP_SAMPLE_TIME)
#trunm$SAMP_SAMPLE_DATE <- month(trunm$SAMP_SAMPLE_DATE)
#der_grp <- group_by(trunm, SAMP_SAMPLE_DATE, Det)
#der_summ <- summarise(der_grp,  freq = n())
#GCMS2 <- head(der_summ,20)
 
#summary(der_summ)
#GCM_Month <- ggplot(GCMS2, aes(x=month(SAMP_SAMPLE_DATE), y =freq, colour = Det)) + geom_point() + 
  #labs(x ="GCMS chemical Month", y =" Number of chemical monthly", colour = "Det")+ scale_x_discrete(limits=c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + theme_bw(base_size=20) +theme(legend.position="right")+ggtitle("DET WITH MONTHS")
#print(GCM_Month)
#================================================================
ICP$Det <- factor(ICP$Det)
ICP <-subset(clean_screen, MEAS_DETERMINAND_CODE ==3401)
table(unlist(ICP$Det))
ICP_chem <- as.data.frame(table(unlist(ICP$Det)))
colnames(ICP_chem)<- c("CHEMICAL", "FREQUENCY")
ICP_chem <- ICP_chem[ order(ICP_chem$FREQUENCY, decreasing=TRUE), ]
write.csv(ICP_chem,"H:/R programming/GWORK/ICP.csv")
ICP1<-head(ICP_chem,21)
ICP1<-ICP1[-c(1),] #removing the Na row
ICP1<- ICP1[ order(ICP1$CHEMICAL),]
ICPs<- ggplot(ICP1, aes(x=CHEMICAL, y=FREQUENCY)) + 
  geom_bar(stat="identity") +
  xlab("Chemical") + 
  ylab("Frequency") +
  theme_bw(base_size=8) +  
  ggtitle("Graph showing frequency of top 20 chemicals in ICP without NA")
print(ICPs)
#=======================================
# re-order the data frequency
ICPCHEM <- data.frame(CHEMICAL = rownames(ICP1), ICP1, row.names = NULL)
ICPCHEM <- ICPCHEM[order(ICPCHEM$FREQUENCY),]
ICPCHEM$CHEMICAL.1 <- factor(ICPCHEM$CHEMICAL.1, levels = ICPCHEM$CHEMICAL.1[order(ICPCHEM$FREQUENCY)])
# re-order the data frequency 
ggplot(ICPCHEM, aes(x = CHEMICAL.1, y = FREQUENCY)) + geom_bar(stat = "identity", fill = "red") + 
  ggtitle("frequency of top 20 chemicals in ICP")
#=======================================================================
# frequncy boxplot for ICP chemicals
ggplot(data= ICPCHEM, aes(x = CHEMICAL.1, y = FREQUENCY)) + geom_boxplot(colour = "green")+ 
  stat_summary(fun.y = mean, geom="point", shape =18, size =5, show_guide = FALSE, position=position_dodge(width=0.7, height = 0))+
  ggtitle("boxplot frequency of top 20 chemicals in ICP")

mean(na.omit(ICP1$FREQUENCY))
median(na.omit(ICP1$FREQUENCY))
summary(ICP1$FREQUENCY)
summary(ICP1)
quantile(ICP1$FREQUENCY)
min(ICP1$FREQUENCY)
max(ICP1$FREQUENCY)
sd(ICP1$FREQUENCY)
# means per chemical rows
icp <- as.data.table(ICP1)
icp[,list(Mean = rowMeans(.SD)), by = CHEMICAL]
# least 10 chemicals in icp
ICP2 <- tail(ICP_chem, 10)
ICP2
ggplot(ICP2, aes(x = CHEMICAL, y = FREQUENCY)) + 
  geom_bar(stat="identity", fill = "blue") + ggtitle("least 10 chemicals in ICP")
# boxplot frequency
ggplot(ICP2, aes(x = CHEMICAL, y = FREQUENCY)) + 
  geom_boxplot()

#runm<- select(ICP, Det, SAMP_SAMPLE_DATE,SAMP_SAMPLE_TIME)
#runm$SAMP_SAMPLE_DATE <- month(runm$SAMP_SAMPLE_DATE)
#ICPder_grp <- group_by(runm, SAMP_SAMPLE_DATE, Det)
#ICPder_summ <- summarise(ICPder_grp,  freq = n())
#ICP_Month <- ggplot(ICPder_summ, aes(x=month(SAMP_SAMPLE_DATE), y =freq, colour = Det)) + geom_point() + 
  #labs(x ="ICP chemical Month", y =" Number of chemical monthly", colour = "Det")+ scale_x_discrete(limits=c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + theme_bw(base_size=20) +theme(legend.position="right")+ggtitle("DET WITH MONTHS")
#print(ICP_Month)
#============================================================
LCMS$Det <- as.vector(LCMS$Det)
LCMS<-subset(clean_screen, MEAS_DETERMINAND_CODE ==7299)
table(unlist(LCMS$Det))
LCMS_chem <- as.data.frame(table(unlist(LCMS$Det)))
colnames(LCMS_chem)<- c("CHEMICAL", "FREQUENCY")
LCMS_chem <- LCMS_chem[ order(LCMS_chem$FREQUENCY, decreasing=TRUE), ]
LCMS_chem1<-head(LCMS_chem,20)
LCMS_chem1<- LCMS_chem1[ order(LCMS_chem1$CHEMICAL),]
LCMS_chems<- ggplot(LCMS_chem1, aes(x=CHEMICAL, y=FREQUENCY)) + 
  geom_bar(stat="identity") +coord_flip()
  xlab("Chemical") + 
  ylab("Frequency") +
  theme_bw(base_size=8) +  
  ggtitle("Graph showing frequency of top 20 chemicals in LCMS without NA")
print(LCMS_chems)
# re oredring the frequency
LCMD_order <- data.frame(CHEMICAL = rownames(LCMS_chem1), LCMS_chem1, row.names = NULL)
LCMD_order <- LCMD_order[order(LCMS_chem1$FREQUENCY),]
LCMD_order$CHEMICAL.1 <- factor(LCMD_order$CHEMICAL.1, levels = LCMD_order$CHEMICAL.1[order(LCMD_order$FREQUENCY)])
ggplot(LCMD_order, aes(x = CHEMICAL.1, y = FREQUENCY)) + geom_bar(stat = "identity", fill = "red") + 
  coord_flip()+ggtitle("frequency of top 20 chemicals in LCMS")
# box plot to calculate the mean
ggplot(data= LCMD_order, aes(x = CHEMICAL.1, y = FREQUENCY)) + geom_boxplot(colour = "red")+
  coord_flip() + 
  stat_summary(fun.y = mean, geom="point", shape =18, size =5, show_guide = FALSE, position=position_dodge(width=0.7, height = 0))+
  ggtitle("boxplot frequency of top 20 chemicals in lcms")

# least bottom 10 chemicals in lcms
LCMS3<- tail(LCMS_chem, 10)
ggplot(LCMS3, aes(x = CHEMICAL, y = FREQUENCY)) + 
  geom_bar(stat="identity", fill = "pink") + coord_flip()
# boxplot frequency
ggplot(LCMS3, aes(x = CHEMICAL, y = FREQUENCY)) + 
  geom_boxplot()+ coord_flip()
# finding the mean, median, first qurter, thrid quarter of the lcms chemical
mean(na.omit(LCMS_chem1$FREQUENCY))
median(na.omit(LCMS_chem1$FREQUENCY))
summary(LCMS_chem1$FREQUENCY)
#summary(GCMS_chem2)
quantile(LCMS_chem1$FREQUENCY)
min(LCMS_chem1$FREQUENCY)
max(LCMS_chem1$FREQUENCY)
sd(LCMS_chem1$FREQUENCY)
# means per chemical
lcms <- as.data.table(LCMS_chem1)
lcms[,list(Mean = rowMeans(.SD)), by = CHEMICAL]

#====================================================
mean(na.omit(LCMS_chem$FREQUENCY),)
median(na.omit(LCMS_chem$FREQUENCY))
summary(LCMS_chem)
summary(LCMS_chem$FREQUENCY)
write.csv(LCMS_chem,"H:/R programming/GWORK/LCMS.csv")
summary(LCMS1)
LCMS1<-head(LCMS_chem,30)
LCMS2<- ggplot(LCMS1, aes(x=CHEMICAL, y=FREQUENCY)) + 
  geom_bar(stat="identity", fill = "red") +geom_text(aes(y=2000, label=CHEMICAL, size=0.1, angle=90))+
  scale_x_discrete(labels="")+
  xlab("Chemical") + 
  ylab("Frequency") +
  theme_bw(base_size=8) +  
  ggtitle("Graph showing frequency of all chemicals in LCM")
print(LCMS2)
sort(table(LCMS$Region), decreasing=TRUE)
unm<- select(LCMS, Det, SAMP_SAMPLE_DATE,SAMP_SAMPLE_TIME)
unm$SAMP_SAMPLE_DATE <- month(unm$SAMP_SAMPLE_DATE)
LCMSUN_grp <- group_by(unm, SAMP_SAMPLE_DATE, Det)
LCMSUN_summ <- summarise(LCMSUN_grp,  freq = n())
LCMS_Month <- ggplot(LCMSUN_summ, aes(x=month(SAMP_SAMPLE_DATE), y =freq, colour = Det)) + geom_point() + 
  labs(x =" chemical Month", y =" Number of chemical monthly", colour = "Det")+ scale_x_discrete(limits=c("J", "F","M","A","Ma","J","Ju","Au","S","O","N","D")) + theme_bw(base_size=20) +theme(legend.position="right")+ggtitle("DET WITH MONTHS")
print(LCMS_Month)
#=================================================================
Regions <- select(GCMS, Region, Det)
Regions_chem<- group_by(Regions, Region,Det)
Regions_summ <- summarise(Regions_chem,  freq = n())
#region<- head(Regions_summ, 20)
L_region <- ggplot(Regions_summ, aes(x=Region, y=freq))+ geom_bar()+stat_bin()+
  theme_bw(base_size=8)   
print(L_region)
mean(Regions_summ$freq)
median(Regions_summ$freq)
#=======================================================================

