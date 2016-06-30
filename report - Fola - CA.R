
# specify working directory
setwd("H:/Data Science Fundamentals/Group work")


#installing packages
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

clean_screen <- read.csv("tbl_final_screen_data_ENG_clean.csv", header=T)

# GCMS

#filtering the category of the chemical groups
GCMS<-subset(clean_screen, MEAS_DETERMINAND_CODE ==4994)

# checking the class of each group using lappy
#lapply(GCMS,class)
#lapply(LCMS,class)
#lapply(ICP,class)

#chemical frequency and arrange in a data frame for GCMS group

GCMS$Det <- as.vector(GCMS$Det)
#table(unlist(GCMS$Det))
GCMS_chem <- as.data.frame(table(unlist(GCMS$Det)))
colnames(GCMS_chem)<- c("CHEMICAL", "FREQUENCY")
GCMS_chem <- GCMS_chem[ order(GCMS_chem$FREQUENCY, decreasing=TRUE), ]
#write the GCMS chemical back to csv file
#write.csv(GCMS_chem,"H:/R programming/GWORK/GCMS_chem.csv")
# GCMS_p<-ggplot(GCMS_chem, aes(x=CHEMICAL, y=FREQUENCY)) + 
#   geom_bar(stat="identity") +
#   xlab("Chemical") + 
#   ylab("Frequency") +
#   theme_bw(base_size=8) +  
#   ggtitle("Graph showing frequency of all chemicals in GCMS")
# print(GCMS_p)
#

GCMS_chem2 <- head(GCMS_chem,21)
# removing no target based from the table
GCMS_chem2<-GCMS_chem2[-c(4),]
GCMS_chem2<- GCMS_chem2[ order(GCMS_chem2$CHEMICAL),]

# ggplot(GCMS_chem2, aes(x = CHEMICAL, y = FREQUENCY)) + 
#   geom_bar(stat="identity", fill = "blue") + coord_flip()+
#   ggtitle("Top 20 chemicals in GCMS with there frequency")


#--------------
TRIAL <- data.frame(CHEMICAL = rownames(GCMS_chem2), GCMS_chem2, row.names = NULL)
TRIAL <- TRIAL[order(TRIAL$FREQUENCY),]
TRIAL$CHEMICAL.1 <- factor(TRIAL$CHEMICAL.1, levels = TRIAL$CHEMICAL.1[order(TRIAL$FREQUENCY)])
#TRIAL$CHEMICAL

# ggplot(TRIAL, aes(x = CHEMICAL.1, y = FREQUENCY)) + geom_bar(stat = "identity", fill = "green") + 
#   coord_flip()+ggtitle("frequency of top 20 chemicals in GCMS")


# ===============================================================
# CA EDITS OF PLOT 1
ggplot(TRIAL, aes(x = CHEMICAL.1, y = FREQUENCY)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  coord_flip() +
  ylab("Frequency") + 
  xlab("") +
  theme_bw(base_size=16) 

# ===============================================================

# Just top 20 freq chemical names
top <- select(TRIAL, CHEMICAL.1)
names(top) <- "Det"

# Concentrations
GCMS_con <- select(GCMS, Det, Result)

# Remove the no target compounds found
GCMS_con <- filter(GCMS_con, Det!= "No target based compounds detected")

# Merge top 20 chemicals 
top_con <- merge(top, GCMS_con, "Det")

# Check

#head(table(top_con$Det),20)

# =======================================================================
# Box plot of concentration

# Without limits
# ggplot(data= top_con, aes(x = Det, y = Result)) + 
#   geom_boxplot(colour = "red") +
#   coord_flip() + 
#   ylab("Concentration") + 
#   xlab("") +
#   theme_bw(base_size=16) 


ggplot(data= top_con, aes(x = Det, y = Result)) + 
      geom_boxplot(colour = "red", outlier.colour="grey") +
      coord_flip() + 
      ylim(c(0,0.012)) +
      ylab("Concentration") + 
      xlab("") +
      stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="blue") +
      theme_bw(base_size=16) 

# Checking how many chemicals just have one record
length(GCMS_chem$FREQUENCY[GCMS_chem$FREQUENCY==1])
# 416 
length(GCMS_chem$FREQUENCY[GCMS_chem$FREQUENCY==2])
# 99
length(GCMS_chem$FREQUENCY[GCMS_chem$FREQUENCY<=5])
# 655



#================================================================
# ICP

ICP <-subset(clean_screen, MEAS_DETERMINAND_CODE ==3401)

ICP$Det <- factor(ICP$Det)
table(unlist(ICP$Det))
ICP_chem <- as.data.frame(table(unlist(ICP$Det)))
colnames(ICP_chem)<- c("CHEMICAL", "FREQUENCY")
ICP_chem <- ICP_chem[ order(ICP_chem$FREQUENCY, decreasing=TRUE), ]
#write.csv(ICP_chem,"H:/R programming/GWORK/ICP.csv")
ICP1<-head(ICP_chem,21)
#ICP1<-ICP1[-c(1),] #removing the Na row
ICP1<- ICP1[ order(ICP1$CHEMICAL),]
===========================
# re-order the data frequency
ICPCHEM <- data.frame(CHEMICAL = rownames(ICP1), ICP1, row.names = NULL)
ICPCHEM <- ICPCHEM[order(ICPCHEM$FREQUENCY),]
ICPCHEM$CHEMICAL.1 <- factor(ICPCHEM$CHEMICAL.1, levels = ICPCHEM$CHEMICAL.1[order(ICPCHEM$FREQUENCY)])
# re-order the data frequency 

# ===============================================================
# CA EDITS OF PLOT 2
# Replace the value of 4,933 for Na with 100 so that it plots ok
ICPCHEM[21, 3] = 100

ggplot(ICPCHEM, aes(x = CHEMICAL.1, y = FREQUENCY)) + 
  geom_bar(stat = "identity", fill = "blue2") + 
  coord_flip() +
  ylab("Frequency") + 
  xlab("") +
  ylim(c(0,100)) +
  geom_text(aes(x="Na", y=85, label="Na frequency = 4,933"), colour= "white") +
  theme_bw(base_size=16) 

# Just top 20 freq chemical names
topICP <- select(ICPCHEM, CHEMICAL.1)
names(topICP) <- "Det"

# Concentrations
ICP_con <- select(ICP, Det, Result)

# Merge top 20 chemicals 
top_conICP <- merge(topICP, ICP_con, "Det")

# Check
#head(table(top_conICP$Det),20)

# =======================================================================
# Box plot of concentration

# Without limits
# ggplot(data= top_conICP, aes(x = Det, y = Result)) + 
#    geom_boxplot(colour = "red") +
#    coord_flip() + 
#    ylab("Concentration") + 
#    xlab("") +
#    theme_bw(base_size=16) 

# With limit of axis reduced
ggplot(data= top_conICP, aes(x = Det, y = Result)) + 
  geom_boxplot(colour = "red", outlier.colour="grey") +
  coord_flip() + 
  ylim(c(0,90)) +
  ylab("Concentration") + 
  xlab("") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="blue") +
  theme_bw(base_size=16)


# Checking how many chemicals just have one record
length(ICP_chem$FREQUENCY[ICP_chem$FREQUENCY==1])
# 12
length(ICP_chem$FREQUENCY[ICP_chem$FREQUENCY==2])
# 3
length(ICP_chem$FREQUENCY[ICP_chem$FREQUENCY<=5])
# 16





# ===============================================================


# ggplot(ICPCHEM, aes(x = CHEMICAL.1, y = FREQUENCY)) + geom_bar(stat = "identity", fill = "red") + 
#   ggtitle("frequency of top 20 chemicals in ICP")
#=======================================================================
# # frequncy boxplot for ICP chemicals
# ggplot(data= ICPCHEM, aes(x = CHEMICAL.1, y = FREQUENCY)) + geom_boxplot(colour = "green")+ 
#   stat_summary(fun.y = mean, geom="point", shape =18, size =5, show_guide = FALSE, position=position_dodge(width=0.7, height = 0))+
#   ggtitle("boxplot frequency of top 20 chemicals in ICP")
# 
# mean(na.omit(ICP1$FREQUENCY))
# median(na.omit(ICP1$FREQUENCY))
# summary(ICP1$FREQUENCY)
# summary(ICP1)
# quantile(ICP1$FREQUENCY)
# min(ICP1$FREQUENCY)
# max(ICP1$FREQUENCY)
# sd(ICP1$FREQUENCY)
# # means per chemical rows
# icp <- as.data.table(ICP1)
# icp[,list(Mean = rowMeans(.SD)), by = CHEMICAL]
# # least 10 chemicals in icp
# ICP2 <- tail(ICP_chem, 10)
# ICP2
# ggplot(ICP2, aes(x = CHEMICAL, y = FREQUENCY)) + 
#   geom_bar(stat="identity", fill = "blue") + ggtitle("least 10 chemicals in ICP")
# # boxplot frequency
# ggplot(ICP2, aes(x = CHEMICAL, y = FREQUENCY)) + 
#   geom_boxplot()
# 
# #runm<- select(ICP, Det, SAMP_SAMPLE_DATE,SAMP_SAMPLE_TIME)
# #runm$SAMP_SAMPLE_DATE <- month(runm$SAMP_SAMPLE_DATE)
# #ICPder_grp <- group_by(runm, SAMP_SAMPLE_DATE, Det)
# #ICPder_summ <- summarise(ICPder_grp,  freq = n())
# #ICP_Month <- ggplot(ICPder_summ, aes(x=month(SAMP_SAMPLE_DATE), y =freq, colour = Det)) + geom_point() + 
#   #labs(x ="ICP chemical Month", y =" Number of chemical monthly", colour = "Det")+ scale_x_discrete(limits=c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + theme_bw(base_size=20) +theme(legend.position="right")+ggtitle("DET WITH MONTHS")
# #print(ICP_Month)

#============================================================
# LCMS

LCMS<-subset(clean_screen, MEAS_DETERMINAND_CODE ==7299)

LCMS$Det <- as.vector(LCMS$Det)
table(unlist(LCMS$Det))
LCMS_chem <- as.data.frame(table(unlist(LCMS$Det)))
colnames(LCMS_chem)<- c("CHEMICAL", "FREQUENCY")
LCMS_chem <- LCMS_chem[ order(LCMS_chem$FREQUENCY, decreasing=TRUE), ]
LCMS_chem1<-head(LCMS_chem,20)
LCMS_chem1<- LCMS_chem1[ order(LCMS_chem1$CHEMICAL),]

# LCMS_chems<- ggplot(LCMS_chem1, aes(x=CHEMICAL, y=FREQUENCY)) + 
#   geom_bar(stat="identity") +coord_flip()
#   xlab("Chemical") + 
#   ylab("Frequency") +
#   theme_bw(base_size=8) +  
#   ggtitle("Graph showing frequency of top 20 chemicals in LCMS without NA")
#print(LCMS_chems)

# re oredring the frequency
LCMD_order <- data.frame(CHEMICAL = rownames(LCMS_chem1), LCMS_chem1, row.names = NULL)
LCMD_order <- LCMD_order[order(LCMS_chem1$FREQUENCY),]
LCMD_order$CHEMICAL.1 <- factor(LCMD_order$CHEMICAL.1, levels = LCMD_order$CHEMICAL.1[order(LCMD_order$FREQUENCY)])


# ===============================================================
# CA EDITS OF PLOT 3
ggplot(LCMD_order, aes(x = CHEMICAL.1, y = FREQUENCY)) + 
  geom_bar(stat = "identity", fill = "blue2") + 
  coord_flip() +
  ylab("Frequency") + 
  xlab("") +
  theme_bw(base_size=16) 

# ===============================================================

# Just top 20 freq chemical names
topLCMS <- select(LCMD_order, CHEMICAL.1)
names(topLCMS) <- "Det"

# Concentrations
LCMS_con <- select(LCMS, Det, Result)

# Merge top 20 chemicals 
top_conLCMS <- merge(topLCMS, LCMS_con, "Det")

# Check
#head(table(top_conLCMS$Det),20)

# =======================================================================
# Box plot of concentration

# Without limits
ggplot(data= top_conLCMS, aes(x = Det, y = Result)) + 
   geom_boxplot(colour = "red") +
   coord_flip() + 
   ylab("Concentration") + 
   xlab("") +
   theme_bw(base_size=16) 

# With limit of axis reduced
ggplot(data= top_conLCMS, aes(x = Det, y = Result)) + 
  geom_boxplot(colour = "red", outlier.colour="grey") +
  coord_flip() + 
  ylim(c(0,0.001)) +
  ylab("Concentration") + 
  xlab("") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, colour="blue") +
  theme_bw(base_size=16)



# Checking how many chemicals just have one record
length(LCMS_chem$FREQUENCY[LCMS_chem$FREQUENCY==1])
# 46
length(LCMS_chem$FREQUENCY[LCMS_chem$FREQUENCY==2])
# 26
length(LCMS_chem$FREQUENCY[LCMS_chem$FREQUENCY<=5])
# 140 




