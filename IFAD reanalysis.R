#getwd()
#setwd
#==========================================================
library(dplyr)
library(ggplot2)
library(stringr)
library(nortest)
library(corrplot)
library(FactoMineR)
library(fastcluster)
library(flashClust)
library(foreign)
#library(Formular)
library(labeling)
library(Hmisc)
library(manipulate)
library(mice)
library(psych)
library(reshape2)
library(pvclust)
library(ade4)
#library(clusplot)
#scales"
library(fpc)
#===========================================================
IFAD <- read.csv("IFAD clean datas.csv", header = TRUE)
dim(IFAD)
summary(IFAD)
#summary(IFAD$Age_Hhhead)
#============checking for missing number
#IFAD[!complete.cases(IFAD),];
# seperate continous and discrete variables
data<-subset(IFAD,select=c(1,3,5,7,8,9,10,11,13,14,16,17,18,19))
datas<-subset(IFAD, select=c(2,4,6,12,15,20))
#========================== summary of the continus and the discrete
#summary(data)
#summary(datas)
#=======================================using imputational method of replacing missing number
md.pattern(data)
p<-md.pairs(data)
p
imp<-mice(data, seed = 1000)
print(imp)
fruty<-complete(imp)
#stripplot(imp, pch=20,cex=1.2, main="continuous variables Predictive mean matching")
summary(fruty)
dim(fruty)
#summary(fruty$Age_Hhhead)
#sd(fruty$farmsize_hecter)
#==============for discrete data===============datas
md.pattern(datas)
p1<-md.pairs(datas)
p1
imp1<-mice(datas, seed = 1000)
#print(imp1)
discre<-complete(imp1)
#stripplot(imp1, pch=20,cex=1.2, main = "stripplot of discrete variable imputed mean number")
#summary(discre)
#table(discre$Gender)->Gender1
#prop.table(Gender1)*100
#===============final data for the analysis=======================
IFADPROJECT<-merge(fruty, discre, by="row.names", all.x=TRUE)
IFADPROJECT$Row.names<-NULL
summary(IFADPROJECT)
#pairs(IFADPROJECT, panel=panel.smooth)
#hist(IFADPROJECT)
#boxplot(dot$Farm_dist)
#====================== correlation test
R<-cor(IFADPROJECT)
corredata<-as.data.frame(R)
#=====================================
# Bartlett test for hypothesis 
n <-nrow(IFADPROJECT)
p <-ncol(IFADPROJECT)
chi2 <- -(n-1-(2*p+5)/6)*log(det(R))
ddl <- p*(p-1)/2
print(chi2)
print(ddl)
print(pchisq(chi2,ddl,lower.tail=F))
#
# KMO test
invR <-solve(R)
A <- matrix(1,nrow(invR),ncol(invR))
for (i in 1:nrow(invR)){
  for (j in (i+1):ncol(invR)){
    A[i,j] <--invR[i,j]/sqrt(invR[i,i]*invR[j,j])
    A[j,i] <- A[i,j]
  }
}
colnames(A) <- colnames(IFADPROJECT)
rownames(A) <- colnames(IFADPROJECT)
totkm<- as.data.frame(A)
#print(A)
kmo.num <-sum(R^2) - sum(diag(R^2))
kmo.denom <-kmo.num + (sum(A^2) - sum(diag(A^2)))
kmo <-kmo.num/kmo.denom
print(kmo)

#===============PCA with ade4=================
#IFAD.pca<-dudi.pca(IFADPROJECT, center=T, scale=T, scannf=F, nf=5)

#row.w=rep(1,nrow(dot))/nrow(dot),col.w=rep(1,ncol(dot)),center=TRUE, scale=TRUE, scannf=FALSE, nf = 8)
#IFAD.pca$eig
#IFAD.pca$cw
#sum(IFAD.pca$eig)
#barplot(IFAD.pca$eig, main="barplot of Eigenvalue from PCA")
#(stp<-100*IFAD.pca$eig/sum(IFAD.pca$eig))
#cumsum(stp)
#IFAD.pca$rank
#IFAD.pca$c1
#IFAD.pca$li
#IFAD.pca$co
#IFAD.pca$call
#IFAD.pca$norm
#s.corcircle(IFAD.pca$co,xax=1,yax=3)#, main= "Relationship between COMP1 and COMP3")
#s.label(IFAD.pca$li, xax=1,yax=3)
#s.label(IFAD.pca$li, xax=1,yax=2)
#s.label(IFAD.pca$li, xax=1,yax=4)
#s.label(IFAD.pca$li, xax=1,yax=5)
#s.label(IFAD.pca$li, xax=1,yax=6)
#s.label(IFAD.pca$li, xax=1,yax=7)
#s.label(IFAD.pca$li, xax=1,yax=8)
#scatter(IFAD.pca)
#s.corcircle(IFAD.pca$co,xax=1, yax=2)
#s.class(IFAD.pca$li, fac=as.factor(IFAD.type))
##########################################Hierachical cluster#############
#IFAD.typo<-hclust(dist(IFAD.pca$li), method="ward")
#barplot(IFAD.typo$height, main="Height of the cluster")
#plot(IFAD.typo)
#groups<-cutree(IFAD.typo,k=5)
#table(groups)
#rect.hclust(IFAD.typo, k=5, border="red")
#IFAD.pv<-pvclust(IFAD.pca$li, method.dist="euclidean")
#plot(IFAD.pv)
#===============================================
#============================ PCA WITH FACTOMiner=================================
IFAD.pca<-PCA(IFADPROJECT,scale.unit = TRUE, graph = TRUE)
IFAD.pca
IFAD.pca$var$cor
IFAD.pca$eig
plot(IFAD.pca, choix="var",shadow=FALSE, cex=0.6)
plot(IFAD.pca, choix="ind",shadow=FALSE, cex=0.6)
IFAD.hcpc<-HCPC(IFAD.pca, nb.clust=5, min=3, max = 10)
#Hum.hcpc$call$t$res
IFAD.hcpc$desc.var
IFAD.hcpc$desc.ind

# K-Mean calgorithm
##for (i in 2:15)wss[i]<-sum(kmeans(IFADPROJECT,centers=i)$withinss)
#plot(1:15, wss, type="b", xlab="Number of clusters", ylab= "withingroups sum of square")

#KIFAD<-scale(IFADPROJECT)
#fit1<-kmeans(KIFAD, 5)
#aggregate(KIFAD, by=list(fit1$cluster),FUN=mean)
#fred<-data.frame(KIFAD, fit1$cluster)
#plot(fit1)

#fit1<-kmeans(IFADPROJECT, 5)
#aggregate(IFADPROJECT, by=list(fit1$cluster),FUN=mean)
#fred<-data.frame(IFADPROJECT, fit1$cluster)
#plot(fit1)
#pamk.best<-pamk(IFAD.pca$li)
#cat("Number of clusters estimated by optimum average", pamk.best$nc, "\n")
#plot(pam(IFAD.pca$li, pamk.best$nc))
#======================================Humditropic section===========

# humidtropic clustering analysis
#almost the same step taken but the different variable samples
fop<-read.csv("Finalcleandata2.csv", header=TRUE)
#discerte<-subset(fop,select=c(9,19,20,21,22))
#continous<-subset(fop,select=c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,20))
#meger<-merge(continous, discerte, by="row.names", all.x=TRUE)
#rm(megers)
#megers<-subset(meger, select=c(8,9,10,11,1,2,13,14,15,3,16,17,21,22,4,5,6,7,12,18,19,20,23,24,25))
#meger$Row.names<-NULL

summary(fop)
R2<-cor(fop)
codata<-as.data.frame(R2)
write.csv(codata, file="correlation table Humid.csv", row.names=FALSE)

# Bartlett test for hypothesis 
n <-nrow(fop)
p <-ncol(fop)
chi2 <- -(n-1-(2*p+5)/6)*log(det(R2))
ddl <- p*(p-1)/2
print(chi2)
print(ddl)
print(pchisq(chi2,ddl,lower.tail=F))
#
# KMO test
invR <-solve(R2)
A <- matrix(1,nrow(invR),ncol(invR))
for (i in 1:nrow(invR)){
  for (j in (i+1):ncol(invR)){
    A[i,j] <--invR[i,j]/sqrt(invR[i,i]*invR[j,j])
    A[j,i] <- A[i,j]
  }
}
colnames(A) <- colnames(meger)
rownames(A) <- colnames(meger)
totkm<- as.data.frame(A)
#print(A)
kmo.num <-sum(R2^2) - sum(diag(R2^2))
kmo.denom <-kmo.num + (sum(A^2) - sum(diag(A^2)))
kmo <-kmo.num/kmo.denom
print(kmo)
#===========================================================================
#pairs(meger, panel=panel.smooth)
#hist(meger)

#boxplot(meger$hiredlabour_cost)
#boxplot(meger$sales_feed, main= "sales_feed plot after removal of outliers")
#meger<-meger[meger$sales_feed<20000,]
#meger<-meger[meger$hiredlabour_cost <20000,]

#meger$row.names<-NULL
#==========================FActoMiner for the clustering and PCA
Hum.pca<-PCA(fop,scale.unit = TRUE, graph = TRUE)
Hum.pca
Hum.pca$var$cor
Hum.pca$eig
plot(Hum.pca, choix="var",shadow=FALSE, cex=0.6)
plot(Hum.pca, choix="ind",shadow=FALSE, cex=0.2)
Hum.hcpc<-HCPC(Hum.pca, nb.clust=5, min=3, max = 10)
#Hum.hcpc$call$t$res
Hum.hcpc$desc.var
Hum.hcpc$desc.ind

# K-Mean calgorithm
wss<-(nrow(fop)-1)*sum(apply(fop,2,var))
for (i in 2:15)wss[i]<-sum(kmeans(fop,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of clusters", ylab= "withingroups sum of square")

huim<-scale(fop)
fit2<-kmeans(huim, 5)
aggregate(huim, by=list(fit1$cluster),FUN=mean)
fred1<-data.frame(huim, fit1$cluster)
plot(fit2)

#fit2<-kmeans(meger, 5)
#aggregate(meger, by=list(fit1$cluster),FUN=mean)
#fred1<-data.frame(meger, fit1$cluster)
#plot(fit2)
#pamk.best<-pamk(IFAD.pca$li)
#cat("Number of clusters estimated by optimum average", pamk.best$nc, "\n")
#plot(pam(IFAD.pca$li, pamk.best$nc))

#pca1=PCA(meger, graph=TRUE, ncp=9)
#summary(pca1)
#plot(pca1,shadow=FALSE, cex=0.6)
#plot(pca1, choix="var",shadow=TRUE, cex=0.6)
#plot(pca1, choix="ind", shadow=TRUE, cex=0.6)
#pca1
#pca1$eig
#pca1$var
#pca1$var$cor
#pca1$ind$coord
#================
#res.hcpc<-HCPC(pca1, consol=TRUE)
#res.hcpc$data.clust
#res.hcpc$call$t$tree
#res.hcpc
#res.hcpc$desc.var
#dim(meger)


#===========================================
#Humid.pca<-dudi.pca(meger, center=T, scale=T, scannf=F, nf=9)
#row.w=rep(1,nrow(meger))/nrow(meger),col.w=rep(1,ncol(meger)),center=TRUE, scale=TRUE, scannf=FALSE, nf = 9)
#Humid.pca
#Humid.pca$eig
#Humid.pca$cw
#sum(Humid.pca$eig)
#barplot(Humid.pca$eig, main="barplot of Eigenvalue from PCA in HUMDITROPIC DATASET")
#(stp<-100*Humid.pca$eig/sum(Humid.pca$eig))
#cumsum(stp)
#boxplot(megers$total_land)
#megers<-megers[megers$total_land<500,]
#boxplot(megers$total_land)
#boxplot(merges$sales_feed)
#megers<-megers[megers$sales_feed<-20000,]
#Humid.pca$rank
#Humid.pca$c1
#Humid.pca$li
#Humid.pca$co
#Humid.pca$call
#Humid.pca$norm
#loading<- as.data.frame(Humid.pca$li)

#####################################################
#Humid.typo<-hclust(dist(Humid.pca$li), method="ward")
#barplot(Humid.typo$height, main="Height of the cluster in the Humidtropic")
#plot(Humid.typo)
#groups<-cutree(Humid.typo,k=8)
#rect.hclust(Humid.typo, k=8, border="red")
#Humid.typo

#====================================================== kmeams
#maga<-scale(meger)
#wss<-(nrow(maga)-1)*sum(apply(maga,2,var))
#for (i in 2:15)wss[i]<-sum(kmeans(maga,centers=i)$withinss)
#plot(1:15, wss, type="b", xlab="Number of clusters", ylab= "withingroups sum of square")


#fits<-kmeans(maga, 8)
#cvty<-aggregate(maga, by=list(fits$cluster),FUN=mean)
#fred<-data.frame(maga, fits$cluster)
#clusplot(maga,fits$cluster, color=TRUE,shade=TRUE,labels=2, lines=0)
#plotcluster(maga,fits$cluster)
#fits$cluster
#=================validation===========
cluster.stats(Humid.typo,fits$cluster)
#==================================
