#-------------------Threshold approach
DO1 <- subset(All_ChemData, select=c("X", "Y", "Oxygen"), Oxygen<=1)
DO2 <- subset(All_ChemData, select=c("X", "Y", "Oxygen"), Oxygen<=2)
DO3 <- subset(All_ChemData, select=c("X", "Y", "Oxygen"), Oxygen<=3)
DO4 <- subset(All_ChemData, select=c("X", "Y", "Oxygen"), Oxygen<=4)

plot(map,col="Beige")

coordinates(DO1) <- ~ X + Y
writePointsShape(DO1, "DO1.shp")
DO1 <- readShapePoints("DO1.shp")
raster(DO1)
plot(DO1,cex = 0.8, pch = 20, col = "blue", add = T, main="")
title(main="537 Boreholess with Oxygen <= 1mg/L")
#legend(x='topright', legend = c("BH Oxygen < 1mg/L"), fill = "blue")
write.csv(DO1, "DO1.csv")
#----------------DO2-----------------------------
plot((map,col="Beige")

coordinates(DO2) <- ~ X + Y
writePointsShape(DO2, "DO2.shp")
DO2 <- readShapePoints("DO2.shp")
raster(DO2)
plot(DO2,cex = 0.8, pch = 20, col = "blue", add = T, main="") 
title(main="599 Boreholes with Oxygen concentration <= 2mg/L")
#legend(x='topright', legend = c("BH Oxygen 1-2mg/L"), fill = "blue")
write.csv(DO2, "DO2.csv")
#----------------DO3---------------------------------------------
plot((map,col="Beige")

coordinates(DO3) <- ~ X + Y
writePointsShape(DO3, "DO3.shp")
DO3 <- readShapePoints("DO3.shp")
raster(DO3)
plot(DO3,cex = 0.8, pch = 20, col = "blue", add = T, main="") 
title(main="620 Boreholes with Oxygen concentration <= 3mg/L")
#legend(x='topright', legend = c("BH Oxygen 2-3mg/L"), fill = "blue")
write.csv(DO3, "DO3.csv")
#----------------DO4---------------------------------------
plot((map,col="Beige")

coordinates(DO4) <- ~ X + Y
writePointsShape(DO4, "DO4.shp")
DO4 <- readShapePoints("DO4.shp")
raster(DO4)
plot(DO4,cex = 0.8, pch = 20, col = "blue", add = T, main="")
title(main="670 Boreholes with Oxygen concentration  <= 4mg/L")
#legend(x='topright', legend = c("BH Oxygen 3-4mg/L"), fill = "blue")
write.csv(DO4, "DO4.csv")
#------------------------------------DOC at 0.4mg/L and Oxygen at 4mg/L

DOCDO4 <- subset(All_ChemData, select=c("X", "Y", "Oxygen", "Carbon"), Oxygen<=4 & Carbon>0.37)
plot((map,col="Beige")

coordinates(DOCDO4) <- ~ X + Y
writePointsShape(DOCDO4, "DOCDO4.shp")
DOCDO4 <- readShapePoints("DOCDO4.shp")
raster(DOCDO4)
plot(DOCDO4,cex = 0.8, pch = 20, col = "blue", add = T, main="") 
title(main="381 Boreholes with DO<=4mg/L & DOC>0.37mg/L")
#legend(x='topright', legend = c("BH DO<4 & DOC>0.4mg/L"), fill = "blue")
write.csv(DOCDO4, "DOCDO4.csv")
#---------------------------BH with NO3 >0===========
NO3 <- subset(All_ChemData, select=c("X", "Y", "Carbon", "Oxygen", "Nitrate"), Nitrate>0 & Carbon>0.37 & Oxygen<=4)
plot((map,col="Beige")

coordinates(NO3) <- ~ X + Y
writePointsShape(NO3, "NO3.shp")
NO3 <- readShapePoints("NO3.shp")
raster(NO3)
plot(NO3,cex = 0.8, pch = 20, col = "blue", add = T, main="") 
title(main="283 Boreholes with DO<=4,DOC>0.37 & NO3>0mg/L")
write.csv(NO3, "NO3.csv")
#===========================NEAP with DOCDO4==============================

#------------------------------------------------------------------------
#########################################################################



buffDOCDO4 <- DOCDO4

write.csv(Load1, "Load1.csv")
########################################################################



#-------------------------STOICHIOMETRY APPROACH-------------------------
#----------------DO0 ----------------------
DO0 <- subset(All_ChemData, select=c("X", "Y", "Oxygen", "Carbon", "Nitrate"), Oxygen==0)

plot((map,col="Beige")

coordinates(DO0) <- ~ X + Y
writePointsShape(DO0, "DO0.shp")
DO0 <- readShapePoints("DO0.shp")
raster(DO0)
plot(DO0,cex = 0.8, pch = 20, col = "blue", add = T, main="")
title(main="491 Boreholes with Oxygen concentration of 0mg/L")
write.csv(DO0, "DO0.csv")
#--------------------------DO=0 and DOC>0-------
#########-----
DO0DOC0 <- subset(All_ChemData, select=c("X", "Y", "Oxygen", "Carbon", "Nitrate"), Oxygen==0 & Carbon>0)
plot((map,col="Beige")

coordinates(DO0DOC0) <- ~ X + Y
writePointsShape(DO0DOC0, "DO0DOC0.shp")
DO0DOC0 <- readShapePoints("DO0DOC0.shp")
raster(DO0DOC0)
plot(DO0DOC0,cex = 0.8, pch = 20, col = "blue", add = T, main="")
title(main="224 Boreholes with DO 0mg/L and DOC>0mg/L")
write.csv(DO0DOC0, "DO0DOC0.csv")
#-------------------------Where DOC >0.37mg/L-----------------------------------------
All_Chem <- subset(All_ChemData, select=c("X", "Y", "Oxygen", "Carbon", "Nitrate"))

All_Chem$DOCR <- All_Chem$Carbon/All_Chem$Oxygen # creating [DOC]/[DO] column 
All_Chem$DOCR[ All_Chem$DOCR == "NaN" ] = 0  
All_Chem$DOCR[ All_Chem$DOCR == "Inf" ] = 0

DOC037 <- subset(All_Chem, select=c("X", "Y", "Oxygen", "Carbon","Nitrate","DOCR"), DOCR>0.37)
plot((map,col="Beige")

coordinates(DOC037) <- ~ X + Y
writePointsShape(DOC037, "DOC037.shp")
DOC037 <- readShapePoints("DOC037.shp")
raster(DOC037)
plot(DOC037,cex = 0.8, pch = 20, col = "blue", add = T, main="")
title(main="221 Boreholes with DOC: DO >0.37")
write.csv(DOC037, "DOC037.csv")
###====================merging DOC0.37 and DO0DOC0=  = DEN==============================

DOC037 <- subset(All_Chem, select=c("X", "Y", "Oxygen", "Carbon","Nitrate","DOCR"), DOCR>0.37)
DO0DOC0 <- subset(All_ChemData, select=c("X", "Y", "Oxygen", "Carbon", "Nitrate"), Oxygen==0 & Carbon>0)
DEN <- merge(DOC037, DO0DOC0, all.x=TRUE, all.y=TRUE);
DEN[is.na(DEN)] <-  0
plot((map,col="Beige")

coordinates(DEN) <- ~ X + Y
writePointsShape(DEN, "DEN.shp")
DEN <- readShapePoints("DEN.shp")
raster(DEN)
plot(DEN,cex = 0.8, pch = 20, col = "blue", add = T, main="")
title(main="445 Boreholes with denitrification potentials")
write.csv(DEN, "DEN.csv")
#----------------------NO3 >0---------------------------------
DOC037 <- subset(All_Chem, select=c("X", "Y", "Oxygen", "Carbon","Nitrate","DOCR"), DOCR>0.37)
DO0DOC0 <- subset(All_ChemData, select=c("X", "Y", "Oxygen", "Carbon", "Nitrate"), Oxygen==0 & Carbon>0)
DEN <- merge(DOC037, DO0DOC0, all.x=TRUE, all.y=TRUE);
DEN[is.na(DEN)] <-  0
SNO3 <- subset(DEN, select=c("X", "Y", "Oxygen", "Carbon","Nitrate","DOCR"), Nitrate>0)
plot((map,col="Beige")

coordinates(SNO3) <- ~ X + Y
writePointsShape(SNO3, "SNO3.shp")
SNO3 <- readShapePoints("SNO3.shp")
raster(SNO3)
plot(SNO3,cex = 0.8, pch = 20, col = "blue", add = T, main="")
title(main="339 Boreholes with the nitrate concentration > 0mg/L")

write.csv(SNO3, "SNO3.csv")

###############---------------------###################-----------------
#########################################################

####nittate available
SNO3$DOUsed <- SNO3$Oxygen*0.37  #DOC used for oxygen
SNO3$DOCavail <- SNO3$Carbon - SNO3$DOUsed
SNO3$DOCNitrat <- SNO3$DOCavail*SNO3$Nitrate/0.93 # DOC needed for nitrate
SNO3$AvailNitrate <-SNO3$Nitrate-SNO3$DOCNitrat 
AvailNitrate <- subset(SNO3, select=c("X", "Y","AvailNitrate"), SNO3$AvailNitrate >0)
write.csv(AvailNitrate, "AvailNitrate.csv")
