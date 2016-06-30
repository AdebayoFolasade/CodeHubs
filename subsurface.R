require(rgdal)
library(data.table)
Eudata = read.csv("27-1997data.csv")
Eudata$long[is.na(Eudata$long)] <- 0
Eudata$lat[is.na(Eudata$lat)] <- 0
Eudata$Notes <- NULL
Eudata <- subset(Eudata, select=c(POINT_ID, OC,N,lat,long))
cord.dec = SpatialPoints(cbind(Eudata$long,Eudata$lat), proj4string=CRS("+proj=longlat"))

# Setting existing coordinate as lat-long system
#cord.dec = SpatialPoints(cbind(Eudata$lat,Eudata$long), proj4string=CRS("+proj=longlat"))
# EPSG codes for BNG, WGS84 and ETRS89, which are 27700, 4326 and 4258 respectively

cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:27700"))
cord.UTM <- round(as.data.frame(cord.UTM))
Eudata <- data.frame(Eudata, "X", "Y")
colnames(Eudata)[6] <- "X"
colnames(Eudata)[7] <- "Y"
Eudata$X <- cord.UTM[,1]
Eudata$Y <- cord.UTM[,2]
rm(cord.UTM)

#----------------------------------------------------------------------------
#Selection of Eudata range that falls within England and Wales
get.rows   <- Eudata$X >= as.numeric("134500") & Eudata$X <= as.numeric("655500") &
Eudata$Y >= as.numeric("11500") & Eudata$Y <= as.numeric("656500")
  
              
Eudata <- Eudata[get.rows,]
Eudata <- subset(Eudata, select=c("X", "Y", "OC"))
SOCarbon <- data.frame(Eudata$X, Eudata$Y, Eudata$OC)
names(SOCarbon) <- c("X", "Y", "OC")
rm(Eudata)
write.csv(SOCarbon, "SOCarbon.csv")
SOCarbon <- read.csv("SOCarbon1.csv")
#==============================================






