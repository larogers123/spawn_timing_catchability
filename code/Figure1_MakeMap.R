# Produce Figure 1 Map for Rogers et al. 2024 Spawn Timing / Catchability Manuscript
# Include core areas for larvae, and sampling area for mature females.


library(sp);library(gstat);library(maptools);library(lattice);library(mapdata);library(mapproj);library(marmap);library(rgdal)
library(dplyr);library(scales);library(readxl)

#First, read in spatial polygon for larval survey, convert to Lat/lon projection.
shp.mp <- readOGR(dsn="../Data/Mapping",layer="regionlatemay")
proj4string(shp.mp)<-CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")  ##Equal area Albers"
shp.mp.LL<-spTransform(shp.mp,CRS("+proj=longlat"))

#Get bathymetry data
bathy<-getNOAA.bathy(-170,-130,52,62,keep=T,resolution=8,path="../Data/Mapping")

#Maturity sample locations
matdat<-read.csv("../Results/CompiledMaturityData_HaulsinAssessment_Females.csv")
matdat <- matdat %>%
  distinct(CruiseHaul,.keep_all=TRUE) %>%
  select(longitude,latitude)

#Shelikof survey typical track lines
shel<-read_xlsx("../Data/Mapping/Shelikof waypoints.xlsx")

#############
# Make map with inset
#############
#############

#pdf("../Figures/Fig1_MainMap_2023_01_06.pdf",width=8.5,height=6)
png("../Figures/Fig1_MainMap_2023_01_06.png",width=8.5,height=6,units="in",res=300)

par(mar=c(4,4,1,1))
myxlims<-c(-165.2,-148)
myylims<-c(53.5,60)
map('worldHires',xlim=myxlims,ylim=myylims,fill=T,col=NA,border=F)
plot(bathy, image=F,deep=-150, shallow=-150, step=100, lwd=0.8, lty=2,col="gray60",add=T,drawlabels=TRUE)

plot(shp.mp.LL,add=T,col= adjustcolor("cadetblue4", alpha.f = 0.3),lwd=2,border=NA)
points(matdat$longitude,matdat$latitude,pch=16, col=alpha("brown",0.2),cex=0.8)
segments(shel$Longitude[seq(1,59,2)],shel$Latitude[seq(1,59,2)],
         shel$Longitude[seq(2,60,2)],shel$Latitude[seq(2,60,2)],
         col="sienna4",lwd=2)
map('worldHires',xlim=c(-165,-148),ylim=c(54,60),fill=T,col="gray60",border=F,add=T)
axis(1,at=c(-165,-160,-155,-150),labels=paste0(c(165,160,155,150),"°W"))
axis(2,at=c(54:60),labels=paste0(54:60,"°N"))
box()
#map.axes(las=1)

#text(x=-153.8, y=58.2, labels="Shelikof Strait",srt=42,font=1,cex=0.8)
#text(x=-153.5,y=57.4,labels="Kodiak Is.",srt=42)
text(x=-157.25,y=59.6,labels="Alaska",cex=1.5)
text(x=-151,y=57.7,labels="Kodiak Is.",cex=1)
lines(x=c(-152,-152.85),y=c(57.7,57.6),lwd=1.2)
text(x=-156.2,y=58.6,labels="Shelikof \nStrait",cex=1,adj=c(0,1))
lines(x=c(-155.1,-154.2),y=c(58.3,57.9),lwd=1.2)
text(x=-151.5,y=56,labels="Gulf of Alaska",font=1,srt=35,cex=1.5)

legend("bottomright",col=c("sienna4",adjustcolor("cadetblue4", alpha.f = 0.3),alpha("brown",0.5)),
       lty=c(1,NA,NA),lwd=c(2,NA,NA),pch=c(NA,15,16),
       legend=c("Winter Shelikof AT survey","Spring larval survey","Maturity samples"),bty="n",pt.cex=c(1,2,0.8))
#legend("bottomright",col=c(adjustcolor("steelblue", alpha.f = 0.4)),pch=c(15),legend=c("Larval sampling area"),bty="n",pt.cex=c(2))


### Add inset map directly

# Next, we create a new graphics space in the corner.  The numbers are proportional distances 
# within the graphics window (xmin,xmax,ymin,ymax) on a scale of 0 to 1.
# "plt" is the key parameter to adjust
par(plt = c(0.12, 0.32, 0.5, 0.68), new = TRUE)
plot.new()
inset<-map('world2',xlim=c(165,250),ylim=c(20,70),fill=T,col="gray60",border=T,wrap=T,
           projection="simpleconic",parameters=c(30,55),lforce="s",add=T,plot=FALSE)
# from http://www.stat.auckland.ac.nz/~paul/RGraphics/examples-map.R
plot.window(xlim=c(-0.45,0.45),ylim=c(-1.3,-0.5))

# draw the map
map('world2',xlim=c(165,250),ylim=c(20,70),fill=T,col="gray60",lwd=0.5, border=T,wrap=T,
    projection="simpleconic",parameters=c(30,55),lforce="s",add=T)
# draw the detail box region
polygon(c(-0.12,0.05,0.05,-0.12),c(-0.88,-0.88,-0.73,-0.73))

text(x=-0.1,y=-1.05,labels="North Pacific Ocean",cex=0.7)
box()

dev.off()


####################
