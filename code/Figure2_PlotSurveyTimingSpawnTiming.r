## Create Figure 2 in Rogers et al. 2024 Spawn timing / catchability paper
## Plot Survey timing and spawn timing, and mismatch.

library(scales)


Timing<-read.csv("../Results/SurveySpawnTiming_to2021.csv")


#### SURVEY TIMING AND SPAWN TIMING #####

today<-Sys.Date()
#pdf(paste0("../Figures/Fig2_SurveyandMedSpawnTiming_",today,".pdf"),width=8,height=6)
png(paste0("../Figures/Fig2_SurveyandMedSpawnTiming_",today,".png"),units="in",width=8,height=6,res=300)

par(mar=c(4,5,1,1))
mycol1<-"darkred"
mycol2<-"darkorange"
mylwd<-1.8
transcol<-(Timing$Year<1992)
mycols1<-ifelse(Timing$Year<1992,alpha(mycol1,0.5),mycol1)
mycols2<-ifelse(Timing$Year<1992,alpha(mycol2,0.5),mycol2)

plot(Timing$Year+0.1,Timing$MeanSpawn,pch=16,col=NA,ylim=c(52,140),xlim=c(1979,2020),axes=F, ylab="",xlab="")
#polygon(x=c(1972,1991.4,1991.4,1972),y=c(46,46,145,145),col="gray90",border=NA)
arrows(Timing$Year+0.1,Timing$StartSpawn,Timing$Year+0.1,Timing$EndSpawn,length=0,col=mycols1,lwd=mylwd)
points(Timing$Year+0.1,Timing$MedSpawn,pch=16,col=mycols1)
#points(Timing$Year-0.1,Timing$Spawn10,pch=1,col="darkred",cex=0.9)
#points(Timing$Year+0.1,Timing$meanDOY,pch=17,col="orange")
arrows(Timing$Year-0.1,Timing$StartDOY,Timing$Year-0.1,Timing$EndDOY,length=0,col=mycols2,lwd=mylwd*1.5)
points(Timing$Year+0.1,Timing$Spawn20,pch=1,col=mycols1,cex=0.8)

legend('topleft',legend=c("Estimated spawning (median with 95% of spawning distribution)","Date 20% spawned",
                           "Shelikof winter acoustic-trawl survey"),
        pch=c(16,1,NA),pt.cex=c(1,0.8,1),lty=c(1,NA,1),col=c(mycol1,mycol1,mycol2),lwd=c(mylwd,NA,mylwd*1.5),bty="n")
#legend('topleft',legend=c("Estimated spawn timing (median with 95% of spawning distribution)",
#                          "Shelikof acoustic survey timing"),
#       pch=c(16,NA),lty=c(1,1),col=c("darkred","darkorange"),bty="n")

axis(2, labels= c("01 Mar", "16 Mar", "01 Apr", "16 Apr","01 May","16 May"), at = c(60, 75, 91, 106, 121, 136),las=1)
axis(1)
mtext("Year",side=1,line=2.5)
mtext("Date",side=2,line=4)
box()
#grid() # add some sort of grid lines for reference? not usually a fan, but may help

dev.off()



#### Mismatch in timing ####

plot(Timing$mismatchmed~Timing$Year,type="o",pch=16,ylab="Days between mid survey date and median spawn date",xlab="Year",xlim=c(1979,2020))
