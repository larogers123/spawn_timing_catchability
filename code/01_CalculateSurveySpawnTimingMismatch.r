## Calculate spawn timing and survey timing mismatch metrics.
## Calculate some simple summaries for Results writeup.

library(dplyr)

SpawnTiming<-read.csv("../Data/MeanVarHatchSpawn_01Sep20_NewAgeLengthREwStn_extendeddates_thru19.csv")
SurveyTiming<-read.csv("../Data/ShelikofStartEndDates_to2021.csv")

Timing<-merge(SpawnTiming, SurveyTiming,all=TRUE)

Timing <- Timing %>%
  mutate(meanDOY = (StartDOY+EndDOY)/2) %>%
  mutate(mismatchmean = (MeanSpawnLR - meanDOY),
         mismatch20 =(Spawn20LR - meanDOY),
         mismatch10 = (Spawn10LR - meanDOY),
         mismatchmed = (MedSpawnLR - meanDOY)) %>%
  rename_with(~ gsub("LR","", .x),ends_with("LR")) %>%
  select(Year, MeanSpawn, MedSpawn, DurSpawn, StartSpawn, Spawn10, Spawn20, EndSpawn, StartDOY, EndDOY, 
         meanDOY, mismatchmean, mismatch20, mismatch10, mismatchmed)


write.csv(Timing,"../Results/SurveySpawnTiming_to2021.csv",row.names=FALSE)


#######################
# Some simple summaries for Results section
#######################

summary(Timing$MeanSpawn)
summary(Timing$MedSpawn)
sd(Timing$MeanSpawn,na.rm=T)
#[1] 7.05888
sd(Timing$MedSpawn,na.rm=T)
#[1] 7.600761
sd(Timing$MeanSpawn[Timing$Year>1991],na.rm=T)
#[1] 8.255424

summary(Timing$meanDOY)
summary(Timing$StartDOY)
summary(Timing$StartDOY[Timing$Year>1991])
summary(Timing$EndDOY[Timing$Year>1991])
summary(Timing$meanDOY[Timing$Year>1991])
sd(Timing$meanDOY[Timing$Year>1991],na.rm=T)

summary(Timing$mismatchmed)
summary(Timing$mismatchmed[Timing$Year>1991])
sd(Timing$mismatchmed[Timing$Year>1991],na.rm=T)

summary(Timing$mismatch20[Timing$Year>1991])
sd(Timing$mismatch20[Timing$Year>1991],na.rm=T)
