## 
## Calculate proportion of females spawning and spent from winter Shelikof AT survey
##  
## 
## 

library(dplyr)

matdat<-read.csv("../Data/CompiledMaturityData_HaulsinAssessment_Females.csv")


########
# Choose which fish - 30cm cutoff, 40 cm, mature
########

## Create subsets to run and save:

inds1<-which(matdat$fork_length >= 40 & matdat$sex == "Female")
inds2<-which(matdat$fork_length >= 30 & matdat$sex == "Female")
inds3<-which(matdat$is_mature == 1 & matdat$sex == "Female")
inds4<-which(matdat$fork_length>=40 & matdat$sex == "Female" & matdat$is_mature == 1)
inds5<-which(matdat$fork_length>=30 & matdat$sex == "Female" & matdat$is_mature == 1)

myinds<-list(inds1,inds2,inds3,inds4,inds5)
matnames<-c("Fem40p","Fem30p","FemMature","Fem40pMature","Fem30pMature")

for(i in 1:length(matnames)){
  myscen<-matnames[i]
  mydat<-matdat[myinds[[i]],]
  
  ######
  # Calculate weighted and unweighted spawning metrics, using same hauls and only hauls used in biomass estimate
  ######
  
  sp_ratio_byhaul <- mydat %>%  #equal weighting by haul
    group_by(CruiseHaul, year) %>%
    summarise(spsp = mean(is_spsp)) %>%
    group_by(year) %>%
    summarise(spsp_haul = mean(spsp))
  
  sp_ratio_byfish <- mydat %>%  #equal weighting by specimen
    group_by(year) %>%
    summarise(spsp_fish = mean(is_spsp))
  
  sp_ratio_weighted <- mydat %>%   #hauls weighted by associated biomass
    group_by(CruiseHaul, year) %>%
    summarise(spsp = mean(is_spsp),wt = mean(weight)) %>%
    group_by(year) %>%
    summarize(spsp_wt = sum(spsp * wt)/sum(wt))
  
  sps<-merge(sp_ratio_byhaul,sp_ratio_byfish)
  sps<-merge(sps,sp_ratio_weighted)
  
  sps<- sps %>% 
    rename_with(function(x){paste0(myscen,"_", sub(".*_", "", x))},starts_with("spsp"))
  
  if(i==1) {tosave <- sps}else{
    tosave <- cbind(tosave, sps[,-1])
  }
}

write.csv(round(tosave,4),"../Results/SpMetrics.csv",row.names=F)

