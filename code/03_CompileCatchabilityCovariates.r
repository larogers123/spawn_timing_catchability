### Compile potential set of covariates, to include spawn timing, survey timing, mismatch, maturity,  
### with stock assessment residuals

library(dplyr);library(gtools)

#read in and calculate residuals from stock assessment model
#For this analysis, use m0, assuming q (catchability) is constant
#RW = random walk
#Ecov = catchability covariate

#(m0 or fit0) q1 is constant
#(m1 or fit1) RW on q1, no Ecov effect (RW sd freely estimated)
#(m2 or fit2) No RW, only Ecov effect
#(m3 or fit3) Both RW and Ecov effect


wham<-read.csv("../Results/WHAM/indices.csv",row.names=1)
obs_index<-read.table("../Data/obs_index1.txt")
colnames(obs_index)<-c("year","observed3p","CV")
whamres<-wham %>% 
  left_join(obs_index) %>%
  mutate(residual=log(observed3p)-log(expected)) %>%
  filter(Ecov %in% c("Fem30p","mismatch"))

#For covariate analysis, want to use only residuals from fit0 (assumes constant q)

whamres0<-whamres %>% 
  filter(model=="fit0" & Ecov == "Fem30p") %>%
  select(year,residual) %>%
  rename(SurveyResiduals = residual,Year = year)


#read in maturation metrics
spsp<-read.csv("../Results/SpMetrics.csv")
spsp<-merge(spsp,whamres0,by.x="year",by.y="Year",all=T)

#read in spawn timing and survey timing mismatch metrics
timing<-read.csv("../Results/SurveySpawnTiming_to2021.csv")

fullcovs<-merge(spsp,timing,all=T,by.x="year",by.y="Year")


#include also March SST as a more naive proxy
SST<-read.csv("../Data/NCEP_reanalysisGOAmonthlySST_thru2021.csv")
colnames(SST)[1]<-"year"

fullcovs<-merge(fullcovs, SST[,c("year","JAN","FEB","MAR")],all.x=T)


#### Select subset and transform

fullcovs <- fullcovs %>%
  select(year | contains("Fem") | contains("mismatch") | contains("resid") | JAN | FEB | MAR) %>%
  select(!contains("Mature")) %>%
  select(!mismatch10) %>%
  filter(year >= 1983) #acoustic survey data starts. But only included in model from 1992

#add logit transformations for maturity metrics:
fullcovs <- fullcovs %>%
  mutate(across(starts_with("Fem"), logit, .names="{.col}_logit"), .keep="unused")


#### Save file for future analysis and plotting

write.csv(fullcovs,"../Results/CatchabilityCovariates_CandidateList.csv",row.names=F)
