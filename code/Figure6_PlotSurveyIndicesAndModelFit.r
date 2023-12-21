## Rogers et al. 2024 Spawn timing / catchability paper
## Create assessment model / survey comparison Figure 6 for manuscript
## 
## 
library(dplyr);library(ggplot2);library(fishualize)

## WHAM model results are given for four model formulations and three covariates:
#(m0 or fit0) q1 is constant
#(m1 or fit1) RW on q1, no Ecov effect (unconstrained RW)
#(m2 or fit2) No RW, only Ecov effect
#(m3 or fit3) Both RW and Ecov effect

#To get constrained RW from original ADMB model, need to read in expected_shelikof_indices

wham<-read.csv("../Results/WHAM/indices.csv",row.names=1)
admb<-read.csv("../Data/expected_shelikof_indices_Jan2022.csv") #from operational assessment
obs_index<-read.table("../Data/obs_index1.txt") #AT survey biomass estimates
colnames(obs_index)<-c("year","obs","CV")

tmp <- data.frame(year=admb$Year, expected=1e6*admb$Full_data_pred,
                  model='ADMB constrained RW')

indices <- bind_rows(wham,
                     cbind(tmp, Ecov='Fem30p'),
                     cbind(tmp, Ecov='mismatch'),
                     cbind(tmp, Ecov='SST')
) %>% filter(year>=1992)


whamres<-indices %>% 
  left_join(obs_index) %>%
  mutate(residual=log(obs)-log(expected))# %>%
#  filter(Ecov %in% c("Fem30p","mismatch"))

#Some quick exploration of residual patterns for diff model configs
ggplot(data=whamres,mapping=aes(year,residual,color=Ecov))+
  geom_point() +
#  geom_line() +
  facet_wrap('model')

#Some quick exploration of residual patterns for diff model configs
ggplot(data=whamres,mapping=aes(year,residual,color=model))+
  geom_point() +
  geom_line() +
  facet_wrap('Ecov')

ggplot(data=whamres[whamres$model=="fit1",],
       mapping=aes(year,expected,color=Ecov)) +
  geom_point()


whamres0<-whamres %>% 
  filter(model=="fit0" & Ecov == "Fem30p") %>%
  rename(Mod0_resid_log = residual,Year = year) %>%
  mutate(lwr=obs/exp(2*sqrt(log(1+((obs*CV)/obs))^2)),
         upr=obs*exp(2*sqrt(log(1+((obs*CV)/obs))^2)))

ggplot(data=whamres0,mapping=aes(Year,obs/1e6)) +
#         geom_point(color=2,shape=3,size=3) +
  geom_line(aes(x=Year,y=expected/1e6),size=1.2,color=3) +
  labs(y='Winter Shelikof AT Biomass (M mt)',x='Year') +
  geom_pointrange(data=whamres0, fatten=2,
        mapping=aes(Year, ymin=(lwr)/1e6, ymax=(upr)/1e6)) + 
  theme_bw()
  
#ggsave("../Figures/Observed_v_model_biomass.png",width=6,height=4)


######## Format data for plotting

whamres1 <- whamres %>%
  mutate (modelname = paste(model, Ecov)) %>%
  filter(modelname %in% c("ADMB constrained RW Fem30p","fit0 Fem30p",
                          "fit1 Fem30p",
                          "fit2 Fem30p","fit2 mismatch","fit2 SST",
                          "fit3 Fem30p","fit3 mismatch","fit3 SST")) %>%
  mutate(modelname = recode(modelname, 
                            'fit0 Fem30p' = "constant q",
                            'fit1 Fem30p' = "RW",
                            'fit2 Fem30p' = "SP30",
                            'fit2 mismatch' = "mismatch",
                            'fit2 SST' = "SST",
                            'fit3 Fem30p' = "SP30 + RW",
                            'fit3 mismatch' = "mismatch + RW",
                            'fit3 SST' = "SST + RW",
                            'ADMB constrained RW Fem30p' = 'fixed RW')) %>%
  mutate(panelplot = recode(modelname,
                            "constant q" = "panel1",
                            "RW" = "panel1",
                            "SP30" = "panel2",
                            "mismatch" = "panel2",
                            "SST" = "panel2",
                            "SP30 + RW" = "panel2",
                            "mismatch + RW" = "panel2",
                            "SST + RW" = "panel2",
                            'fixed RW' = "panel1")) %>%
  mutate(modelname = factor(modelname)) %>%
  mutate(lwr=obs/exp(2*sqrt(log(1+((obs*CV)/obs))^2)),
       upr=obs*exp(2*sqrt(log(1+((obs*CV)/obs))^2)))


##########
########## 
## Four panels: No Covariate, Mismatch, SST, Sp

whamres2 <- whamres1 %>%
  mutate(panelplot = recode(modelname,
                            "constant q" = "No ~ ~Covariate",
                            "RW" = "No ~ ~Covariate",
                            'fixed RW' = "No ~ ~Covariate",
                            "SP30" = "SP[30-wt]",
                            "SP30 + RW" = "SP[30-wt]",
                            "mismatch" = "Mismatch[med]",
                            "mismatch + RW" = "Mismatch[med]",
                            "SST" = "SST[Mar]",
                            "SST + RW" = "SST[Mar]"
                            )) 



#fishypal<-c("gray","#FFBF0FFF", "#FBE144FF", "#CEDF8EFF", "#8FCDCAFF", "#4EACEAFF", "#0382E5FF", "#0056B3FF", "#223052FF","#CE3D21FF", "#992216FF")
fishypal<-c("gray","#FFBF0FFF", "#FBE144FF",  "#8FCDCAFF","#CEDF8EFF",  "#0056B3FF","#4EACEAFF", "#992216FF","#CE3D21FF")

g2 <- ggplot(whamres2, aes(year, expected/1e6, color=modelname))  +

  geom_line(data=whamres2 %>% filter(modelname=="constant q") %>% select(modelname,year,expected),
            aes(year, expected/1e6),color="gray",linewidth=0.8) +
#  geom_line(linewidth=1.25,aes(linetype=linetypes)) +
  geom_line(linewidth=1,aes(linetype=modelname),alpha=0.75) +
  geom_pointrange(data=whamres2, fatten=1,linewidth=0.4,
                  mapping=aes(year, obs/1e6, ymin=lwr/1e6, ymax=upr/1e6,
                              color=NULL),show.legend=FALSE) +
  facet_wrap(.~panelplot, ncol=2,labeller = label_parsed) +
  labs(y='Winter Shelikof AT Biomass (M mt)',
       color='Catchability formulation', linetype='Catchability formulation', x='Year') + ylim(0,2.1) +
  theme_bw()+
  scale_color_manual(values=fishypal,limits=c("constant q","fixed RW","RW",
                                                    "mismatch","mismatch + RW",
                                                    "SP30","SP30 + RW",
                                                    "SST","SST + RW"))  +
  scale_linetype_manual(values=c(1,1,2,1,2,1,2,1,2), #"solid","solid","dashed","solid","dashed","solid","dashed","solid","dashed"),
                              limits=c("constant q","fixed RW","RW",
                              "mismatch","mismatch + RW",
                               "SP30","SP30 + RW",
                               "SST","SST + RW")) +
  theme(legend.key.width=unit(2,"lines"))

g2

ggsave(paste0("../Figures/Fig6_Observed_v_model_biomass_4panels_",Sys.Date(),".png"),width=12,height=6)

