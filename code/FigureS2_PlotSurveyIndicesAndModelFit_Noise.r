## Create Figure S2 in Rogers et al. 2024 Spawn timing / catchability paper
# Plot survey Indices and model fits for Noise covariates
#

library(dplyr);library(ggplot2);library(fishualize);library(gtools);library(tidyr)

#(m0 or fit0) q1 is constant
#(m1 or fit1) RW on q1, no Ecov effect (unconstrained RW)
#(m2 or fit2) No RW, only Ecov effect
#(m3 or fit3) Both RW and Ecov effect
#
#
wham<-read.csv("../Results/WHAM/indices.csv",row.names=1)
admb<-read.csv("../Data/expected_shelikof_indices_Jan2022.csv")
obs_index<-read.table("../Data/obs_index1.txt") #survey biomass
colnames(obs_index)<-c("year","obs","CV")

tmp <- data.frame(year=admb$Year, expected=1e6*admb$Full_data_pred,
                  model='ADMB constrained RW')

indices <- bind_rows(wham,
                     cbind(tmp, Ecov='noise1'),
                     cbind(tmp, Ecov='noise2')
) %>% filter(year>=1992)


whamres<-indices %>% 
  left_join(obs_index) %>%
  mutate(residual=log(obs)-log(expected))


whamres3 <- whamres %>%
  mutate(modelname = paste(model, Ecov)) %>%
  filter(modelname %in% c("ADMB constrained RW Fem30p","fit0 Fem30p",
                          "fit2 noise1","fit2 noise2",
                          "fit3 noise1","fit3 noise2")) %>%
  mutate(modelname = recode(modelname, 
                            'fit0 Fem30p' = "constant q",
                            'fit2 noise1' = "noise1",
                            'fit2 noise2' = "noise2",
                            'fit3 noise1' = "noise1 + RW",
                            'fit3 noise2' = "noise2 + RW")) %>%
  mutate(panelplot = recode(modelname,
                            "noise1" = "noise1",
                            "noise1 + RW" = "noise1",
                            "noise2" = "noise2",
                            "noise2 + RW" = "noise2",
                            )) %>%
  mutate(modelname = factor(modelname)) %>%
  mutate(lwr=obs/exp(2*sqrt(log(1+((obs*CV)/obs))^2)),
         upr=obs*exp(2*sqrt(log(1+((obs*CV)/obs))^2)))

consq<-whamres3 %>% filter(modelname=="constant q") %>% select(modelname,year,expected)

whamres3 <- whamres3 %>%
  filter(modelname!="constant q")

fishypal<-c("gray","#FFBF0FFF", "#FBE144FF",  "#8FCDCAFF","#CEDF8EFF",  "#0056B3FF","#4EACEAFF", "#992216FF","#CE3D21FF")

g2 <- ggplot(whamres3, aes(year, expected/1e6, color=modelname))  +
  
  geom_line(data=consq,
            aes(year, expected/1e6),color="gray",linewidth=0.8) +
  #  geom_line(linewidth=1.25,aes(linetype=linetypes)) +
  geom_line(linewidth=1,aes(linetype=modelname),alpha=0.75) +
  geom_pointrange(data=whamres3, fatten=1,linewidth=0.4,
                  mapping=aes(year, obs/1e6, ymin=lwr/1e6, ymax=upr/1e6,
                              color=NULL),show.legend=FALSE) +
  facet_wrap(.~panelplot, ncol=2) +
  labs(y='Winter Shelikof AT Biomass (M mt)',
       color='Catchability formulation', linetype='Catchability formulation', x='Year') + ylim(0,2.1) +
  theme_bw()+
  scale_color_manual(values=fishypal[c(1,6:9)],limits=c("constant q",
                                              "noise1","noise1 + RW",
                                              "noise2","noise2 + RW"))  +
  scale_linetype_manual(values=c(1,1,2,1,2,1,2), #"solid","solid","dashed","solid","dashed","solid","dashed","solid","dashed"),
                        limits=c("constant q",
                                   "noise1","noise1 + RW",
                                   "noise2","noise2 + RW")) +
  theme(legend.key.width=unit(2,"lines"))

g2

ggsave(paste0("../Figures/FigS2_Observed_v_model_biomass_noise_",Sys.Date(),".png"),width=12,height=3.5)
