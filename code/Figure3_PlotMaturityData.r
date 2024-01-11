## Create Figure 3 in Rogers et al. 2024 Spawn timing / catchability paper
## Plot maturity data - from individual level data

library(ggplot2);library(dplyr)

## Data from hauls used in biomass estimate
matdat<-read.csv("../Data/CompiledMaturityData_HaulsinAssessment_Females.csv")


## Plots of maturity stage over time

matset<-matdat[matdat$sex=="Female" & matdat$fork_length>=30,]

# For weighted proportions, need to modify:
# Calculate proportions within hauls, then multiply by haul weights and divide by total haul weight (n hauls)

mat_stage_weighted <- matset %>%
  group_by(CruiseHaul, year,mat5stage) %>%
  summarise(wt = mean(weight), num = n()) %>%
  mutate(freq = num/sum(num)) %>%
  mutate(wt_freq = freq*wt) %>%  # weighted freqeuncy. now sum across hauls and divide by n hauls.
  group_by(year) %>%
  mutate(n_hauls=n_distinct(CruiseHaul)) %>%
  group_by(year,mat5stage) %>%
  summarize(wt_prop = sum(wt_freq)/n_hauls) %>% 
  unique() %>% 
  mutate(mat5stage=factor(mat5stage,levels=c("Immature","Developing","Prespawning","Spawning","Spent")))
  

#Weighted plots:
ggplot(mat_stage_weighted, aes(x=year,y=wt_prop,fill=factor(mat5stage))) + 
  geom_bar(position="fill",stat="identity") +
#  scale_y_continuous(labels = scales::percent) +
  labs( x = "Year", y = "Weighted proportion (females > 30 cm)") +
  scale_fill_brewer(palette = "RdYlBu",name = "Maturity Stage") +
  theme_bw() +
  ylim(0,1)#+
#  geom_line(data=pmat, mapping= aes(x=year,y=Fem30p_wt),inherit.aes=FALSE) # Confirmed that this is same as Fem30p_wt for Pspsp

ggsave("../Figures/Fig3_Maturity_AllFemales30p_wt.png",width=8,height=5)
#ggsave("../Figures/Fig3_Maturity_AllFemales30p_wt.pdf",width=8,height=5)


