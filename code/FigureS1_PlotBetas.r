# Rogers et al. 2024 Spawn timing/catchability paper
# Create SI Figure S1: Estimated slopes for catchability covariates in WHAM stock assessment model
# 


library(ggplot2);library(dplyr)
betas<-read.csv("../Results/WHAM/beta_table.csv",row.names=1) %>%
  mutate(model = recode(model,"fit2" = "Covariate",
                     "fit3" = "Covariate + RW")) %>%
  mutate(Ecov = factor(Ecov, 
                       levels = c("mismatch","Fem30p","SST",
                                  "noise1","noise2"))) %>%
  mutate(EcovName = recode(Ecov, "Fem30p" = "SP[30~wt]",
                           "mismatch" = "Mismatch[med]",
                           "SST" = "SST[Mar]"))


ggplot(betas,aes(EcovName, est, ymin=lwr,ymax=upr,color=model)) +
  geom_pointrange(fatten=3,linewidth=1,position = position_dodge2(width=0.5))+
  geom_abline(slope=0,intercept=0,linetype=2,color="gray") +
  scale_color_manual(values=c("#0382E5FF","#FFBF0FFF"), limits=c("Covariate","Covariate + RW")) +
  theme_bw() +
  labs(y =parse(text="Estimated~slope~(beta)"), 
       x = "Catchability covariate",
       color = "Catchability formulation") +
  scale_x_discrete(labels = parse(text=levels(betas$EcovName)))

ggsave(paste0("../Figures/FigS1_Betas_",Sys.Date(),".png"),width=8,height=5)
