## Create Figures 4 and 5 in Rogers et al. 2024 Spawn timing / catchability paper
## Plot catchability covariates versus survey residuals 

 
library(tidyr);library(dplyr);library(scales);library(gtools);library(ggplot2)
library(gridExtra);library(ggrepel);library(ggtext)

fullcovs<-read.csv("../Results/CatchabilityCovariates_CandidateList.csv")

#########
# MS Figure 4: 
# Compare timing vs maturation based indicators: mismatchmed vs SP30_wt

r2<-cor(fullcovs$mismatchmed,fullcovs$Fem30p_wt_logit,use="p")

p1<-ggplot(fullcovs, aes(x=mismatchmed,y=Fem30p_wt_logit,label=year)) +
  geom_point() +
  geom_text_repel(color="gray30",size=2) +
  labs(
    x = "*Mismatch<sub>med</sub>* (days)", # (days from mid AT survey date to median spawn date)",
    y = "*SP<sub>30_wt</sub>* (logit-transformed proportion)", #(logit-transformed proportion females > 30 cm in spawning or spent stage)"
  ) +
  annotate("text", x=42, y=1.2,label=bquote(r == .(round(r2,2))))+
  theme_bw() +
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = ggtext::element_markdown(),
          axis.title.y = ggtext::element_markdown()
)

print(p1)

ggsave(filename = "../Figures/Fig4_SP30_wt_vs_Mismatchmed.pdf",
       width = 4, height = 3.7)
ggsave(filename = "../Figures/Fig4_SP30_wt_vs_Mismatchmed.png",
       width = 4, height = 3.7)


# Survey residuals timeseries

ps1<-ggplot(fullcovs,aes(year,SurveyResiduals)) +
  geom_point() +
  geom_path() +
  geom_hline(yintercept=0, linetype = 2, color="gray") +
  theme_bw() +
  xlim(1992,2021) +
  ylim(-1.05,1.28) +
  labs(y="Shelikof survey residuals",x="Year", tag="a") 


### Survey residuals and mismatch with median spawn date

r2b<-cor(fullcovs$mismatchmed,fullcovs$SurveyResiduals,use="p")
p2<-ggplot(fullcovs, aes(x=mismatchmed,y=SurveyResiduals,label=year)) +
  geom_point() +
  geom_hline(yintercept=0,linetype=2,color="gray") +
  geom_text_repel(color="gray30",size=2) +
  geom_smooth(method="lm") +
  scale_y_continuous(limits = c(-1.05, 1.28)) +
  labs(
    tag="b",
    x = "*Mismatch<sub>med</sub>* (days)", # (days from mid AT survey date to median spawn date)",
    y = ""#Shelikof survey residuals"
  ) +
  annotate("text", x=42, y=1.28,label=bquote(r == .(round(r2b,2))))+
  theme_bw() +
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = ggtext::element_markdown()
          )


### Survey residuals and weighted proportion females (>30cm) spawning & spent

r2c<-cor(fullcovs$Fem30p_wt_logit,fullcovs$SurveyResiduals,use="p")
p3<-ggplot(fullcovs, aes(x=Fem30p_wt_logit,y=SurveyResiduals,label=year)) +
  geom_point() +
  geom_hline(yintercept=0,linetype=2,color="gray") +
  geom_text_repel(color="gray30",size=2) +
  geom_smooth(method="lm") +
  scale_y_continuous(limits = c(-1.05, 1.28)) +
    labs(
      tag="c",
    x = "*SP<sub>30_wt</sub>* (logit-transformed proportion)", # (logit-transformed proportion females > 30 cm in spawning or spent stage)",
    y = "Shelikof survey residuals"
  ) +
  annotate("text", x=0.93, y=1.28,label=bquote(r == .(round(r2c,2))))+
  theme_bw() +
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = ggtext::element_markdown()
  )

### Survey residuals and MAR SST

r2d<-cor(fullcovs$MAR,fullcovs$SurveyResiduals,use="p")
p4<-ggplot(fullcovs, aes(x=MAR,y=SurveyResiduals,label=year)) +
  geom_point() +
  geom_hline(yintercept=0,linetype=2,color="gray") +
  geom_text_repel(color="gray30",size=2) +
  geom_smooth(method="lm") +
  scale_y_continuous(limits = c(-1.05, 1.28)) +
  labs(
    tag="d",
    x = "*SST<sub>MAR</sub>* (°C)", # March SST
    y ="" #"Shelikof survey residuals"
  ) +
  annotate("text", x=4.9, y=1.28,label=bquote(r == .(round(r2d,2))))+
  theme_bw() +
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = ggtext::element_markdown()
   )


#Plot All 3 covariates with residual timeseries as panel 1

ggsave(filename = "../Figures/Fig5_Mod0SurveyResids_vs_3Covariates_wTimeseries.png",
       plot=grid.arrange(ps1,p2, p3, p4, ncol=2),
       width = 9, height = 8)




