## Rogers et al. 2024 Spawn timing / catchability paper
##
## Fit regressions of winter Shelikof AT survey residuals and catchability covariates
## Create formatted table of model results


library(gtsummary);library(MuMIn)
library(tidyverse)
library(broom);library(dplyr);library(gtools)


## Try fitting models and reporting results together:
#https://stackoverflow.com/questions/42139772/tidy-output-from-many-single-variable-models-using-purrr-broom

fullcovs<-read.csv("../Results/CatchabilityCovariates_CandidateList.csv")



# Create subsets of data to use: 1) no NAs for fair comparison (equal n), 2) like (1) but prior to 2015 only,
#  3) all available years for each covariate


### Create reduced datasets with only pMature, Mismatch, SST, timing metrics:
subcovs<- fullcovs %>%
  select(year | ends_with("logit") | SurveyResiduals | contains("mismatch") | JAN:MAR)  %>%
  drop_na(.)

subcovs_early<- fullcovs %>%
  select(year | ends_with("logit") | SurveyResiduals | contains("mismatch") | JAN:MAR)  %>%
  drop_na(.) %>%
  filter(year < 2015) #using <2017 results in same model order, nearly same estimates

subcovs_NAs<- fullcovs %>%
  select(year | ends_with("logit") | SurveyResiduals | contains("mismatch") | JAN:MAR)


## Function to run models, compile results, format table

modelTable <- function(df) {
  output <- df %>%
    select(!c(SurveyResiduals, year)) %>%
    names() %>%
    paste('SurveyResiduals~', .) %>%
    map_df( ~ tidy(lm(as.formula(.x),
                      data = df),
                   conf.int = TRUE)) %>%
    filter(term != "(Intercept)")
  
  output2 <- df %>%
    select(!c(SurveyResiduals, year)) %>%
    names() %>%
    paste('SurveyResiduals~', .) %>%
    map_df( ~ glance(lm(as.formula(.x),
                        data = df)))
  
  ## This works, just need to get some of the glance output added to the tidy output. Can do second.
  output3 <- cbind(output, select(output2, !p.value))
  
  #Format full table with all models for supplement?
  
  AllModTable <- output3 %>%
    select(term, estimate, std.error, p.value, r.squared, nobs) %>%
    rename(
      covariate = term,
      s.e = std.error,
      R2 = r.squared,
      Nobs = nobs
    ) %>%
    arrange(desc(R2)) %>%
    mutate_at(2:3, round, 3) %>%
    mutate_at(4, round, 4) %>%
    mutate_at(5, round, 2)
  
  return(AllModTable)
}


tab_comp <- modelTable(subcovs)
tab_early <- modelTable(subcovs_early)
tab_NAs <- modelTable(subcovs_NAs)

# Formatted nicely as a wide table with 3 sections/spanning headers.

tab_all1<-full_join(tab_comp,tab_early,by="covariate",suffix=c(".comp",".early"))
tab_all2<-full_join(tab_all1,tab_NAs,by="covariate") #can't specify suffix bc already different.

#Rename covariates
covariate<-c("mismatchmed","mismatch20","mismatch10","mismatchmean",
             "Fem30p_wt_logit", "Fem30p_fish_logit", "Fem30p_haul_logit", 
             "Fem40p_fish_logit", "Fem40p_haul_logit", "Fem40p_wt_logit", 
             "MAR", "FEB", "JAN")
covnames<-c( "mismatchmed" ,"mismatch20" ,"mismatch10" ,"mismatchmean",
             "SP30_wt", "SP30_fish","SP30_haul",
             "SP40_fish","SP40_haul","SP40_wt",
             "SSTMAR","SSTFEB","SSTJAN")
CovNames<-data.frame(covariate,covnames)

tab_all2_newnames<-merge(tab_all2,CovNames) %>%
  arrange(desc(R2.comp)) %>%
  select(!"covariate") %>%
  relocate(covnames, .before = 1) %>%
  rename(covariate = covnames)

write.csv(tab_all2_newnames, "../Results/Table1_CovariateRegressions.csv",row.names=F)


