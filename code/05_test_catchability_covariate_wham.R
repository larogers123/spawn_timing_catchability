## This file runs the GOA pollock assessement in WHAM under some
## different configurations regarding a timing covariate linked
## to catchability. Takes about 20 minutes to run all the models
## as setup currently.

## working directory assumed to be set to "code" folder in repo
getwd()

## This analysis is very sensitive to the version of WHAM that is
## used, for reasons other than what is explored here. We
## strongly recommend installing this version:
## devtools::install_github("timjmiller/wham", ref='v1.0.6', dependencies=TRUE)
## See further installation instructions here:
## https://github.com/timjmiller/wham?tab=readme-ov-file#installation

library(wham)
library(dplyr)
library(tidyr)
## A package to work with the ADMB model output
## devtools::install_github('afsc-assessments/GOApollock')
library(GOApollock)
library(ggplot2)
theme_set(theme_bw())
library(purrr)
source("wham_functions.R")

## Get WHAM initial values setup to be close by using the output
## from ADMB
## original ADMB fit in 2021
arep <- read_pk_rep('../Data/pk_wham', version='pkwham', endyr=2021)
asap3 <- read_asap3_dat("../Data/goa_pk_asap3.txt")
input <- readRDS('../Data/akwham_input_2021.RDS')
years <- arep$years
## saveRDS(input, '../Data/akwham_input_2021.RDS')


## check that wham matches admb still
catchability <- list(re=c('ar1',rep('none',5))) ## RW as penalized likelihood
ecov <- NULL
input0 <- match_input(arep, asap3, ecov=ecov, catchability=catchability)
input0$random <- NULL
## fix SD at same value as in ADMB
input0$map$q_repars <- factor(NA*input0$map$q_repars)
input0$par$q_repars
fit0 <- fit_wham(input0, do.osa=FALSE, do.fit=0, do.retro=FALSE,
                 do.sdrep=0, MakeADFun.silent=0)

## Compare WHAM at initial values to ADMB model
png('../Results/WHAM/wham_vs_admb_check.png', width=5, height=6,
    units='in', res=300)
par(mfrow=c(3,1))
plot(years, arep$Expected_spawning_biomass*1e6, ylab='SSB')
lines(years, fit0$rep$SSB, col=1)
plot(years, arep$Survey_1_q[-1], ylab='Catchability')
lines(years, fit0$rep$q[,1])
plot(years, arep$Expected_survey_1_index[2,]*1e6, ylab='Expected index')
lines(years, exp(fit0$rep$pred_log_indices[,1]))
## Yes matches q and index perfectly, some small differences in
## SSB due to structural differences. This shows that we were
## able to closely match the structure of the two modeling
## platforms. Lingering differnces are highlighted in the main
## text.
dev.off()

## Setup the Ecov list.
set.seed(12131)
n.noise <- 2 ## adding fake covariates to understand behavior when beta=0
env.dat <- read.csv('../Data/CatchabilityCovariates_2023-03-29.csv') %>% filter(year>=1992)
 ## normalize the covariates?
env.dat[,2] <-
  (env.dat[,2]-mean(env.dat[,2],na.rm=TRUE))/sd(env.dat[,2],na.rm=TRUE)
env.dat[,3] <-
  (env.dat[,3]-mean(env.dat[,3],na.rm=TRUE))/sd(env.dat[,3],na.rm=TRUE)
env.dat[,4] <-
  (env.dat[,4]-mean(env.dat[,4],na.rm=TRUE))/sd(env.dat[,4],na.rm=TRUE)
## tack on full years
env.dat <- merge(env.dat, data.frame(year=1970:2021), all.y=TRUE)
## Add on a random white noise as a test
env.dat <- cbind(env.dat, matrix(rnorm(n.noise*nrow(env.dat)), ncol=n.noise))
names(env.dat) <- c('year', 'Fem30p', 'mismatch', 'SST', paste0('noise', 1:n.noise))
  ## replace NA with means (shoudl be 0s?)
na.ind <- !apply(env.dat[,-1], 2, is.na)
## make sense to only have this in survey years?
na.ind[,-(1:3)] <- na.ind[,1]
env.dat[which(is.na(env.dat[,2])),2] <-
  mean(env.dat[,2],na.rm=TRUE)
env.dat[which(is.na(env.dat[,3])),3] <-
  mean(env.dat[,3],na.rm=TRUE)


q.all <- ssb.all <- repars.tab.all <- indices.all <- beta.tab.all <- par.tab.all <- aic.tab.all <- list()
for(which.covariate in 1:(3+n.noise)){
  cov.name <- names(env.dat)[which.covariate+1]
  ## Build ecov list for inputs, depends on covariate selected
  ecov <- list(
    label = cov.name,
    mean = as.matrix(env.dat[,which.covariate+1]),
    logsigma = log(.02),#'est_1', # estimate obs sigma, 1 value shared across years
    year = env.dat$year,
    use_obs = matrix(1*na.ind[,which.covariate], ncol=1, nrow=dim(env.dat)[1]),
    lag = 0,
    process_model = 'ar1',
    where = "q",
    indices=list(1), # only affects first index (Shelikof)
    how = 0, # 0 = no effect (but still fit Ecov to compare AIC), 1 = mean
    link_model = "linear")

  ## Model 0: constant q1, no ecov
  catchability <- list(re=rep('none',6))
  ecov$how <- 0;  ecov$where <- "none"
  input0 <- match_input(arep, asap3, ecov=ecov, catchability=catchability)
  input0$random <- 'Ecov_re'
  ## SSB and uncertainty for the base case model
  fit0 <- fit_wham(input0, do.osa=FALSE, do.fit=TRUE, do.retro=FALSE,
                   do.sdrep=TRUE, MakeADFun.silent=TRUE)

  ## Model 1: RW on q1, no ecov
  catchability <- list(re=c('ar1',rep('none',5))) ## RW as penalized likelihood
  ecov$how <- 0; ecov$where <- "none"
  input1 <- match_input(arep, asap3, ecov=ecov, catchability=catchability)
  input1$random <- c('Ecov_re','q_re')
  ## turn on only the SD not the rho
  input1$map$q_repars <- factor(c(1,rep(NA,11)))
  ## SSB and uncertainty for the two for base case model
  fit1 <- fit_wham(input1, do.osa=FALSE, do.fit=TRUE, do.retro=FALSE,
                   do.sdrep=TRUE, MakeADFun.silent=TRUE)

  ## Model 2:Only Ecov, no RW
  catchability <- list(re=rep('none',6))
  ecov$how <- 1; ecov$where <- "q"
  input2 <- match_input(arep, asap3, ecov=ecov, catchability=catchability)
  input2$random <- 'Ecov_re'
  ## SSB and uncertainty for the two for base case model
  fit2 <- fit_wham(input2, do.osa=FALSE, do.fit=TRUE, do.retro=FALSE,
                   do.sdrep=TRUE, MakeADFun.silent=TRUE)

  ## Model 3: RW and ecov
  catchability <- list(re=c('ar1',rep('none',5)))
  ecov$how <- 1; ecov$where <- "q"
  input3 <- match_input(arep, asap3, ecov=ecov, catchability=catchability)
  input3$random <- c("Ecov_re", 'q_re')
  ## turn on only the SD not the rho
  input3$map$q_repars <- factor(c(1,rep(NA,11)))
  ## SSB and uncertainty for the two for base case model
  fit3 <- fit_wham(input3, do.osa=FALSE, do.fit=TRUE, do.retro=FALSE,
                   do.sdrep=TRUE, MakeADFun.silent=TRUE)

  fits <- list(fit0=fit0, fit1=fit1, fit2=fit2, fit3=fit3)
  ## saveRDS(fits, file=paste0('fits_penalized_likelihood_', cov.name,'.RDS'))
  ## rm(fit0, fit1, fit2, fit3)
  (maxgrads <- lapply(fits, function(x) max(abs(x$gr()))))

  aic.tab <- compare_wham_models(fits,do.plot=FALSE, table.opts=list(calc.rho=FALSE), sort=FALSE)$tab
  aic.tab <- aic.tab[order(dimnames(aic.tab)[[1]]),]
  aic.tab.all[[which.covariate]] <-
    data.frame(model=dimnames(aic.tab)[[1]], Ecov=cov.name, aic.tab)
  q.all[[which.covariate]] <- lapply(1:4, function(x) get_q_ci(fits[[x]], names(fits)[x])) %>%
    bind_rows %>%  filter(year>1990) %>% mutate(Ecov=cov.name)
  g <-  q.all[[which.covariate]] %>%
    ggplot(aes(year, est, ymin=lwr, ymax=upr,
               fill=model, color=model)) +
    geom_ribbon(alpha=.5) + geom_line() + facet_wrap('model', ncol=1) +
    labs(y='q1') + geom_hline(yintercept=1) + ggtitle(label=cov.name)
  ggsave(paste0('../Results/WHAM/q1_compare_', cov.name,'.png'), g, width=7, height=6)
  env.df <- env.dat[na.ind[,which.covariate],]
  xx <- lapply(1:4, function(x) get_ecov_ci(fits[[x]], names(fits)[x])) %>%
    bind_rows %>% filter(year>1990)
  g <-
    ggplot(xx, aes(year, est, ymin=lwr, ymax=upr,
                   fill=model, color=model)) +
    geom_ribbon(alpha=.5) + geom_line() + facet_wrap('model') +
    labs(y='Ecov') +
    geom_point(mapping=aes_string('year', y=cov.name), color=1)
  ggsave(paste0('../Results/WHAM/Ecov_compare_',cov.name,'.png'), g, width=7, height=6)
  ssb.all[[which.covariate]] <- lapply(1:4, function(x) get_ssb_ci(fits[[x]],
    names(fits)[x])) %>% bind_rows %>% mutate(Ecov=cov.name)
  g <- ssb.all[[which.covariate]] %>%
    bind_rows %>% #filter(year>1990) %>%
    ggplot(aes(year, est, ymin=lwr, ymax=upr, fill=model, color=model)) +
    geom_ribbon(alpha=.25) + labs(y='SSB')+
    geom_line() #+ facet_wrap('model')
  ggsave(paste0('../Results/WHAM/ssb_compare_',cov.name,'.png'), g, width=7, height=4)

  ## par.tab.all[[which.covariate]] <- lapply(1:4, function(x) par_table(fits[[x]], names(fits)[x])) %>%
  ##   bind_rows %>%
  ##   pivot_wider(id_cols=par, values_from='length',
  ##               names_from='version')

  ## get residuals for each one
  obs_index1 <- read.table('../Data/obs_index1.txt',sep='\t') %>%
    setNames(c('year', 'index', 'CV')) %>%
    mutate(obs=(index),
           lwr=obs/exp(2*sqrt(log(1+((obs*CV)/obs))^2)),
           upr=obs*exp(2*sqrt(log(1+((obs*CV)/obs))^2)),
           model='real')
  indices <- lapply(1:4, function(x)
    data.frame(model=names(fits)[x],
               year=1970:2021,
               expected=exp(fits[[x]]$rep$pred_log_indices[,1]))) %>%
    bind_rows %>% filter(year>1991)
  indices.all[[which.covariate]] <- indices %>% cbind(Ecov=cov.name)
  g <- ggplot(indices, aes(year, expected, color=model)) + geom_line(lwd=1.25) +
    geom_pointrange(data=obs_index1, fatten=2,
                    mapping=aes(year, obs, ymin=lwr, ymax=upr, color=NULL)) +
    labs(y='Survey 1 biomass') + ylim(0,2200000)
  ggsave(paste0('../Results/WHAM/index_compare_', cov.name,'.png'), g, width=7, height=4)
  beta.tab.all[[which.covariate]] <-  lapply(1:4, function(x)
    get_Ecov_beta(fits[[x]], names(fits)[x])) %>% bind_rows %>%
    cbind(Ecov=cov.name)
  repars.tab.all[[which.covariate]] <-  lapply(1:4, function(x)
    get_repars(fits[[x]], names(fits)[x])) %>% bind_rows %>%
    cbind(Ecov=cov.name)
  file.remove('model_comparison.csv')

  write.csv(bind_rows(beta.tab.all), file=paste0('../Results/WHAM/beta_table.csv'))
  write.csv(bind_rows(repars.tab.all), file=paste0('../Results/WHAM/repars_table.csv'))
  write.csv(bind_rows(aic.tab.all), file=paste0('../Results/WHAM/aic_table.csv'))
  ## write.csv(bind_rows(par.tab.all), file=paste0('../Results/WHAM/par_table.csv'))
  write.csv(bind_rows(indices.all), file=paste0('../Results/WHAM/indices.csv'))
  write.csv(bind_rows(ssb.all), file='../Results/WHAM/ssb.csv')
  write.csv(bind_rows(q.all), file='../Results/WHAM/q.csv')
  message("done with ", cov.name)
}


g <- bind_rows(beta.tab.all) %>% filter(model=='fit2') %>%
  ggplot(aes(x=Ecov, y=est, ymin=lwr, ymax=upr)) +
  geom_pointrange() + geom_hline(yintercept=0, col='red') +
  labs(y='Estimated slope effect')
ggsave("../Results/WHAM/beta_comparison.png", g, width=7, height=5)

g <- bind_rows(repars.tab.all) %>%
  filter(model %in% c('fit1', 'fit3')) %>%
  ggplot(aes(x=model, y=exp(est), ymin=exp(lwr), ymax=exp(upr))) +
  geom_pointrange() + geom_hline(yintercept=0, col='red') +
  labs(y='Estimated RW process SD') + facet_wrap('Ecov')
ggsave("../Results/WHAM/repars_comparison.png", g, width=7, height=5)

## ## Session info from last time run
## R version 4.3.1 (2023-06-16 ucrt)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19045)

## Matrix products: default


## locale:
## [1] LC_COLLATE=English_United States.utf8
## [2] LC_CTYPE=English_United States.utf8
## [3] LC_MONETARY=English_United States.utf8
## [4] LC_NUMERIC=C
## [5] LC_TIME=English_United States.utf8

## time zone: America/Los_Angeles
## tzcode source: internal

## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base

## other attached packages:
## [1] purrr_1.0.2           GOApollock_0.1.0.9000 ggplot2_3.4.4
## [4] tidyr_1.3.0           dplyr_1.1.4           wham_1.0.6

## loaded via a namespace (and not attached):
##  [1] utf8_1.2.4       R6_2.5.1         tidyselect_1.2.0 magrittr_2.0.3
##  [5] gtable_0.3.4     glue_1.6.2       tibble_3.2.1     pkgconfig_2.0.3
##  [9] generics_0.1.3   lifecycle_1.0.4  cli_3.6.2        fansi_1.0.6
## [13] scales_1.3.0     grid_4.3.1       vctrs_0.6.5      withr_2.5.2
## [17] compiler_4.3.1   tools_4.3.1      munsell_0.5.0    pillar_1.9.0
## [21] colorspace_2.1-0 rlang_1.1.2
