
### install libraries
library(panelView)
library(dplyr)
library(gsynth)
library(ggplot2)
library(cowplot)
library(rstudioapi)

### set working directory to source file location

setwd(dirname(getActiveDocumentContext()$path))

### import sample data

dat_smp <- read.csv("./GSC_sample_data/dat_sample.csv", stringsAsFactors = FALSE)

### create new exposure indicator
### 0 for all weeks prior to index fire week and 1 only if exposed and during index fire week

dat_smp$exp_weekly <- ifelse(dat_smp$exp_bin==1& dat_smp$week>20,1,0)

### check missingness

missing <- dat_smp[!complete.cases(dat_smp),]

### exclude those with missing

dat_smp2 <- dat_smp[complete.cases(dat_smp),]

### check number of unique zip codes

length(unique(dat_smp2$zip_id))

### factor zip id

dat_smp2$zip_id <- as.factor(dat_smp2$zip_id)

### set seed

set.seed(4982)

### visualize treatment status by week for all units

panelView(resp_count ~ exp_weekly, data = dat_smp2,  index = c("zip_id","week"), pre.post = TRUE) 

### visualize raw outcome by week for all units

panelView(resp_count ~ exp_weekly, data = dat_smp2,  index = c("zip_id","week"), type = "outcome") 

### implement generalized synthetic control

gsynth.out <- gsynth(resp_count ~ exp_weekly + meantemp, data = dat_smp2, index = c("zip_id","week"), force="none",    
                     CV = TRUE, r = c(0,5), se = TRUE, 
                     inference = "nonparametric", nboots = 1000, parallel = TRUE, cores = 4)

### view ATT by period

gsynth.out$est.att

### gaps plot - difference between treated and counterfactual

plot(gsynth.out, type = "gap", ylim = c(-2,2), xlab = "Week", ylab="Difference Between Treated and Counterfactual", main="", theme.bw=TRUE)

### treated average and estimated counterfactual average outcomes

plot(gsynth.out, type = "counterfactual", raw = "none", main="", xlab = "Week", ylab='Average Hospitalizations', theme.bw=TRUE)



