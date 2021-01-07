library(car)
library(stargazer)
library(texreg)
library(heplots)
library(plyr)
library(xtable)
library(splitstackshape)
library(dplyr)
library(lme4)

library(Hmisc)
library(lmerTest)
library(e1071)
library("rms")
library(ggplot2)

set.seed(17) # so can reproduce the results

#PATH TO DATA WIT SIMULATED VARIABLE
prep_combined <- read.csv('/Data/R scripts/pull_requests.csv')

#CREATE THE FINAL DATASET (INPUT IS COMING FROM MYSQL)
prep_combined$prs_white<-as.numeric(as.character(prep_combined$prs_white))
prep_combined$prs_black<-as.numeric(as.character(prep_combined$prs_black))
prep_combined$prs_api<-as.numeric(as.character(prep_combined$prs_api))
prep_combined$prs_hispanic<-as.numeric(as.character(prep_combined$prs_hispanic))

prep_combined$pri_white<-as.numeric(as.character(prep_combined$pri_white))
prep_combined$pri_black<-as.numeric(as.character(prep_combined$pri_black))
prep_combined$pri_api<-as.numeric(as.character(prep_combined$pri_api))
prep_combined$pri_hispanic<-as.numeric(as.character(prep_combined$pri_hispanic))

prep_combined$prs_continent <- as.character(prep_combined$prs_continent)
prep_combined$prs_continent[prep_combined$prs_continent == "Australia"]<- "Oceania"
prep_combined$prs_continent[prep_combined$prs_continent == "\\N"]<- "Unknown"
prep_combined$prs_continent <- as.factor(prep_combined$prs_continent)
prep_combined$prs_continent <- factor(prep_combined$prs_continent,levels=c("Asia","Africa","South America","Antarctica", "Unknown", "North America", "Europe", "Oceania"))
prep_combined <- within(prep_combined, prs_continent <- relevel(prs_continent, ref='North America'))

prep_combined$prs_pri_same_nationality = "Different"
prep_combined$prs_country <- as.character(prep_combined$prs_country)
prep_combined$prc_country <- as.character(prep_combined$prc_country)
prep_combined[which(prep_combined$prs_country == prep_combined$prc_country),]$prs_pri_same_nationality = "Same"
prep_combined[which(prep_combined$prs_country == "\\N"),]$prs_pri_same_nationality = "Unknown"
prep_combined[which(prep_combined$prc_country == "\\N"),]$prs_pri_same_nationality = "Unknown"
prep_combined$prs_pri_same_nationality<-factor(prep_combined$prs_pri_same_nationality,levels=c("Same","Different","Unknown"))
prep_combined <- within(prep_combined, prs_pri_same_nationality <- relevel(prs_pri_same_nationality, ref='Same'))

#SKIP THIS PART IF YOU ALREADY HAVE THE LATEST VERSION OF THE DATA
## submitter ##
prep_combined$prs_eth_7 = "Unknown"
prep_combined[which(prep_combined$prs_hispanic >= 0.7),]$prs_eth_7 = "Hispanic"
prep_combined[which(prep_combined$prs_white >= 0.7),]$prs_eth_7 = "White"
prep_combined[which(prep_combined$prs_api >= 0.7),]$prs_eth_7 = "API"
prep_combined[which(prep_combined$prs_black >= 0.7),]$prs_eth_7 = "Black"

prep_combined$prs_eth_8 = prep_combined$prs_eth

prep_combined$prs_eth_9 = "Unknown"
prep_combined[which(prep_combined$prs_hispanic > 0.9),]$prs_eth_9 = "Hispanic"
prep_combined[which(prep_combined$prs_white > 0.9),]$prs_eth_9 = "White"
prep_combined[which(prep_combined$prs_api > 0.9),]$prs_eth_9 = "API"
prep_combined[which(prep_combined$prs_black > 0.9),]$prs_eth_9 = "Black"

prep_combined$prs_eth_diff = "Unknown"
prep_combined[which(prep_combined$prs_hispanic > 0.7),]$prs_eth_diff = "Hispanic"
prep_combined[which(prep_combined$prs_white > 0.8),]$prs_eth_diff = "White"
prep_combined[which(prep_combined$prs_api > 0.6),]$prs_eth_diff = "API"
prep_combined[which(prep_combined$prs_black > 0.8),]$prs_eth_diff = "Black"

prep_combined$prs_eth_diff_2 = "Unknown"
prep_combined[which(prep_combined$prs_hispanic > 0.7),]$prs_eth_diff_2 = "Hispanic"
prep_combined[which(prep_combined$prs_white > 0.8),]$prs_eth_diff_2 = "White"
prep_combined[which(prep_combined$prs_api > 0.6),]$prs_eth_diff_2 = "API"
prep_combined[which(prep_combined$prs_black > 0.9),]$prs_eth_diff_2 = "Black"


## integrator ##
prep_combined$pri_eth_7 = "Unknown"
prep_combined[which(prep_combined$pri_hispanic >= 0.7),]$pri_eth_7 = "Hispanic"
prep_combined[which(prep_combined$pri_white >= 0.7),]$pri_eth_7 = "White"
prep_combined[which(prep_combined$pri_api >= 0.7),]$pri_eth_7 = "API"
prep_combined[which(prep_combined$pri_black >= 0.7),]$pri_eth_7 = "Black"

prep_combined$pri_eth_8 = prep_combined$prc_eth

prep_combined$pri_eth_9 = "Unknown"
prep_combined[which(prep_combined$pri_hispanic > 0.9),]$pri_eth_9 = "Hispanic"
prep_combined[which(prep_combined$pri_white > 0.9),]$pri_eth_9 = "White"
prep_combined[which(prep_combined$pri_api > 0.9),]$pri_eth_9 = "API"
prep_combined[which(prep_combined$pri_black > 0.9),]$pri_eth_9 = "Black"

prep_combined$pri_eth_diff = "Unknown"
prep_combined[which(prep_combined$pri_hispanic > 0.7),]$pri_eth_diff = "Hispanic"
prep_combined[which(prep_combined$pri_white > 0.8),]$pri_eth_diff = "White"
prep_combined[which(prep_combined$pri_api > 0.6),]$pri_eth_diff = "API"
prep_combined[which(prep_combined$pri_black > 0.8),]$pri_eth_diff = "Black"

prep_combined$pri_eth_diff_2 = "Unknown"
prep_combined[which(prep_combined$pri_hispanic > 0.7),]$pri_eth_diff_2 = "Hispanic"
prep_combined[which(prep_combined$pri_white > 0.8),]$pri_eth_diff_2 = "White"
prep_combined[which(prep_combined$pri_api > 0.6),]$pri_eth_diff_2 = "API"
prep_combined[which(prep_combined$pri_black > 0.9),]$pri_eth_diff_2 = "Black"
################################################################################


print("Convert all variables to appropriate data types")
prep_combined$pr_status<-factor(prep_combined$pr_status,levels=c("not-merged","merged"))
prep_combined$prs_eth <- factor(prep_combined$prs_eth,levels=c("White","Black","Hispanic","API", "Unknown","AIAN"))
prep_combined$prm_eth <- factor(prep_combined$prm_eth,levels=c("White","Black","Hispanic","API", "Unknown"))
prep_combined$prc_eth <- factor(prep_combined$prc_eth,levels=c("White","Black","Hispanic","API", "Unknown"))

prep_combined$repo_pr_tenure_mnth<-as.integer(prep_combined$repo_pr_tenure_mnth)
prep_combined$repo_pr_tenure_mnth<-scale(log(prep_combined$repo_pr_tenure_mnth +1))
prep_combined$repo_pr_popularity<-as.integer(prep_combined$repo_pr_popularity)
prep_combined$repo_pr_popularity<-scale(log(prep_combined$repo_pr_popularity +1))
prep_combined$repo_pr_team_size<-as.integer(prep_combined$repo_pr_team_size)
prep_combined$repo_pr_team_size<-scale(log(prep_combined$repo_pr_team_size +1))
prep_combined$perc_external_contribs<-as.integer(prep_combined$perc_external_contribs)
prep_combined$perc_external_contribs<-scale(log(prep_combined$perc_external_contribs +1))

prep_combined$prs_experience<-as.integer(prep_combined$prs_experience)
prep_combined$prs_experience<-scale(log(prep_combined$prs_experience +1))
prep_combined$prs_succ_rate<-as.integer(prep_combined$prs_succ_rate)
prep_combined$prs_succ_rate<-scale(log(prep_combined$prs_succ_rate +1))
prep_combined$prs_main_team_member<-factor(prep_combined$prs_main_team_member,levels=c(0,1))
prep_combined$prs_popularity<-as.integer(prep_combined$prs_popularity)
prep_combined$prs_popularity<-scale(log(prep_combined$prs_popularity +1))
prep_combined$prs_tenure_mnth<-as.integer(prep_combined$prs_tenure_mnth)
prep_combined$prs_tenure_mnth<-scale(log(prep_combined$prs_tenure_mnth +1))
prep_combined$pr_comments_counts<-as.integer(prep_combined$pr_comments_counts)
prep_combined$pr_comments_counts<-scale(log(prep_combined$pr_comments_counts +1))
prep_combined$pr_num_commits<-as.integer(prep_combined$pr_num_commits)
prep_combined$pr_num_commits<-scale(log(prep_combined$pr_num_commits +1))
prep_combined$pr_nth<-as.integer(prep_combined$pr_nth)
prep_combined$pr_nth<-scale(log(prep_combined$pr_nth +1))
prep_combined$prs_watched_repo<-factor(prep_combined$prs_watched_repo,levels=c(0,1))
prep_combined$prs_watched_repo<-factor(prep_combined$prs_watched_repo,levels=c(0,1))
prep_combined$prs_followed_pri<-factor(prep_combined$prs_followed_pri,levels=c(0,1))
prep_combined$intra_branch<-factor(prep_combined$intra_branch,levels=c(0,1))

temp <- prep_combined

#################################################################
# Dataset for All Ethnicities (threshold 0.8), ##################
### Excluding All unknowns                     ##################
#################################################################

prep_combined <- temp
prep_combined <- prep_combined[ which(prep_combined$prs_eth_8!="AIAN"), ]
prep_combined <- prep_combined[ which(prep_combined$prs_eth_8!="Unknown"), ]
prep_combined <- prep_combined[ which(prep_combined$pri_eth_8!="Unknown"), ]

prep_combined$prs_eth_8 <- factor(prep_combined$prs_eth_8,levels=c("White","Black","API","Hispanic"))
prep_combined$pri_eth_8 <- factor(prep_combined$pri_eth_8,levels=c("White","Black","API","Hispanic"))
prep_combined <- within(prep_combined, prs_eth_8 <- relevel(prs_eth_8, ref='White'))
prep_combined <- within(prep_combined, pri_eth_8 <- relevel(pri_eth_8, ref='White'))

eth_08_EU <- prep_combined


##################################################################
# Dataset for All Ethnicities (threshold 0.7),  ##################
### Excluding All unknowns                      ##################
##################################################################

prep_combined <- temp
prep_combined <- prep_combined[ which(prep_combined$prs_eth_7!="AIAN"), ]
prep_combined <- prep_combined[ which(prep_combined$prs_eth_7!="Unknown"), ]
prep_combined <- prep_combined[ which(prep_combined$pri_eth_7!="Unknown"), ]

prep_combined$prs_eth_7 <- factor(prep_combined$prs_eth_7,levels=c("White","Black","API","Hispanic"))
prep_combined$pri_eth_7 <- factor(prep_combined$pri_eth_7,levels=c("White","Black","API","Hispanic"))
prep_combined <- within(prep_combined, prs_eth_7 <- relevel(prs_eth_7, ref='White'))
prep_combined <- within(prep_combined, pri_eth_7 <- relevel(pri_eth_7, ref='White'))

eth_07_EU <- prep_combined


##################################################################
# Dataset for All Ethnicities (threshold 0.9),  ##################
### Excluding All unknowns, With same ethnicity ##################
##################################################################

prep_combined <- temp
prep_combined <- prep_combined[ which(prep_combined$prs_eth_9!="AIAN"), ]
prep_combined <- prep_combined[ which(prep_combined$prs_eth_9!="Unknown"), ]
prep_combined <- prep_combined[ which(prep_combined$pri_eth_9!="Unknown"), ]

prep_combined$prs_eth_9 <- factor(prep_combined$prs_eth_9,levels=c("White","Black","API","Hispanic"))
prep_combined$pri_eth_9 <- factor(prep_combined$pri_eth_9,levels=c("White","Black","API","Hispanic"))
prep_combined <- within(prep_combined, prs_eth_9 <- relevel(prs_eth_9, ref='White'))
prep_combined <- within(prep_combined, pri_eth_9 <- relevel(pri_eth_9, ref='White'))

eth_09_EU <- prep_combined


########### Base MODEL Submitter  #########
###########################################

model<-pr_status~repo_pr_tenure_mnth+
  repo_pr_popularity+
  repo_pr_team_size+
  perc_external_contribs+
  prs_succ_rate+
  pr_files_changed +
  prs_main_team_member+
  prs_popularity+
  prs_watched_repo+
  prs_followed_pri+
  prs_tenure_mnth+
  pr_comments_counts+
  pr_num_commits+
  #prs_eth_8 +
  prs_experience+
  pr_nth+
  prs_continent+
  prs_pri_same_nationality+
  intra_branch+
  (1|prs_id) + (1|repo_id)

glmer_model_ethnicities<-glmer(model,data=eth_08_EU, family=binomial,control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(maxeval = 50)))
print(summary(glmer_model_ethnicities), correlation=FALSE)
car::vif(glmer_model_ethnicities)

effect_size_glmer_model_ethnicities = anova(glmer_model_ethnicities,test='Chisq')
print(effect_size_glmer_model_ethnicities)


########### MODEL Submitter_08  ##################
### Submitor as random effect   ##################
################################################

model<-pr_status~repo_pr_tenure_mnth+
  repo_pr_popularity+
  repo_pr_team_size+
  perc_external_contribs+
  prs_succ_rate+
  pr_files_changed +
  prs_main_team_member+
  prs_popularity+
  prs_watched_repo+
  prs_followed_pri+
  prs_tenure_mnth+
  pr_comments_counts+
  pr_num_commits+
  prs_eth_8 +
  prs_experience+
  pr_nth+
  prs_continent+
  prs_pri_same_nationality+
  intra_branch+
  (1|prs_id) + (1|repo_id)

glmer_model_ethnicities<-glmer(model,data=eth_08_EU, family=binomial,control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(maxeval = 50)))
print(summary(glmer_model_ethnicities), correlation=FALSE)
car::vif(glmer_model_ethnicities)

effect_size_glmer_model_ethnicities = anova(glmer_model_ethnicities,test='Chisq')
print(effect_size_glmer_model_ethnicities)

########### MODEL Submitter NoAssociation.  ####################
### NoAssociation pri_prs association       ####################
### Submitor as random effect               ####################
################################################################

model<-pr_status~repo_pr_tenure_mnth+
  repo_pr_popularity+
  repo_pr_team_size+
  perc_external_contribs+
  prs_succ_rate+
  pr_files_changed +
  prs_main_team_member+
  prs_popularity+
  prs_watched_repo+
  #prs_followed_pri+
  prs_tenure_mnth+
  pr_comments_counts+
  pr_num_commits+
  prs_eth_8 +
  prs_experience+
  pr_nth+
  prs_continent+
  prs_pri_same_nationality+
  intra_branch+
  (1|prs_id) + (1|repo_id)

glmer_model_ethnicities<-glmer(model,data=eth_08_EU, family=binomial,control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(maxeval = 50)))
print(summary(glmer_model_ethnicities), correlation=FALSE)
car::vif(glmer_model_ethnicities)

effect_size_glmer_model_ethnicities = anova(glmer_model_ethnicities,test='Chisq')
print(effect_size_glmer_model_ethnicities)


########### MODEL Submitter Interaction      ####################
### With interaction term (prs_eth*pri_eth ) ####################
### Submitor as random effect                ####################
#################################################################

model<-pr_status~repo_pr_tenure_mnth+
  repo_pr_popularity+
  repo_pr_team_size+
  perc_external_contribs+
  prs_succ_rate+
  pr_files_changed +
  prs_main_team_member+
  prs_popularity+
  prs_watched_repo+
  prs_followed_pri+
  prs_tenure_mnth+
  pr_comments_counts+
  pr_num_commits+
  #prs_eth_8+
  prs_eth_8*pri_eth_8 +
  prs_experience+
  pr_nth+
  prs_continent+
  prs_pri_same_nationality+
  intra_branch+
  (1|prs_id) + (1|repo_id)

glmer_model_ethnicities<-glmer(model,data=eth_08_EU, family=binomial,control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(maxeval = 50)))
print(summary(glmer_model_ethnicities), correlation=FALSE)
car::vif(glmer_model_ethnicities)

effect_size_glmer_model_ethnicities = anova(glmer_model_ethnicities,test='Chisq')
print(effect_size_glmer_model_ethnicities)


########### MODEL Submitter_07  ##################
### Submitor as random effect   ##################
################################################

model<-pr_status~repo_pr_tenure_mnth+
  repo_pr_popularity+
  repo_pr_team_size+
  perc_external_contribs+
  prs_succ_rate+
  pr_files_changed +
  prs_main_team_member+
  prs_popularity+
  prs_watched_repo+
  prs_followed_pri+
  prs_tenure_mnth+
  pr_comments_counts+
  pr_num_commits+
  prs_eth_7 +
  prs_experience+
  pr_nth+
  prs_continent+
  prs_pri_same_nationality+
  intra_branch+
  (1|prs_id) + (1|repo_id)

glmer_model_ethnicities<-glmer(model,data=eth_07_EU, family=binomial,control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(maxeval = 50)))
print(summary(glmer_model_ethnicities), correlation=FALSE)
car::vif(glmer_model_ethnicities)

effect_size_glmer_model_ethnicities = anova(glmer_model_ethnicities,test='Chisq')
print(effect_size_glmer_model_ethnicities)

########### MODEL Submitter_09  ##################
### Submitor as random effect   ##################
################################################

model<-pr_status~repo_pr_tenure_mnth+
  repo_pr_popularity+
  repo_pr_team_size+
  perc_external_contribs+
  prs_succ_rate+
  pr_files_changed +
  prs_main_team_member+
  prs_popularity+
  prs_watched_repo+
  prs_followed_pri+
  prs_tenure_mnth+
  pr_comments_counts+
  pr_num_commits+
  prs_eth_9 +
  prs_experience+
  pr_nth+
  prs_continent+
  prs_pri_same_nationality+
  intra_branch+
  (1|prs_id) + (1|repo_id)

glmer_model_ethnicities<-glmer(model,data=eth_09_EU, family=binomial,control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(maxeval = 50)))
print(summary(glmer_model_ethnicities), correlation=FALSE)
car::vif(glmer_model_ethnicities)

effect_size_glmer_model_ethnicities = anova(glmer_model_ethnicities,test='Chisq')
print(effect_size_glmer_model_ethnicities)
