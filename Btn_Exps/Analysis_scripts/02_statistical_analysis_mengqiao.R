################# statistical analysis ######################

library(here)
library(tidyverse)
library(ez)
library(feather)
library(rstatix)
library(wesanderson)
library(ggsignif)

###########################################################
########### the Analysis on the Reaction time #############
###########################################################

# load the dataframe of data from experiment 1
bigdf_v1 <- read_feather(paste0(here(),"/data/bigdf_cleanSubTri.feather")) %>%
  mutate(exp = "v1",
         stim_flex = TRUE,
         rule_flex = TRUE)


# load the dataframe of data from experiment version 2
bigdf_v2 <- read_feather(paste0(here(),"/Experiment_var2/data/bigdf_cleanSubTri.feather")) %>%
  mutate(exp = "v2",
         stim_flex = TRUE,
         rule_flex = TRUE)


# load the dataframe of data from control experiment
bigdf_ctl <- read_feather(paste0(here(),"/control_experiment/data/bigdf_cleanSubTri.feather")) %>%
  mutate(block_type = str_replace(block_type, "OT", "TF")) %>% # change the block_type from "OT" to "TF" 
  mutate(exp = "control",
         stim_flex = FALSE,
         rule_flex = FALSE)

# load the dataframe of data from experiment rule transform
load(file = paste0(here(), "/Experiment_rule_tran/result/bigdf_clean.Rdata"))

bigdf_rule <- bigdf_clean %>%
  mutate(exp = "rule_tran",
         stim_flex = FALSE,
         rule_flex = TRUE)

# load the dataframe of data from experiment stim transform
load(file = paste0(here(), "/Experiment_stim_tran/result/bigdf_clean.Rdata"))

bigdf_stim <- bigdf_clean %>%
  mutate(exp = "stim_tran",
         stim_flex = TRUE,
         rule_flex = FALSE)

rm(bigdf_clean)

# bind all three datasets and only include the regular trials for analysis ###

hugedf <- bind_rows(bigdf_v1, bigdf_v2, bigdf_ctl, bigdf_rule, bigdf_stim) %>%
  select(rt, block_type, block_id, trial_nature, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins, 
         incongruence_score_stim, incongruence_score_rule, exp, stim_flex, rule_flex)

huge_regular_trials <- hugedf %>%
  filter(trial_nature == "RG")

###########################################################
################### mixed effect model ####################
###########################################################

library(lmerTest)
library(lme4)
library(emmeans)

options(contrasts = c("contr.sum","contr.poly"))

################  comparing 2 experiment ##################
# select the data sets that shall be compared #

huge_regular_trials_lme <- huge_regular_trials %>%
  filter(exp %in% c("rule_tran", "stim_tran")) %>% # IMPORTANT!! the comparison between which 2 or more datasets
  mutate(CTI_sec = CTI/1000) %>%
  convert_as_factor(exp, block_type, subject) %>%
  ungroup()

tranblk_regular_trials_lme <- huge_regular_trials %>%
  filter(exp %in% c("rule_tran", "stim_tran"),
         block_type == "TF") %>% 
  mutate(CTI_sec = CTI/1000) %>%
  convert_as_factor(exp, block_type, subject) %>%
  ungroup()

contrasts(huge_regular_trials_lme$exp)

# specify the model #

model_RT_1 <- lmer(formula = rt ~ CTI_sec * block_type * exp
                 + (1 | subject), # nested structure
                 data = huge_regular_trials_lme,
                 control = lmerControl(optimizer = "bobyqa"))

model_RT_2 <- lmer(formula = rt ~ (poly(CTI_sec,2) + incongruence_score_stim + incongruence_score_rule) * block_type * exp
                    + (1 | subject),
                    data = huge_regular_trials_lme,
                   control = lmerControl(optimizer = "bobyqa"))

model_RT_3 <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type * exp
                   + (1 | subject),
                   data = huge_regular_trials_lme,
                   control = lmerControl(optimizer = "bobyqa"))

model_RT_4 <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * exp
                   + (1 | subject),
                   data = tranblk_regular_trials_lme,
                   control = lmerControl(optimizer = "bobyqa"))

anova(model_RT_2)
summary(model_RT_4, correlation= FALSE)

# post-hoc check on all the interactions #
emtrends(model_RT_3, ~ exp, var = 'incongruence_score_rule')
test(emtrends(model_RT_3, ~ exp, var = 'incongruence_score_rule'))

emmip(model_RT_2, block_type ~ poly(CTI_sec,1) | exp, cov.reduce = range) +
  scale_color_manual(values = c("#4E84C4", "#FC4E07"))


###################  comparing all experiments #################
### focus on the congruency effect (rule_flex and stim_flex) ###

huge_regular_trials_lme_all <- huge_regular_trials %>%
  mutate(CTI_sec = CTI/1000) %>%
  convert_as_factor(rule_flex, stim_flex, block_type, subject) %>%
  ungroup()

contrasts(huge_regular_trials_lme_all$block_type)

# specify the model #

model_RT_all <- lmer(formula = rt ~ (poly(CTI_sec,2) + incongruence_score_stim + incongruence_score_rule) * block_type * rule_flex * stim_flex
                             + (1 | subject),
                     data = huge_regular_trials_lme_all,
                     control = lmerControl(optimizer = "bobyqa"))

model_RT_all_2 <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type * rule_flex * stim_flex
                     + (1 | subject),
                     data = huge_regular_trials_lme_all,
                     control = lmerControl(optimizer = "bobyqa"))

summary(model_RT_all_2, correlation= FALSE)


# post-hoc check on all the significant interaction

emmip(model_RT_all_2, stim_flex ~ incongruence_score_rule, cov.reduce = range) +
  scale_color_manual(values = c("#4E84C4", "#FC4E07"))
emtrends(model_RT_all, ~ stim_flex, var = 'incongruence_score_rule')

emmip(model_RT_all_2, block_type ~ CTI_sec|stim_flex, cov.reduce = range) +
  scale_color_manual(values = c("#4E84C4", "#FC4E07"))

emmip(model_RT_all_2, block_type ~ CTI_sec|rule_flex:stim_flex, cov.reduce = range) +
  scale_color_manual(values = c("#4E84C4", "#FC4E07"))

emmip(model_RT_all_2, rule_flex ~ incongruence_score_rule, cov.reduce = range) +
  scale_color_manual(values = c("#4E84C4", "#FC4E07"))
emtrends(model_RT_all, ~ rule_flex, var = 'incongruence_score_rule')

###############################################################
############# the Analysis on the Accuracy rate ###############
###############################################################

# load the dataframe of data from experiment version 2
bigdf_v1_error <- read_feather(paste0(here(),"/data/bigdf_cleanSubTri_errorback.feather")) %>%
  mutate(exp = "v1",
         stim_flex = TRUE,
         rule_flex = TRUE)

# load the dataframe of data from experiment version 2
bigdf_v2_error <- read_feather(paste0(here(),"/Experiment_var2/data/bigdf_cleanSubTri_errorback.feather")) %>%
  mutate(exp = "v2",
         stim_flex = TRUE,
         rule_flex = TRUE)

# load the dataframe of data from control experiment
bigdf_ctl_error <- read_feather(paste0(here(),"/control_experiment/data/bigdf_cleanSubTri_errorback.feather")) %>%
  mutate(block_type = str_replace(block_type, "OT", "TF")) %>% # change the block_type from "OT" to "TF" 
  mutate(exp = "control",
         stim_flex = FALSE,
         rule_flex = FALSE)

# load the dataframe of data from experiment rule transform
load(file = paste0(here(), "/Experiment_rule_tran/result/bigdf_clean_error.Rdata"))

bigdf_rule_error <- bigdf_clean_error %>%
  mutate(exp = "rule_tran",
         stim_flex = FALSE,
         rule_flex = TRUE)

# load the dataframe of data from experiment stim transform
load(file = paste0(here(), "/Experiment_stim_tran/result/bigdf_clean_error.Rdata"))

bigdf_stim_error <- bigdf_clean_error %>%
  mutate(exp = "stim_tran",
         stim_flex = TRUE,
         rule_flex = FALSE)

rm(bigdf_clean_error)

# bind all the datasets and only include the regular trials for analysis ###

hugedf_error <- bind_rows(bigdf_v1_error, bigdf_v2_error, bigdf_ctl_error, bigdf_rule_error, bigdf_stim_error) %>%
  filter(!is.na(rt)) %>%
  select(rt, block_type, block_id, trial_nature, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins, 
         incongruence_score_stim, incongruence_score_rule, exp, stim_flex, rule_flex)

huge_regular_trials_error <- hugedf_error %>%
  filter(trial_nature == "RG")


############## the mixed effect logistic regression model on ACC #############

library(lmerTest)
library(lme4)
library(emmeans)

options(contrasts = c("contr.sum","contr.poly"))

######  comparing 2 experiment ######

# only select the relevant dataset to compare

huge_regular_trials_error_lme <- huge_regular_trials_error %>%
  filter(exp == "rule_tran" | exp == "stim_tran") %>%
  mutate(CTI_sec = CTI/1000,
         ACC_bi = if_else(accuracy == TRUE, 1, 0)) %>%
  convert_as_factor(exp, block_type, subject) %>%
  ungroup()

tranblk_regular_trials_error_lme <- huge_regular_trials_error %>%
  filter(exp == "rule_tran" | exp == "stim_tran",
         block_type == "TF") %>%
  mutate(CTI_sec = CTI/1000,
         ACC_bi = if_else(accuracy == TRUE, 1, 0)) %>%
  convert_as_factor(exp, block_type, subject) %>%
  ungroup()

contrasts(huge_regular_trials_error_lme$block_type)

model_acc <- glmer(formula = ACC_bi ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type * exp 
                   + (1|subject),
                   data = huge_regular_trials_error_lme,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

model_acc2 <- glmer(formula = ACC_bi ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * exp 
                   + (1|subject),
                   data = tranblk_regular_trials_error_lme,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

anova(model_acc)
summary(model_acc2, correlation= FALSE)

# post-hoc test: 
emmeans(model_acc, "incongruence_score_rule", type = "response", cov.reduce = range)
emtrends(model_acc, ~ exp, var = 'incongruence_score_rule', type = "response")
test(emtrends(model_acc, ~ exp, var = 'incongruence_score_rule', type = "response"))

emmip(model_acc, block_type ~ incongruence_score_stim | exp, cov.reduce = range, type = "response") 
emmip(model_acc, exp ~ incongruence_score_rule, cov.reduce = range, type = "response")

emtrends(model_acc2, ~ exp, var = 'incongruence_score_stim', type = "response")



######  comparing ALL experiments ######

huge_regular_trials_error_lme_all <- huge_regular_trials_error %>%
  mutate(CTI_sec = CTI/1000,
         ACC_bi = if_else(accuracy == TRUE, 1, 0)) %>%
  convert_as_factor(rule_flex, stim_flex, block_type, subject) %>%
  ungroup()

contrasts(huge_regular_trials_error_lme_all$block_type)

model_acc_all <- glmer(formula = ACC_bi ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type * rule_flex * stim_flex 
                           + (1|subject),
                   data = huge_regular_trials_error_lme_all,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

summary(model_acc_all, correlation= FALSE)

# post-hoc tests
emmip(model_acc_all, rule_flex ~ incongruence_score_rule, cov.reduce = range, type = "response")+
  scale_color_manual(values = c("#4E84C4", "#FC4E07"))
emtrends(model_acc_all, ~ rule_flex, var = 'incongruence_score_rule')

emmip(model_acc_all, stim_flex ~ incongruence_score_rule, cov.reduce = range, type = "response")
emtrends(model_acc_all, ~ stim_flex, var = 'incongruence_score_rule')

emmip(model_acc_all, block_type ~ incongruence_score_stim | stim_flex, cov.reduce = range, type = "response")

emmip(model_acc_all, rule_flex ~ incongruence_score_rule | stim_flex, cov.reduce = range, type = "response")

###############################################################
################## simulating data for DDM ####################
###############################################################

nsub_v1 <- unique(bigdf_v1$subject)
nsub_ctl <- unique(bigdf_ctl$subject)
nsub_rule <- unique(bigdf_rule$subject)
nsub_stim <- unique(bigdf_stim$subject)

exp_list <- list(bigdf_v1, bigdf_ctl, bigdf_rule, bigdf_stim)

count_trials_incon <- function(exp_df, incong_type) {
  # count the number of trials for each incongruency score level
  avg_trialcount <- exp_df %>%
    group_by({{incong_type}}) %>%
    summarise(count = n(),
              nsub = length(unique(subject))) %>%
    mutate(avg_count = floor(count/nsub)) %>%
    pull(avg_count)
  return(avg_trialcount)
}
  
a <- lapply(exp_list, count_trials_incon, incongruence_score_rule)
