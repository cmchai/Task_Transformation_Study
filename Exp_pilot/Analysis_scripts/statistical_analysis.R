################# statistical analysis ######################

setwd("D:/Ghent_Braem/Experiment/2nd_experiment/data")

library(tidyverse)
library(ez)
library(feather)
library(rstatix)
library(wesanderson)
library(ggsignif)
library(ggnewscale)
library(papaja)
library(rempsyc)
###########################################################
########### the Analysis on the Reaction time #############
###########################################################

bigdf_clean <- read_feather("bigdf_cleanSubTri.feather")
glimpse(bigdf_clean)

### step1. only include the regular trials for analysis ###

regular_trials <- bigdf_clean %>%
  filter(trial_nature == "RG") %>%
  select(rt, block_type, block_id, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins, incongruence_score_stim, incongruence_score_rule)

regular_nonstart_trials <- regular_trials %>%
  filter(str_detect(block_id, "_1", negate = TRUE))

regular_firsthalf_trials <- regular_trials %>%
  filter(str_detect(block_id, "_1") | str_detect(block_id, "_2"))

regular_firsthalf_RGstart_trials <- regular_firsthalf_trials %>%
  filter(start_block == "RG")
# length(unique(regular_firsthalf_RGstart_trials$subject))

regular_firsthalf_TFstart_trials <- regular_firsthalf_trials %>%
  filter(start_block == "TF")
# length(unique(regular_firsthalf_TFstart_trials$subject))

regular_secondhalf_trials <- regular_trials %>%
  filter(str_detect(block_id, "_1", negate = TRUE),
         str_detect(block_id, "_2", negate = TRUE))

regular_secondhalf_RGstart_trials <- regular_secondhalf_trials %>%
  filter(start_block == "RG")
# length(unique(regular_firsthalf_RGstart_trials$subject))

regular_secondhalf_TFstart_trials <- regular_secondhalf_trials %>%
  filter(start_block == "TF")
# length(unique(regular_firsthalf_TFstart_trials$subject))

regular_RGstart_trials <- regular_trials %>%
  filter(start_block == "RG")

regular_TFstart_trials <- regular_trials %>%
  filter(start_block == "TF")

### step1. only include the transform trials for analysis ###
transform_trials <- bigdf_clean %>%
  filter(trial_nature == "TF") %>%
  select(rt, block_type, block_id, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins, incongruence_score_stim, incongruence_score_rule)

############################################################################################
### step2. compare the subject level RT between regular and transform block and plotting ###
############################################################################################

rt_descrip <- regular_secondhalf_trials %>%
  convert_as_factor(CTI_bins, subject, block_type) %>%
  group_by(block_type, subject, CTI_bins) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

# mean RT of each subject
(
  p <- ggplot(rt_descrip, aes(x = CTI_bins, y = meanRT, color = block_type)) +
    geom_point(size = 4, position = position_dodge(0.2)) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position = position_dodge(0.2), alpha = .5) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    theme_bw()
)

# subplots for each subject
(
  p <- ggplot(rt_descrip, aes(x = CTI_bins, y = meanRT, color = block_type)) +
    geom_point(size = 4, position = position_dodge(0.2)) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position = position_dodge(0.2), alpha = .5) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    theme_bw() +
    facet_wrap(~ subject, nrow = 5)
)

### average over all the subjects ###
rt_descrip2 <- rt_descrip %>%
  group_by(block_type, CTI_bins) %>%
  summarise(subRT = mean(meanRT, na.rm = TRUE),
            subsd = sd(meanRT, na.rm = TRUE)) %>%
  ungroup()

group_vars(rt_descrip) # need to check the grouping variables after using "summarize" since it will drop one grouping variable 

(
  p <- ggplot(rt_descrip2, aes(x = CTI_bins, y = subRT, color = block_type)) +
    geom_point(data = rt_descrip2, aes(y = subRT), size = 5,
               position=position_dodge(0.2)) +
    geom_line(aes(group = block_type), size = 1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1080, 1200) +
    theme_bw()
)

(
  p <- ggplot(rt_descrip, aes(x = CTI_bins, y = meanRT, color = block_type)) +
    geom_point(size = 4, alpha = .06) +
    geom_point(data = rt_descrip2, aes(y = subRT), size = 5,
               position=position_dodge(0.2)) +
    geom_line(data = rt_descrip2, aes(y = subRT, group = block_type), size = 1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1100, 1200) +
    theme_bw()
)

# add error bar
(
  p <- ggplot(rt_descrip, aes(x = CTI_bins, y = meanRT, color = block_type)) +
    geom_point(size = 4, alpha = .06) +
    geom_point(data = rt_descrip2, aes(y = subRT), size = 5,
               position=position_dodge(0.2)) +
    geom_pointrange(data = rt_descrip2, aes(y = subRT, ymin = subRT - subsd, ymax = subRT + subsd), size =.8,
                    position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(900, 1350) +
    theme_bw()
)

### visualize the data in another way using CTI instead of CTI bins ###
rt_descrip3 <- regular_secondhalf_trials %>%
  convert_as_factor(subject, block_type) %>%
  group_by(block_type, subject, CTI) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

rt_descrip4 <- rt_descrip3 %>%
  group_by(block_type, CTI) %>%
  summarise(subRT = mean(meanRT, na.rm = TRUE),
            subsd = sd(meanRT, na.rm = TRUE)) %>%
  ungroup()  

(
  p <- ggplot(rt_descrip3, aes(x = CTI, y = meanRT, color = block_type)) +
    geom_point(size = 3,position=position_dodge(0.2)) +
    geom_line(aes(group = interaction(subject, block_type)), size = 1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(500, 2000) +
    theme_bw()
)


(
  p <- ggplot(rt_descrip4, aes(x = CTI, y = subRT, color = block_type)) +
    geom_point(size = 5,position=position_dodge(0.2)) +
    geom_line(aes(group = block_type), size = 1) +
    geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1050, 1300) +
    theme_bw()
)

# add an smooth line for publication
(
  p <- ggplot(rt_descrip4, aes(x = CTI, y = subRT, color = block_type)) +
    geom_point(size = 5,position=position_dodge(0.2), alpha = .3) +
    geom_smooth(method = "loess", span = 0.75, size = 2, linetype="dashed") +
    geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07"),
                       name = "block type:",
                       breaks = c("RG", "TF"),
                       labels = c("Regular", "Transform")) +
    coord_cartesian(ylim=c(1050, 1300)) +
    theme_apa(base_size = 14) +
    labs(x = "CTI in ms", y = "Reaction time in ms")
)


### descriptive data on transform trials ###
rt_tran_descrip1 <- transform_trials %>%
  convert_as_factor(subject) %>%
  group_by(subject, CTI) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

rt_tran_descrip2 <- rt_tran_descrip1 %>%
  group_by(CTI) %>%
  summarise(subRT = mean(meanRT, na.rm = TRUE),
            subsd = sd(meanRT, na.rm = TRUE)) %>%
  ungroup()

(
  p <- ggplot(rt_tran_descrip2, aes(x = CTI, y = subRT)) +
    geom_point(size = 5,position=position_dodge(0.2)) +
    geom_line(size = 1)  +
    scale_color_manual(values = "#4E84C4") +
    ylim(1050, 1200) +
    theme_bw()
)

#### the across-subject correlation between block-type diff in the regular trials and
#### the performance of transform trials

rt_regular_6bin <- regular_trials %>%
  filter(CTI_bins != 6) %>%
  group_by(subject, block_type) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  pivot_wider(names_from = block_type, values_from = meanRT) %>%
  mutate(sixbin_effect = TF - RG) %>%
  ungroup() %>%
  select(subject, sixbin_effect)

subject_regularblock_mean <- regular_trials %>%
  filter(block_type == "TF", CTI_bins == 6) %>%
  group_by(subject) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

subject_tranTrial_mean <- transform_trials %>%
  group_by(subject) %>%
  summarise(meanRT_tran = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(subject_regularblock_mean, by = "subject") %>%
  mutate(tranRT = meanRT_tran - meanRT) %>%
  left_join(rt_regular_6bin, by = "subject")
  
cor.test(subject_tranTrial_mean$tranRT, subject_tranTrial_mean$sixbin_effect)
cor.test(subject_tranTrial_mean$meanRT_tran, subject_tranTrial_mean$sixbin_effect)


### step3 run ANOVA model ###

# the ANOVA model
rt.aov <- anova_test(
  data = rt_descrip,
  dv = meanRT,
  wid = subject,
  within = c(block_type, CTI_bins)
)

get_anova_table(rt.aov)

# pairwise t tests
rt_descrip %>%
  pairwise_t_test(meanRT ~ CTI_bins,
                  paired = TRUE,
                  p.adjust.method = "bonferroni")

rt_descrip %>%
  pairwise_t_test(meanRT ~ block_type,
                  paired = TRUE,
                  p.adjust.method = "bonferroni")

### Split the data into short CTIS and long CTIS and run ANOVA on both parts

# the short CTIs and the ANOVA model
rt_descrip_short <- regular_trials %>%
  filter(CTI_bins %in% c(1,2,3,4)) %>%
  convert_as_factor(CTI_bins, subject, block_type) %>%
  group_by(block_type, subject, CTI_bins) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

rt_shortCTI.aov <- anova_test(
  data = rt_descrip_short,
  dv = meanRT,
  wid = subject,
  within = c(block_type, CTI_bins)
)

get_anova_table(rt_shortCTI.aov)

# the long CTIs and the ANOVA model

rt_descrip_long <- regular_trials %>%
  filter(CTI_bins %in% c(3,4,5,6)) %>%
  convert_as_factor(CTI_bins, subject, block_type) %>%
  group_by(block_type, subject, CTI_bins) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

rt_longCTI.aov <- anova_test(
  data = rt_descrip_long,
  dv = meanRT,
  wid = subject,
  within = c(block_type, CTI_bins)
)

get_anova_table(rt_longCTI.aov)

one.way <- rt_descrip_long %>%
  group_by(CTI_bins) %>%
  anova_test(dv = meanRT, wid = subject, within = block_type) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

one.way

###########################################################
############# Congruence effect on RT #####################
###########################################################

## visualize the congruence effect

# the stimulus congruence effect
rt_cgc_stim <- regular_trials %>%
  group_by(subject, block_type, incongruence_score_stim) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

rt_cgc_stim2 <- regular_trials %>%
  group_by(block_type, incongruence_score_stim) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()  


(
  p <- ggplot(regular_trials, aes(x = incongruence_score_stim, y = rt, color = block_type)) +
    geom_point(size = 0.4, position = "jitter") +
    geom_point(data = rt_cgc_stim, aes(y = meanRT), size = 3,
               position=position_dodge(0.2)) +
    geom_line(data = rt_cgc_stim, aes(y = meanRT, group = interaction(subject, block_type)), 
              size = 1, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    theme_bw()
)


(
  p <- ggplot(regular_trials, aes(x = incongruence_score_stim, y = rt, color = block_type)) +
    geom_point(size = 0.4, position = "jitter") +
    geom_point(data = rt_cgc_stim2, aes(y = meanRT), size = 5,
               position=position_dodge(0.2)) +
    geom_line(data = rt_cgc_stim2, aes(y = meanRT, group = block_type), 
              size = 1, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1000, 1250) +
    theme_bw()
)

(
  p <- ggplot(rt_cgc_stim, aes(x = incongruence_score_stim, y = meanRT, color = block_type)) +
    geom_point(size = 4, position = position_dodge(0.2), alpha = .08) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position = position_dodge(0.2), alpha = .2) +
    geom_point(data = rt_cgc_stim2, aes(y = meanRT), size = 7,
               position=position_dodge(0.2)) +
    geom_line(data = rt_cgc_stim2, aes(y = meanRT, group = block_type), 
              size = 1.6, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(900, 1400) +
    theme_bw()
)


cgc_stim.aov <- anova_test(
  data = rt_cgc_stim,
  dv = meanRT,
  wid = subject,
  within = c(block_type, incongruence_score_stim)
)

get_anova_table(cgc_stim.aov)


# the rule congruence effect

rt_cgc_rule <- regular_trials %>%
  group_by(subject, block_type, incongruence_score_rule) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

rt_cgc_rule2 <- regular_trials %>%
  group_by(block_type, incongruence_score_rule) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()  


(
  p <- ggplot(regular_trials, aes(x = incongruence_score_rule, y = rt, color = block_type)) +
    geom_point(size = 0.4, position = "jitter") +
    geom_point(data = rt_cgc_rule, aes(y = meanRT), size = 3,
               position=position_dodge(0.2)) +
    geom_line(data = rt_cgc_rule, aes(y = meanRT, group = interaction(subject, block_type)), 
              size = 1, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    theme_bw()
)


(
  p <- ggplot(regular_trials, aes(x = incongruence_score_rule, y = rt, color = block_type)) +
    geom_point(size = 0.4, position = "jitter") +
    geom_point(data = rt_cgc_rule2, aes(y = meanRT), size = 5,
               position=position_dodge(0.2)) +
    geom_line(data = rt_cgc_rule2, aes(y = meanRT, group = block_type), 
              size = 1, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1000, 1250) +
    theme_bw()
)

(
  p <- ggplot(rt_cgc_rule, aes(x = incongruence_score_rule, y = meanRT, color = block_type)) +
    geom_point(size = 4, position = position_dodge(0.2), alpha = .08) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position = position_dodge(0.2), alpha = .2) +
    geom_point(data = rt_cgc_rule2, aes(y = meanRT), size = 7,
               position=position_dodge(0.2)) +
    geom_line(data = rt_cgc_rule2, aes(y = meanRT, group = block_type), 
              size = 1.6, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1000, 1250) +
    theme_bw()
)


cgc_rule.aov <- anova_test(
  data = rt_cgc_rule,
  dv = meanRT,
  wid = subject,
  within = c(block_type, incongruence_score_rule)
)

get_anova_table(cgc_rule.aov)


###########################################################
################### mixed effect model ####################
###########################################################

library(lmerTest)
library(lme4)
library(emmeans)

options(contrasts = c("contr.sum","contr.poly"))

scale_this <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


regular_trials_lme <- regular_secondhalf_trials %>%
  mutate(CTI_sec = CTI/1000,
         CTI_sq = CTI*CTI,
         CTI_sec_sq = round(CTI_sec * CTI_sec, 3),
         CTI_bins_sq = CTI_bins*CTI_bins) %>%
  convert_as_factor(block_type, subject, CTI_bins, CTI_bins_sq) %>%
  group_by(subject) %>%
  mutate(CTI_scale_ind = scale_this(CTI),
         CTI_sq_scale_ind = scale_this(CTI_sq)) %>%
  ungroup()

contrasts(regular_trials_lme$CTI_bins)
contrasts(regular_trials_lme$block_type)


### specify the model

# still use the bin as the IV(categorical)
model_RT <- lmer(formula = rt ~ (CTI_bins + CTI_bins_sq) * block_type 
                 + (1 | subject),
                 data = regular_trials_lme)
anova(model_RT)
summary(model_RT)


# use scaled(at the individual level) CTI and its quadratic term as the IVs
model_RT2 <- lmer(formula = rt ~ (CTI_sec + CTI_sec_sq) * block_type 
                    + (1 | subject),
                  data = regular_trials_lme)

summary(model_RT2)
anova(model_RT2)


# use poly arguement in the lmer and CTI in second as predictors
model_RT2.1 <- lmer(formula = rt ~ poly(CTI_sec, 2) * block_type 
                  + (1 | subject),
                  data = regular_trials_lme)

summary(model_RT2.1)
anova(model_RT2.1)


# more complicated random effect on subject
model_RT2.2 <- lmer(formula = rt ~ (CTI_scale_ind + CTI_sq_scale_ind) * block_type
                    + (1 + (CTI_scale_ind + CTI_sq_scale_ind) * block_type | subject),
                  data = regular_trials_lme) # none converging :(

# make the random effect un-correlated on subject
model_RT2.3 <- lmer(formula = rt ~ (CTI_scale_ind + CTI_sq_scale_ind) * block_type 
                      + (1 + (CTI_scale_ind + CTI_sq_scale_ind) * block_type || subject),
                    data = regular_trials_lme) # still none converging :(


# try other random effect structure
model_RT2.4 <- lmer(formula = rt ~ (CTI_scale_ind + CTI_sq_scale_ind) * block_type 
                      + (1 | subject)
                      + (0 + CTI_scale_ind * block_type | subject)
                      + (0 + CTI_sq_scale_ind * block_type | subject),
                    data = regular_trials_lme) # still none converging :(

# even simpler random effect structure without the interaction term
model_RT2.5 <- lmer(formula = rt ~ (CTI_scale_ind + CTI_sq_scale_ind) * block_type 
                    + (1 | subject)
                    + (0 + CTI_scale_ind + CTI_sq_scale_ind + block_type | subject),
                    data = regular_trials_lme,
                    control = lmerControl(optimizer = "Nelder_Mead")) # still none converging :(

# adding incongruence effect as the IVs
model_RT3 <- lmer(formula = rt ~ (CTI_scale_ind + CTI_sq_scale_ind + incongruence_score_stim + incongruence_score_rule) * block_type
                    + (1 | subject),
                  data = regular_trials_lme)

anova(model_RT3)

# using the poly arguement in the lme
model_RT3.1 <- lmer(formula = rt ~ (poly(CTI_sec, 2) + incongruence_score_stim + incongruence_score_rule) * block_type
                  + (1 | subject),
                  data = regular_trials_lme)
anova(model_RT3.1)
summary(model_RT3.1, correlation = FALSE)

regular_trials$fittedRT_quad <- fitted(model_RT3.1)


# without the quadratic term -- which is the REAL final model
model_RT3.2 <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type
                    + (1 | subject),
                    data = regular_trials_lme)
anova(model_RT3.2)

summary(model_RT3.2, correlation= FALSE)
contrasts(regular_trials_lme$block_type)

regular_trials$fittedRT <- fitted(model_RT3.2)

regular_trials %>%
  group_by(block_type) %>%
  summarise(meanRT = mean(fittedRT))

# post-hoc test: the interaction between block type and CTI length
emtrends(model_RT3.2, ~ block_type, var = 'CTI_sec')

##-- the power analysis --##
library(simr)
fixef(model_RT3.2)["CTI_sec:block_type1"]

powerSim(model_RT3.2, test = fixed("CTI_sec:block_type", "lr"), nsim=300, seed = 123) # the power of the observed interaction result

model_weak_inter <- model_RT3.2
fixef(model_weak_inter)["CTI_sec:block_type1"] <- fixef(model_RT3.2)["CTI_sec:block_type1"] * 0.8

powerSim(model_weak_inter, test = fixed("CTI_sec:block_type", "lr"), nsim=300, seed = 123) # the power of the observed interaction result

model_weak_inter <- extend(model_weak_inter, within = "subject + block_type + CTI_sec", n = 3) # reduce from 4 each block type to 3 each block type
model_weak_inter <- extend(model_weak_inter, along = "subject", n = 50) # add subjects to maximum 50 ppl

pc_1 <- powerCurve(model_weak_inter, test = fixed("CTI_sec:block_type", "lr"), along = 'subject', breaks = c(30, 35, 40, 45, 50))
plot(pc_1)
print(pc_1)

# the model test the 3-way interaction between congruence effect * block type * CTI

model_RT3.3 <- lmer(formula = rt ~ CTI_sec * incongruence_score_stim * block_type 
                    + CTI_sec * incongruence_score_rule * block_type
                    + (1 | subject),
                    data = regular_trials_lme)
anova(model_RT3.3)
summary(model_RT3.3)
regular_trials_lme$fitted_3way <- fitted(model_RT3.3)

regular_trials_lme %>%
  convert_as_factor(incongruence_score_rule) %>%
  mutate(CTI_bins = as.numeric(CTI_bins)) %>%
  group_by(incongruence_score_rule, CTI_bins) %>%
  summarise(meanfitRT = mean(rt, na.rm = TRUE)) %>%
  ggplot(aes(x = incongruence_score_rule, y = meanfitRT, color = CTI_bins)) +
  geom_point(size = 4) +
  geom_line(aes(group = CTI_bins), size = 1) +
  theme_bw()

a <- as.numeric(regular_trials_lme$CTI_bins)

# the final model which include the quadratic term
model_RT4 <- lmer(formula = rt ~ poly(CTI_sec, 2) * block_type 
                    + (1 | subject)
                    + (0 + poly(CTI_sec, 2) + block_type | subject),
                  data = regular_trials_lme,
                  control = lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5))) # not converging

#### the mixed model on the transformed trials ####
tran_trials_lme <- transform_trials %>%
  mutate(CTI_sec = CTI/1000) %>%
  convert_as_factor(subject)

model_tranRT <- lmer(formula = rt ~ CTI_sec
                     + (1 | subject),
                     data = tran_trials_lme)
anova(model_tranRT)
summary(model_tranRT)

########## model comparison between the linear and quadratic models ###########

regular_trials_lme_RG <- regular_trials_lme %>%
  filter(block_type == "RG")

regular_trials_lme_TF <- regular_trials_lme %>%
  filter(block_type == "TF")

## model comparison for the RG block ##
model_RGRT_ln <- lmer(formula = rt ~ CTI_scale_ind 
                        + (1 | subject),
                      data = regular_trials_lme_RG)

model_RGRT_qd <- lmer(formula = rt ~ CTI_scale_ind + CTI_sq_scale_ind
                        + (1 | subject),
                      data = regular_trials_lme_RG)

AIC(model_RGRT_ln, model_RGRT_qd)
BIC(model_RGRT_ln, model_RGRT_qd)
anova(model_RGRT_ln, model_RGRT_qd)

## model comparison for the TF block ##
model_TFRT_ln <- lmer(formula = rt ~ CTI_scale_ind 
                      + (1 | subject),
                      data = regular_trials_lme_TF)

model_TFRT_qd <- lmer(formula = rt ~ CTI_scale_ind + CTI_sq_scale_ind
                      + (1 | subject),
                      data = regular_trials_lme_TF)

AIC(model_TFRT_ln, model_TFRT_qd)
BIC(model_TFRT_ln, model_TFRT_qd)
anova(model_TFRT_ln, model_TFRT_qd)

###########################################################
########### the Analysis on the Accuracy rate #############
###########################################################

bigdf_clean_error <- read_feather("bigdf_cleanSubTri_errorback.feather")
bigdf_clean_error <- bigdf_clean_error %>%
  filter(!is.na(rt)) # get rid of missing trials before analyzing accuracy

### step1. only include the regular trials for analysis ###

regular_trials_error <- bigdf_clean_error %>%
  filter(trial_nature == "RG") %>%
  select(rt, block_type, block_id, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins,
         incongruence_score_stim, incongruence_score_rule)

regular_trials_error_RGstart <- regular_trials_error %>%
  filter(start_block == "RG")

regular_trials_error_TFstart <- regular_trials_error %>%
  filter(start_block == "TF")

tran_trials_error <- bigdf_clean_error %>%
  filter(trial_nature == "TF") %>%
  select(rt, block_type, block_id, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins,
         incongruence_score_stim, incongruence_score_rule) %>%
  mutate(ACC_bi = if_else(accuracy == TRUE, 1, 0))

### step2. compare the subject level accuracy rate between regular and transform block

accRate_descrip <- regular_trials_error_TFstart %>%
  convert_as_factor(CTI_bins) %>%
  group_by(block_type, subject, CTI_bins) %>%
  summarise(count = n(),
            accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

accRate_descrip2 <- accRate_descrip %>%
  group_by(block_type, CTI_bins) %>%
  summarise(subaccRate = mean(accRate, na.rm = TRUE)) %>%
  ungroup()

(
  p <- ggplot(accRate_descrip, aes(x = CTI_bins, y = accRate, color = block_type)) +
    geom_point(size = 4, alpha = .05) +
    geom_point(data = accRate_descrip2, aes(y = subaccRate), 
               size = 5, position=position_dodge(0.2)) +
    geom_line(data = accRate_descrip2, aes(y = subaccRate, group = block_type), 
              size = 1, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(0.78, 0.9) +
    theme_bw()
)

### step3 run ANOVA model

# the ANOVA model
acc.aov <- anova_test(
  data = accRate_descrip,
  dv = accRate,
  wid = subject,
  within = c(block_type, CTI_bins)
)

get_anova_table(acc.aov)

### step 4. visualize the data w/o the bins ###

accRate_descrip4 <- regular_trials_error %>%
  group_by(block_type, CTI) %>%
  summarise(grandAccRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

(
  p <- ggplot(accRate_descrip4, aes(x = CTI, y = grandAccRate, color = block_type)) +
    geom_point(size = 5, position=position_dodge(0.2)) +
    geom_line(aes(group = block_type), 
              size = 1, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(0.77, 0.97) +
    theme_bw()
)

### the mixed effect logistic regression model on ACC ###

library(lmerTest)
library(lme4)
library(emmeans)

options(contrasts = c("contr.sum","contr.poly"))

regular_trials_error_lme <- regular_trials_error %>%
  mutate(CTI_sec = CTI/1000,
         ACC_bi = if_else(accuracy == TRUE, 1, 0)) %>%
  convert_as_factor(block_type, subject) %>%
  ungroup()

contrasts(regular_trials_error_lme$block_type)

model_acc <- glmer(formula = ACC_bi ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type 
                   + (1|subject),
                   data = regular_trials_error_lme,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

anova(model_acc)
summary(model_acc, correlation= FALSE)
emtrends(model_acc, ~ block_type, var = 'incongruence_score_rule')


### logistic regression on acc of transform trials ###

tran_trials_error_lme <- tran_trials_error %>%
  mutate(CTI_sec = CTI/1000)

model_tranAcc <- glmer(formula = ACC_bi ~ CTI_sec + (1|subject),
                       data = tran_trials_error_lme,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"))

summary(model_tranAcc)

###########################################################
############# Congruence effect on accRate ################
###########################################################

## visualize the congruence effect

## the stimulus congruence effect
acc_cgc_stim <- regular_trials_error %>%
  convert_as_factor(subject, block_type, incongruence_score_stim) %>%
  group_by(block_type, subject, incongruence_score_stim) %>%
  summarise(count = n(),
            accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

acc_cgc_stim2 <- acc_cgc_stim %>%
  group_by(block_type, incongruence_score_stim) %>%
  summarise(subaccRate = mean(accRate, na.rm = TRUE)) %>%
  ungroup()

(
  p <- ggplot(acc_cgc_stim, aes(x = incongruence_score_stim, y = accRate, color = block_type)) +
    geom_point(size = 2, position=position_dodge(0.2), alpha = .08) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position=position_dodge(0.2), alpha = .2) +
    geom_point(data = acc_cgc_stim2, aes(y = subaccRate), 
               size = 6, position=position_dodge(0.2)) +
    geom_line(data = acc_cgc_stim2, aes(y = subaccRate, group = block_type),
              size = 1.7, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(0.6, 1.0) +
    theme_bw()
)


acc_cgc_stim.aov <- anova_test(
  data = acc_cgc_stim,
  dv = accRate,
  wid = subject,
  within = c(block_type, incongruence_score_stim)
)

get_anova_table(acc_cgc_stim.aov)


# the rule congruence effect

acc_cgc_rule <- regular_trials_error %>%
  convert_as_factor(subject, block_type, incongruence_score_rule) %>%
  group_by(block_type, subject, incongruence_score_rule) %>%
  summarise(count = n(),
            accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

acc_cgc_rule2 <- acc_cgc_rule %>%
  group_by(block_type, incongruence_score_rule) %>%
  summarise(subaccRate = mean(accRate, na.rm = TRUE)) %>%
  ungroup()

(
  p <- ggplot(acc_cgc_rule, aes(x = incongruence_score_rule, y = accRate, color = block_type)) +
    geom_point(size = 2, position=position_dodge(0.2), alpha = .08) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position=position_dodge(0.2), alpha = .2) +
    geom_point(data = acc_cgc_rule2, aes(y = subaccRate), 
               size = 6, position=position_dodge(0.2)) +
    geom_line(data = acc_cgc_rule2, aes(y = subaccRate, group = block_type),
              size = 1.7, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(0.6, 1.0) +
    theme_bw()
)


acc_cgc_rule.aov <- anova_test(
  data = acc_cgc_rule,
  dv = accRate,
  wid = subject,
  within = c(block_type, incongruence_score_rule)
)

get_anova_table(acc_cgc_rule.aov)


################################################################################
############ post-hoc check on the accuracy on the tasks and stimulus ##########
################################################################################

bigdf_clean_error <- bigdf_clean_error %>%
  mutate(image_tar_id = str_extract(image_target, "(?<=id\":\").+(?=\",\"stim_type)"))

length(unique(bigdf_clean_error$image_tar_id)) # should be 24

acc_stim <- bigdf_clean_error %>%
  filter(trial_nature == "RG") %>%
  group_by(image_tar_id) %>%
  summarise(count = n(),
            accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

glimpse(acc_stim)

ggplot(acc_stim, aes(x = as.factor(image_tar_id), y = accRate)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim=c(0.5,1)) +
  coord_flip(ylim=c(0.5,1))
  

acc_place_y_s_w <-  bigdf_clean_error %>%
  filter(trial_nature == "RG",
         image_tar_id == "place_y_s_w") %>%
  group_by(rule) %>%
  summarise(count = n(),
            accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()  





