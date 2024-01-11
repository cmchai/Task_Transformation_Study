################# statistical analysis ######################
############ task transform paradigm : control ##############
setwd("D:/Ghent_Braem/Experiment/2nd_experiment/control_experiment/data")

library(tidyverse)
library(ez)
library(feather)
library(rstatix)
library(wesanderson)
library(ggsignif)
library(ggnewscale)
library(papaja)
library(rempsyc)
library(patchwork)

###########################################################
########### the Analysis on the Reaction time #############
###########################################################
bigdf_clean <- read_feather("bigdf_cleanSubTri.feather")
# save(bigdf_clean, file = "D:/Ghent_Braem/Experiment/2nd_experiment/control_experiment/data/bigdf_clean.Rdata")
glimpse(bigdf_clean)

### step1. only include the regular trials for analysis ###
regular_trials <- bigdf_clean %>%
  filter(trial_nature == "RG") %>%
  select(rt, block_type, block_id, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins, incongruence_score_stim, incongruence_score_rule)

regular_RGstart_trials <- regular_trials %>%
  filter(start_block == "RG")

regular_OTstart_trials <- regular_trials %>%
  filter(start_block == "OT")

############################################################################################
### step2. compare the subject level RT between regular and transform block and plotting ###
############################################################################################

rt_descrip <- regular_trials %>%
  convert_as_factor(CTI_bins, subject, block_type) %>%
  group_by(block_type, subject, start_block, CTI_bins) %>%
  summarise(count = n(),
            meanCTI = mean(CTI),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

n_sub <- length(unique(rt_descrip$subject))

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
    geom_point(size = 2.5, position = position_dodge(0.2)) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 0.8, position = position_dodge(0.2), alpha = .5) +
    scale_color_manual(values = c("#FC4E07","#4E84C4")) +
    theme_bw() +
    facet_wrap(~ subject, nrow = 7)
)

### average over all the subjects ###
rt_descrip2 <- rt_descrip %>%
  group_by(block_type, CTI_bins) %>%
  summarise(submeanCTI = mean(meanCTI),
            subRT = mean(meanRT, na.rm = TRUE),
            subsd = sd(meanRT, na.rm = TRUE),
            se = subsd/((n_sub)^.5)) %>%   # standard error
  ungroup()

group_vars(rt_descrip) # need to check the grouping variables after using "summarize" since it will drop one grouping variable 

(
  p <- ggplot(rt_descrip2, aes(x = CTI_bins, y = subRT, color = block_type)) +
    geom_point(size = 5, position=position_dodge(0.2)) +
    geom_line(aes(group = block_type), size = 1) +
    scale_color_manual(values = c("#FC4E07","#4E84C4")) +
    ylim(1050, 1250) +
    theme_bw()
)

#### plotting for publication
# plot the CTI bins as categorical variables with equal distance between bins
(
  p <- ggplot(rt_descrip2, aes(x = CTI_bins, y = subRT, group = block_type, color = block_type)) +
    geom_point(size = 5, position=position_dodge(0.2)) +
    geom_line(size = 1, position=position_dodge(0.2)) +
    geom_errorbar(aes(ymin = subRT - se, ymax = subRT + se),
                  width = .2,
                  position = position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#696969"),
                       name = "block type:",
                       breaks = c("RG", "OT"),
                       labels = c("Regular", "Omit")) +
    ylim(1050, 1250) +
    theme_apa(base_size = 14) +
    labs(x = "CTI bin", y = "Reaction time in ms")
)

# plot the CTI bins as continuous variables -- using the mean CTI as distance between bins
cti_mean <- regular_trials %>%
  convert_as_factor(CTI_bins) %>%
  group_by(CTI_bins) %>%
  summarise(meanCTI = mean(CTI)) %>%
  ungroup()

(
  p <- ggplot(rt_descrip2, aes(x = submeanCTI, y = subRT, group = block_type, color = block_type)) +
    geom_point(size = 5, position=position_dodge(0.2)) +
    geom_line(size = 1, position=position_dodge(0.2)) +
    geom_errorbar(aes(ymin = subRT - se, ymax = subRT + se),
                  width = .2,
                  position = position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#696969"),
                       name = "block type:",
                       breaks = c("RG", "OT"),
                       labels = c("Regular", "Omit")) +
    scale_x_continuous(breaks = cti_mean$meanCTI,
                       labels = seq(1,6)) +
    ylim(1050, 1250) +
    theme_apa(base_size = 14) +
    labs(x = "CTI bin", y = "Reaction time in ms")
)

# plotting both individual and group level rt pattern
(
  p <- ggplot(rt_descrip, aes(x = CTI_bins, y = meanRT, color = block_type)) +
    geom_point(size = 4, alpha = .06) +
    geom_point(data = rt_descrip2, aes(y = subRT), size = 5,
               position=position_dodge(0.2)) +
    geom_line(data = rt_descrip2, aes(y = subRT, group = block_type), size = 1) +
    scale_color_manual(values = c("#FC4E07","#4E84C4")) +
    ylim(1075, 1200) +
    theme_bw()
)

### plotting for poster
(
  p <- ggplot(rt_descrip, aes(x = CTI_bins, y = meanRT, color = block_type)) +
    geom_point(size = 4, alpha = .06) +
    geom_point(data = rt_descrip2, aes(y = subRT), size = 9,
               position=position_dodge(0.2)) +
    geom_line(data = rt_descrip2, aes(y = subRT, group = block_type), size = 1) +
    scale_color_manual(values = c("#FF0033", "#4E84C4")) +
    ylim(1075, 1225) +
    ylab("RT(ms)") +
    xlab("CTI bin") +
    theme_bw() +
    theme(axis.text = element_text(size = 20),
          axis.title=element_text(size=20))
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
rt_descrip3 <- regular_trials %>%
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
    geom_line(aes(group = block_type), size = 1)+
    geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
    scale_color_manual(values = c("#FC4E07","#4E84C4")) +
    ylim(1000, 1300) +
    theme_bw()
)

# add an smooth line for publication
(
  p2 <- ggplot(rt_descrip4, aes(x = CTI, y = subRT, color = block_type)) +
    geom_point(size = 5,position=position_dodge(0.2), alpha = .3) +
    geom_smooth(method = "loess", span = 0.75, size = 2, linetype="dashed") +
    geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
    scale_color_manual(values = c("#4E84C4", "#696969"),
                       name = "block type:",
                       breaks = c("RG", "OT"),
                       labels = c("Regular", "Omit")) +
    coord_cartesian(ylim=c(1050, 1300)) +
    theme_apa(base_size = 14) +
    labs(x = "CTI in ms", y = "Reaction time in ms") +
    ggtitle("Control Experiment")
)

# combine p1(from experiment v2) and p2(from control experiment)
p1 + p2 +
  plot_layout(nrow = 2, byrow = TRUE)

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

##########################################################
############# Congruence effect on RT ####################
##########################################################

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
options(scipen=999)

scale_this <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

regular_trials_lme <- regular_trials %>%
  mutate(CTI_sec = CTI/1000) %>%
  convert_as_factor(block_type, subject) %>%
  ungroup()

contrasts(regular_trials_lme$block_type)

### specify the model ----
### new approach ####
# maximal model #
regular_trials_lme <- regular_trials_lme %>%
  mutate(block_type_binary = if_else(block_type == "RG", 1, -1))

model_RT_max <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type_binary
                     + ((CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type_binary || subject),
                     data = regular_trials_lme,
                     REML = TRUE,
                     control = lmerControl(optimizer = "bobyqa",
                                           calc.derivs = FALSE,
                                           optCtrl = list(maxfun = 2e5)))

anova(model_RT_max)
summary(model_RT_max, correlation= FALSE)

# maximal model including quadratic term of CTI #
model_RT_max2 <- lmer(formula = rt ~ (poly(CTI_sec, 2) + incongruence_score_stim + incongruence_score_rule) * block_type_binary
                     + ((poly(CTI_sec, 2) + incongruence_score_stim + incongruence_score_rule) * block_type_binary || subject),
                     data = regular_trials_lme,
                     REML = TRUE,
                     control = lmerControl(optimizer = "bobyqa",
                                           calc.derivs = FALSE,
                                           optCtrl = list(maxfun = 2e5)))

anova(model_RT_max2)
summary(model_RT_max2, correlation= FALSE)

# only model CTI
model_RT_cti <- lmer(formula = rt ~ CTI_sec * block_type
                    + (CTI_sec * block_type | subject),
                    data = regular_trials_lme,
                    control = lmerControl(optimizer = "bobyqa",
                                          calc.derivs = FALSE,
                                          optCtrl = list(maxfun = 2e5)))

anova(model_RT_cti)
summary(model_RT_cti, correlation= FALSE)

### old approach ####
# only the linear term of CTI
model_RT <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type
                    + (1 | subject),
                    data = regular_trials_lme)

model_logRT <- lmer(formula = log(rt) ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type
                 + (1 | subject),
                 data = regular_trials_lme)

anova(model_logRT)
summary(model_RT, correlation= FALSE)

regular_trials$fittedRT <- fitted(model_RT)

regular_trials %>%
  group_by(block_type) %>%
  summarise(meanRT = mean(fittedRT))

# for reporting #
stats.table <- as.data.frame(summary(model_RT_max)$coefficients)
names(stats.table) <- c("estimate", "SE", "df", "t value", "p")

stats.table2 <- as.data.frame(matrix(ncol = 4, nrow = 8))
colnames(stats.table2) <- c('Coefficient', 'Estimate', 'SE', 'p value')

stats.table2$Coefficient <- c("(Intercept)",
                              "CTI",
                              "stimulus type incongruency",
                              "task rule incongruency",
                              "block type",
                              "CTI:block type",
                              "stimulus type incongruency:block type",
                              "task rule incongruency:block type")
stats.table2$Estimate <- round(stats.table$estimate, digit= 3)
stats.table2$SE <- round(stats.table$SE, digit= 3)
stats.table2$"p value" <- round(stats.table$p, digit= 4)

fun_round <- function(x) {formatC(x, format = "f", digits = 3)}

tableRT_ready <- nice_table(stats.table2, 
                          col.format.p = 4, 
                          col.format.custom = 2:3,
                          format.custom = "fun_round",
                          title = "Table \nThe result of mixed effect regression model on reaction time in control experiment", 
                          footnote = "SE = standard error.\n* denotes p < .05,
                          ** denotes p < .01, *** denates p < .001")
tableRT_ready
save_as_docx(table_ready, path = paste0(getwd(), "/nice_tablehere.docx"))

# both linear and quadratic term of CTI
model_RT_2 <- lmer(formula = rt ~ (poly(CTI_sec,2) + incongruence_score_stim + incongruence_score_rule) * block_type
                 + (1 | subject),
                 data = regular_trials_lme)

summary(model_RT_2, correlation= FALSE)

regular_trials$fittedRT_quad <- fitted(model_RT_2)

# post-hoc test: the interaction between block type and CTI length
emtrends(model_RT, ~ block_type, var = 'CTI_sec')

# In response to reviewer's comment: trying using glmer to better fit the data using Gamma or inverse Gaussian distribution
model_RT_gamma <- glmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type
                        + (1 | subject),
                        data = regular_trials_lme,
                        family = Gamma(link ="identity"),
                        control = glmerControl(optimizer = "nloptwrap",
                                               calc.derivs = FALSE,
                                               optCtrl = list(maxfun = 2e5)))

summary(model_RT_gamma, correlation= FALSE)

model_RT_invgau <- glmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type
                         + (1 | subject),
                         data = regular_trials_lme,
                         family = inverse.gaussian(link ="identity"),
                         control = glmerControl(optimizer = "nloptwrap",
                                                calc.derivs = FALSE,
                                                optCtrl = list(maxfun = 2e5)))

summary(model_RT_invgau, correlation= FALSE)

# Model selection between parsimonious and maximal model or other models
anova(model_rt_max, model_RT)
anova(model_RT_gamma, model_RT_invgau, model_RT)





###########################################################
########### the Analysis on the Accuracy rate #############
###########################################################

bigdf_clean_error <- read_feather("bigdf_cleanSubTri_errorback.feather")
# save(bigdf_clean_error, file = "D:/Ghent_Braem/Experiment/2nd_experiment/control_experiment/data/bigdf_clean_error.Rdata")
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

regular_trials_error_OTstart <- regular_trials_error %>%
  filter(start_block == "OT")

### step2. compare the subject level accuracy rate between regular and transform block ###

accRate_descrip <- regular_trials_error %>%
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
    scale_color_manual(values = c("#FC4E07", "#4E84C4")) +
    ylim(0.75, 0.95) +
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
    scale_color_manual(values = c("#FC4E07", "#4E84C4")) +
    ylim(0.77, 0.95) +
    theme_bw()
)

### the mixed effect logistic regression model on ACC ###

library(lmerTest)
library(lme4)
library(emmeans)

options(contrasts = c("contr.sum","contr.poly"))
options(scipen = 999)

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

# maximal model
model_acc_max <- glmer(formula = ACC_bi ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type 
                       + ((CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type || subject),
                       data = regular_trials_error_lme,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              calc.derivs = FALSE,
                                              optCtrl = list(maxfun = 2e5)))
summary(model_acc_max, correlation= FALSE)

# for reporting #
statsACC.table <- as.data.frame(summary(model_acc_max)$coefficients)
names(statsACC.table) <- c("estimate", "SE", "z value", "p")

statsACC.table2 <- as.data.frame(matrix(ncol = 4, nrow = 8))
colnames(statsACC.table2) <- c('Coefficient', 'Estimate', 'SE', 'p value')

statsACC.table2$Coefficient <- c("(Intercept)",
                                 "CTI",
                                 "stimulus type incongruency",
                                 "task rule incongruency",
                                 "block type",
                                 "CTI:block type",
                                 "stimulus type incongruency:block type",
                                 "task rule incongruency:block type")
statsACC.table2$Estimate <- round(statsACC.table$estimate, digit= 3)
statsACC.table2$SE <- round(statsACC.table$SE, digit= 3)
statsACC.table2$"p value" <- round(statsACC.table$p, digit= 4)

fun_round <- function(x) {formatC(x, format = "f", digits = 3)}

tableACC_ready <- nice_table(statsACC.table2, 
                             col.format.p = 4, 
                             col.format.custom = 2:3,
                             format.custom = "fun_round",
                             title = "Table \nThe result of mixed effect regression model on accuray in experiment 1", 
                             footnote = "SE = standard error.\n* denotes p < .05,
                          ** denotes p < .01, *** denates p < .001")
tableACC_ready
save_as_docx(table_ready, path = paste0(getwd(), "/nice_tablehere.docx"))

# merging RT and ACC data #
colnames(stats.table2) <- c('Coefficient', 'RT.Estimate', 'RT.SE', 'RT.p value')
colnames(statsACC.table2) <- c('Coefficient', 'ACC.Estimate', 'ACC.SE', 'ACC.p value')

statsALL.table <- merge(x=stats.table2, y=statsACC.table2,
                        by='Coefficient', all.x = TRUE)

tableALL_ready <- nice_table(statsALL.table, 
                             col.format.p = c(4,7), 
                             col.format.custom = c(2:3,5:6),
                             format.custom = "fun_round",
                             separate.header = TRUE,
                             italics = seq(statsALL.table),
                             title = "Table 2 \nThe result of mixed effect regression model on RT and accuracy in control experiment", 
                             footnote = "RT = reaction time, ACC = accuracy, SE = standard error.\n* denotes p < .05, ** denotes p < .01, *** denotes p < .001")

tableALL_ready
save_as_docx(tableALL_ready, path = paste0(getwd(), "/APA_table_control_exp_sm.docx"))

###########################################################
############# Congruence effect on accRate ################
###########################################################

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
    ylim(0.64, 1.0) +
    theme_bw()
)


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





