################# statistical analysis ####################
######### task transform paradigm: stim transform #########
library(here)
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
# load(file = paste0("D:/Ghent_Braem/Experiment/2nd_experiment/Experiment_stim_tran", "/result/bigdf_clean.Rdata"))
load(file = paste0(here(), "/result/bigdf_clean.Rdata"))
glimpse(bigdf_clean)
length(unique(bigdf_clean$subject))
n_sub <- length(unique(bigdf_clean$subject))

### step1. only include the regular trials for analysis ###
regular_trials <- bigdf_clean %>%
  filter(trial_nature == "RG") %>%
  select(rt, block_type, block_id, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins, incongruence_score_stim, incongruence_score_rule) %>%
  mutate(incongruence_score = incongruence_score_stim + incongruence_score_rule)

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

rt_descrip <- regular_trials %>%
  convert_as_factor(CTI_bins, subject, block_type) %>%
  group_by(block_type, subject, CTI_bins) %>%
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
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    theme_bw() +
    facet_wrap(~ subject, nrow = 7)
)

### average over all the subjects ###
rt_descrip2 <- rt_descrip %>%
  group_by(block_type, CTI_bins) %>%
  summarise(submeanCTI = mean(meanCTI),
            subRT = mean(meanRT, na.rm = TRUE),
            subsd = sd(meanRT, na.rm = TRUE),
            se = subsd/((n_sub)^.5)) %>%
  ungroup()

group_vars(rt_descrip) # need to check the grouping variables after using "summarize" since it will drop one grouping variable 

(
  p <- ggplot(rt_descrip2, aes(x = CTI_bins, y = subRT, color = block_type)) +
    geom_point(size = 5, position=position_dodge(0.2)) +
    geom_line(aes(group = block_type), size = 1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
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
    scale_color_manual(values = c("#4E84C4", "#FC4E07"),
                       name = "block type:",
                       breaks = c("RG", "TF"),
                       labels = c("Regular", "Transform")) +
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
    geom_point(size = 5) +
    geom_line(size = 1) +
    geom_errorbar(aes(ymin = subRT - se, ymax = subRT + se),
                  width = .2) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07"),
                       name = "block type:",
                       breaks = c("RG", "TF"),
                       labels = c("Regular", "Transform")) +
    scale_x_continuous(breaks = cti_mean$meanCTI,
                       labels = seq(1,6)) +
    ylim(1048, 1250) +
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
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1050, 1250) +
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
    geom_line(aes(group = block_type), size = 1) +
    geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1000, 1300) +
    theme_bw()
)

# add an smooth line for publication
(
  p2 <- ggplot(rt_descrip4, aes(x = CTI, y = subRT, color = block_type)) +
    geom_point(size = 5,position=position_dodge(0.2), alpha = .3) +
    geom_smooth(method = "loess", span = 0.75, size = 2, linetype="dashed") +
    geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07"),
                       name = "block type:",
                       breaks = c("RG", "TF"),
                       labels = c("Regular", "Transform")) +
    coord_cartesian(ylim=c(1000, 1300)) +
    theme_apa(base_size = 14) +
    labs(x = "CTI in ms", y = "Reaction time in ms") +
    ggtitle("Experiment 4")
)

# combine p1(from experiment 3) and p2(from experiment 4)
p1 + p2 +
  plot_layout(nrow = 2, byrow = TRUE)

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
    ylim(1000, 1250) +
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


###########################################################
############# Congruency effect on RT #####################
###########################################################

## visualize the congruence effect

# the overall congruency effect ----
rt_cgc <- regular_trials %>%
  convert_as_factor(subject, block_type, incongruence_score) %>%
  group_by(subject, block_type, incongruence_score) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()

rt_cgc2 <- regular_trials %>%
  group_by(block_type, incongruence_score) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()  


(
  p <- ggplot(regular_trials, aes(x = as.factor(incongruence_score), y = rt, color = block_type)) +
    geom_point(size = 0.4, position = "jitter") +
    geom_point(data = rt_cgc, aes(y = meanRT), size = 3,
               position=position_dodge(0.2)) +
    geom_line(data = rt_cgc, aes(y = meanRT, group = interaction(subject, block_type)), 
              size = 1, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    theme_bw()
)


(
  p <- ggplot(regular_trials, aes(x = as.factor(incongruence_score), y = rt, color = block_type)) +
    geom_point(size = 0.4, position = "jitter") +
    geom_point(data = rt_cgc, aes(y = meanRT), size = 5,
               position=position_dodge(0.2)) +
    geom_line(data = rt_cgc_stim2, aes(y = meanRT, group = block_type), 
              size = 1, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1000, 1250) +
    theme_bw()
)

(
  p <- ggplot(rt_cgc, aes(x = as.factor(incongruence_score), y = meanRT, color = block_type)) +
    geom_point(size = 4, position = position_dodge(0.2), alpha = .08) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position = position_dodge(0.2), alpha = .2) +
    geom_point(data = rt_cgc2, aes(y = meanRT), size = 7,
               position=position_dodge(0.2)) +
    geom_line(data = rt_cgc2, aes(y = meanRT, group = block_type), 
              size = 1.6, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1000, 1250) +
    theme_bw()
)

# the stimulus incongruency effect ----
rt_cgc_stim <- regular_trials %>%
  convert_as_factor(subject, block_type, incongruence_score_stim) %>%
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
    ylim(1000, 1250) +
    theme_bw()
)

# plotting for publication
rt_cgc_stim3 <- rt_cgc_stim %>%
  group_by(block_type, incongruence_score_stim) %>%
  summarise(subRT = mean(meanRT, na.rm = TRUE),
            subRTsd = sd(meanRT, na.rm = TRUE),
            se = subRTsd/((n_sub)^.5)) %>%
  ungroup()

(
  p3 <- ggplot(rt_cgc_stim3, aes(x = incongruence_score_stim, y = subRT, group = block_type, color = block_type)) +
    geom_point(size = 6, position=position_dodge(0.1)) +
    geom_line(size = 1.4, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = subRT - se, ymax = subRT + se),
                  width = .4,
                  position = position_dodge(0.1)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07"),
                       name = "block type:",
                       breaks = c("RG", "TF"),
                       labels = c("Regular", "Transform")) +
    coord_cartesian(ylim=c(1000, 1250)) +
    theme_apa(base_size = 14) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    labs(x = "stimulus type incongruency", y = "reaction time in ms")
)

cgc_stim.aov <- anova_test(
  data = rt_cgc_stim,
  dv = meanRT,
  wid = subject,
  within = c(block_type, incongruence_score_stim)
)

get_anova_table(cgc_stim.aov)


# the rule congruence effect ----
rt_cgc_rule <- regular_trials %>%
  convert_as_factor(subject, block_type, incongruence_score_rule) %>%
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

# plotting for publication
rt_cgc_rule3 <- rt_cgc_rule %>%
  group_by(block_type, incongruence_score_rule) %>%
  summarise(subRT = mean(meanRT, na.rm = TRUE),
            subRTsd = sd(meanRT, na.rm = TRUE),
            se = subRTsd/((n_sub)^.5)) %>%
  ungroup()

(
  p1 <- ggplot(rt_cgc_rule3, aes(x = incongruence_score_rule, y = subRT, group = block_type, color = block_type)) +
    geom_point(size = 5, position=position_dodge(0.1)) +
    geom_line(size = 1.4, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = subRT - se, ymax = subRT + se),
                  width = .4,
                  position = position_dodge(0.1)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07"),
                       name = "block type:",
                       breaks = c("RG", "TF"),
                       labels = c("Regular", "Transform")) +
    coord_cartesian(ylim=c(1000, 1250)) +
    theme_apa(base_size = 14) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    labs(x = "task rule incongruency", y = "reaction time in ms")
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

glimpse(regular_trials_lme)

contrasts(regular_trials_lme$block_type)

### specify the model ###
model_RT <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type
                    + (1 | subject),
                    data = regular_trials_lme)

anova(model_RT)
summary(model_RT, correlation= FALSE)

regular_trials$fittedRT <- fitted(model_RT)

regular_trials %>%
  group_by(block_type) %>%
  summarise(meanRT = mean(fittedRT))

# maximal model
model_rt_max <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type
                     + ((CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type | subject),
                     data = regular_trials_lme,
                     REML = TRUE,
                     control = lmerControl(optimizer = "nloptwrap",
                                           calc.derivs = FALSE,
                                           optCtrl = list(maxfun = 2e5)))

anova(model_rt_max)
summary(model_rt_max)

# for reporting #
stats.table <- as.data.frame(summary(model_rt_max)$coefficients)
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
                            title = "Table \nThe result of mixed effect regression model on reaction time in experiment 3", 
                            footnote = "SE = standard error.\n* denotes p < .05,
                            ** denotes p < .01, *** denates p < .001")
tableRT_ready
save_as_docx(table_ready, path = paste0(getwd(), "/nice_tablehere.docx"))

# post-hoc test: the interaction between block type and CTI length
emtrends(model_RT, ~ block_type, var = 'CTI_sec')

### the model test the 3-way interaction between congruence effect * block type * CTI  ###

model_RT3.3 <- lmer(formula = rt ~ (incongruence_score_stim + incongruence_score_rule) * CTI_sec * block_type
                    + (1 | subject),
                    data = regular_trials_lme)
anova(model_RT3.3)
summary(model_RT3.3, correlation= FALSE)
regular_trials_lme$fitted_3way <- fitted(model_RT3.3)

# post-hoc test: the interaction between block type and incongruency scores
emtrends(model_RT3.3, ~ block_type, var = 'incongruence_score_stim')
emtrends(model_RT3.3, ~ block_type, var = 'incongruence_score_rule')


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

#### the mixed model on the transformed trials ####
tran_trials_lme <- transform_trials %>%
  mutate(CTI_sec = CTI/1000) %>%
  convert_as_factor(subject)

model_tranRT <- lmer(formula = rt ~ CTI_sec
                     + (1 | subject),
                     data = tran_trials_lme)
anova(model_tranRT)
summary(model_tranRT)

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
# load(file = paste0("D:/Ghent_Braem/Experiment/2nd_experiment/Experiment_stim_tran", "/result/bigdf_clean_error.Rdata"))
load(file = paste0(here(), "/result/bigdf_clean_error.Rdata"))
bigdf_clean_error <- bigdf_clean_error %>%
  filter(!is.na(rt)) # get rid of missing trials before analyzing accuracy

### step1. only include the regular trials for analysis ###
regular_trials_error <- bigdf_clean_error %>%
  filter(trial_nature == "RG") %>%
  select(rt, block_type, block_id, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins,
         incongruence_score_stim, incongruence_score_rule) %>%
  mutate(incongruence_score = incongruence_score_stim + incongruence_score_rule)

regular_trials_error_RGstart <- regular_trials_error %>%
  filter(start_block == "RG")

regular_trials_error_TFstart <- regular_trials_error %>%
  filter(start_block == "TF")

transform_trials_error <- bigdf_clean_error %>%
  filter(trial_nature == "TF") %>%
  select(rt, block_type, block_id, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins,
         incongruence_score_stim, incongruence_score_rule) %>%
  mutate(ACC_bi = if_else(accuracy == TRUE, 1, 0))

### step2. compare the subject level accuracy rate between regular and transform block

accRate_descrip <- regular_trials_error %>%
  convert_as_factor(CTI_bins) %>%
  group_by(block_type, subject, CTI_bins) %>%
  summarise(count = n(),
            accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

accRate_descrip2 <- accRate_descrip %>%
  group_by(block_type, CTI_bins) %>%
  summarise(subaccRate = mean(accRate, na.rm = TRUE),
            subsd = sd(accRate, na.rm = TRUE),
            se = subsd/((n_sub)^.5)) %>%
  ungroup()

(
  p <- ggplot(accRate_descrip, aes(x = CTI_bins, y = accRate, color = block_type)) +
    geom_point(size = 4, alpha = .05) +
    geom_point(data = accRate_descrip2, aes(y = subaccRate), 
               size = 5, position=position_dodge(0.2)) +
    geom_line(data = accRate_descrip2, aes(y = subaccRate, group = block_type), 
              size = 1, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(0.80, 0.90) +
    theme_bw()
)

### step 3. visualize the data w/o the bins ###
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
    ylim(0.75, 0.99) +
    theme_bw()
)

### the mixed effect logistic regression model on ACC ###

library(lmerTest)
library(lme4)
library(emmeans)

options(contrasts = c("contr.sum","contr.poly"))
options(scipen=999)

regular_trials_error_lme <- regular_trials_error %>%
  mutate(CTI_sec = CTI/1000,
         ACC_bi = if_else(accuracy == TRUE, 1, 0)) %>%
  convert_as_factor(block_type, subject, incongruence_score_stim, incongruence_score_rule) %>% # add incongruency score as factors when computing contrast of diff level of incongruency
  ungroup()

contrasts(regular_trials_error_lme$block_type)
contrasts(regular_trials_error_lme$incongruence_score_stim) # only if incongruency score is a factor instead of numerical value
contrasts(regular_trials_error_lme$incongruence_score_rule)

model_acc <- glmer(formula = ACC_bi ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type 
                   + (1|subject),
                   data = regular_trials_error_lme,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

anova(model_acc)
summary(model_acc, correlation= FALSE)

# try the maximal model
model_acc_max <- glmer(formula = ACC_bi ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type 
                       + ((CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type | subject),
                       data = regular_trials_error_lme,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              calc.derivs = FALSE,
                                              optCtrl = list(maxfun = 2e5)))

anova(model_acc_max)
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
                             title = "Table \nThe result of mixed effect regression model on accuracy in experiment 4", 
                             footnote = "SE = standard error.\n* denotes p < .05, ** denotes p < .01, *** denates p < .001")
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
                             title = "Table 4 \nThe result of mixed effect regression model on RT and accuray in experiment 4", 
                             footnote = "RT = reaction time, ACC = accuracy, SE = standard error.\n* denotes p < .05, ** denotes p < .01, *** denotes p < .001")

tableALL_ready
save_as_docx(tableALL_ready, path = paste0(getwd(), "/APA_table_stim_tran_sm.docx"))

# post-hoc test: the interaction between block type and incongruency scores
emtrends(model_acc, ~ block_type, var = 'incongruence_score_stim')
test(emtrends(model_acc, ~ block_type, var = 'incongruence_score_stim'))
contrast(emtrends(model_acc, ~ block_type, var = 'incongruence_score_stim'), 'pairwise', by=NULL)

emns1 <- emmeans(model_acc, pairwise ~ block_type|incongruence_score_stim, type = "response") # to make this pairwise comparison works, the incongruency score must be converted to factor before the modelling
emns1

emns2 <- emmeans(model_acc, pairwise ~ incongruence_score_stim|block_type, type = "response")
emns2

emmeans(model_acc, ~ block_type, type = "response")
test(emmeans(model_acc, ~ block_type, type = "response"))
# 3-way interaction #
model_acc2 <- glmer(formula = ACC_bi ~ (incongruence_score_stim + incongruence_score_rule) * CTI_sec * block_type
                    + (1 | subject),
                   data = regular_trials_error_lme,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))
anova(model_acc2)
summary(model_acc2, correlation= FALSE)

regular_trials_error_lme %>%
  convert_as_factor(incongruence_score_rule) %>%
  mutate(CTI_bins = as.numeric(CTI_bins)) %>%
  group_by(incongruence_score_rule, CTI_bins) %>%
  summarise(meanAcc = sum(accuracy)/length(accuracy)) %>%
  ggplot(aes(x = incongruence_score_rule, y = meanAcc, color = CTI_bins)) +
  geom_point(size = 4) +
  geom_line(aes(group = CTI_bins), size = 1) +
  theme_bw()


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

# the overall congruence effect ----
acc_cgc <- regular_trials_error %>%
  convert_as_factor(subject, block_type, incongruence_score) %>%
  group_by(block_type, subject, incongruence_score) %>%
  summarise(count = n(),
            accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

acc_cgc2 <- acc_cgc %>%
  group_by(block_type, incongruence_score) %>%
  summarise(subaccRate = mean(accRate, na.rm = TRUE)) %>%
  ungroup()

(
  p <- ggplot(acc_cgc, aes(x = as.factor(incongruence_score), y = accRate, color = block_type)) +
    geom_point(size = 2, position=position_dodge(0.2), alpha = .08) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position=position_dodge(0.2), alpha = .2) +
    geom_point(data = acc_cgc2, aes(y = subaccRate), 
               size = 6, position=position_dodge(0.2)) +
    geom_line(data = acc_cgc2, aes(y = subaccRate, group = block_type),
              size = 1.7, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(0.75, 0.95) +
    theme_bw()
)

# the stimulus congruence effect ----
acc_cgc_stim <- regular_trials_error %>%
  convert_as_factor(subject, block_type, incongruence_score_stim) %>%
  group_by(block_type, subject, incongruence_score_stim) %>%
  summarise(count = n(),
            accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

acc_cgc_stim2 <- acc_cgc_stim %>%
  group_by(block_type, incongruence_score_stim) %>%
  summarise(subaccRate = mean(accRate, na.rm = TRUE),
            subaccsd = sd(accRate, na.rm = TRUE),
            se = subaccsd/((n_sub)^.5)) %>%
  ungroup()

(
  p <- ggplot(acc_cgc_stim, aes(x = incongruence_score_stim, y = accRate, color = block_type)) +
    geom_point(size = 2, position=position_dodge(0.2), alpha = .05) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position=position_dodge(0.2), alpha = .15) +
    geom_point(data = acc_cgc_stim2, aes(y = subaccRate), 
               size = 9, position=position_dodge(0.2)) +
    geom_line(data = acc_cgc_stim2, aes(y = subaccRate, group = block_type),
              size = 1.7, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FF0033")) +
    ylim(0.80, 0.92) +
    ylab("ACC") +
    xlab("incongruency score") +
    theme_bw() +
    theme(axis.text = element_text(size = 20),
          axis.title=element_text(size=20))
)

#### plotting for publication
(
  p4 <- ggplot(acc_cgc_stim2, aes(x = incongruence_score_stim, y = subaccRate, group = block_type, color = block_type)) +
    geom_point(size = 6, position=position_dodge(0.1)) +
    geom_line(size = 1.4, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = subaccRate - se, ymax = subaccRate + se),
                  width = .4,
                  position = position_dodge(0.1)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07"),
                       name = "block type:",
                       breaks = c("RG", "TF"),
                       labels = c("Regular", "Transform")) +
    ylim(0.78, 0.95) +
    theme_apa(base_size = 14) +
    theme(legend.position = c(.8, .9), plot.title = element_text(hjust = 0.5)) +
    labs(x = "stimulus type incongruency", y = "ACC")
)

# produce 2 panel plots for congruency effects both Exp3 and Exp4

p1 + p2 + p3 + p4 + 
  plot_layout(nrow = 2, byrow = TRUE, guides = "collect") + 
  plot_annotation(tag_levels = 'A')

# the plotting for the poster

(
  p <- ggplot(acc_cgc_stim, aes(x = incongruence_score_stim, y = accRate, color = block_type)) +
    geom_point(size = 2, position=position_dodge(0.2), alpha = .05) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position=position_dodge(0.2), alpha = .15) +
    geom_point(data = acc_cgc_stim2, aes(y = subaccRate), 
               size = 9) +
    geom_line(data = acc_cgc_stim2, aes(y = subaccRate, group = block_type),
              size = 1.7) +
    scale_color_manual(values = c("#4E84C4", "#FF0033")) +
    ylim(0.80, 0.92) +
    ylab("ACC") +
    xlab("incongruency score") +
    theme_bw() +
    theme(axis.text = element_text(size = 20),
          axis.title=element_text(size=20))
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
  summarise(subaccRate = mean(accRate, na.rm = TRUE),
            subaccsd = sd(accRate, na.rm = TRUE),
            se = subaccsd/((n_sub)^.5)) %>%
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
    ylim(0.75, 0.95) +
    theme_bw()
)

#### plotting for publication
(
  p2 <- ggplot(acc_cgc_rule2, aes(x = incongruence_score_rule, y = subaccRate, group = block_type, color = block_type)) +
    geom_point(size = 5, position=position_dodge(0.1)) +
    geom_line(size = 1.4, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = subaccRate - se, ymax = subaccRate + se),
                  width = .4,
                  position = position_dodge(0.1)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07"),
                       name = "block type:",
                       breaks = c("RG", "TF"),
                       labels = c("Regular", "Transform")) +
    ylim(0.78, 0.95) +
    theme_apa(base_size = 14) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    labs(x = "task rule incongruency", y = "ACC")
)

acc_cgc_rule.aov <- anova_test(
  data = acc_cgc_rule,
  dv = accRate,
  wid = subject,
  within = c(block_type, incongruence_score_rule)
)

get_anova_table(acc_cgc_rule.aov)

## group plot for publication
p1 + p2 + p3 + p4 + 
  plot_layout(nrow = 2, byrow = TRUE, guides = "collect") + 
  plot_annotation(tag_levels = 'A')

#################################################################################
############# Congruency effect on IES(inverse efficiency score) ################
#################################################################################

######### compute and visualize the IES of stim congruency effect ###########
ies_cgc_stim <- left_join(rt_cgc_stim, acc_cgc_stim, by = c("subject", "block_type", "incongruence_score_stim")) %>%
  mutate(ies = meanRT / accRate) # compute the IES score

ies_cgc_stim2 <- ies_cgc_stim %>%
  group_by(block_type, incongruence_score_stim) %>%
  summarise(sub_ies = mean(ies, na.rm = TRUE)) %>%
  ungroup()

(
  p <- ggplot(ies_cgc_stim, aes(x = incongruence_score_stim, y = ies, color = block_type)) +
    geom_point(size = 2, position=position_dodge(0.2), alpha = .08) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position=position_dodge(0.2), alpha = .2) +
    geom_point(data = ies_cgc_stim2, aes(y = sub_ies), 
               size = 6, position=position_dodge(0.2)) +
    geom_line(data = ies_cgc_stim2, aes(y = sub_ies, group = block_type),
              size = 1.7, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1000, 1500) +
    theme_bw()
)

### the anova test
ies_cgc_stim.aov <- anova_test(
  data = ies_cgc_stim,
  dv = ies,
  wid = subject,
  within = c(block_type, incongruence_score_stim)
)

get_anova_table(ies_cgc_stim.aov)


######### compute and visualize the IES of rule congruency effect ###########
ies_cgc_rule <- left_join(rt_cgc_rule, acc_cgc_rule, by = c("subject", "block_type", "incongruence_score_rule")) %>%
  mutate(ies = meanRT / accRate) # compute the IES score

ies_cgc_rule2 <- ies_cgc_rule %>%
  group_by(block_type, incongruence_score_rule) %>%
  summarise(sub_ies = mean(ies, na.rm = TRUE)) %>%
  ungroup()

(
  p <- ggplot(ies_cgc_rule, aes(x = incongruence_score_rule, y = ies, color = block_type)) +
    geom_point(size = 2, position=position_dodge(0.2), alpha = .08) +
    geom_line(aes(group = interaction(subject, block_type)), 
              size = 1, position=position_dodge(0.2), alpha = .2) +
    geom_point(data = ies_cgc_rule2, aes(y = sub_ies), 
               size = 6, position=position_dodge(0.2)) +
    geom_line(data = ies_cgc_rule2, aes(y = sub_ies, group = block_type),
              size = 1.7, position=position_dodge(0.2)) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1000, 1500) +
    theme_bw()
)

### the anova test
ies_cgc_rule.aov <- anova_test(
  data = ies_cgc_rule,
  dv = ies,
  wid = subject,
  within = c(block_type, incongruence_score_rule)
)

get_anova_table(ies_cgc_rule.aov)

### the comparison between the rule congruency and the stim congruency effect ###
ies_cgc_stim_comb <- ies_cgc_stim %>%
  rename(incongruence_score = incongruence_score_stim) %>%
  select(!c(meanRT:accRate)) %>%
  mutate(dim = "stim")

ies_cgc_rule_comb <- ies_cgc_rule %>%
  rename(incongruence_score = incongruence_score_rule) %>%
  select(!c(meanRT:accRate)) %>%
  mutate(dim = "rule")

ies_cgc_comb <- bind_rows(ies_cgc_stim_comb, ies_cgc_rule_comb)

ies_cgc_comb.aov <- anova_test(
  data = ies_cgc_comb,
  dv = ies,
  wid = subject,
  within = c(block_type, incongruence_score, dim)
)

get_anova_table(ies_cgc_comb.aov)

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


###########################################################
########### the Analysis on the transform trials ##########
###########################################################

## subject level average reaction time ##
transform_trials <- bigdf_clean %>%
  filter(trial_nature == "TF") %>%
  select(rt, block_type, block_id, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins, incongruence_score_stim, incongruence_score_rule) %>%
  group_by(subject) %>%
  summarise(count = n(),
            meanRT = mean(rt)) %>%
  ungroup()

## subject level average accuracy rate ##

transform_trials_error <- bigdf_clean_error %>%
  filter(trial_nature == "TF") %>%
  select(rt, block_type, block_id, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins,
         incongruence_score_stim, incongruence_score_rule) %>%
  group_by(subject) %>%
  summarise(count = n(),
            accRate = sum(accuracy)/length(accuracy))
  
stim_tran_tran_perf <- transform_trials %>%
  select(-count) %>%
  left_join(transform_trials_error, by = "subject") %>%
  select(-count)

save(stim_tran_tran_perf, file = paste0(here(), "/result/stimTranTranPerf.Rdata"))

################ between-subject analysis ################ ----

#### the association between CIT*block type effect and the performance on transform trials----
## stronger the interaction(more negative), the better people should perform the transform tasks(shorter or more accurate)

model_estRT_sub <- coef(model_rt_max)$subject %>% # the CTI:block type effect in the RT model
  rownames_to_column("subject") %>%
  select(c(1,7))

model_estACC_sub <- coef(model_acc_max)$subject %>%
  rownames_to_column("subject") %>%
  select(c(1,8))

tran_rt_sub <- transform_trials %>%
  group_by(subject) %>%
  summarise(meanRT = mean(rt)) %>%
  ungroup()

tran_acc_sub <- transform_trials_error %>%
  group_by(subject) %>%
  summarise(accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

tran_sub <- purrr::reduce(list(model_estRT_sub, model_estACC_sub, tran_rt_sub, tran_acc_sub), dplyr::left_join, by = "subject")

cor_model_rt <- cor.test(tran_sub$`CTI_sec:block_type1`, tran_sub$meanRT, method = "pearson")
cor_model_rt

cor_model_acc <- cor.test(tran_sub$`CTI_sec:block_type1`, tran_sub$accRate, method = "pearson")
cor_model_acc

cor_model_rt2 <- cor.test(tran_sub$`incongruence_score_stim:block_type1`, tran_sub$meanRT, method = "pearson")
cor_model_rt2

cor_model_acc2 <- cor.test(tran_sub$`incongruence_score_stim:block_type1`, tran_sub$accRate, method = "pearson")
cor_model_acc2



