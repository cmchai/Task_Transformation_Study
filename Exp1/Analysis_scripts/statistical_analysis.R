################# statistical analysis ######################
############# task transform paradigm - main(v2) ############
setwd("D:/Ghent_Braem/Experiment/2nd_experiment/Experiment_var2/data")

library(tidyverse)
library(ez)
library(feather)
library(rstatix)
library(wesanderson)
library(ggsignif)
library(ggnewscale)
library(papaja)
library(rempsyc)

# for animation making
library(gganimate)
library(gifski)

##### split half violin plot ------
GeomSplitViolin <- ggproto(
  "GeomSplitViolin", 
  GeomViolin, 
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <- transform(data, 
                      xminv = x - violinwidth * (x - xmin), 
                      xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1,'group']
    newdata <- plyr::arrange(
      transform(data, x = if(grp%%2==1) xminv else xmaxv), 
      if(grp%%2==1) y else -y
    )
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin", 
                       grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    } else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <- function (mapping = NULL, 
                               data = NULL, 
                               stat = "ydensity", 
                               position = "identity", ..., 
                               draw_quantiles = NULL, 
                               trim = TRUE, 
                               scale = "area", 
                               na.rm = FALSE, 
                               show.legend = NA, 
                               inherit.aes = TRUE) {
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomSplitViolin, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(trim = trim, 
                      scale = scale, 
                      draw_quantiles = draw_quantiles, 
                      na.rm = na.rm, ...)
  )
}

###########################################################
########### the Analysis on the Reaction time #############
###########################################################

bigdf_clean <- read_feather("bigdf_cleanSubTri.feather")
# save(bigdf_clean, file = "D:/Ghent_Braem/Experiment/2nd_experiment/Experiment_var2/data/bigdf_clean.Rdata")

glimpse(bigdf_clean)

### step1. only include the regular trials for analysis ###
regular_trials <- bigdf_clean %>%
  filter(trial_nature == "RG") %>%
  select(rt, block_type, block_id, trial_id, trial_num, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins, incongruence_score_stim, incongruence_score_rule)

### step1. only include the transform trials for analysis ###
transform_trials <- bigdf_clean %>%
  filter(trial_nature == "TF") %>%
  select(rt, block_type, block_id, trial_id, trial_num, stim, rule, CTI, resp_map_age,
         resp_map_size, resp_map_location, start_block, subject, accuracy, CTI_bins, incongruence_score_stim, incongruence_score_rule)

transform_trials_descrip <- transform_trials %>%
  group_by(subject) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  summarise(subRT = mean(meanRT),
            sdRT = sd(meanRT))

# check how many transform trials per subject
tran_sub <- transform_trials %>%
  group_by(subject) %>%
  summarise(count = n())

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
            se = subsd/((n_sub)^.5)) %>%   # standard error
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

#### plotting for publication ####
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
    ylim(1050, 1250) +
    theme_apa(base_size = 14) +
    labs(x = "CTI bin", y = "Reaction time in ms")
)

# plot both individual and group level rt pattern
(
  p <- ggplot(rt_descrip, aes(x = CTI_bins, y = meanRT, color = block_type)) +
    geom_point(size = 4, alpha = .06) +
    geom_point(data = rt_descrip2, aes(y = subRT), size = 9,
               position=position_dodge(0.2)) +
    geom_line(data = rt_descrip2, aes(y = subRT, group = block_type), size = 1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1075, 1225) +
    theme_bw() +
    theme(axis.text = element_text(size = 20))
)

# plotting for poster
(
  p <- ggplot(rt_descrip, aes(x = CTI_bins, y = meanRT, color = block_type)) +
    geom_point(size = 4, alpha = .06) +
    geom_point(data = rt_descrip2, aes(y = subRT), size = 9,
               position=position_dodge(0.2)) +
    geom_line(data = rt_descrip2, aes(y = subRT, group = block_type), size = 1) +
    scale_color_manual(values = c("#4E84C4", "#FF0033")) +
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


# the individual level plot:
(
  p <- ggplot(rt_descrip3, aes(x = CTI, y = meanRT, color = block_type)) +
    geom_point(size = 3,position=position_dodge(0.2)) +
    geom_line(aes(group = interaction(subject, block_type)), size = 1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(500, 2000) +
    theme_bw()
)


# the main group level plot:
(
  p <- ggplot(rt_descrip4, aes(x = CTI, y = subRT, color = block_type)) +
    geom_point(size = 4,position=position_dodge(0.2)) +
    geom_line(aes(group = block_type), size = 1) +
    geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    ylim(1050, 1300) +
    theme_bw() +
    transition_reveal(CTI)
)

p_ani <- ggplot(rt_descrip4, aes(x = CTI, y = subRT, color = block_type)) +
  geom_point(size = 4,position=position_dodge(0.2)) +
  geom_line(aes(group = block_type), size = 1) +
  geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
  scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
  ylim(1050, 1300) +
  theme_bw() +
  transition_reveal(CTI)

animate(p_ani, duration = 10, fps = 20, width = 1200, height = 600, renderer = gifski_renderer())
anim_save("output2.gif")


# add an smooth line for publication
(
  p1 <- ggplot(rt_descrip4, aes(x = CTI, y = subRT, color = block_type)) +
    geom_point(size = 5,position=position_dodge(0.2), alpha = .3) +
    geom_smooth(method = "loess", span = 0.75, size = 2, linetype="dashed") +
    geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
    scale_color_manual(values = c("#4E84C4", "#FC4E07"),
                       name = "block type:",
                       breaks = c("RG", "TF"),
                       labels = c("Regular", "Transform")) +
    coord_cartesian(ylim=c(1050, 1300)) +
    theme_apa(base_size = 14) +
    theme(axis.title.x = element_blank()) +
    labs(x = "CTI in ms", y = "Reaction time in ms") +
    ggtitle("Experiment 1")
)

# try creating animation ####
# fitting the loess function
rt_descrip_anim <- rt_descrip4 %>%
  group_by(block_type) %>%
  tidyr::nest() %>%
  dplyr::mutate(models = purrr::map(data, loess, formula = subRT ~ CTI, span = 0.75),
                fitted = purrr::map(models, `[[`, "fitted"))                
                
                
rt_descrip_anim_unnest <- rt_descrip_anim %>%
  dplyr::select(- models) %>%
  tidyr::unnest(-block_type)


p1_ani <- ggplot(rt_descrip_anim_unnest, aes(x = CTI, color = block_type)) +
          geom_point(aes(y = subRT, group = block_type), size = 5,position=position_dodge(0.2), alpha = .3) +
          geom_path(aes(y = subRT, group = block_type), alpha = .2) +
          geom_line(aes(y = fitted, group = block_type), size = 3, linetype="dashed") +
          geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
          scale_color_manual(values = c("#4E84C4", "#FC4E07"),
                             name = "block type:",
                             breaks = c("RG", "TF"),
                             labels = c("Regular", "Transform")) +
          coord_cartesian(ylim=c(1050, 1300)) +
          theme_apa(base_size = 14) +
          theme(axis.title.x = element_blank()) +
          labs(x = "CTI in ms", y = "Reaction time in ms") +
          ggtitle("Experiment 1") +
          transition_reveal(along = CTI) +
          shadow_mark()


animate(p1_ani, duration = 5, fps = 20, width = 1200, height = 600, renderer = gifski_renderer())
anim_save("output11.gif")

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
############# Congruency effect on RT #####################
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
options(scipen=999)

scale_this <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

regular_trials_lme <- regular_trials %>%
  mutate(CTI_sec = round(CTI/1000,3)) %>%
  convert_as_factor(block_type, subject, start_block) %>%
  ungroup()

contrasts(regular_trials_lme$block_type)

### specify the model ----
### new approaches ###
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
coef(model_rt_max)$subject

# quadratic maximal model --- not converged
regular_trials_lme <- regular_trials_lme %>%
  mutate(block_type_binary = if_else(block_type == "RG", 1, -1))

model_rt_max2 <- lmer(formula = rt ~ (poly(CTI_sec, 2) + incongruence_score_stim + incongruence_score_rule) * block_type_binary
                     + ((poly(CTI_sec, 2) + incongruence_score_stim + incongruence_score_rule) * block_type_binary || subject),
                     data = regular_trials_lme,
                     REML = TRUE,
                     control = lmerControl(optimizer = "nloptwrap",
                                           calc.derivs = FALSE,
                                           optCtrl = list(maxfun = 4e5)))

anova(model_rt_max2)
summary(model_rt_max2)

# if non-converging, we can start from the previous fit -- still not converged
ss = getME(model_rt_max2,c("theta","fixef"))
model_rt_max2_2 <- update(model_rt_max2, start = ss,control=lmerControl(optimizer = "nloptwrap",
                                                                        calc.derivs = FALSE,
                                                                        optCtrl = list(maxfun = 2e5)))

# model the main interactions separately
model_RT_cti <- lmer(formula = rt ~ CTI_sec * block_type
                 + (CTI_sec * block_type | subject),
                 data = regular_trials_lme,
                 control = lmerControl(optimizer = "bobyqa",
                                       calc.derivs = FALSE,
                                       optCtrl = list(maxfun = 2e5)))

model_RT_rule <- lmer(formula = rt ~ incongruence_score_rule * block_type
                     + (incongruence_score_rule * block_type || subject),
                     data = regular_trials_lme,
                     REML = FALSE,
                     control = lmerControl(optimizer = "bobyqa",
                                           calc.derivs = FALSE,
                                           optCtrl = list(maxfun = 2e5)))

anova(model_RT_cti)
summary(model_RT_cti, correlation= FALSE)

### old approaches, which is how they are reported in the article ###
model_RT <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type
                    + (1 | subject),
                    data = regular_trials_lme,
                    control = lmerControl(optimizer = "nloptwrap",
                                          calc.derivs = FALSE,
                                          optCtrl = list(maxfun = 2e5)))

model_logRT <- lmer(formula = log(rt) ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type
                 + (1 | subject),
                 data = regular_trials_lme)

model_RT2 <- lmer(formula = rt ~ (poly(CTI_sec, 2) + incongruence_score_stim + incongruence_score_rule) * block_type
                 + (1 | subject),
                 data = regular_trials_lme)

anova(model_RT)
summary(model_RT, correlation= FALSE)

regular_trials$fittedRT_linear <- fitted(model_RT)
regular_trials$fittedRT_quad <- fitted(model_RT2)

regular_trials %>%
  group_by(block_type) %>%
  summarise(meanRT = mean(fittedRT))

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
                            title = "Table \nThe result of mixed effect regression model on reaction time in experiment 1", 
                            footnote = "SE = standard error.\n* denotes p < .05,
                            ** denotes p < .01, *** denates p < .001")
tableRT_ready
save_as_docx(table_ready, path = paste0(getwd(), "/nice_tablehere_supp.docx"))

# post-hoc test: the interaction between block type and CTI length
emm_options(opt.digits = FALSE)

emtrends(model_RT, ~ block_type, var = 'CTI_sec')
test(emtrends(model_RT, ~ block_type, var = 'CTI_sec'))
emtrends(model_logRT, ~ block_type, var = 'CTI_sec')
emtrends(model_RT2, ~ block_type, var = 'CTI_sec') # still only show the linear trend

emm_quad <- emmeans(model_RT2, ~ block_type * CTI_sec, at = list(CTI_sec = -1:1))
contrast(emm_quad, "poly", by = "block_type")

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

# to see if starting block makes a difference or not
model_RT_start <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type * start_block
                     + (1 | subject),
                     data = regular_trials_lme)
summary(model_RT_start, correlation= FALSE)

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
# save(bigdf_clean_error, file = "D:/Ghent_Braem/Experiment/2nd_experiment/Experiment_var2/data/bigdf_clean_error.Rdata")

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
    ylim(0.78, 0.95) +
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
    ylim(0.75, 0.99) +
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

# maximal model
model_acc_max <- glmer(formula = ACC_bi ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type 
                       + ((CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type | subject),
                       data = regular_trials_error_lme,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              calc.derivs = FALSE,
                                              optCtrl = list(maxfun = 2e5)))

anova(model_acc_max)
summary(model_acc_max)

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
                             title = "Table 1 \nThe result of mixed effect regression model on RT and accuray in experiment 1", 
                             footnote = "RT = reaction time, ACC = accuracy, SE = standard error.\n* denotes p < .05, ** denotes p < .01, *** denotes p < .001")

tableALL_ready
save_as_docx(tableALL_ready, path = paste0(getwd(), "/nice_tablehere_sm.docx"))

# visualize the interaction between rule congruency score and block type
emmip(model_acc, block_type ~ incongruence_score_rule, cov.reduce = range, type = "response") 
emtrends(model_acc, ~ block_type, var = 'incongruence_score_rule')
test(emtrends(model_acc, ~ block_type, var = 'incongruence_score_rule'))
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
    ylim(0.75, 0.95) +
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
    ylim(0.75, 0.95) +
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

################ Other exploratory stuff ##################

# check the task rule congruency level for each image #
a <- bigdf_clean %>%
      filter(trial_nature == "RG") %>%
      group_by(subject, image_target, incongruence_score_rule) %>%
      summarise(count = n())



################ between-subject analysis ################ ----

#### the association between CIT*block type effect and the performance on transform trials----
## stronger the interaction(more negative), the better people should perform the transform tasks(shorter or more accurate)

model_est_sub <- coef(model_rt_max)$subject %>%
  rownames_to_column("subject") %>%
  select(c(1,7))

tran_rt_sub <- transform_trials %>%
  group_by(subject) %>%
  summarise(meanRT = mean(rt)) %>%
  ungroup()

tran_acc_sub <- transform_trials_error %>%
  group_by(subject) %>%
  summarise(accRate = sum(accuracy)/length(accuracy)) %>%
  ungroup()

tran_sub <- purrr::reduce(list(model_est_sub, tran_rt_sub, tran_acc_sub), dplyr::left_join, by = "subject")

cor_model_rt <- cor.test(tran_sub$`CTI_sec:block_type1`, tran_sub$meanRT, method = "pearson")
cor_model_rt

cor_model_acc <- cor.test(tran_sub$`CTI_sec:block_type1`, tran_sub$accRate, method = "pearson")
cor_model_acc

########## Analysis required by reviewers ######### ----

# No.1 compare the RT before transform trials and after transform trials to see any local effect
trial_id_sur_tran <- transform_trials %>%
  mutate(pre_trial_num = trial_num - 1,
         post_trial_num = trial_num + 1,
         post2_trial_num = trial_num + 2,
         pre_trial_id = str_c(subject, block_id, pre_trial_num, sep = "_"),
         post_trial_id = str_c(subject, block_id, post_trial_num, sep = "_"),
         post2_trial_id = str_c(subject, block_id, post2_trial_num, sep = "_")) %>%
  select(trial_id, pre_trial_id, post_trial_id, post2_trial_id)

trials_pre_tran <- regular_trials %>%
  filter(trial_id %in% trial_id_sur_tran$pre_trial_id) %>%
  mutate(trial_pos = "pre")

trials_post_tran <- regular_trials %>%
  filter(trial_id %in% trial_id_sur_tran$post_trial_id) %>%
  mutate(trial_pos = "post")

trials_post2_tran <- regular_trials %>%
  filter(trial_id %in% trial_id_sur_tran$post2_trial_id) %>%
  mutate(trial_pos = "post2")

setdiff(trials_pre_tran$trial_id, trial_id_sur_tran$pre_trial_id) # sanity check: should be zero
setdiff(trial_id_sur_tran$pre_trial_id, trials_pre_tran$trial_id) # sanity check

setdiff(trials_post_tran$trial_id, trial_id_sur_tran$post_trial_id) # sanity check: should be zero
setdiff(trial_id_sur_tran$post_trial_id, trials_post_tran$trial_id) # sanity check

# the Dataframe of all the pre and post transformation trials
trials_pre_post_tran <- rbind(trials_pre_tran, trials_post_tran)

pre_post_tran_descrip <- trials_pre_post_tran %>%
  group_by(trial_pos, CTI) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE))

pre_post_tran_descrip2 <- pre_post_tran_descrip %>%
  group_by(trial_pos) %>%
  summarise(meanmeanRT = mean(meanRT))


# visualize the interaction between trial position and CTI length
(
  p <- ggplot(pre_post_tran_descrip, aes(x = CTI, y = meanRT, color = trial_pos)) +
    geom_point(size = 4,position=position_dodge(0.2), alpha = 0.5) +
    geom_smooth(method = "loess", span = 0.75, size = 2, linetype="dashed") +
    geom_vline(aes(xintercept=4652),linetype="dashed", size=1) +
    scale_color_manual(values = c("#DAA520", "#B22222"),
                       name = "trial position:",
                       breaks = c("pre", "post"),
                       labels = c("pre transform", "post transform")) +
    coord_cartesian(ylim = c(900, 1620)) +
    theme_bw()
)

# some descriptive data about this
descrip_forreview <- trials_pre_post_tran %>%
  group_by(subject, trial_pos) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(trial_pos) %>%
  summarise(subRT = mean(meanRT),
            subsd = sd(meanRT))

# run mixed effect model to compare pre versus post RT  ----
library(lmerTest)
library(lme4)
library(emmeans)

options(contrasts = c("contr.sum","contr.poly"))
options(scipen=999)

trials_pre_post_tran_lme <- trials_pre_post_tran %>%
  mutate(CTI_sec = round(CTI/1000,3)) %>%
  convert_as_factor(trial_pos, subject) %>%
  ungroup()

contrasts(trials_pre_post_tran_lme$trial_pos)


### specify the model ----
# maximal model
model_pre_post_rt <- lmer(formula = rt ~ CTI_sec * trial_pos + ( 1 | subject),
                              data = trials_pre_post_tran_lme,
                              REML = TRUE,
                              control = lmerControl(optimizer = "nloptwrap",
                                                    calc.derivs = FALSE,
                                                    optCtrl = list(maxfun = 2e5)))

anova(model_pre_post_rt)
summary(model_pre_post_rt, correlation = FALSE)

# No.2 get rid of n+1, or even n+2 transform trials to see if the effect still here
regular_trials_lme_post <- regular_trials %>%
  filter(!(trial_id %in% c(trial_id_sur_tran$post_trial_id, trial_id_sur_tran$post2_trial_id))) %>%
  mutate(CTI_sec = round(CTI/1000,3)) %>%
  convert_as_factor(block_type, subject) %>%
  ungroup()

contrasts(regular_trials_lme_post$block_type)

model_RT_post <- lmer(formula = rt ~ (CTI_sec + incongruence_score_stim + incongruence_score_rule) * block_type
                 + (1 | subject),
                 data = regular_trials_lme_post,
                 control = lmerControl(optimizer = "nloptwrap",
                                       calc.derivs = FALSE,
                                       optCtrl = list(maxfun = 2e5)))

anova(model_RT_post)
summary(model_RT_post, correlation= FALSE)

###############################################################################
#################### in response to Reviewer #3 ###############################
###############################################################################
bigdf_cleanSub <- read_feather("bigdf_cleanSub.feather")

trialtype <- bigdf_cleanSub %>%
  group_by(subject, block_id) %>%
  mutate(pre_stim = lag(stim),
         pre_rule = lag(rule),
         stim_repet = (stim == pre_stim),
         rule_repet = (rule == pre_rule),
         total_repet = (stim_repet & rule_repet)) %>%
  ungroup() %>%
  select(trial_id, pre_stim, pre_rule, stim_repet, rule_repet, total_repet)

bigdf_clean_forreview <- bigdf_clean %>%
  left_join(trialtype, by = "trial_id")

regular_trials_forreview <- bigdf_clean_forreview %>%
  filter(trial_nature == "RG")

regular_trials_trialtype <- regular_trials_forreview %>%
  filter(!is.na(total_repet)) %>%
  group_by(total_repet) %>%
  summarise(count = n())

regular_trialtype_descrip <- regular_trials_forreview %>%
  filter(!is.na(total_repet)) %>%
  group_by(subject, block_type, total_repet) %>%
  summarise(count = n(),
            meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup()
  
(p_regular_trialtypes <- ggplot(regular_trialtype_descrip, aes(x = block_type, y = meanRT, fill = total_repet)) +
    geom_split_violin(trim = FALSE, alpha = .3, width = .7)+
    scale_fill_manual(values = alpha(c("#4E84C4", "#FC4E07"), .6),
                      name = "trial type",
                      breaks = c(TRUE, FALSE),
                      labels = c("repeat", "switch")) +
    geom_boxplot(width = 0.34, fatten = NULL, show.legend = TRUE, size = 1, position = position_dodge(width = .4))  +
    stat_summary(fun = mean, geom="point", shape=20, size=5, color="black", position = position_dodge(.37)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = .2, position = position_dodge(width = .37))+
    theme_apa(base_size = 14) +
    labs(x = "block type", y = "reaction time in ms") +
    scale_x_discrete(labels = c("RG" = "regular", "TF" = "transform")) +
    theme(axis.title.x = element_blank()))

# using ezANOVA to compute ----
rt_target_anova <- ezANOVA(data = regular_trialtype_descrip, 
                           dv = .(meanRT), 
                           wid = .(subject), 
                           within = .(block_type, total_repet),
                           type = 3)

rt_target_anova

# mixed effect model 
regular_trials_lme_forreview <- regular_trials_forreview %>%
  mutate(CTI_sec = round(CTI/1000,3)) %>%
  convert_as_factor(total_repet, block_type) %>%
  ungroup()

model_RT_forreview <- lmer(formula = rt ~ (CTI_sec + total_repet + incongruence_score_stim + incongruence_score_rule) * block_type
                      + (1 | subject),
                      data = regular_trials_lme_forreview,
                      control = lmerControl(optimizer = "nloptwrap",
                                            calc.derivs = FALSE,
                                            optCtrl = list(maxfun = 2e5)))

summary(model_RT_forreview, correlation = FALSE)

emms <- emmeans(model_RT_forreview, ~ total_repet|block_type)
con1 <- contrast(emms, interaction = "pairwise")
pairs(con1, by = NULL)

######### for transform trials #########

tran_trials_forreview <- bigdf_cleanSub %>%
  left_join(trialtype, by = "trial_id") %>%
  filter(trial_nature == "TF") %>%
  mutate(tran_stim = ifelse(dim_tran == "stim", task_tran, stim),
         tran_rule = ifelse(dim_tran == "rule", task_tran, rule),
         tran_stim_repeat = (tran_stim == pre_stim),
         tran_rule_repeat = (tran_rule == pre_rule),
         tran_total_repeat = (tran_stim_repeat & tran_rule_repeat)) %>%
  filter(!is.na(tran_total_repeat)) %>%
  group_by(subject, tran_total_repeat) %>%
  summarise(count = n())

# the RT of transform trials
transform_trials_descrip <- transform_trials %>%
  group_by(subject) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  summarise(subRT = mean(meanRT),
            sdRT = sd(meanRT))

# the RT of regular trials of short CTIs

regular_trials_short_descrip <- regular_trials %>%
  filter(CTI_bins == 1) %>%
  group_by(subject) %>%
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>%
  ungroup() %>%
  summarise(subRT = mean(meanRT),
            sdRT = sd(meanRT))
  




