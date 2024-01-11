#######################################################################
################# the Analysis of the DDM Result ######################
#######################################################################
library(here)
library(tidyverse)
library(ggdist)
library(bayestestR)
library(scales)
library(wesanderson)
library(patchwork)
library(papaja)
library(rempsyc)

color_exp = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")
names(color_exp) = c("control", "rule_tran", "stim_tran", "v1", "v2")
color_exp["control"]

#### load the squash y axis function ####

require(ggplot2)
squash_axis <- function(from, to, factor) { 
  # A transformation function that squashes the range of [from, to] by factor on a given axis 
  
  # Args:
  #   from: left end of the axis
  #   to: right end of the axis
  #   factor: the compression factor of the range [from, to]
  #
  # Returns:
  #   A transformation called "squash_axis", which is capsulated by trans_new() function
  
  trans <- function(x) {    
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squash_axis", trans, inv))
}


#######################################################################
########### the DDM result of stim transform congruency effect ########
#######################################################################

psamples_0 <- read_csv(paste0(here(),"/stim_tran_cong/results/postsamples_2.csv"))
parameter_vector <- colnames(psamples_0)

parameter_vector[!str_detect(colnames(psamples_0), "subj")] # the group level parameters (fixed effect)

psamples <- psamples_0 %>%
  select(! contains("subj")) %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

### the distribution of slope of drift rate ###

psamples %>%
  filter(parameters %in% c("v_Intercept",
                           "v_C(block_type, Treatment('RG'))[T.TF]",
                           "v_incongruence_score_stim", 
                           "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]")) %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("drift rate") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels=c("v_Intercept" = "RG",
                            "v_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
                            "v_incongruence_score_stim" = "congr_slope_RG",
                            "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]" = "congr_slope_TF-RG")) +
  theme_minimal()

dist_postV <- psamples_0 %>%
  select("v_Intercept",
         "v_C(block_type, Treatment('RG'))[T.TF]",
         "v_incongruence_score_stim", 
         "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]")

describe_posterior(dist_postV,
                   centrality = "median",
                   test = "p_direction",
                   ci = 0.89)

  
### the distribution of slope of non-decision time ###

psamples %>%
  filter(parameters == "t_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters)) + 
  ylab("non-decision time") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  scale_x_discrete(labels=c("t_Intercept" = "RG")) +
  theme_minimal()

psamples %>%
  filter(parameters %in% c(
                           "t_C(block_type, Treatment('RG'))[T.TF]",
                           "t_incongruence_score_stim", 
                           "t_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]")) %>%
  ggplot(aes(y = ddm_result, x = parameters)) + 
  ylab("non-decision time") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels=c(
                            "t_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
                            "t_incongruence_score_stim" = "congr_slope_RG",
                            "t_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]" = "congr_slope_TF-RG")) +
  theme_minimal()

dist_postT <- psamples_0 %>%
  select("t_Intercept",
         "t_C(block_type, Treatment('RG'))[T.TF]",
         "t_incongruence_score_stim", 
         "t_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]")

describe_posterior(dist_postT,
                   centrality = "median",
                   test = "p_direction")

### the distribution of slope of boundary ###

psamples %>%
  filter(parameters == "a_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("boundary") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  scale_x_discrete(labels=c("a_Intercept" = "RG")) +
  theme_minimal()


psamples %>%
  filter(parameters %in% c(
                           "a_C(block_type, Treatment('RG'))[T.TF]",
                           "a_incongruence_score_stim", 
                           "a_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]")) %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("boundary") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels=c(
                            "a_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
                            "a_incongruence_score_stim" = "congr_slope_RG",
                            "a_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]" = "congr_slope_TF-RG")) +
  theme_minimal()

dist_postA <- psamples_0 %>%
  select("a_Intercept",
         "a_C(block_type, Treatment('RG'))[T.TF]",
         "a_incongruence_score_stim", 
         "a_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]")

describe_posterior(dist_postA,
                   centrality = "median",
                   test = "p_direction")


###################################################################################
########### the DDM result of stim transform time-based expectancy effect ########

stimtran_CTI_psamples_0 <- read_csv(paste0(here(),"/stim_tran_CTI/results/stimtran_CTI_postsamples_2.csv"))
colnames(stimtran_CTI_psamples_0)

stimtran_CTI_psamples <- stimtran_CTI_psamples_0 %>%
  select(! contains("subj")) %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

### the distribution of slope of drift rate ###
stimtran_CTI_psamples %>%
  filter(parameters == "v_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("drift rate") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  scale_x_discrete(labels=c("v_Intercept" = "RG")) +
  theme_minimal()

stimtran_CTI_psamples %>%
  filter(parameters %in% c(
                           "v_C(block_type, Treatment('RG'))[T.TF]",
                           "v_CTI", 
                           "v_CTI:C(block_type, Treatment('RG'))[T.TF]")) %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("drift rate") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels=c(
                            "v_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
                            "v_CTI" = "CTI_slope_RG",
                            "v_CTI:C(block_type, Treatment('RG'))[T.TF]" = "CTI_slope_TF-RG")) +
  theme_minimal()

dist_postV <- stimtran_CTI_psamples_0 %>%
  select("v_Intercept",
         "v_C(block_type, Treatment('RG'))[T.TF]",
         "v_CTI", 
         "v_CTI:C(block_type, Treatment('RG'))[T.TF]")

describe_posterior(dist_postV,
                   centrality = "median",
                   test = "p_direction",
                   ci = 0.89)


### the distribution of slope of non-decision time ###
stimtran_CTI_psamples %>%
  filter(parameters =="t_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("non-decision time") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  scale_x_discrete(labels=c("t_Intercept" = "RG")) +
  theme_minimal()

stimtran_CTI_psamples %>%
  filter(parameters %in% c(
                           "t_C(block_type, Treatment('RG'))[T.TF]",
                           "t_CTI", 
                           "t_CTI:C(block_type, Treatment('RG'))[T.TF]")) %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("non-decision time") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels=c(
                            "t_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
                            "t_CTI" = "CTI_slope_RG",
                            "t_CTI:C(block_type, Treatment('RG'))[T.TF]" = "CTI_slope_TF-RG")) +
  theme_minimal()

dist_postT <- stimtran_CTI_psamples_0 %>%
  select("t_Intercept",
         "t_C(block_type, Treatment('RG'))[T.TF]",
         "t_CTI", 
         "t_CTI:C(block_type, Treatment('RG'))[T.TF]")

describe_posterior(dist_postT,
                   centrality = "median",
                   test = "p_direction",
                   ci = 0.89)

### the distribution of slope of boundary ###
stimtran_CTI_psamples %>%
  filter(parameters == "a_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("boundary") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  scale_x_discrete(labels=c("a_Intercept" = "RG")) +
  theme_minimal()

stimtran_CTI_psamples %>%
  filter(parameters %in% c(
                           "a_C(block_type, Treatment('RG'))[T.TF]",
                           "a_CTI", 
                           "a_CTI:C(block_type, Treatment('RG'))[T.TF]")) %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("boundary") +
  stat_halfeye(aes(fill = stat(cut_cdf_qi(cdf, .width = c(.89, .95, 1)))),
               width = .6,
               justification = -.2,
               .width = 0,
               point_colour = NA) +
  scale_fill_manual(values = c("skyblue", "skyblue4", "gray85")) +
  geom_boxplot(width = .12,
               outlier.color = NA) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels=c(
                            "a_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
                            "a_CTI" = "CTI_slope_RG",
                            "a_CTI:C(block_type, Treatment('RG'))[T.TF]" = "CTI_slope_TF-RG")) +
  theme_minimal()

dist_postA <- stimtran_CTI_psamples_0 %>%
  select("a_Intercept",
         "a_C(block_type, Treatment('RG'))[T.TF]",
         "a_CTI", 
         "a_CTI:C(block_type, Treatment('RG'))[T.TF]")

describe_posterior(dist_postA,
                   centrality = "median",
                   test = "p_direction",
                   ci = 0.89)

#######################################################################
########### compare the DDM result of all exp and all models ##########
#######################################################################

postsamples_files <- list.files(paste0(here(), "/allExp_quadCTI/full_results"), 
                                               pattern="postsamples.*\\.csv$", 
                                               full.names=TRUE,
                                               recursive = TRUE)

read_csv_filename <- function(filename){
  onedf <- read_csv(filename) %>%
    mutate(file_name = filename,
           exp = case_when(
            str_detect(filename, "v1") ~ "v1",
            str_detect(filename, "v2") ~ "v2",
            str_detect(filename, "control") ~ "control",
            str_detect(filename, "rule_tran") ~ "rule_tran",
            str_detect(filename, "stim_tran") ~ "stim_tran"
           )
           ) %>%
    select(! contains("subj"))
  return(onedf)
}

list_postsamples <- lapply(postsamples_files, read_csv_filename)
big_ddm_postsamples <- bind_rows(list_postsamples)
save(big_ddm_postsamples, file = paste0(here(), "/results/quadCTI_ddm_postsamples.RData"))

########### the CTI effect: drift rate(v) ##########

psamples <- big_ddm_postsamples %>%
  filter(exp != "v1") %>%
  filter(effect == "CTI") %>%
  select(exp,
         "v_Intercept",
         "v_C(block_type, Treatment('RG'))[T.TF]",
         "v_CTI", 
         "v_CTI:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  filter(parameters == "v_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("drift rate") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c("v_Intercept" = "RG")) +
  theme_minimal()


psamples %>%
  filter(parameters != "v_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("drift rate") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c(
    "v_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
    "v_CTI" = "CTI_slope_RG",
    "v_CTI:C(block_type, Treatment('RG'))[T.TF]" = "CTI_slope_TF-RG")) +
  theme_minimal() +
  ylim(-0.15, 0.15)


########### the CTI effect: boundry(a) ##########

psamples <- big_ddm_postsamples %>%
  filter(effect == "CTI") %>%
  select(exp,
         "a_Intercept",
         "a_C(block_type, Treatment('RG'))[T.TF]",
         "a_CTI", 
         "a_CTI:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  filter(parameters == "a_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("decision boundary") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c("a_Intercept" = "RG")) +
  theme_minimal() +
  ylim(1.5, 2.25)


psamples %>%
  filter(parameters != "a_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("decision boundary") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c(
    "a_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
    "a_CTI" = "CTI_slope_RG",
    "a_CTI:C(block_type, Treatment('RG'))[T.TF]" = "CTI_slope_TF-RG")) +
  theme_minimal() +
  ylim(-0.15, 0.2)

## compute and visualize the marginal mean of effect of decision boundary(threshold) from Experiment 2 ##

########### the CTI effect: non-decision time(t) ##########

psamples <- big_ddm_postsamples %>%
  filter(effect == "CTI") %>%
  select(exp,
         "t_Intercept",
         "t_C(block_type, Treatment('RG'))[T.TF]",
         "t_CTI", 
         "t_CTI:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  filter(parameters == "t_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("non-decision time") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c("t_Intercept" = "RG")) +
  theme_minimal() +
  ylim(.4, .7)


psamples %>%
  filter(parameters != "t_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("non-decision time") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c(
    "t_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
    "t_CTI" = "CTI_slope_RG",
    "t_CTI:C(block_type, Treatment('RG'))[T.TF]" = "CTI_slope_TF-RG")) +
  theme_minimal() +
  ylim(-0.05, 0.05)

########### the rule congruency effect: drift rate(v) ##########

psamples <- big_ddm_postsamples %>%
  filter(effect == "ruleCon") %>%
  select(exp,
         "v_Intercept",
         "v_C(block_type, Treatment('RG'))[T.TF]",
         "v_incongruence_score_rule", 
         "v_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  filter(parameters == "v_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("drift rate") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c("v_Intercept" = "RG")) +
  theme_bw()


psamples %>%
  filter(parameters != "v_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("drift rate") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c(
    "v_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
    "v_incongruence_score_rule" = "ruleCon_slope_RG",
    "v_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]" = "ruleCon_slope_TF-RG")) +
  theme_bw()


########### the rule congruency effect: decision boundary(a) ##########

psamples <- big_ddm_postsamples %>%
  filter(effect == "ruleCon") %>%
  select(exp,
         "a_Intercept",
         "a_C(block_type, Treatment('RG'))[T.TF]",
         "a_incongruence_score_rule", 
         "a_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  filter(parameters == "a_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("decision boundary") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c("a_Intercept" = "RG")) +
  theme_bw()


psamples %>%
  filter(parameters != "a_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("decision boundary") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c(
    "a_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
    "a_incongruence_score_rule" = "ruleCon_slope_RG",
    "a_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]" = "ruleCon_slope_TF-RG")) +
  theme_bw()

########### the rule congruency effect: non-decision time(t) ##########

psamples <- big_ddm_postsamples %>%
  filter(effect == "ruleCon") %>%
  select(exp,
         "t_Intercept",
         "t_C(block_type, Treatment('RG'))[T.TF]",
         "t_incongruence_score_rule", 
         "t_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  filter(parameters == "t_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("non-decision time") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c("t_Intercept" = "RG")) +
  theme_bw()


psamples %>%
  filter(parameters != "t_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("non-decision time") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c(
    "t_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
    "t_incongruence_score_rule" = "ruleCon_slope_RG",
    "t_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]" = "ruleCon_slope_TF-RG")) +
  theme_bw()

## compute and visualize the marginal mean of effect of non-decision time from rule transform paradigm ##

psamples <- big_ddm_postsamples %>%
  filter(effect == "ruleCon") %>%
  select(exp,
         "t_Intercept",
         "t_C(block_type, Treatment('RG'))[T.TF]",
         "t_incongruence_score_rule", 
         "t_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  rename("intercept_RG" = "t_Intercept",
         "block_TF_RG" = "t_C(block_type, Treatment('RG'))[T.TF]",
         "slope_RG" = "t_incongruence_score_rule",
         "interaction_TF_RG" = "t_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  mutate(RG_0 = intercept_RG + 0 * slope_RG,
         RG_1 = intercept_RG + 1 * slope_RG,
         RG_2 = intercept_RG + 2 * slope_RG,
         TF_0 = intercept_RG + block_TF_RG + 0 * (slope_RG + interaction_TF_RG),
         TF_1 = intercept_RG + block_TF_RG + 1 * (slope_RG + interaction_TF_RG),
         TF_2 = intercept_RG + block_TF_RG + 2 * (slope_RG + interaction_TF_RG)) %>%
  select(exp, RG_0 : TF_2) %>%
  pivot_longer(cols = c(RG_0 : TF_2),
               names_to = "vertex",
               values_to = "non_decision_time") %>%
  mutate(block_type = str_sub(vertex, 1, 2),
         incongrucency_score_rule = str_sub(vertex, -1)) %>%
  filter(exp == "rule_tran")

# visualization #
(
  p <- ggplot(psamples, aes(y = non_decision_time, x = incongrucency_score_rule, fill = block_type)) +
    stat_halfeye(position = position_dodge(width = 0.5),
                 point_interval = "median_hdi",
                 .width = 0.95,
                 point_size = 3,
                 alpha = .8) +
    scale_fill_manual(values = c("#4E84C4", "#FC4E07"),
                      name = "Block type:",
                      breaks = c("RG", "TF"),
                      labels = c("Regular", "Transform")) +
    labs(x = "Incongruency score of rule", y = "Non-decision time(t)") +
    ylim(0.4, 0.72) +
    theme_apa(base_size = 14) +
    theme(text = element_text(family = "Times"))
)

# pairwise comparison #
t_paircomP <- psamples %>%
  select(-c(4, 5)) %>%
  group_by(vertex) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = vertex, values_from = non_decision_time) %>%
  mutate(diff0 = TF_0 - RG_0,
         diff1 = TF_1 - RG_1,
         diff2 = TF_2 - RG_2)

hdi(t_paircomP$diff2, ci = 0.95, verbose = TRUE)
hdi(t_paircomP$diff2, ci = 0.89, verbose = TRUE)

########### the Stimulus Congruency effect: drift rate(v) ##########

psamples <- big_ddm_postsamples %>%
  filter(effect == "stimCon") %>%
  select(exp,
         "v_Intercept",
         "v_C(block_type, Treatment('RG'))[T.TF]",
         "v_incongruence_score_stim", 
         "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  filter(parameters == "v_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("drift rate") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c("v_Intercept" = "RG")) +
  theme_bw()


psamples %>%
  filter(parameters != "v_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("drift rate") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c(
    "v_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
    "v_incongruence_score_stim" = "stimCon_slope_RG",
    "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]" = "stimCon_slope_TF-RG")) +
  theme_bw()

## compute and visualize the marginal mean of effect of drift rate from stimulus transform paradigm ##

psamples <- big_ddm_postsamples %>%
  filter(effect == "stimCon") %>%
  select(exp,
         "v_Intercept",
         "v_C(block_type, Treatment('RG'))[T.TF]",
         "v_incongruence_score_stim", 
         "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
  rename("intercept_RG" = "v_Intercept",
         "block_TF_RG" = "v_C(block_type, Treatment('RG'))[T.TF]",
         "slope_RG" = "v_incongruence_score_stim",
         "interaction_TF_RG" = "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
  mutate(RG_0 = intercept_RG + 0 * slope_RG,
         RG_1 = intercept_RG + 1 * slope_RG,
         RG_2 = intercept_RG + 2 * slope_RG,
         TF_0 = intercept_RG + block_TF_RG + 0 * (slope_RG + interaction_TF_RG),
         TF_1 = intercept_RG + block_TF_RG + 1 * (slope_RG + interaction_TF_RG),
         TF_2 = intercept_RG + block_TF_RG + 2 * (slope_RG + interaction_TF_RG)) %>%
  select(exp, RG_0 : TF_2) %>%
  pivot_longer(cols = c(RG_0 : TF_2),
               names_to = "vertex",
               values_to = "drift_rate") %>%
  mutate(block_type = str_sub(vertex, 1, 2),
         incongrucency_score_stim = str_sub(vertex, -1)) %>%
  filter(exp == "stim_tran")

# visualization #
(
  p <- ggplot(psamples, aes(y = drift_rate, x = incongrucency_score_stim, fill = block_type)) +
    stat_halfeye(position = position_dodge(width = 0.5),
                 point_interval = "median_hdi",
                 .width = 0.95,
                 point_size = 3,
                 alpha = .8) +
    scale_fill_manual(values = c("#4E84C4", "#FC4E07"),
                      name = "Block type:",
                      breaks = c("RG", "TF"),
                      labels = c("Regular", "Stimulus transform")) +
    labs(x = "stimulus type incongruency", y = "Drift rate(v)") +
    theme_apa(base_size = 14)
)

# pair-wise comparison #
v_paircomP <- psamples %>%
  select(-c(4, 5)) %>%
  group_by(vertex) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = vertex, values_from = drift_rate) %>%
  mutate(diff0 = TF_0 - RG_0,
         diff1 = TF_1 - RG_1,
         diff2 = TF_2 - RG_2)

hdi(v_paircomP$diff2, ci = 0.95, verbose = TRUE)
hdi(v_paircomP$diff2, ci = 0.89, verbose = TRUE)

########### the rule congruency effect: decision boundary(a) ##########

psamples <- big_ddm_postsamples %>%
  filter(effect == "stimCon") %>%
  select(exp,
         "a_Intercept",
         "a_C(block_type, Treatment('RG'))[T.TF]",
         "a_incongruence_score_stim", 
         "a_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  filter(parameters == "a_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("decision boundary") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c("a_Intercept" = "RG")) +
  theme_bw() +
  ylim(1.5, 2.5)


psamples %>%
  filter(parameters != "a_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("decision boundary") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c(
    "a_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
    "a_incongruence_score_stim" = "stimCon_slope_RG",
    "a_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]" = "stimCon_slope_TF-RG")) +
  theme_bw()

########### the Stimulus Congruency effect: non-decision time(t) ##########

psamples <- big_ddm_postsamples %>%
  filter(effect == "stimCon") %>%
  select(exp,
         "t_Intercept",
         "t_C(block_type, Treatment('RG'))[T.TF]",
         "t_incongruence_score_stim", 
         "t_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  filter(parameters == "t_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("non-decision time") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c("t_Intercept" = "RG")) +
  theme_bw()+
  ylim(0.3, 0.6)


psamples %>%
  filter(parameters != "t_Intercept") %>%
  ggplot(aes(y = ddm_result, x = parameters, fill = exp)) +
  ylab("non-decision time") +
  stat_halfeye(position = position_dodge(width = 0.5),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3,
               alpha = .5) +
  geom_hline(yintercept = 0, linetype='dotted', size = 1) +
  scale_fill_manual(values = c("#999999", "#FF0033", "#FF6600", "#3399FF", "#6666FF")) +
  scale_x_discrete(labels=c(
    "t_C(block_type, Treatment('RG'))[T.TF]" = "TF-RG",
    "t_incongruence_score_stim" = "stimCon_slope_RG",
    "t_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]" = "stimCon_slope_TF-RG")) +
  theme_bw()

############################################################
################ between subject analysis ##################
############################################################

psamples <- read_csv(paste0(here(),"/allExp_allCon/full_results/stim_tran_stimCon/postsamples_stim_tran_stimCon.csv"))
load(file = "D:/Ghent_Braem/Experiment/2nd_experiment/Between_exp/result/subjectIndex.Rdata")

vec_mean_psamples <- colMeans(psamples)

df_mean_psamples <- as_tibble(as.list(vec_mean_psamples))

bysubj_psamples <- df_mean_psamples %>%
  pivot_longer(cols = everything(),
               names_to = "parameter",
               values_to = "mean_of_psamples")
  

load(file = "D:/Ghent_Braem/Experiment/2nd_experiment/Experiment_stim_tran/result/stimTranTranPerf.Rdata")


v_bysubj_psamples <- bysubj_psamples %>%
  filter(str_detect(parameter, "v_incongruence_score_stim:C\\(block_type, Treatment\\('RG'\\)\\)\\[T\\.TF\\]_sub")) %>%
  mutate(subj_idx = str_extract(parameter, "(\\d)+")) %>%
  select(subj_idx, mean_of_psamples) %>%
  mutate(across(subj_idx, as.integer)) %>%
  left_join(subject_idx, by = "subj_idx") %>%
  left_join(stim_tran_tran_perf, by = "subject")

cor(v_bysubj_psamples$mean_of_psamples, v_bysubj_psamples$meanRT)
cor(v_bysubj_psamples$mean_of_psamples, v_bysubj_psamples$accRate)

###################################################################
################ for interpretation and reporting #################
###################################################################

load(paste0(here(), "/results/quadCTI_ddm_postsamples.RData"))

##############################################################################
############# Linear CTI effects of DDM parameters of all the experiments ----
##############################################################################

psamples <- big_ddm_postsamples %>%
  select(! contains("_std")) %>%
  select(! file_name) %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

( # decision boundary
  p1<- psamples %>%
    filter(exp != "v1") %>%
    filter(parameters == "a_CTI_quad:C(block_type, Treatment('RG'))[T.TF]") %>%
    ggplot(aes(y = ddm_result, x = factor(exp, level = c("v2", "control", "rule_tran", "stim_tran")) , fill = exp)) +
    ylab("CTI by block type interaction:linear") +
    stat_halfeye(position = position_dodge(width = 0.5),
                 point_interval = "median_hdi",
                 .width = 0.89,
                 point_size = 4,
                 interval_size = 8) +
    geom_hline(yintercept = 0, linetype = 'dotted', size = 1) +
    scale_fill_manual(values = c(color_exp["v2"], color_exp["control"], color_exp["rule_tran"], color_exp["stim_tran"])) +
    scale_x_discrete(labels = c("Exp.1", "Control", "Exp.3", "Exp.4")) +
    ggtitle("Threshold(a)") +
    theme_apa(base_size = 14) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 10, angle = 20),
          plot.title = element_text(size = 18))
)


( # drift rate
  p2<- psamples %>%
    filter(exp != "v1") %>%
    filter(parameters == "v_CTI_quad:C(block_type, Treatment('RG'))[T.TF]") %>%
    ggplot(aes(y = ddm_result, x = factor(exp, level = c("v2", "control", "rule_tran", "stim_tran")) , fill = exp)) +
    stat_halfeye(position = position_dodge(width = 0.5),
                 point_interval = "median_hdi",
                 .width = 0.89,
                 point_size = 4,
                 interval_size = 8) +
    geom_hline(yintercept = 0, linetype = 'dotted', size = 1) +
    scale_fill_manual(values = c(color_exp["v2"], color_exp["control"], color_exp["rule_tran"], color_exp["stim_tran"])) +
    scale_x_discrete(labels = c("Exp.1", "Control", "Exp.3", "Exp.4")) +
    ggtitle("Drift rate (v)") +
    theme_apa(base_size = 14) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10, angle = 20),
          plot.title = element_text(size = 18))
)

( # non-decision time
  p3<- psamples %>%
    filter(exp != "v1") %>%
    filter(parameters == "t_CTI_quad:C(block_type, Treatment('RG'))[T.TF]") %>%
    ggplot(aes(y = ddm_result, x = factor(exp, level = c("v2", "control", "rule_tran", "stim_tran")) , fill = exp)) +
    stat_halfeye(position = position_dodge(width = 0.5),
                 point_interval = "median_hdi",
                 .width = 0.89,
                 point_size = 4,
                 interval_size = 8) +
    geom_hline(yintercept = 0, linetype = 'dotted', size = 1) +
    scale_fill_manual(values = c(color_exp["v2"], color_exp["control"], color_exp["rule_tran"], color_exp["stim_tran"])) +
    scale_x_discrete(labels = c("Exp.1", "Control", "Exp.3", "Exp.4")) +
    ggtitle("Non-decision time(t)") +
    theme_apa(base_size = 14) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10, angle = 20),
          plot.title = element_text(size = 18))
)

(p1 | p2 | p3) + 
  plot_annotation(title = 'The DDM results of CTI effect across all participants',
                  tag_levels = 'A')


### the marginal means of CTI effect including the quadratic term of CTI ----

param_median <- psamples %>%
  group_by(exp, parameters) %>%
  summarise(estimate = median(ddm_result)) %>%
  pivot_wider(names_from = parameters, values_from = estimate) %>%
  rename("a_Intercept_TF" = "a_C(block_type, Treatment('RG'))[T.TF]",
         "a_CTI_linear_TF" = "a_CTI_linear:C(block_type, Treatment('RG'))[T.TF]",
         "a_CTI_quad_TF" = "a_CTI_quad:C(block_type, Treatment('RG'))[T.TF]",
         "t_Intercept_TF" = "t_C(block_type, Treatment('RG'))[T.TF]",
         "t_CTI_linear_TF" = "t_CTI_linear:C(block_type, Treatment('RG'))[T.TF]",
         "t_CTI_quad_TF" = "t_CTI_quad:C(block_type, Treatment('RG'))[T.TF]",
         "v_Intercept_TF" = "v_C(block_type, Treatment('RG'))[T.TF]",
         "v_CTI_linear_TF" = "v_CTI_linear:C(block_type, Treatment('RG'))[T.TF]",
         "v_CTI_quad_TF" = "v_CTI_quad:C(block_type, Treatment('RG'))[T.TF]")


ddm_data <- read_csv(file = paste0(here(), "/allExp_quadCTI/raw_data/allExp_CTIquad_ddm_data.csv"))

ddm_data_param <- left_join(ddm_data, param_median, by = "exp")

ddm_data_param2 <- ddm_data_param %>%
  mutate(driftRate = if_else(block_type == "RG", 
                             v_Intercept + CTI_linear * v_CTI_linear + CTI_quad * v_CTI_quad,
                             v_Intercept + v_Intercept_TF + CTI_linear * (v_CTI_linear + v_CTI_linear_TF) + CTI_quad * (v_CTI_quad + v_CTI_quad_TF)),
         nonDeTime = if_else(block_type == "RG", 
                             t_Intercept + CTI_linear * t_CTI_linear + CTI_quad * t_CTI_quad,
                             t_Intercept + t_Intercept_TF + CTI_linear * (t_CTI_linear + t_CTI_linear_TF) + CTI_quad * (t_CTI_quad + t_CTI_quad_TF)),
         threshold = if_else(block_type == "RG", 
                             a_Intercept + CTI_linear * a_CTI_linear + CTI_quad * a_CTI_quad,
                             a_Intercept + a_Intercept_TF + CTI_linear * (a_CTI_linear + a_CTI_linear_TF) + CTI_quad * (a_CTI_quad + a_CTI_quad_TF)))


(
  p <- ddm_data_param2 %>%
    filter(exp == "stim_tran") %>%
    ggplot(aes(x = CTI_linear, y = threshold, color = block_type)) +
    geom_point(size = 4, position=position_dodge(0.2)) +
    geom_line(aes(group = block_type), size = 1) +
    ggtitle("threshold : Exp.4") +
    scale_color_manual(values = c("#4E84C4", "#FC4E07")) +
    theme_bw()
)

##########################################################################################
##- for rule incongruency effects of DDM parameters between rule and stim transform exp ----

############ for the main incongruency effect ###########
psamples <- big_ddm_postsamples %>%
  filter(exp == "rule_tran" | exp == "stim_tran") %>%
  filter(effect == "ruleCon") %>%
  select(exp,
         "a_incongruence_score_rule",
         "v_incongruence_score_rule",
         "t_incongruence_score_rule") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

dist_post1 <- big_ddm_postsamples %>% # statistics of the posterior samples of rule transform experiment
  filter(exp == "rule_tran") %>%
  filter(effect == "ruleCon") %>%
  select("a_incongruence_score_rule",
         "v_incongruence_score_rule",
         "t_incongruence_score_rule") %>%
  describe_posterior(centrality = "median",
                     test = "p_direction",
                     ci = 0.89) %>%
  mutate(CI_lower = round(CI_low,3),
         CI_upper = round(CI_high,3))

dist_post2 <- big_ddm_postsamples %>% # statistics of the posterior samples of rule transform experiment
  filter(exp == "stim_tran") %>%
  filter(effect == "ruleCon") %>%
  select("a_incongruence_score_rule",
         "v_incongruence_score_rule",
         "t_incongruence_score_rule") %>%
  describe_posterior(centrality = "median",
                     test = "p_direction",
                     ci = 0.89)%>%
  mutate(CI_lower = round(CI_low,3),
         CI_upper = round(CI_high,3))

v_diffDist <- psamples %>%
  filter(parameters == "v_incongruence_score_rule") %>%
  group_by(exp) %>%
  mutate(row = row_number()) %>% # create a unique identifier for each data entry
  pivot_wider(names_from = exp, values_from = ddm_result) %>%
  select(-row) %>%
  mutate(diff = rule_tran - stim_tran)

a <- hdi(v_diffDist$diff, ci = 0.95, verbose = TRUE)
b <- hdi(v_diffDist$diff, ci = 0.89, verbose = TRUE)

############ for the interaction effect ###############
psamples <- big_ddm_postsamples %>%
  filter(exp == "rule_tran" | exp == "stim_tran") %>%
  filter(effect == "ruleCon") %>%
  select(exp,
         "a_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]",
         "v_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]",
         "t_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

dist_post1 <- big_ddm_postsamples %>% # statistics of the posterior samples of rule transform experiment
  filter(exp == "rule_tran") %>%
  filter(effect == "ruleCon") %>%
  select("a_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]",
         "v_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]",
         "t_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  describe_posterior(centrality = "median",
                     test = "p_direction",
                     ci = 0.89) %>%
  mutate(CI_lower = round(CI_low,3),
         CI_upper = round(CI_high,3))

dist_post2 <- big_ddm_postsamples %>% # statistics of the posterior samples of rule transform experiment
  filter(exp == "stim_tran") %>%
  filter(effect == "ruleCon") %>%
  select("a_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]",
         "v_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]",
         "t_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  describe_posterior(centrality = "median",
                     test = "p_direction",
                     ci = 0.89)%>%
  mutate(CI_lower = round(CI_low,3),
         CI_upper = round(CI_high,3))

( # non-decision time
  p3<- psamples %>%
    filter(parameters == "t_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
    ggplot(aes(y = ddm_result, x = exp , fill = exp)) +
    ylab("Para. Est.") +
    stat_halfeye(position = position_dodge(width = 1),
                 point_interval = "median_hdi",
                 .width = 0.95,
                 point_size = 4,
                 interval_size = 8,
                 alpha = .7) +
    geom_hline(yintercept = 0, linetype = 'dotted', size = 1) +
    scale_fill_manual(values = c(color_exp["rule_tran"], color_exp["stim_tran"])) +
    scale_x_discrete(labels = c("Exp. 3", "Exp. 4")) +
    ggtitle("Non-decision time(t)") +
    theme_apa(base_size = 14) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size = 12, angle = 20),
          plot.title = element_text(size = 18))
)

t_diffDist <- psamples %>%
  filter(parameters == "t_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  group_by(exp) %>%
  mutate(row = row_number()) %>% # create a unique identifier for each data entry
  pivot_wider(names_from = exp, values_from = ddm_result) %>%
  select(-row) %>%
  mutate(diff = rule_tran - stim_tran)

a <- hdi(t_diffDist$diff, ci = 0.89, verbose = TRUE)
a <- hdi(t_diffDist$diff, ci = 0.95, verbose = TRUE)


( # decision boundary
  p1<- psamples %>%
    filter(parameters == "a_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
    ggplot(aes(y = ddm_result, x = exp , fill = exp)) +
    ylab("Para. Est.") +
    stat_halfeye(position = position_dodge(width = 1),
                 point_interval = "median_hdi",
                 .width = 0.95,
                 point_size = 4,
                 interval_size = 8,
                 alpha = .7) +
    geom_hline(yintercept = 0, linetype = 'dotted', size = 1) +
    scale_fill_manual(values = c(color_exp["rule_tran"], color_exp["stim_tran"])) +
    scale_x_discrete(labels = c("Exp. 3", "Exp. 4")) +
    ggtitle("Threshold(a)") +
    theme_apa(base_size = 14) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, angle = 20),
          plot.title = element_text(size = 18))
)

a_diffDist <- psamples %>%
  filter(parameters == "a_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  group_by(exp) %>%
  mutate(row = row_number()) %>% # create a unique identifier for each data entry
  pivot_wider(names_from = exp, values_from = ddm_result) %>%
  select(-row) %>%
  mutate(diff = rule_tran - stim_tran)

a <- hdi(a_diffDist$diff, ci = 0.89, verbose = TRUE)
b <- hdi(a_diffDist$diff, ci = 0.95, verbose = TRUE)

( # drift rate
  p2<- psamples %>%
    filter(parameters == "v_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
    ggplot(aes(y = ddm_result, x = exp , fill = exp)) +
    ylab("Para. Est.") +
    stat_halfeye(position = position_dodge(width = 1),
                 point_interval = "median_hdi",
                 .width = 0.95,
                 point_size = 4,
                 interval_size = 8,
                 alpha = .7) +
    geom_hline(yintercept = 0, linetype = 'dotted', size = 1) +
    scale_fill_manual(values = c(color_exp["rule_tran"], color_exp["stim_tran"])) +
    scale_x_discrete(labels = c("Exp. 3", "Exp. 4")) +
    ggtitle("Drift rate(v)") +
    theme_apa(base_size = 14) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, angle = 20),
          plot.title = element_text(size = 18))
)


(p3 | (p1 / p2)) + 
  plot_annotation(title = 'The DDM results of task rule congruency effect',
                  tag_levels = 'A')

#########################################################################################
##- for stim congruency effects of DDM parameters between rule and stim transform exp -##
psamples <- big_ddm_postsamples %>%
  filter(exp == "rule_tran" | exp == "stim_tran") %>%
  filter(effect == "stimCon") %>%
  select(exp,
         "a_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]",
         "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]",
         "t_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

dist_post1 <- big_ddm_postsamples %>% # statistics of the posterior samples of stimulus transform experiment
  filter(exp == "stim_tran") %>%
  filter(effect == "stimCon") %>%
  select("a_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]",
         "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]",
         "t_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
  describe_posterior(centrality = "median",
                     test = "p_direction",
                     ci = 0.89) %>%
  mutate(CI_lower = round(CI_low,3),
         CI_upper = round(CI_high,3))

dist_post2 <- big_ddm_postsamples %>% # statistics of the posterior samples of rule transform experiment
  filter(exp == "rule_tran") %>%
  filter(effect == "stimCon") %>%
  select("a_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]",
         "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]",
         "t_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
  describe_posterior(centrality = "median",
                     test = "p_direction",
                     ci = 0.89)%>%
  mutate(CI_lower = round(CI_low,3),
         CI_upper = round(CI_high,3))

( # drift rate
  p2<- psamples %>%
    filter(parameters == "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
    ggplot(aes(y = ddm_result, x = exp , fill = exp)) +
    ylab("Para. Est.") +
    stat_halfeye(position = position_dodge(width = 1),
                 point_interval = "median_hdi",
                 .width = 0.89,
                 point_size = 4,
                 interval_size = 8,
                 alpha = .7) +
    geom_hline(yintercept = 0, linetype = 'dotted', size = 1) +
    scale_fill_manual(values = c(color_exp["rule_tran"], color_exp["stim_tran"])) +
    scale_x_discrete(labels = c("Exp. 3", "Exp. 4")) +
    ggtitle("Drift rate(v)") +
    theme_apa(base_size = 14) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size = 12, angle = 20),
          plot.title = element_text(size = 18))
)

v_diffDist <- psamples %>%
  filter(parameters == "v_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
  group_by(exp) %>%
  mutate(row = row_number()) %>% # create a unique identifier for each data entry
  pivot_wider(names_from = exp, values_from = ddm_result) %>%
  select(-row) %>%
  mutate(diff = stim_tran - rule_tran)

a <- hdi(v_diffDist$diff, ci = 0.95, verbose = TRUE)
b <- hdi(v_diffDist$diff, ci = 0.89, verbose = TRUE)


( # decision boundary
  p1<- psamples %>%
    filter(parameters == "a_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
    ggplot(aes(y = ddm_result, x = exp , fill = exp)) +
    ylab("Para. Est.") +
    stat_halfeye(position = position_dodge(width = 1),
                 point_interval = "median_hdi",
                 .width = 0.89,
                 point_size = 4,
                 interval_size = 8,
                 alpha = .7) +
    geom_hline(yintercept = 0, linetype = 'dotted', size = 1) +
    scale_fill_manual(values = c(color_exp["rule_tran"], color_exp["stim_tran"])) +
    scale_x_discrete(labels = c("Exp. 3", "Exp. 4")) +
    ggtitle("Threshold(a)") +
    theme_apa(base_size = 14) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, angle = 20),
          plot.title = element_text(size = 18))
)

( # non-decision time
  p3<- psamples %>%
    filter(parameters == "t_incongruence_score_stim:C(block_type, Treatment('RG'))[T.TF]") %>%
    ggplot(aes(y = ddm_result, x = exp , fill = exp)) +
    ylab("Para. Est.") +
    stat_halfeye(position = position_dodge(width = 1),
                 point_interval = "median_hdi",
                 .width = 0.89,
                 point_size = 4,
                 interval_size = 8,
                 alpha = .7) +
    geom_hline(yintercept = 0, linetype = 'dotted', size = 1) +
    scale_fill_manual(values = c(color_exp["rule_tran"], color_exp["stim_tran"])) +
    scale_x_discrete(labels = c("Exp. 3", "Exp. 4")) +
    scale_y_continuous(breaks = c(-0.05, 0, 0.05)) +
    ggtitle("Non-decision time(t)") +
    theme_apa(base_size = 14) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, angle = 20),
          plot.title = element_text(size = 18))
)

(p2 | (p1 / p3)) + 
  plot_annotation(title = 'The DDM results of stimulus congruency effect',
                  tag_levels = 'A')

## for CTI effects of non-decision time from rule transform paradigm ##

psamples <- big_ddm_postsamples %>%
  filter(exp == "rule_tran") %>%
  filter(effect == "CTI") %>%
  select("t_CTI",
         "t_CTI:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")


psamples %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("non-decision time") +
  stat_halfeye(position = position_dodge(width = 1),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c(color_exp["control"], color_exp["v2"])) +
  scale_x_discrete(labels=c("t_CTI" = "CTI_slope_RG",
                            "t_CTI:C(block_type, Treatment('RG'))[T.TF]" = "CTI_slope_TF-RG")) +
  ggtitle("rule transform paradigm") +
  theme_minimal()


# exf stim transform exp, the decision boundary difference between RG and TF blocks

psamples <- big_ddm_postsamples %>%
  filter(exp == "stim_tran") %>%
  select(effect,
         "a_C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  ggplot(aes(y = ddm_result, x = effect)) +
  ylab("decision boundary") +
  stat_halfeye(position = position_dodge(width = 1),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3) +
  geom_hline(yintercept = 0) +
  theme_minimal()

# for rule congruency effect of drift rate from v2

psamples <- big_ddm_postsamples %>%
  filter(exp == "v2") %>%
  filter(effect == "ruleCon") %>%
  select("v_incongruence_score_rule",
         "v_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]") %>%
  pivot_longer(cols = contains("_"),
               names_to = "parameters",
               values_to = "ddm_result")

psamples %>%
  ggplot(aes(y = ddm_result, x = parameters)) +
  ylab("drift rate") +
  stat_halfeye(position = position_dodge(width = 1),
               point_interval = "median_hdi",
               .width = 0.95,
               point_size = 3) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels=c("v_incongruence_score_rule" = "ruleCon_slope_RG",
                            "v_incongruence_score_rule:C(block_type, Treatment('RG'))[T.TF]" = "ruleCon_slope_TF-RG")) +
  ggtitle("v2 paradigm") +
  theme_minimal()
