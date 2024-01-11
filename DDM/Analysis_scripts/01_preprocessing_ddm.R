######################################################################
################# Preprocessing for DDM fitting ######################
######################################################################

library(here)
library(tidyverse)
library(feather)
library(rstatix)

# load the dataframe of data from experiment 1
bigdf_v1_cleanSub <- read_feather(paste0(here(),"/data/bigdf_cleanSub.feather")) %>%
  mutate(exp = "v1",
         stim_flex = TRUE,
         rule_flex = TRUE)


# load the dataframe of data from experiment version 2
bigdf_v2_cleanSub <- read_feather(paste0(here(),"/Experiment_var2/data/bigdf_cleanSub.feather")) %>%
  mutate(exp = "v2",
         stim_flex = TRUE,
         rule_flex = TRUE)


# load the dataframe of data from control experiment
bigdf_ctl_cleanSub <- read_feather(paste0(here(),"/control_experiment/data/bigdf_cleanSub.feather")) %>%
  mutate(block_type = str_replace(block_type, "OT", "TF")) %>% # change the block_type from "OT" to "TF" 
  mutate(exp = "control",
         stim_flex = FALSE,
         rule_flex = FALSE)

# load the dataframe of data from experiment rule transform
load(file = paste0(here(), "/Experiment_rule_tran/result/bigdf_clean_subject.Rdata"))

bigdf_rule_cleanSub <- bigdf_clean_subject %>%
  mutate(exp = "rule_tran",
         stim_flex = FALSE,
         rule_flex = TRUE)

# load the dataframe of data from experiment stim transform
load(file = paste0(here(), "/Experiment_stim_tran/result/bigdf_clean_subject.Rdata"))

bigdf_stim_cleanSub <- bigdf_clean_subject %>%
  mutate(exp = "stim_tran",
         stim_flex = TRUE,
         rule_flex = FALSE)

rm(bigdf_clean_subject)

##### bind all five datasets and only include the regular trials for analysis #####

Regdf_cleanSub <- bind_rows(bigdf_v1_cleanSub, bigdf_v2_cleanSub, bigdf_ctl_cleanSub, bigdf_rule_cleanSub, bigdf_stim_cleanSub) %>%
  filter(trial_nature == "RG") %>%
  select(rt, accuracy, exp, subject, block_type, block_id, trial_id, trial_num, stim, rule, CTI, CTI_bins, 
         incongruence_score_stim, incongruence_score_rule, stim_flex, rule_flex,
         resp_map_age, resp_map_size, resp_map_location, start_block
         )

# Step 1. exclude trials with too short RT(200 ms) #
short_trialID <- Regdf_cleanSub %>%
  filter(rt <= 200) %>%
  pull(trial_id)
  
# Step 2. exclude post-error trials #
posterror_trialID <- Regdf_cleanSub %>%
  filter(accuracy == FALSE) %>%
  mutate(next_trial_num = trial_num + 1,
         next_trial_id = str_c(subject, block_id, next_trial_num, sep = "_")) %>%
  pull(next_trial_id)

# final step. Implementing the data exclusion #

ddm_data <- Regdf_cleanSub %>%
  filter(!is.na(rt)) %>% # exclude missing trials
  filter(!(trial_id %in% unique(c(short_trialID,  # short trials
                                  posterror_trialID)))) %>% # post-error trials
  convert_as_factor(subject) %>%
  mutate(rt = rt/1000,  # turn ms into s
         CTI = CTI/1000,  # turn ms into s
         response = as.numeric(accuracy),  # recode TRUE as 1, FALSE as 0
         subj_idx = as.numeric(subject)) %>%  # recode subject ID into numbers
  select(subj_idx, rt, response, incongruence_score_stim, incongruence_score_rule,
         block_type, exp, stim_flex, rule_flex, CTI, CTI_bins, subject)

write.csv(ddm_data, paste0(here(),"/Between_exp/result/all_tasktran_ddm_data.csv"), 
          row.names = FALSE)
  
  
## randomly pick several participants and check their RT distribution ##
subject_ids <- sample(unique(ddm_data$subj_idx), 15)

(
ddm_data %>%
  filter(subj_idx %in% subject_ids)%>%
  ggplot(aes(x = rt, fill = response)) +
  geom_histogram(position="identity", alpha=0.4)+
  scale_fill_manual(values = c("#FC4E07", "#4E84C4")) +
  theme_bw() +
  facet_wrap(~ subj_idx, nrow = 3)
)

a <- ddm_data %>%
  count(subject)

glimpse(ddm_data)

######## check the data structure #########

ddm_data <- read_csv(paste0(here(),"/Between_exp/result/all_tasktran_ddm_data.csv"))

group_ruleflex <- ddm_data %>%
  group_by(rule_flex, subj_idx) %>%
  summarise(count = n())

ddm_data %>%
  group_by(exp) %>%
  summarise(nSub = length(unique(subject)))

######## add more columns to include the quadratic terms of CTI #########

## exploration of different ways of computing the quadratic or polynomial terms

ddm_data <- read_csv(paste0(here(),"/Between_exp/result/all_tasktran_ddm_data.csv"))

CTI_min <- min(ddm_data$CTI)
CTI_max <- max(ddm_data$CTI)
CTI_mid <- (CTI_min + CTI_max)/2

ddm_data_CTIquad <- ddm_data %>%
  select(!c(incongruence_score_stim, incongruence_score_rule, stim_flex, rule_flex, CTI_bins)) %>%
  mutate(CTI_quad = round(CTI^2, 3),
         CTI_quad_orth_ish = round((CTI - CTI_mid)^2, 3))

cortest1 <- cor.test(ddm_data_CTIquad$CTI, ddm_data_CTIquad$CTI_quad, method = "pearson")
cortest2 <- cor.test(ddm_data_CTIquad$CTI, ddm_data_CTIquad$CTI_quad_orth, method = "pearson")

CTI_polyterms <- poly(ddm_data$CTI, degree = 2) * 10^3
cortest3 <- cor.test(CTI_polyterms[ ,1], CTI_polyterms[ ,2], method = "pearson")
cortest4 <- cor.test(ddm_data$CTI, CTI_polyterms[ ,1], method = "pearson")

## Include the final true quadratic term in the data and save the data
CTI_polyterms <- round(poly(ddm_data$CTI, degree = 2) * 10^3, 3) # 1st col is linear, 2nd col is quadratic term

ddm_data_CTIquad <- ddm_data %>%
  select(!c(incongruence_score_stim, incongruence_score_rule, stim_flex, rule_flex, CTI_bins)) %>%
  mutate(CTI_raw = CTI,
         CTI_linear = CTI_polyterms[ ,1],
         CTI_quad = CTI_polyterms[ ,2])

write.csv(ddm_data_CTIquad, paste0(here(),"/Between_exp/result/allExp_CTIquad_ddm_data.csv"), 
          row.names = FALSE)

##### create the subject index file #####

ddm_data <- read_csv(file = paste0(here(), "/Between_exp/result/all_tasktran_ddm_data.csv"))

subject_idx <- ddm_data %>%
  group_by(subject, subj_idx) %>%
  summarise(meanRT = mean(rt)) %>%
  select(-meanRT) %>%
  ungroup()

save(subject_idx, file = paste0(here(), "/Between_exp/result/subjectIndex.Rdata"))




