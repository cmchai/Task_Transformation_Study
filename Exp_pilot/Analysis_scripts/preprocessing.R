############### preprocessing script ####################

#### all the functions ####


### load big data frame and all the necessary packages ###
setwd("D:/Ghent_Braem/Experiment/2nd_experiment/data")
library(feather)
library(tidyverse)
library(rstatix)

bigdf <- read_feather("bigdf.feather")
glimpse(bigdf)

#############################################
#######  Add some columns ###################
#############################################

# stimulus congruence effect

fun_arrangeimages <- function(stimulus, image1, image2, image3, index) {
  imgvec <- c(image1, image2, image3)
  logicalvec <- str_detect(imgvec, stimulus)
  target_img <- imgvec[logicalvec]
  nontarget_img1 <- imgvec[!logicalvec][1]
  nontarget_img2 <- imgvec[!logicalvec][2]
  if (index == 1) {
    return(target_img)
  } else if (index == 2) {
    return(nontarget_img1)
  } else {
    return(nontarget_img2)
  }
}

fun_category <- function(img_id, rule) {
  category <- case_when(
    rule == "age" ~ str_sub(img_id, -5, -5),
    rule == "size" ~ str_sub(img_id, -3, -3),
    rule == "location" ~ str_sub(img_id, -1, -1)
  )
  return(category)
}

fun_fullcat <- function(img_id) {
  first_cat <- str_sub(img_id, -5, -5)
  second_cat <- str_sub(img_id, -3, -3)
  third_cat <- str_sub(img_id, -1, -1)
  fullcat <- c(first_cat, second_cat, third_cat)
  return(fullcat)
}

fun_irrelavant_cat <- function(fullcat, relavantcat, index) {
  logicalvec <- str_detect(fullcat, relavantcat)
  irrelavant_cat_1 <- fullcat[!logicalvec][1]
  irrelavant_cat_2 <- fullcat[!logicalvec][2]
  if (index == 1) {
    return(irrelavant_cat_1)
  } else {
    return(irrelavant_cat_2)
  }
}

fun_irrelavant_cat_mapping <- function(cat, map_age, map_size, map_location) {
  mapping <- case_when(
    cat %in% c("o", "y") ~ map_age,
    cat %in% c("b", "s") ~ map_size,
    cat %in% c("w", "l") ~ map_location
  )
  return(mapping)
}

fun_irrelavant_cat_key <- function(cat, mapping) {
  if (mapping == 0) {
    correctkey <- case_when(
      cat %in% c("y", "s", "w") ~ "f",
      cat %in% c("o", "b", "l") ~ "j"
    )
  } else {
    correctkey <- case_when(
      cat %in% c("y", "s", "w") ~ "j",
      cat %in% c("o", "b", "l") ~ "f"
    )
  }
  return(correctkey)
}


bigdf <- bigdf %>%
  mutate(# fot the congruence effect on nontarget images
         target_img_id = pmap(list(stim, image_up_id, image_mid_id, image_low_id, 1), fun_arrangeimages),
         nontarget_img1_id = pmap(list(stim, image_up_id, image_mid_id, image_low_id, 2), fun_arrangeimages),
         nontarget_img2_id = pmap(list(stim, image_up_id, image_mid_id, image_low_id, 3), fun_arrangeimages),
         target_img_cat = map2_chr(target_img_id, rule, fun_category),
         nontarget_img1_cat = map2_chr(nontarget_img1_id, rule, fun_category),
         nontarget_img2_cat = map2_chr(nontarget_img2_id, rule, fun_category),
         incongruence_score_stim = case_when(
           target_img_cat == nontarget_img1_cat & target_img_cat == nontarget_img2_cat ~ 0,
           target_img_cat != nontarget_img1_cat & target_img_cat != nontarget_img2_cat ~ 2,
           TRUE ~ 1
         ),
         # for the congruence effect on irrelavant rules
         target_img_allcat = map(target_img_id, fun_fullcat),
         target_img_irrelavantcat1 = pmap(list(target_img_allcat, target_img_cat, 1), fun_irrelavant_cat),
         target_img_irrelavantcat2 = pmap(list(target_img_allcat, target_img_cat, 2), fun_irrelavant_cat),
         target_img_irrelavantcat1_mapping = pmap(list(target_img_irrelavantcat1, resp_map_age, resp_map_size, resp_map_location), fun_irrelavant_cat_mapping),
         target_img_irrelavantcat2_mapping = pmap(list(target_img_irrelavantcat2, resp_map_age, resp_map_size, resp_map_location), fun_irrelavant_cat_mapping),
         target_img_irrelavantcat1_key = map2(target_img_irrelavantcat1,  target_img_irrelavantcat1_mapping, fun_irrelavant_cat_key),
         target_img_irrelavantcat2_key = map2(target_img_irrelavantcat2,  target_img_irrelavantcat2_mapping, fun_irrelavant_cat_key),
         incongruence_score_rule = case_when(
           correct_key == target_img_irrelavantcat1_key & correct_key == target_img_irrelavantcat2_key ~ 0,
           correct_key != target_img_irrelavantcat1_key & correct_key != target_img_irrelavantcat2_key ~ 2,
           TRUE ~ 1
         )         
         ) %>%
  select(!(target_img_id:nontarget_img2_cat)) %>%
  select(!(target_img_allcat:target_img_irrelavantcat2_key))



#############################################
####### Subject level exclusion #############
#############################################


### Step 1. exclude subjects whose acc rate is below a threshold ###

accThres <- 0.6

lowacc_subject <- bigdf %>%
  group_by(subject, block_type, trial_nature) %>%
  summarise(
    count = n(),
    accRate = sum(accuracy)/length(accuracy)
  ) %>%
  mutate(exclude = (accRate < accThres)) %>%
  filter(exclude == TRUE) %>%
  ungroup()


# exclude them
length(unique(lowacc_subject$subject)) # the number of subjects excluded
bigdf_ex_lowacc_sub <- filter(bigdf, !(subject %in% unique(lowacc_subject$subject))) # the data frame without low acc subjects
sum(bigdf_ex_lowacc_sub$subject %in% unique(lowacc_subject$subject))                 # should be zero


### Step 2. compute the between-subject mean of RT and ER and the boundary of subject-level exclusion ###

# for the target RT and acc rate
target_outlier_subject <- bigdf_ex_lowacc_sub %>%
  group_by(subject) %>%
  summarise(
    count = n(),
    meanRT = mean(rt, na.rm = TRUE),
    medianRT = median(rt, na.rm = TRUE),
    accRate = sum(accuracy)/length(accuracy)
  ) %>%
  mutate(
    grandmean_RT = mean(medianRT),
    sd_RT = sd(medianRT),
    lowBound_RT = grandmean_RT - 2.5*sd_RT,
    highBound_RT = grandmean_RT + 2.5*sd_RT,
    
    grandmean_acc = mean(accRate),
    sd_acc = sd(accRate),
    lowBound_acc = grandmean_acc - 2.5*sd_acc,
    highBound_acc = grandmean_acc + 2.5*sd_acc,
    
    exclude_RT = (medianRT < lowBound_RT | medianRT > highBound_RT),
    exclude_acc = (accRate < lowBound_acc | accRate > highBound_acc),
    exclude = exclude_RT | exclude_acc
  ) %>%
  filter(exclude == TRUE) %>%
  ungroup()


# exclude outliers
bigdf_clean_subject <- bigdf_ex_lowacc_sub %>% 
  filter(!(subject %in% unique(target_outlier_subject$subject))) %>%
  mutate(CTI_bins = ntile(CTI, 6))

write_feather(bigdf_clean_subject, "bigdf_cleanSub.feather")

(nrow(bigdf_ex_lowacc_sub) - nrow(bigdf_clean_subject))/432 # should equal to the number of subject level outliers


##########################################
############ trial level exclusion #######
##########################################


### Step 3. exclude trials with too short RT(200 ms) ###

short_RT <- bigdf_clean_subject %>%
  filter(rt <= 200)

short_trialID <- unique(short_RT$trial_id)


### Step 4. exclude error trials and the post-error trials ###

error_trials <- bigdf_clean_subject %>%
  filter(accuracy == FALSE) %>%
  mutate(next_trial_num = trial_num + 1,
         next_trial_id = str_c(subject, block_id, next_trial_num, sep = "_"))

error_trialID <- unique(error_trials$trial_id)
post_error_trialID <- unique(error_trials$next_trial_id) # may include trial number above 60

length(error_trialID) - length(intersect(short_trialID, error_trialID)) # the number of error trials except the short trials


# exclude these short, error, and post-error trials

exclude_trials <- unique(c(short_trialID,
                          error_trialID,
                          post_error_trialID))

bigdf_ex_short_error_post <- filter(bigdf_clean_subject, !(trial_id %in% exclude_trials))


### Step 5. compute the within-subject mean and sd, set excluding threshold for each subject ###

# During the target part
outlier_target_trials <- bigdf_ex_short_error_post %>%
  group_by(subject) %>%
  mutate(mean_RT_subject = mean(rt, na.rm = TRUE),
         median_RT_subject = median(rt, na.rm = TRUE),
         sd_RT_subject = sd(rt, na.rm = TRUE),
         lowbound = mean_RT_subject - 2.5*sd_RT_subject,
         highbound = mean_RT_subject + 2.5*sd_RT_subject,
         lowquantile = unname(quantile(rt, na.rm = TRUE))[2],
         highquantile = unname(quantile(rt, na.rm = TRUE))[4],
         exclude = (rt < lowbound | rt > highbound)) %>%
  filter(exclude == TRUE)

outlier_target_trialID <- unique(outlier_target_trials$trial_id)

### excluding the trials
bigdf_clean <- filter(bigdf_ex_short_error_post, !(trial_id %in% outlier_target_trialID))

### final step. save the clean data before analysis ###
write_feather(bigdf_clean, "bigdf_cleanSubTri.feather")



### final step 2. implementing data exclusion except for the error trials in order to compare the accuracy rate ###


# not exclude the error trials
exclude_trial_error <- unique(c(short_trialID,
                                post_error_trialID))

bigdf_ex_short_post <- filter(bigdf_clean_subject, !(trial_id %in% exclude_trial_error))



# recalculating the trial level outliers
outlier_target_error_trials <- bigdf_ex_short_post %>%
  group_by(subject) %>%
  mutate(mean_RT_subject = mean(rt, na.rm = TRUE),
         median_RT_subject = median(rt, na.rm = TRUE),
         sd_RT_subject = sd(rt, na.rm = TRUE),
         lowbound = mean_RT_subject - 2.5*sd_RT_subject,
         highbound = mean_RT_subject + 2.5*sd_RT_subject,
         lowquantile = unname(quantile(rt, na.rm = TRUE))[2],
         highquantile = unname(quantile(rt, na.rm = TRUE))[4],
         exclude = (rt < lowbound | rt > highbound)) %>%
  filter(exclude == TRUE)

outlier_target_error_trialID <- unique(outlier_target_error_trials$trial_id)

bigdf_clean_error <- filter(bigdf_ex_short_post, !(trial_id %in% outlier_target_error_trialID))

write_feather(bigdf_clean_error, "bigdf_cleanSubTri_errorback.feather")

################################################
#### check all the across-subject variables ####
################################################

bigdf_clean_subject <- read_feather("bigdf_cleanSub.feather")

subject_summary <- bigdf_clean_subject %>%
  group_by(subject, resp_map_age, resp_map_location, resp_map_size, CTI_color_regular, start_block) %>%
  summarise(count = n()) %>%
  select(-count) %>%
  ungroup()

subject_summary %>%
  group_by(resp_map_age) %>%
  summarise(count = n())

subject_summary %>%
  group_by(resp_map_size) %>%
  summarise(count = n())

subject_summary %>%
  group_by(resp_map_location) %>%
  summarise(count = n())

subject_summary %>%
  group_by(CTI_color_regular) %>%
  summarise(count = n())

subject_summary %>%
  group_by(start_block) %>%
  summarise(count = n())

## check the trial type average accuracy rate from the included subjects ##

acc_blktritype <- bigdf_clean_subject %>%
  group_by(subject, block_type, trial_nature) %>%
  summarise(
    count = n(),
    accRate = sum(accuracy)/length(accuracy)
  ) %>%
  group_by(trial_nature, .add = TRUE) %>% # add back the trial nature grouping
  ungroup(subject) %>%
  summarise(
    count = n(),    # should be the number of clean subjects
    avg_accRate = mean(accRate)
  )

group_keys(acc_blktritype)
group_vars(acc_blktritype)






