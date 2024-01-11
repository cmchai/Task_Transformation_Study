############ the initial check of S-PRO data ################# 

### set wd and load packages ###

setwd("D:/Ghent_Braem/Experiment/2nd_experiment/data")
library(tidyverse)
library(feather)


### import data and merge them into one file ###

dir_rawdata <- "D:/Ghent_Braem/Experiment/2nd_experiment/data"
list_rawdata <- list.files(path = dir_rawdata,
                                   pattern = "*.csv",
                                   full.names = TRUE) %>%
  lapply(read_csv)


fun_RtToDbl <- function(dataframe) {
  if (typeof(dataframe$rt) != "double"){ # if the rt data include null value, change it to numerical
    dataframe$rt <- parse_double(dataframe$rt, na = "null")
  }
  return(dataframe)
}

nlist_rawdata <- lapply(list_rawdata, fun_RtToDbl)
bigdf <- bind_rows(nlist_rawdata)
bigdf$response[bigdf$response == "null"] <- NA

### check the browser information ###
names(bigdf)
unique(bigdf$browser_name)
prolificIDs <- unique(bigdf$prolific)


### check if all the subjects finish all the blocks and trials ###
subject_trial_finish <- aggregate(bigdf$trial_num,
                                  by = list(block = bigdf$block_id,
                                            subject = bigdf$prolific),
                                  max,
                                  na.rm=TRUE)

### check the target Rt mean and distribution ###
aggregate(bigdf$rt,
          by = list(subject = bigdf$subject,
                    prolific = bigdf$prolific),
          mean,
          na.rm=TRUE)

### check the RT distribution of each subject ###

bigdf %>%
  drop_na(rt) %>%
  ggplot(aes(rt)) +
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=.2, fill="#B0E0E6")+
  facet_wrap(~ subject, nrow = 6)

### check the accuracy rate ###
acc_overall <- sum(bigdf$accuracy)/length(bigdf$accuracy)

fun_accurate <- function(acc_vec){
  acc_rate <- sum(acc_vec)/length(acc_vec)
  return(acc_rate)
}

aggregate(bigdf$accuracy,
          by = list(subject = bigdf$subject,
                    prolific = bigdf$prolific),
          fun_accurate)

aggregate(bigdf$accuracy,
          by = list(blocktype = bigdf$block_id,
                    subject = bigdf$subject),
          fun_accurate)

aggregate(bigdf$accuracy,
          by = list(trialtype = bigdf$trial_nature,
                    subject = bigdf$subject),
          fun_accurate)

### check response bias (either pressing the left or right key very often)
resp_bias <- bigdf %>%
  filter(!is.na(response)) %>%
  group_by(subject, response) %>%
  summarise(count = n()) %>%
  ungroup()

na_count <- bigdf %>%
  filter(is.na(response)) %>%
  group_by(subject) %>%
  summarise(count = n()) %>%
  ungroup()

### check missing value ###
sum(is.na(bigdf$rt))/length(bigdf$rt)

################## savedata for the next processing stage #####################
library(feather)

write_feather(bigdf, "bigdf.feather")

################# allocating money #######################

bigdf %>%
  group_by(subject, prolific, extra_payment, total_payment) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  print(n=40)



