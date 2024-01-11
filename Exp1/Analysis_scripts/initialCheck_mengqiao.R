############ the initial check of belief state v2 data ################# 

### set wd and load packages ###

setwd("D:/Ghent_Braem/Experiment/2nd_experiment/Experiment_var2/data")
library(tidyverse)
library(feather)

### import data and merge them into one file ###

dir_rawdata <- "D:/Ghent_Braem/Experiment/2nd_experiment/Experiment_var2/data"
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
sonaIDs <- unique(bigdf$sona)


### check if all the subjects finish all the blocks and trials ###
subject_trial_finish <- aggregate(bigdf$trial_num,
                                  by = list(block = bigdf$block_id,
                                            sonaID = bigdf$sona),
                                  max,
                                  na.rm=TRUE)

### check the target Rt mean and distribution ###
aggregate(bigdf$rt,
          by = list(subject = bigdf$subject,
                    sona = bigdf$sona),
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
                    sonaID = bigdf$sona),
          fun_accurate)

aggregate(bigdf$accuracy,
          by = list(startBlock = bigdf$start_block,
                    blocktype = bigdf$block_type,
                    subjectID = bigdf$subject,
                    sonaID = bigdf$sona),
          fun_accurate)

aggregate(bigdf$accuracy,
          by = list(startBlock = bigdf$start_block,
                    trialtype = bigdf$trial_nature,
                    subjectID = bigdf$subject,
                    sonaID = bigdf$sona),
          fun_accurate) %>%
  filter(x < 0.6)

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


### check if the starting block is counter-balanced ###

start_block <- bigdf %>%
  group_by(subject, start_block) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(start_block) %>%
  summarise(count = n())

### check weird subjects ###

bigdf <- read_feather("bigdf.feather")

data_subject <- bigdf %>%
  filter(subject == "etde7whd", !is.na(rt)) %>%
  summarise(accrate = sum(accuracy)/length(accuracy),
            meanRT = mean(rt, na.rm = TRUE))


################## savedata for the next processing stage #####################
library(feather)

write_feather(bigdf, "bigdf.feather")

################# allocating money #######################

unique(bigdf$sona)


