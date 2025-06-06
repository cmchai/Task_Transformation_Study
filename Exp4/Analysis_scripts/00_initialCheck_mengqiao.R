############ the initial check of belief state stimulus transform data ################# 

### load packages ###

library(here)
library(tidyverse)

### import data and merge them into one file ###

data_path <- here("data")
list_rawdata <- list.files(path = data_path,
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
unique(bigdf$sona)

### check the wholeness of the dataset for each subject ###
ntrials_sub <- bigdf %>%
  group_by(sona, subject) %>%
  summarise(count = n())

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
  facet_wrap(~ subject, nrow = 7)

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


### check if the starting block is counter-balanced ###

start_block <- bigdf %>%
  group_by(subject, start_block) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(start_block) %>%
  summarise(count = n())

### check weird subjects ###

load(file = paste0(here(), "/result/bigdf.Rdata"))

data_subject_sum <- bigdf %>%
  filter(subject == "bacjehbn") %>%
  group_by(block_type, trial_nature) %>%
  summarise(accrate = sum(accuracy)/length(accuracy),
            meanRT = mean(rt, na.rm = TRUE))

data_subject <- bigdf %>%
  filter(subject == "bacjehbn", !is.na(rt)) %>%
  mutate(response_bi = case_when(
    response == "f" ~ 1,
    response == "j" ~ 0,
    TRUE ~ NA_real_
  ))
  
plot(data_subject$response_bi[1:100]) # plot the response sequence

func_accum_acc <- function(win_size, acc_vec) {
  acc_accum <- rep(0, length(acc_vec)); # create the accumulating acc vector
  accum <- win_size;  # the start value of accumulator
  while(accum <= length(acc_vec)){
    acc_vec_aux <- acc_vec[(accum - win_size + 1):accum];
    acc_aux <- sum(acc_vec_aux)/win_size;
    acc_accum[accum] <- acc_aux;
    accum <- accum + 1;
  }
  return(acc_accum)
}

a <- func_accum_acc(10, data_subject$accuracy)

################## savedata for the next processing stage #####################

save(bigdf, file = paste0(here(), "/result/bigdf.Rdata"))

################# allocating money #######################

unique(bigdf$sona)


