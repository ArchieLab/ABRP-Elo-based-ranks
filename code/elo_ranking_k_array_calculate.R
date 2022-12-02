#==========================================
# Title:  elo_ranking_k_array_calculate.R
# Author: David Jansen
# Archie lab; University of Notre Dame
# 01DEC22
# david.awam.jansen@gmail.com OR djansen@nd.edu
#==========================================

## open libraries
rm(list = ls())
library(lubridate)
library(purrr)
library(tidyverse)

## get the data the data was downloaded
current_date <- read_lines('./data/last_update.txt')

## This part is only needed if running as an array on cluster
args <- commandArgs(trailingOnly = TRUE)
kk  = as.integer( args[1] )

## setting some parameters
set_startvalue = 1000
set_shape = 0

## if not run as array use a simple tibble with fix k.
kk = 1
k_values = tibble(Wk = 100, Lk = 100)
set_Wk <- k_values[kk,]$Wk
set_Lk <- k_values[kk,]$Lk

## to analyse the effect of different k  we need a sequences of k_values that are the used in array loop 
## uncomment the part below if you want to run it as an array

#k_values <- tibble(Wk = seq(from = 10, to = 500, by = 10))
#set_Wk <- k_values[kk,]$Wk
#set_Lk <- k_values[kk,]$Lk

## open the functions
#source(file = './code/elo_ranking_k_array_functions.R')
source('elo_ranking_k_array_functions.R')

## load all the data needed for the calculations of elo scores
load(file = paste("./data/data_for_elo_rank_calculations_", current_date,".RData", sep = ""))

## In the next part the actual elo ranks are calculated. 
## This takes a long time. 
## as part of the next part the actual scores get added to step1

step1 <- step1 %>%  mutate(Wc = NA, Wn = NA, Lc = NA, Ln = NA)

step2 <- step1 %>%
  mutate(Wk_value = set_Wk,
         Lk_value = set_Lk) %>% 
  mutate(scores =  
           pmap_chr(.l = list(actor, actee, actor_sex, rnkdate, grp, 
                              calculation_step, Wk_value, Lk_value),
                    .f = get_elo_score))

## in the process of calculating elo scores the step1 and elo_ranks files is updated
## All these files are  saved for later analysis and visualizations

write_rds(step1, 
file = paste0("./data/step1_", current_date, ".rds"))

write_rds(step2, 
          file = paste0("./data/step2_", current_date, ".rds"))

write_csv(elo_ranks,  paste0("./data/elo_ranks_", current_date, ".csv"))


## add matured and ranked dates to determine if an animal is an adult
## for the ranked date a different method has to be considered for future use
## currently it is based on the matrix-based ranks

## There are also some missing values that are for now filled with the 
## previous score of that individual
step3 <- elo_ranks %>%
  inner_join(select(biograph_l, sname, sex)) %>%
  left_join(select(maturedates_l, sname, matured)) %>%
  left_join(select(rankdates_l, sname, ranked)) %>%
  mutate(adult_date = if_else(sex == "F", matured, ranked)) %>% 
  arrange(grp, sname, rnkdate) %>%
  group_by(grp, sname) %>%
  ## if an elo score is missimng use the previous one
  mutate(elo_score2 = if_else(is.na(elo_score), lag(elo_score), elo_score)) %>%
  fill(elo_score2, .direction = "up") %>% 
  select(sex, grp, rnkdate, adult_date, sname, contains('elo_score')) %>%
  group_by(sex, grp, rnkdate) %>%
  arrange(sex, rnkdate, -elo_score2) %>%
  # To assign ordinal rankings for a given month based on continuous Elo scores, we examine all the Elo scores at the end of that month and we order the individuals in the group according to the magnitude of their Elo scores, from the highest Elo score (highest ranking individual, assigned an ordinal score of 1) to lowest Elo score (lowest ranking individual, assigned an ordinal score of N where N is the number of individuals in the hierarchy).
  mutate(elo_rank = 1:n()) %>%
  mutate(is_adult = floor_date(adult_date, 'month') <= rnkdate) %>% 
  ## rank to create adult ranks
  group_by(sex, grp, rnkdate, is_adult) %>% 
  arrange(rnkdate, -elo_score2) %>%
  ## rank
  mutate(elo_adult_rank = 1:n()) %>%
  ## if not adult set adult rank to NA
  mutate(elo_adult_rank = if_else(is_adult, elo_adult_rank, NA_integer_)) %>% 
  ungroup()

## save this final step with all scores and ranks  
write_csv(step3, paste0("./data/step3_", current_date, ".csv"))
