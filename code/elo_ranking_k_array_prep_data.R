rm(list = ls())
library(lubridate)
library(purrr)
library(tidyverse)

load("./data/data_for_elo_ranking.RData")

## create datasets 
group_history_l <- group_history_l %>% 
  select(grp = gid, from_group, name, permanent, impermanent, cease_to_exist, last_reg_census) %>% 
  filter(grp < 3) %>% 
  group_by(grp) %>% 
  mutate(last_date = min(impermanent, cease_to_exist, last_reg_census, ymd("2021-07-30"), na.rm = TRUE)) %>% 
  ungroup()

xdata_behavior <-actor_actees_l %>% 
  filter(act %in% c("A", "AS", "DS", "OS")) %>% 
  filter(actor_grp == actee_grp) %>% 
  select(iid, grp = actor_grp, date, actor, actee, act) %>% 
  arrange(date, grp, iid) %>% 
  inner_join(select(biograph_l, actor = sname, actor_sex = sex)) %>% 
  inner_join(select(biograph_l, actee = sname, actee_sex = sex)) %>% 
  select(grp, date, iid, actor, actee, act, contains('sex')) %>% 
  inner_join(select(group_history_l, grp, name, permanent, last_date)) %>% ## 837,478 x 11 
  filter((actor_sex == "F" & actee_sex == "F") |
           (actor_sex == "M" & actee_sex == "M")) %>% ## 423,969 x 11 (We loose a lot od data between F and M)
  filter(date >= permanent & date <= last_date) ## 412,462

elo_ranks <- members_l %>% 
  inner_join(select(group_history_l, grp, name, permanent, last_date)) %>% 
  filter(date >= permanent & date <= last_date) %>% 
  filter(date > min(actor_actees_l %>%
                      filter(act %in% c("AS", "DS", "OS")) %>% 
                      select(date) %>%  pull())) %>% 
  inner_join(select(biograph_l, sname, sex)) %>% 
  mutate(rnkdate = floor_date(date, 'month')) %>% 
  group_by(sex, grp, rnkdate, sname) %>% 
  summarise(days_present = n()) %>% 
  arrange(grp,rnkdate, sex, sname) %>% 
  mutate(elo_score = NA_real_,
         notes = NA_character_)

latest_elo_score <- elo_ranks %>% 
  ungroup %>% 
  select(sex, grp, sname) %>% 
  distinct() %>% 
  mutate(elo_score = NA_real_,
         notes = NA_character_)

group_startvalues <- elo_ranks %>% 
  inner_join(xdata_behavior %>% 
               group_by(grp) %>% 
               summarise(first_month = floor_date(min(date), "month"))) %>% 
  filter(rnkdate == first_month) %>% 
  select(grp, sname, elo_startvalue = elo_score) %>% 
  arrange(grp, sname) %>% 
  ungroup()

first_dates <- xdata_behavior %>% 
  select(grp, date, actor, actee) %>% 
  pivot_longer(names_to = "role", values_to = "sname", actor:actee) %>% 
  inner_join(select(biograph_l, sname, birth, matgrp)) %>% 
  filter(matgrp == grp) %>% 
  group_by(sname, matgrp) %>% 
  summarise(birthdate = min(birth),
            first_date = min(date)) %>% 
  ungroup()

## calculate elo
step1 <- xdata_behavior %>% 
  arrange(grp, date, iid) %>% 
  mutate(rnkdate = floor_date(date, 'month')) %>% 
  select(date, rnkdate, grp, actor_sex, actor, actee) %>% 
  mutate(calculation_step = 1:n())

save(group_history_l, xdata_behavior, 
     latest_elo_score, elo_ranks,
     group_startvalues, 
     first_dates,
     biograph_l, 
     maturedates_l, 
     rankdates_l,
     step1,
     file = "./data/data_for_elo_rank_calculations.RData")
     
save(behave_gaps_l,
     group_history_l,
     file = './data/data_groups_history.Rdata')
  
  
  

