#==========================================
# Title:  elo_ranking_k_array_get_data.R
# Author: David Jansen
# Archie lab; University of Notre Dame
# DEC012022
# david.awam.jansen@gmail.com OR djansen@nd.edu
#==========================================

## The following script uses long-term data from the Amboseli Baboon Research Project (https://amboselibaboons.nd.edu/). Please contact Susan Alberts (alberts@duke.edu) or Elizabeth Archie (earchie@nd.edu) for details or data requests. 

## This script downloads all the data used for this analysis. 

## Open libraries
library(lubridate)
library(RPostgreSQL)
library(purrr)
library(tidyverse)

# connect to database
babase <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "localhost",
  port = 22222,
  user = rstudioapi::askForPassword("Database username"),
  dbname = "babase",
  password = rstudioapi::askForPassword("Database password"))

## get data
## gets currrent data and saves it as a file. 
## This way they lastest version of data can be opened in later steps
current_date <- toupper(format(today(), '%d%b%y'))
write_lines(current_date, './data/last_update.txt', append = FALSE)

actor_actees_l <- tbl(babase, "actor_actees") %>%  collect()
biograph_l <- tbl(babase, "biograph") %>%  collect()
behave_gaps_l <- tbl(babase, "behave_gaps") %>%  collect()
dispersedates_l <- tbl(babase, "dispersedates") %>%  collect()
group_history_l <- tbl(babase, "groups_history") %>%  collect()
parents_l <- tbl(babase, "parents") %>%  collect()
maturedates_l <- tbl(babase, "maturedates") %>%  collect()
rankdates_l <- tbl(babase, "rankdates") %>%  collect()
members_l <- tbl(babase, "members") %>%  collect()
ranks_l <- tbl(babase, "ranks") %>%  collect()

save(actor_actees_l, behave_gaps_l, biograph_l, dispersedates_l, 
     group_history_l,  maturedates_l,  members_l, 
     parents_l, rankdates_l, ranks_l,
     file = paste("./data/data_for_elo_ranking_", current_date,".RData", sep = ""))

## Give overview of what are the most up-to-date rankdata sets. 
tibble(dataset = c(
  "actor_actees",
  "maturedates",
  "rankdates",
  "ranks ADF",
  "ranks ADM"),
  date = c(max(actor_actees_l$date),
           max(maturedates_l$matured),
           max(rankdates_l$ranked),
           max(ranks_l$rnkdate[ranks_l$rnktype == "ADF"]),
           max(ranks_l$rnkdate[ranks_l$rnktype == "ADM"])))






  
  
  



