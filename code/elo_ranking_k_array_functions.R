#==========================================
# Title:  elo_ranking_k_array_functions.R
# Author: David Jansen
# Archie lab; University of Notre Dame
# david.awam.jansen@gmail.com OR djansen@nd.edu
#==========================================


## Some of these functions are based on the EloRating package in R.
## See Albers and de Vries, 2001 for details
## They were adapted to work for the ABRP data. 

## The following function calculates the probability of a win
## Here Wc and Lc are respectively the current score of the winner and looser.
get_probrabilities <- function (Wc, Lc, normprob = TRUE, fac = NULL) {
  zscore <- (Wc - Lc)/(200 * sqrt(2))
  p_win <- pnorm(zscore)
  return(p_win)
}

## The functions that calulate the new elo score for winner and looser
calculate_elo_score <- function(Wc, Lc, Wk = set_Wk, Lk = set_Lk) {
  Wn <- as.numeric(Wc + Wk*(1 - get_probrabilities(Wc, Lc)))
  Ln <- as.numeric(Lc + Lk*(0 - get_probrabilities(Lc, Wc)))
  return(c(Wn = Wn, Ln = Ln))
}

## The actual new Elo scores are calculated with the following function
get_elo_score <- function(W, L, focal_sex, focal_rnkdate, focal_grp, calculation_step, set_Wk, set_Lk) {
  print(paste("Step =", calculation_step, W, L, sep = " "))
  startvalue_used = FALSE
  
  ## create indexes
  W_xx <- which(elo_ranks$grp == focal_grp
                & elo_ranks$rnkdate == focal_rnkdate
                & elo_ranks$sname == W)
  
  W_lxx <- which(latest_elo_score$grp == focal_grp
                 & latest_elo_score$sname == W)
  
  L_xx <- which(elo_ranks$grp == focal_grp
                & elo_ranks$rnkdate == focal_rnkdate
                & elo_ranks$sname == L)
  
  L_lxx <- which(latest_elo_score$grp == focal_grp
                 & latest_elo_score$sname == L)
  
  ## Winner part
  ### Does winner already have an eloscore in the group?
  if (!is.na(latest_elo_score[W_lxx, 'elo_score'])) {
    Wc = latest_elo_score[W_lxx, 'elo_score']
    Wc_message <- NA_character_
  } else { ## if not get a starting value
    print("A start value for W is being established")
    startvalue_used = TRUE
    startvalue = get_startvalue(W, focal_sex, focal_rnkdate, focal_grp, "bottom")
    Wc = startvalue$startvalue
    
    Wc_message <- startvalue$message
    
    elo_ranks[W_xx, "notes"] <<- Wc_message
    latest_elo_score[W_lxx, "notes"] <<- Wc_message
  }
  
  
  ## looser part
  if (!is.na(latest_elo_score[L_lxx, 'elo_score'])) {
    Lc = latest_elo_score[L_lxx, 'elo_score']
    Lc_message <- NA_character_
  } else {
    startvalue_used = TRUE
    startvalue = get_startvalue(L, focal_sex, focal_rnkdate, focal_grp, "bottom")
    Lc = startvalue$startvalue
    
    Lc_message <- startvalue$message
    
    elo_ranks[L_xx, "notes"] <<- Wc_message
    latest_elo_score[L_lxx, "notes"] <<- Lc_message
  }
  
  update_score <-
    calculate_elo_score(as.numeric(Wc), as.numeric(Lc), set_Wk, set_Lk, Wac, Lac)
  
  elo_ranks[W_xx, "elo_score"] <<- update_score["Wn"]
  latest_elo_score[latest_elo_score$sname == W &
                     latest_elo_score$grp == focal_grp, "elo_score"] <<-
    update_score["Wn"]
  
  elo_ranks[L_xx, "elo_score"] <<- update_score["Ln"]
  latest_elo_score[L_lxx, "elo_score"] <<- update_score["Ln"]
  
  step1[calculation_step, "Wc"] <<-  Wc
  step1[calculation_step, "Wn"] <<-  update_score["Wn"]
  step1[calculation_step, "Wmessage"] <<-  Wc_message
  step1[calculation_step, "Lc"] <<-  Lc
  step1[calculation_step, "Ln"] <<-  update_score["Ln"]
  step1[calculation_step, "Lmessage"] <<-  Lc_message
  #print(paste("Wn = ", update_score["Wn"], "with k = ", set_k))
  
  if (startvalue_used == TRUE)
    return("A value was assigned")
  return("Updated")
}


## if an individual has not lived in group before it needs a starting value
get_startvalue <- function(focal_sname, focal_sex, focal_rnkdate, focal_grp, method) {
  
  ## There are a few reasons an individual will not  have an eloscore
  # 1. The individual was born in the rnk month(give lowest elo score)
  # 2. Fission or fusion of the old group (relative elo score based on  old group)
  # 3. New group because of dispersal (Males only) assign elo socre above Sub Adult males
  
  if(sum(!is.na(group_startvalues[group_startvalues$grp == focal_grp, "elo_startvalue"])) == 0) {
    if(focal_grp == 1.211) {
      previous_scores <- latest_elo_score %>% 
        filter(grp %in% c(1.21, 2.2)) 
    } else {
      previous_scores <- latest_elo_score %>% 
        filter(grp == c(group_history_l %>%
                          filter(grp == focal_grp) %>% 
                          pull(from_group)))
    }
    
    focal_grp_start_values <- elo_ranks %>% 
      filter(grp == focal_grp & rnkdate == focal_rnkdate) %>% 
      left_join(select(previous_scores, sname, previous_elo_score = elo_score)) %>% 
      inner_join(select(biograph_l, sname, sex, birth)) %>% 
      arrange(sex, -previous_elo_score, birth) %>% 
      mutate(has_previous_elo_score = !is.na(previous_elo_score)) %>% 
      group_by(sex, has_previous_elo_score) %>% 
      mutate(relative_old_rank = seq(1:n()))  %>% 
      mutate(relative_old_rank = if_else(has_previous_elo_score, relative_old_rank, 99L)) %>% 
      group_by(sex) %>% 
      nest() %>% 
      mutate(relative_old_elo = map(data, 
                                    ~ EloRating::createstartvalues(
                                      ranks = .x$relative_old_rank)$res, 
                                    shape=set_shape,
                                    startvalue = set_startvalue, 
                                    k = set_start_k)) %>% 
      unnest(cols = c(data, relative_old_elo)) %>% 
      select(sname, grp, sex, relative_old_elo) %>% 
      arrange(sname)
    
    
    group_startvalues[group_startvalues$grp == focal_grp, "elo_startvalue"] <<-  focal_grp_start_values$relative_old_elo
  }
  
  if(focal_sname %in% group_startvalues$sname[group_startvalues$grp == focal_grp] |
     focal_sname %in% first_dates$sname[first_dates$matgrp == focal_grp]){
    
    if(focal_sname %in% group_startvalues$sname[group_startvalues$grp == focal_grp]) {
      return(tibble(
        startvalue = group_startvalues$elo_startvalue[group_startvalues$grp == focal_grp & group_startvalues$sname == focal_sname],
        message = paste0(focal_sname, " was given a relative rank based on previous group ",
                         group_startvalues$elo_startvalue[group_startvalues$grp == focal_grp & group_startvalues$sname == focal_sname])))
    } else if(between(first_dates$first_date[first_dates$sname == focal_sname], focal_rnkdate, focal_rnkdate + months(1)-days(1))) {
      l_xx <- which(latest_elo_score$grp == focal_grp 
                    & !is.na(latest_elo_score$elo_score))
      
      s_xx <- which(group_startvalues$grp == focal_grp 
                    & !is.na(group_startvalues$elo_startvalue))
      
      if(length(l_xx) > 0) {
        min_elo_score <- min(latest_elo_score[l_xx, "elo_score"])
        return(tibble(
          startvalue = min_elo_score ,
          message = paste0(focal_sname, " was born and placed at the bottom of the ranking with an elo score of ",
                           min_elo_score)))
      } else {
        min_elo_score <- min(group_startvalues[s_xx, "elo_startvalue"])
        return(tibble(
          startvalue = min_elo_score,
          message = paste0(id, " was born in group gap and placed at the bottom of the ranking with an elo score of ",
                           min_elo_score)))
      }} ## end if born
  } else {
    ## Should only happen to males?
    if(focal_sex == "F") {
      print(paste("This is a funny female ", focal_sname,  "that lived in grp", focal_grp, "on", focal_rnkdate, sep = " ")) 
      
      female_start_value <- latest_elo_score %>% 
        filter(sex  == "F") %>% 
        left_join(select(maturedates_l, sname, matured)) %>% 
        mutate(age_class = case_when(is.na(matured) ~ "juvenile",
                                     focal_rnkdate < matured  ~ "juvenile",
                                     focal_rnkdate >= matured ~ "adult")) %>% 
        arrange(age_class, -elo_score) %>% 
        inner_join(elo_ranks %>% 
                     filter(grp == focal_grp 
                            & rnkdate == focal_rnkdate) %>% 
                     select(grp, sname) %>% 
                     distinct()) %>% 
        ungroup() %>% 
        filter(age_class == "juvenile" & !is.na(elo_score)) %>% 
        head(1) %>% 
        select(elo_score) %>% 
        pull()
      
      return(tibble(
        startvalue = female_start_value,
        message = paste0(focal_sname, " moved into group (as immigrant female)and was placed at the bottom of the adult females ranking with an elo score of ",
                         female_start_value)))
      
    }
    
    temp_male_values <- latest_elo_score %>% 
      filter(sex  == "M") %>% 
      left_join(select(maturedates_l, sname, matured)) %>% 
      left_join(select(rankdates_l, sname, ranked)) %>% 
      mutate(age_class = case_when(is.na(matured) ~ "juvenile",
                                   focal_rnkdate < matured ~ "juvenile",
                                   focal_rnkdate >= matured & is.na(ranked) ~ "sub-adult",
                                   focal_rnkdate >= matured & focal_rnkdate < ranked ~ "sub-adult",
                                   focal_rnkdate >= ranked ~ "adult" )) %>% 
      mutate(age_class = forcats::fct_relevel(age_class, "juvenile", after = Inf)) %>% 
      arrange(age_class, -elo_score) %>% 
      inner_join(elo_ranks %>% 
                   filter(grp == focal_grp 
                          & rnkdate == focal_rnkdate) %>% 
                   select(grp, sname) %>% 
                   distinct()) %>% 
      ungroup() 
    
    nr_SM <- temp_male_values %>% 
      filter(age_class == "sub-adult" & !is.na(elo_score)) %>% 
      nrow()
    
    nr_juv <- temp_male_values %>% 
      filter(age_class == "juvenile" & !is.na(elo_score)) %>% 
      nrow()
    
    nr_adult <- temp_male_values %>% 
      filter(age_class == "adult" & !is.na(elo_score)) %>% 
      nrow()
    
    if(nr_SM > 0) {
      male_start_value <- temp_male_values %>% 
        filter(age_class == "sub-adult" & !is.na(elo_score)) %>% 
        head(1) %>% 
        select(elo_score) %>% 
        pull()
      
      male_start_message = paste0(focal_sname, " moved into group and was placed at the top of the sub-adult males ranking with an elo score of ", male_start_value)
    } else if(nr_juv > 0) {
      male_start_value <- temp_male_values %>% 
        filter(age_class == "juvenile" & !is.na(elo_score)) %>% 
        head(1) %>% 
        select(elo_score) %>% 
        pull()
      
      male_start_message = paste0(focal_sname, " moved into group and was placed at the top of the juvenile males ranking with an elo score of ", male_start_value, "(No sub-adults present)")
    } else if(nr_adult > 0) {
      male_start_value <- temp_male_values %>% 
        filter(age_class == "adult" & !is.na(elo_score)) %>% 
        tail(1) %>% 
        select(elo_score) %>% 
        pull()
      male_start_message = paste0(focal_sname, " moved into group and was placed at the bottom of the adult males ranking with an elo score of ", male_start_value)
      
    }
    
    
    return(tibble(
      startvalue = male_start_value,
      message = male_start_message))
  } # end of new male
  
}

## Function to get some values to run checks on code
get_test_variables <- function(df) {
  W <<- df$actor
  L <<- df$actee
  focal_sex <<- df$actor_sex
  focal_rnkdate <<- df$rnkdate
  focal_grp <<- df$grp 
  calculation_step <<- df$calculation_step
  print(paste(W, L, focal_sex, focal_rnkdate, focal_grp, calculation_step, sep = " "))
}
