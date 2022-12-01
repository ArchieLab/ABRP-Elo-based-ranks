#==========================================
# Title:  elo_ranking_k_array_calculate.R
# Author: David Jansen
# Archie lab; University of Notre Dame
# david.awam.jansen@gmail.com OR djansen@nd.edu
#==========================================

## this script create some functions to generate plots regarding elo ranking
## the are focused on comparing matrix based ranks and elo based ranks

## open libraries
library(broom)
library(cowplot)
library(ggrepel)
library(kableExtra)
library(knitr)
library(lubridate)
library(tidyverse)


## setting some colors for the plots
rnktype_colors <- c("forestgreen", "dodgerblue2")
role_colors <- c("darkgreen", "firebrick1", "steelblue1")
role_shapes <- c(16, 25, 17)
type_colors <- c("grey", "firebrick", "steelblue1")

## setting names for these colors
names(role_colors) <- c("elo score (monthly)",  "L", "W")
names(role_shapes) <- c("elo score (monthly)", "L", "W")
names(type_colors) <- c("expected", "fall", "rise")

## with the 
get_dom_matrix <- function(focal, focal_grp, focal_rnkdate, focal_sex, rank_range) {
  temp <- xdata_behavior %>% 
    filter(grp == focal_grp) %>% 
    mutate(rnkdate = floor_date(date, 'month')) %>%  
    filter(rnkdate == focal_rnkdate) %>% 
    filter(actor_sex == focal_sex & actee_sex == focal_sex)
  
  temp2 <- temp %>% 
    select(grp, rnkdate, actor, actee, iid) %>% 
    full_join(expand.grid(actor = unique(c(temp$actor, temp$actee)), 
                          actee = unique(c(temp$actor, temp$actee))) %>% 
                as_tibble(),
              by = c("actor", "actee")) %>% 
    fill(c(grp, rnkdate)) %>% 
    mutate(previous_month = ymd(rnkdate) - months(1)) %>% 
    inner_join(ranks_l %>% filter(rnktype %in% c("ALF", "ALM")) %>% 
                 select(actor = sname,  grp, 
                        previous_month = rnkdate, actor_rank = rank),
               by = c("actor", "grp", "previous_month")) %>%
    inner_join(ranks_l %>%  filter(rnktype %in% c("ALF", "ALM")) %>% 
                 select(actee = sname, grp = grp, previous_month = rnkdate,
                        actee_rank = rank),
               by = c("actee", "grp", "previous_month")) %>%
    mutate(actor = forcats::fct_reorder(.f = actor, .x =actor_rank, .fun = min)) %>%
    mutate(actee = forcats::fct_reorder(.f = actee, .x =actee_rank, .fun = min)) %>% 
    filter(actor_rank %in% rank_range) %>% 
    filter(actee_rank %in% rank_range) %>% 
    group_by(actor, actee) %>% 
    summarise(nr_interactions = sum(!is.na(iid)), .groups = 'drop')
  
  temp3 <- temp2 %>% 
    mutate(lower = temp2 %>% 
              pivot_wider(names_from = actee, values_from = nr_interactions,
                          values_fill = 0)  %>%
             select(-actor) %>% 
             lower.tri() %>% 
             as_tibble() %>% 
             pivot_longer(names_to = "actee", values_to = "lower", 
                          cols = everything()) %>% 
             select(lower) %>% 
             pull()) %>%
    mutate(nr_interactions = if_else(nr_interactions == 0, NA_integer_,
                                     nr_interactions)) 
  temp3 %>% 
    ggplot(aes(x = actee, 
               y = reorder(actor, desc(actor)), 
                   #fill = nr_interactions,
               label = nr_interactions)) +
      geom_tile(fill = "white") +
      geom_tile(data = temp2 %>%  filter(actor == focal|actee == focal), 
                fill = 'lightgrey') +
      geom_tile(data = temp2 %>%  filter(actor == actee), fill = 'black') +
      geom_label(aes(fill = lower), size =8, color = "black") +
      scale_fill_manual(values = c("white", "red")) +
      scale_x_discrete(position = "top") +
      labs(x = "", y ="", 
           tag = paste(zoo::as.yearmon(ymd(focal_rnkdate), '%B %Y'))) +
      theme(legend.position = "none",
            text = element_text(size = 8),
            plot.tag.position = c(0.2, 0.001)) 
}

rank_matrix_overview <- function(focal, focal_grp, start, end, focal_sex, rank_range) {
  xx <- tibble(focal_rnkdate = seq(ymd(start), ymd(end), by = 'month'),
               focal_grp,
               focal_sex) %>% 
    mutate(matrix = pmap(.l = list(focal, focal_grp, focal_rnkdate,focal_sex),
                         .f = get_dom_matrix, rank_range))
  
  p2 <- cowplot::plot_grid(plotlist = xx$matrix)
  ggsave(filename = './matrix_overview.pdf', 
         plot = p2, 
         device = 'pdf',
         width =11, height = 8.5)
  
  p2          
}


rank_trend_plot <- function(focal, start, end, rankclasses = NA) {
  
  if(sum(!is.na(rankclasses)) == 0) stop(paste('Provide the desired rankclasses'))
  #   print('Provide the desired rankclasses')
  #   } else {
  df <-  step4_long %>% 
    filter(sname %in% focal &
             between(rnkdate, ymd(start), ymd(end))) %>% 
    inner_join(select(maturedates_l, sname, matured)) %>% 
    mutate(rank_offset = case_when(rnktype == 'ALF' ~ .1,
                                   rnktype == 'ALM' ~  .1,
                                   rnktype == 'elo_rank' ~  .05,
                                   rnktype == 'elo_adult_rank' ~ - .05,
                                   rnktype == 'ADF' ~ - .1,
                                   rnktype == 'ADM' ~  - .1,
                                   TRUE ~ 0)) %>% 
    filter(!is.na(rank_value)) %>% 
    mutate(rank_value = as.integer(rank_value))
  
  df_p1 <- df %>% filter(rnktype %in% rankclasses) %>% 
    inner_join(maturedates_l)
  
  
  p1 <- 
    ggplot(data = df_p1 , aes(x=rnkdate, y = rank_value, 
                                  color = rnktype, shape = rnktype)) +
      geom_point() +
      geom_line() +
      geom_vline(aes(xintercept = matured), color = "blue", linetype = "dashed") +
      scale_y_reverse(limits = c(plyr::round_any(max(df_p1$rank_value), 
                                                 accuracy = 10,
                                                 f = ceiling),
                                 plyr::round_any(min(df_p1$rank_value), 
                                                 accuracy = 10, 
                                                 f = floor) + 1),
                      label = scales::label_number(accuracy = 1)) +
      scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
      theme_classic() +
      #scale_color_manual(values = rnktype_colors) + 
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            legend.position =  "right",
            legend.title = element_blank()) +
      labs(x ="", y = "Ordinal rank") 
  
  p2_data = step1 %>%
    filter((actor %in% focal | actee %in% focal),
           between(rnkdate, ymd(start), ymd(end))) %>%
    mutate(test = row_number()) %>%
    mutate(role = if_else(focal == actor, "W", "L"),
           score_c = if_else(focal == actor, Wc, Lc),
           score_n = if_else(focal == actor, Wn, Ln)) %>%
    mutate(dx = 0,
           dy = score_n - score_c) %>%
    mutate(rnktype = "Interactions") 
    
    p2_data <- p2_data %>% 
      mutate(type = case_when(Wc > Lc ~ "expected", 
                              Wc < Lc & actor == focal ~ 'rise',
                              Wc < Lc & actee == focal ~ 'fall'))  %>% 
      mutate(dyad = paste0(actor, "-", actee)) %>% 
      mutate(dyad = if_else(type == 'expected', NA_character_, dyad))
    
    p2 <- p2_data %>% 
      inner_join(df %>%
                 filter(rnktype == 'elo_score') %>%
                 select(rnkdate, elo_score = rank_value)) %>%
      ggplot(aes(x = date, xmin = date, xmax = date,
                 y = score_c, ymin = score_c, ymax = score_n,
                 group = interaction(date, test))) +
      geom_vline(xintercept = seq(ymd(start), ymd(end), by = 'month'), 
                 linetype = 'dashed', alpha = 0.2, size = .5) +
      geom_text(data = p2_data %>%  filter(!is.na(dyad)),
                       aes(label = dyad),
                   size = 2,
                   hjust = 1,
                   vjust = 1)+
      geom_point(position = position_dodge(.5)) +
      geom_point(aes(y = score_n, fill = role,  color = role, shape = role),
                 position = position_dodge(.5)) +
      geom_linerange(aes(color = role), position = position_dodge(.5)) +
      scale_color_manual(values = role_colors) +
      scale_fill_manual(values = role_colors)+
      scale_shape_manual(values = role_shapes) +
      theme_classic() +
      #facet_wrap(. ~ rnktype) +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            legend.title = element_blank(),
            legend.position =  "right")  +
      scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
      labs(x ="", y = "Elo scores") 
    
    plots <- cowplot::plot_grid(p1, p2, ncol =1, rel_heights = c(0.3, 1) )
    title_gg <- ggplot() + 
      labs(title = paste0(focal, " between ",  start, " and ", end))
    
    plot_grid(title_gg, plots, ncol = 1, rel_heights = c(0.1, 1))
}

rank_overview_plot <- function(focal, focal_grp, start, end, 
                               rankclasses = NA,
                               rank_range = NA) {
  plotdata <- step4_long %>% 
      filter(grp == focal_grp 
             & between(rnkdate, ymd(start), ymd(end)) 
             & rnktype %in% rankclasses
             ) %>% 
    mutate(rnktype2= case_when(rnktype == "ADM" ~ "Matrix-based adult ranks",
                              rnktype == "ALM" ~ "Matrix-based overall ranks",
                              rnktype == "elo_adult_rank" ~ "Matrix-based adult ranks",
                              rnktype == "elo_rank" ~ "Matrix-based overall ranks",
                              TRUE ~ "Elo score")) %>% 
    filter(!is.na(rank_value)) %>% 
    droplevels() %>% 
    mutate(is_focal = sname == focal) %>% 
    droplevels() %>% 
    arrange(rnkdate, sname, rnktype, rnktype2) 
  
  selected_snames <- plotdata %>% 
    filter(!str_detect(rnktype, "elo")) %>% 
    filter(rank_value %in% rank_range) %>% 
    pull(sname)
  
  plotdata <- plotdata %>% 
    filter(sname %in% selected_snames) %>% 
    mutate(sname_label = if_else(str_detect(rnktype, "elo"), NA_character_, sname)) %>%     
    mutate(rank_value = if_else(str_detect(rnktype, "score"),
                                rank_value * -1,
                                rank_value))
  
  first_plotdata <- plotdata %>% 
    group_by(sname, rnktype) %>% 
    filter(rnkdate == min(rnkdate)) %>% 
    rename(first_rnkdate = rnkdate)
  
  
p1 <-  plotdata %>%
    ggplot(aes(x = rnkdate, y = rank_value, color = sname, size = is_focal)) +
    geom_point(size = 2) +
    geom_line() +
    geom_label(data = first_plotdata, 
              aes(label = sname, 
                  x = first_rnkdate, 
                  fill = sname), 
              size = 2, 
              color = 'black') +
    scale_y_reverse() +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y", 
                 limits =c(ymd(start) - days(5), ymd(end))) +
    facet_wrap(. ~ rnktype, scales = 'free_y', nrow = 1) +
    theme_classic() +
    scale_size_manual(values = c(1, 1.2)) +
    scale_alpha_manual(values = c(.2, 1)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          legend.position = "none") +
    labs(x ="", y = "Ordinal rank")

ggsave(filename = './rank_overview.pdf', plot = p1, device = 'pdf',width =11, height = 8.5)

p1
}

rank_zoom_plot <- function(focal, focal_grp, start, end) {
  # focal_acts <- xdata_behavior %>% 
  #   select(grp, actor, actee, date, act) %>% 
  #   filter(actor == focal | actee == focal) %>% 
  #   filter(date >= start &
  #            date <= end &
  #            grp == focal_grp) %>% 
  #   mutate(to_rnkdate = date - months(1),
  #          rnkdate = floor_date(date, 'month'))  %>% 
  #   inner_join(ranks_l %>% 
  #                filter(rnktype %in% rankclasses) %>% 
  #                select(grp, rnkdate, actor = sname, actor_rank = rank)) %>% 
  #   inner_join(ranks_l %>% 
  #                filter(rnktype %in% rankclasses) %>% 
  #                select(grp, rnkdate, actee = sname, actee_rank = rank)) %>% 
  #   mutate(type = case_when(actor_rank < actee_rank ~ "expected", 
  #                           actor_rank > actee_rank & actor == focal ~ 'rise',
  #                           actor_rank > actee_rank & actee == focal ~ 'fall')) %>% 
  #   mutate(dyad = paste0(actor, "-", actee))  
  
  df <-  step4_long %>% 
    filter(grp %in% focal_grp &
             between(rnkdate, ymd(start), ymd(end))) %>% 
    inner_join(select(maturedates_l, sname, matured)) %>% 
    filter(!is.na(rank_value)) %>% 
    mutate(rank_value = as.integer(rank_value)) %>% 
    filter(!str_detect(rnktype, 'elo'))
  
  p1_data <- step1 %>%
    filter((actor %in% focal | actee %in% focal),
           between(rnkdate, ymd(start), ymd("2020-08-01"))) %>%
   mutate(role = if_else(focal == actor, "W", "L"),
           score_c = if_else(focal == actor, Wc, Lc),
           score_n = if_else(focal == actor, Wn, Ln)) %>%
    mutate(dx = 0,
           dy = score_n - score_c) %>%
    inner_join(df %>%
                 filter(!str_detect(rnktype, 'elo')) %>%
                 select(actor = sname,grp, rnkdate, actor_rank = rank_value))  %>% 
    inner_join(df %>%
                 filter(!str_detect(rnktype, 'elo')) %>%
                 select(actee = sname,grp, rnkdate, actee_rank = rank_value)) %>% 
    mutate(type = case_when(Wc > Lc ~ "expected", 
                          Wc < Lc & actor == focal ~ 'rise',
                          Wc < Lc & actee == focal ~ 'fall')) %>% 
    mutate(dyad = paste0(actor, "-", actee)) %>% 
    mutate(dyad = if_else(type == 'expected', NA_character_, dyad))
  
p1 <-   p1_data %>% 
    ggplot(aes(x=actor_rank, y = actee_rank, color = type, label = dyad)) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
          legend.position = "none") +
      #facet_wrap(. ~ rnkdate) +
      scale_color_discrete(guide = "none") +
      geom_point() +
      geom_text_repel(color = 'black', size = 2) +
    labs(x ="Rank of actor", y = "Rank of actee")
  # 
  # 
  # 
  # p1 <- focal_acts %>% 
  #   #filter(actor_rank > actee_rank) %>% 
  #   ggplot(aes(x=actor_rank, y = actee_rank, color = type, label = dyad)) +
  #   geom_text_repel(data = focal_acts %>% filter(type != 'expected'),size = 2) +
  #   geom_point() +
  #   #xlim(c(1, 30)) +
  #   #scale_x_reverse(limits = c(30, 1)) +
  #   scale_y_reverse() +
  #   facet_wrap(. ~ rnkdate) +
  #   scale_color_manual(values = type_colors) +
  #   theme_classic() +
  #   theme(axis.text.x = element_text(angle = 60, hjust = 1),
  #         legend.position = "none") +
  #   labs(x ="Rank of actor", y = "Rank of actee")
  
  title_gg <- ggplot() + 
    labs(title = paste0(focal, " between ",  start, " and ", end))
  
  plot_grid(title_gg, p1, ncol = 1, rel_heights = c(0.05, 1))
}



# #load(paste("./data/data_for_elo_ranking_", current_date,".RData", sep = ""))
# elo_ranks <- read_csv("./data/elo_ranks_21NOV22.csv")
# 
# elo_ranks %>% 
#   inner_join(select(biograph_l, sname, birth)) %>% 
#   filter(!is.na(elo_score)) %>% 
#   mutate(age = as.numeric(rnkdate - floor_date(birth, 'month'))/365.25) %>% 
#   ggplot(aes(x=age, y = elo_score)) + geom_point() +
#     geom_smooth()
# 
# 
# # # step1_k100 <- read_csv("./data/step1_Wk_100_Lk_100.csv")
# # # step3_k100 <- read_csv("./data/step3_Wk_100_Lk_100.csv")
# # # ranks_l <- read_csv("./data/ranks_l.csv")
# # # load('./data/data_for_elo_rank_calculations.RData ')
# # # step1 <- step1_k100 
# # 
# # 
# # elo_ranks <- read_csv("./data/elo_ranks_Wk_100_Lk_100_06JUN22.csv")
# # 
# # elo_ranks

# step4_wide <- step3 %>% 
#   mutate(is_adult = if_else(is.na(is_adult), FALSE, is_adult)) %>% 
#   select(sex, grp, rnkdate, sname, is_adult, elo_score = elo_score2, elo_rank, elo_adult_rank) %>% 
#   left_join(ranks_l %>% 
#               select(grp, rnkdate, sname, rnktype, rank) %>% 
#               pivot_wider(names_from = rnktype, values_from = rank)) 
# 
# step4_long <- step4_wide %>% 
#   pivot_longer(names_to = "rnktype", values_to = "rank_value", elo_score:ALM)
# 
# 
# 
