library(tidyverse)
library(lubridate)
library(RPostgreSQL)
library(DBI)
library(lme4)
library(mgcv)
library(splines)
library(zoo)

source("./helpers.R")
source("./get_data.R")
target_years <- c(2023:2025)

# load data
conn <- pg_connect()
player_proj <- get_projections(conn)
primary_tour <- get_primary_tour(conn)
auto_qual <- get_major_qualifiers(conn)
rounds <- get_rounds(conn)
events <- get_events(conn)
players <- get_players(conn)
dbDisconnect(conn)

# models
mod_promotion_prob <- readRDS("models/promotion_prob.rds")
mod_attrition_prob <- readRDS("models/attrition_prob.rds")

majors <- rounds %>% 
  filter(year == min(target_years) - 1) %>%
  filter(str_detect(event_name, "Masters|U.S. Open|Open Championship|PGA Championship")) %>%
  distinct(tour, year, event_id, event_name, dg_id) %>%
  group_by(event_id, event_name) %>%
  summarise(
    participants = n()
  ) %>%
  ungroup() %>%
  mutate(
    cut_line = ifelse(event_id == 14, 50, 70)
  )

# any kft players last year
kft_players <- player_proj %>%
  inner_join(
    primary_tour %>%
    filter(year == min(target_years - 1)) %>%
    filter(primary_tour != 'pga') %>%
      distinct(dg_id)
  )

# any pga players last two years
pga_players <- player_proj %>%
  inner_join(
    primary_tour %>%
      filter(year >= min(target_years) - 2 & year != min(target_years)) %>%
      filter(primary_tour == 'pga') %>%
      distinct(dg_id)
  )


kft_players <- kft_players %>%
  filter(!dg_id %in% pga_players$dg_id)

kft_players$promotion_prob <- predict(
  mod_promotion_prob,
  kft_players %>% select(dg_id, projection_year, age = future_age, mean_latent_skill = pred_latent_skill),
  type = "response"
)

pga_players$attrition_prob <- predict(
  mod_attrition_prob,
  pga_players %>% select(dg_id, projection_year, age = future_age, mean_latent_skill = pred_latent_skill),
  type = "response"
)
pga_players$attrition_prob <- as.numeric(pga_players$attrition_prob)

# will take the auto qualifiers and n next-best based on skill projections (where n = limit - auto qualified)
# 90 
# top 50 players and ties make cut at Augusta
# top 70 players and ties make cut otherwise
get_tour_players <- function(kft_players, pga_players, target_year, n_sims){
  
  promoted_players <- kft_players %>%
    filter(projection_year == target_year) %>%
    crossing(
      sim = c(1:n_sims)
    ) %>%
    rowwise() %>%
    mutate(
      promotion = sample(c(0,1), size = 1, prob = c((1-promotion_prob), promotion_prob))
    ) %>%
    filter(promotion == 1) %>%
    distinct(dg_id, sim)
  
  retained_players <- pga_players %>%
    filter(projection_year == target_year) %>%
    crossing(
      sim = c(1:n_sims)
    ) %>%
    rowwise() %>%
    mutate(
      attrition = sample(c(0,1), size = 1, prob = c((1-attrition_prob), attrition_prob))
    ) %>%
    filter(attrition == 0) %>%
    distinct(dg_id, sim)
  
  players <- bind_rows(promoted_players, retained_players)
  
  return(players)
  
}

sims = 10000
all_results <- tibble()
for(year in target_years){
# for(year in c(2023)){
  tictoc::tic()
  message(str_glue("simulating {year} majors..."))
  
  # get players likely to be on the tour in a given year/sim
  tour_players <- get_tour_players(
    kft_players = kft_players, pga_players = pga_players, target_year = year, n_sims = sims
  )
  
  # get skill projections for the target year
  projections <- player_proj %>%
    filter(projection_year == target_year)
  
  for(event_id in unique(majors$event_id)){
  # for(event_id in c(14)){
    
    # set max participants and cut line
    participants <- ifelse(event_id==14, 90, 156)
    cut_line <- ifelse(event_id == 14, 50, 70)
    
    # get the automatic qualifiers
    initial_field <- auto_qual %>%
      filter(future_year == target_year) %>%
      {
        if(event_id == 14)
          filter(., masters == 1)
        else if(event_id == 26)
          filter(., us == 1)
        else if(event_id == 33)
          filter(., pga == 1)
        else
          filter(., open == 1)
      } %>%
      mutate(auto = 1) %>%
      distinct(dg_id, auto) %>%
      crossing(
        sim = 1:sims
      )
    
    # number of auto qualified players
    n_auto <- length(unique(initial_field$dg_id))
    
    # initial field is auto qualified plus next best n players, where n = participants - n_auto
    fields <- tour_players %>%
      anti_join(distinct(initial_field, dg_id)) %>%
      bind_rows(
        initial_field
      ) %>%
      distinct(dg_id,auto, sim)%>%
      inner_join(projections) %>%
      rowwise() %>%
      mutate(
        skill = rnorm(1, mean = pred_latent_skill, sd = sd_latent_skill)
      ) %>%
      ungroup() %>%
      group_by(auto, sim) %>%
      arrange(skill) %>%
      mutate(rank = row_number()) %>%
      mutate(
        in_field = ifelse(auto == 1 | (is.na(auto) & rank <= participants -  n_auto), 1, 0)
        ) %>%
      ungroup() %>%
      filter(in_field == 1)
    
    # simulate performance in each round
    # top n players (where n = cut_line) make cut, plus ties
    results <- fields %>%
      rowwise() %>%
      mutate(
        round_1 = rnorm(n=1, skill, sd = sd_round),
        round_2 = rnorm(n=1, skill, sd = sd_round),
        round_3 = rnorm(n=1, skill, sd = sd_round),
        round_4 = rnorm(n=1, skill, sd = sd_round),
        pre_cut = round_1 + round_2
      ) %>%
      ungroup() %>%
      group_by(sim) %>%
      filter(min_rank(round(pre_cut)) <= cut_line)%>%
      mutate(
        total = round_1 + round_2 + round_3 + round_4
      ) %>%
      arrange(total) %>%
      mutate(finish = row_number())
    
    # summarise results
    results <- results %>%
      group_by(dg_id) %>%
      summarise(
        win = sum(ifelse(finish == 1, 1, 0))/sims,
        t5 = sum(ifelse(finish <= 5, 1, 0))/sims,
        t10 = sum(ifelse(finish <= 10, 1, 0))/sims,
        made_cut = n()/sims
      ) %>%
      ungroup() %>%
      mutate(
        year = year,
        event_id = event_id
      )
    
    all_results <- bind_rows(all_results, results)
    
  }
  tictoc::toc()
}

all_results_final <- all_results %>%
  left_join(
    players %>%
      distinct(dg_player_name, dg_id)
  ) %>%
  left_join(
    majors %>% distinct(event_id, event_name)
  ) %>%
  arrange(-win)

conn <- pg_connect()
dbWriteTable(
  conn,
  SQL("gold.major_projections"),
  all_results %>%
    mutate(last_updated = lubridate::now()),
  append = F,
  row.names = F,
  overwrite = T
)
dbDisconnect(conn)




