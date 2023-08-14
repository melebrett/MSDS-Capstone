# adjust strokes gained in each round (similar to dg model https://datagolf.com/predictive-model-methodology & openWAR https://arxiv.org/abs/1312.7158)
# - estimate effects of individual golfer, tour and event/round/year
# - adjusted strokes gained is the residual (with random effects removed) between actual strokes gained and predicted strokes gained
#   - interpreted as strokes gained relative to average golfer

library(tidyverse)
library(lubridate)
library(RPostgreSQL)
library(DBI)
library(odbc)
library(RODBC)
library(lme4)

source("./helpers.R")
source("./get_data.R")

conn <- pg_connect()
rounds <- get_rounds(conn)
events <- get_events(conn)
players <- get_players(conn)
dbDisconnect(conn)

# features
rounds <- rounds %>% filter(round_score > 0) # remove bad event (zurich match play)
rounds <- rounds %>%
  left_join(events, by = c("event_id", "year" = "calendar_year", "tour")) %>% 
  mutate(
    event_year = paste(year, event_id, sep = "_"),
    round_score_ou = round_score - course_par,
    across(c(starts_with('sg_')), ~.*-1),
    date = date + (round_num - 1)
  )

# build models to adjust raw score (calculate adjusted strokes gained)
# - adjusted strokes gained defined as strokes below average tour golfer
all_estimates <- tibble()
adj_sg_mods <- list()
for(yr in unique(rounds$year)){
  
  mod <- lmer(
    data = rounds %>% filter(year == yr),
    round_score_ou ~ (1|dg_id) + round_num*event_year + tour
  )
  
  estimates <- rounds %>%
    filter(year == yr) %>%
    mutate(b_event_round_tour = predict(mod, re.form = NA)) %>%
    dplyr::select(
      year, tour, event_year, dg_id, round_num, b_event_round_tour
    )
  
  all_estimates <- bind_rows(all_estimates, estimates)
  adj_sg_mods[[as.character(yr)]] <- mod
  
}

final <- rounds %>%
  left_join(all_estimates) %>%
  mutate(
    adj_sg_total = round_score_ou - b_event_round_tour
  ) %>%
  dplyr::select(
    year, tour, event_id, year, dg_id, round_num, adj_sg_total
  )

conn <- pg_connect()
dbWriteTable(
  conn,
  SQL("gold.adjusted_strokes_gained"),
  final %>%
    mutate(last_updated = lubridate::now()),
  append = F,
  row.names = F,
  overwrite = T
)
dbDisconnect(conn)

for(i in names(adj_sg_mods)){
  write_rds(adj_sg_mods$i, str_glue("models/adj_sg_{i}.rds"))
}


# SG Components (putt, around green, approach, off tee)
all_component_estimates <- tibble()
adj_component_mods <- list()
for(yr in unique(rounds$year)){
  
  mod_putt <- lmer(
    data = rounds %>% filter(year == yr) %>% drop_na(sg_putt),
    sg_putt ~ (1|dg_id) + round_num*event_year
  )
  
  mod_arg <- lmer(
    data = rounds %>% filter(year == yr) %>% drop_na(sg_arg),
    sg_arg ~ (1|dg_id) + round_num*event_year
  )
  
  mod_app <- lmer(
    data = rounds %>% filter(year == yr) %>% drop_na(sg_app),
    sg_app ~ (1|dg_id) + round_num*event_year
  )
  
  mod_off_tee <- lmer(
    data = rounds %>% filter(year == yr) %>% drop_na(sg_off_tee),
    sg_off_tee ~ (1|dg_id) + round_num*event_year
  )
  
  estimates <- rounds %>%
    drop_na(sg_putt) %>%
    filter(year == yr) %>%
    mutate(putt_b_event_round = predict(mod_putt, re.form = NA),
           arg_b_event_round = predict(mod_arg, re.form = NA),
           app_b_event_round = predict(mod_app, re.form = NA),
           ott_b_event_round = predict(mod_off_tee, re.form = NA)) %>%
    dplyr::select(
      year, tour, event_year, dg_id, round_num, ends_with('event_round')
    )
  
  all_component_estimates <- bind_rows(all_component_estimates, estimates)
  # adj_component_mods[[as.character(yr)]] <- mod
  
}

final_components <- rounds %>%
  drop_na(sg_off_tee) %>%
  left_join(all_component_estimates) %>%
  mutate(
    adj_sg_putt = sg_putt - putt_b_event_round,
    adj_sg_arg = sg_arg - arg_b_event_round,
    adj_sg_app = sg_app - app_b_event_round,
    adj_sg_ott = sg_off_tee - ott_b_event_round
  ) %>%
  dplyr::select(
    year, tour, event_id, year, dg_id, round_num, starts_with('adj')
  )

conn <- pg_connect()
dbWriteTable(
  conn,
  SQL("gold.adjusted_strokes_gained_components"),
  final_components %>%
    mutate(last_updated = lubridate::now()),
  append = F,
  row.names = F,
  overwrite = T
)
dbDisconnect(conn)
