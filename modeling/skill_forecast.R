# forecast skill in each of the next 3 seasons (years)
# - include estimates for variance/sd

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
target_year <- 2023

# load data
conn <- pg_connect()
rounds <- get_rounds(conn)
adj_sg <- get_adj_sg(conn)
events <- get_events(conn)
players <- get_players(conn)
dbDisconnect(conn)

# load models
mod_ls_current <- readRDS("models/latent_skill.rds")
mod_ls_future <- readRDS("models/latent_skill_next.rds")
# mod_ac_putt <- readRDS("models/aging_curve_putt.rds")
# mod_ac_arg <- readRDS("models/aging_curve_arg.rds")
# mod_ac_app <- readRDS("models/aging_curve_app.rds")
# mod_ac_ott <- readRDS("models/aging_curve_ott.rds")

# prep data
rounds <- rounds %>%
  filter(round_score > 0 & year < target_year) %>% # remove bad event (zurich match play)
  left_join(events, by = c("event_id", "year" = "calendar_year", "tour")) %>% 
  mutate(
    event_year = paste(year, event_id, sep = "_"),
    round_score_ou = round_score - course_par,
    across(c(starts_with('sg_')), ~.*-1),
    date = date + (round_num - 1)
  )

# add adj sg
rounds <- rounds %>%
  left_join(adj_sg)

# add age & impute
rounds <- rounds %>%
  left_join(players %>% dplyr::select(dg_id, birthdate)) %>%
  mutate(
    age = lubridate::time_length(interval(birthdate, date), unit ="years")
  ) %>%
  group_by(tour, year) %>%
  mutate(
    imp_age = ifelse(is.na(age),1,0),
    age = coalesce(age, quantile(age, 0.5, na.rm=T))
  ) %>%
  ungroup()

rounds$latent_skill <- predict(mod_ls_current, rounds)

# moving averages (by round, not date; ~60 rounds in 1 year)
mas <- rounds %>%
  group_by(dg_id) %>%
  arrange(date) %>%
  nest() %>%
  mutate(
    ma_1 = purrr::map(data, ~calculate_ma(.,15)),
    ma_2 = purrr::map(data, ~calculate_ma(.,30)),
    ma_3 = purrr::map(data, ~calculate_ma(.,60))
  )

mas <- mas %>%
  dplyr::select(dg_id, starts_with('ma')) %>%
  unnest(c(ma_1, ma_2, ma_3), names_sep = "_") %>%
  dplyr::select(
    dg_id, date = ma_1_date, ma_1 = ma_1_ma, ma_2 = ma_2_ma, ma_3 = ma_3_ma
  )

# get the last observation for each player in a year
mas <- mas %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(dg_id, year) %>%
  arrange(date) %>%
  slice_tail(n=1) %>%
  ungroup()

# summarize within year
years <- rounds %>%
  group_by(dg_id, year) %>%
  summarise(
    rounds = n(),
    mean_adj_sg = mean(adj_sg_total),
    mean_latent_skill = mean(latent_skill)
  )

# add ma to yearly summary
years <- years %>%
  left_join(mas) %>%
  rename(last_round = date)

# add aging effect
age <- rounds %>%
  group_by(dg_id, year, birthdate) %>%
  summarise(
    age = mean(age),
    imp_age = min(imp_age)
  ) %>%
  ungroup() %>%
  mutate(
    age_next = ifelse(imp_age == 1, age, age + 1)
  )

age$delta_ls <- predict(mod_ls_final,age %>% dplyr::select(age = age_next), re.form = NA) - predict(mod_ls_final,age %>% dplyr::select(age), re.form = NA)

model_df <- model_df %>%
  left_join(age %>% distinct(dg_id, year, delta_ls)) %>%
  mutate(mean_latent_skill = mean_latent_skill + delta_ls)





  