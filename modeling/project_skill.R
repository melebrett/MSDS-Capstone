# forecast skill in each of the next 5 seasons (years)
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
mod_ls_ac <- readRDS("models/aging_curve.rds")
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

# primary tour played in each year
primary_tour <- rounds %>%
  group_by(dg_id, year) %>%
  summarise(
    pga = mean(ifelse(tour == 'pga',1,0))
  ) %>%
  mutate(
    primary_tour = ifelse(pga > 0.5, 'pga', 'kft')
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

# add skill prediction
rounds$latent_skill <- predict_ls(mod_ls_current, rounds)

# calculate residual standard deviation for each golfer (w/ empirical bayes)

# rounds %>%
#   filter(tour == 'pga') %>%
#   group_by(year, dg_id) %>%
#   tally() %>%
#   ungroup() %>%
#   mutate(
#     n_ = quantile(n, 0.6975)
#   )

resid_sd <- rounds %>%
  group_by(year, tour) %>%
  mutate(
    resid_sd_prior = sd(adj_sg_total - latent_skill)
  ) %>%
  group_by(dg_id, year) %>%
  summarise(
    rounds = n(),
    resid_sd = sd(adj_sg_total - latent_skill),
    resid_sd_prior = mean(resid_sd_prior)
  ) %>%
  ungroup() %>%
  mutate(
    # prior_rounds = quantile(rounds, 0.67),
    resid_sd = (resid_sd*rounds + resid_sd_prior*40)/(rounds+40)
  ) %>%
  arrange(resid_sd) %>%
  dplyr::select(dg_id, year, resid_sd)

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

# if a player doesn't have data in the current year, take their most recent year
years <- years %>%
  group_by(dg_id) %>%
  filter(year >= target_year - 2) %>%
  filter(year == max(year)) %>%
  mutate(
    year_offset = ifelse(year == target_year - 2, 1, 0),
    year = target_year - 1
  )

# build projection table
player_proj <- primary_tour %>%
  filter(year >= target_year - 2) %>%
  group_by(dg_id) %>%
  arrange(-year) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  dplyr::select(dg_id, primary_tour)

imp_age <- rounds %>%
  group_by(tour, year) %>%
  summarise(
    imp_age = median(age)
  )

# start with next year
# calculate age in current years and projection year
player_proj <- player_proj %>%
  left_join(players %>% dplyr::select(dg_id, birthdate)) %>%
  mutate(
    year = target_year - 1,
    projection_year = target_year
  ) %>%
  left_join(
    imp_age,
    by = c("primary_tour" = "tour", "year")
  ) %>%
  mutate(
    current_age = lubridate::time_length(interval(birthdate, paste(year,"-04-01",sep = "")), unit ="years"),
    current_age = coalesce(current_age, imp_age),
    future_age = current_age + (projection_year - year)
  ) %>%
  dplyr::select(-imp_age)

# add performance
player_proj <- player_proj %>%
  left_join(years)

# add aging effect
age_effect <- function(proj_df){
  
  current <- predict(
    mod_ls_ac,
    proj_df %>%
      mutate(current_age = current_age - year_offset) %>%
      dplyr::select(age = current_age),
    re.form = NA
  )
  future <- predict(
    mod_ls_ac,
    proj_df %>%
      dplyr::select(age = future_age),
    re.form = NA
  )
  
  future - current
}

# update ls estimate with age effect & predict next year
player_proj$delta_ls <- age_effect(player_proj)
player_proj$mean_latent_skill <- player_proj$mean_latent_skill + player_proj$delta_ls
player_proj$pred_latent_skill <- predict(mod_ls_future, player_proj)

# add variance estimates ([1] latent skill variance & [2] round by round variance)
ranefs <- ranef(mod_ls_current, condVar = TRUE)
ls_variation <- data.frame(ranefs)
ls_variation <- ls_variation[ls_variation[, "term"] == "(Intercept)", c("grp","condsd")]
ls_variation$grp <- as.integer(as.character(ls_variation$grp))

resid_sd <- resid_sd %>%
  group_by(dg_id) %>%
  filter(year >= target_year - 2) %>%
  filter(year == max(year)) %>%
  mutate(year = target_year - 1)

player_proj1 <- player_proj %>%
  left_join(ls_variation, by = c("dg_id" = "grp")) %>%
  left_join(resid_sd) %>%
  dplyr::select(dg_id, primary_tour, birthdate, year, projection_year,year_offset, current_age, future_age, pred_latent_skill, sd_latent_skill = condsd, sd_round = resid_sd) %>%
  arrange(pred_latent_skill)


avg_sd_round <- mean(resid_sd$resid_sd, na.rm=T)
player_proj2 <- tibble()
for(fy in 2:5){
  
  tmp <- player_proj1 %>%
    mutate(
      projection_year = year + fy,
      future_age = current_age + fy
    )
  
  tmp$delta_ls <- age_effect(tmp)
  tmp$pred_latent_skill <- tmp$pred_latent_skill + tmp$delta_ls
  tmp$sd_latent_skill <- tmp$sd_latent_skill*(.075*(fy-1) + 1) # add noise to ls prediction in each year
  tmp$sd_round <- tmp$sd_round*(1-(0.075 * (fy-1))) + avg_sd_round*(0.075 * (fy-1)) # same thing but regressing to mean progressively each year
  
  tmp <- tmp %>% 
    dplyr::select(dg_id, primary_tour, birthdate, year, projection_year,year_offset, current_age, future_age, pred_latent_skill, sd_latent_skill, sd_round)

  player_proj2 <- bind_rows(player_proj2, tmp)
}

player_proj_final <- bind_rows(player_proj1, player_proj2)

conn <- pg_connect()
dbWriteTable(
  conn,
  SQL("gold.skill_projections"),
  player_proj_final %>%
    mutate(last_updated = lubridate::now()),
  append = F,
  row.names = F,
  overwrite = T
)
dbDisconnect(conn)
  
