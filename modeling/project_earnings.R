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
target_year <- 2023

conn <- pg_connect()
player_proj <- get_projections(conn)
primary_tour <- get_primary_tour(conn)
auto_qual <- get_major_qualifiers(conn)
rounds <- get_rounds(conn)
events <- get_events(conn)
players <- get_players(conn)
winnings <- get_winnings(conn)
dbDisconnect(conn)

# models
mod_promotion_prob <- readRDS("models/promotion_prob.rds")
mod_attrition_prob <- readRDS("models/attrition_prob.rds")
mod_earnings <- readRDS("models/earnings.rds")

# prep data
finishes <- rounds %>%
  filter(tour == 'pga' & !fin_text %in% c("CUT", "WD", "DQ", "MDF")) %>%
  mutate(
    fin = as.integer(str_remove(fin_text,"T"))
  ) %>%
  group_by(year, tour, season, event_id, dg_id, player_name, fin_text, fin) %>%
  summarise(
    score = sum(round_score)
  ) %>%
  group_by(year, tour, season, event_id) %>%
  arrange(score) %>%
  mutate(
    fin_order = row_number()
  )

payouts <- winnings %>%
  group_by(calendar_year, season, event_name, dg_event_id, dg_event_name) %>%
  arrange(-money) %>%
  mutate(
    fin_order = row_number()
  ) %>%
  distinct(season, dg_event_id, fin_order, money) %>%
  ungroup()

money <- finishes %>%
  left_join(
    payouts, by = c("event_id" = "dg_event_id", "season", "fin_order")
  )

money <- money %>% 
  group_by(season, tour, dg_id) %>%
  summarise(total_money = sum(money, na.rm = T)) %>%
  group_by(season, tour) %>%
  mutate(season_pool = sum(total_money)) %>%
  ungroup()

base_pool <- unique(money[money$season == target_year - 5, ]$season_pool)

# inflation adjustment
money <- money %>%
  dplyr::select(dg_id, season, total_money, season_pool) %>%
  mutate(
    total_money_pct = total_money/season_pool,
    total_money_real = total_money*(base_pool/season_pool)
  )

# add promotion & attrition
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
kft_players$promotion_prob <- as.numeric(kft_players$promotion_prob)

pga_players$attrition_prob <- predict(
  mod_attrition_prob,
  pga_players %>% select(dg_id, projection_year, age = future_age, mean_latent_skill = pred_latent_skill),
  type = "response"
)
pga_players$attrition_prob <- as.numeric(pga_players$attrition_prob)

# predict real earnings
kft_players$pred_earnings <- as.numeric(
  predict(
    mod_earnings,
    kft_players %>% rename(age = future_age, mean_latent_skill = pred_latent_skill),
    type = 'response'
  )
)

pga_players$pred_earnings <- as.numeric(
  predict(
    mod_earnings,
    pga_players %>% rename(age = future_age, mean_latent_skill = pred_latent_skill),
    type = 'response'
  )
)

# multiply earnings by tour probability (pga = 1-attrition_prob, kft = promotion_prob)
kft_players$pred_earnings <- kft_players$pred_earnings * kft_players$promotion_prob
pga_players$pred_earnings <- pga_players$pred_earnings * (1-pga_players$attrition_prob)

# bind kft and pga players
earnings_proj <- bind_rows(
  pga_players %>% 
    dplyr::select(
      dg_id, primary_tour, year, projection_year, future_age, pred_earnings
    ),
  
  kft_players %>%
    dplyr::select(
      dg_id, primary_tour, year, projection_year, future_age, pred_earnings
    )
)

# get the total pool in each year
season_pools <- money %>%
  distinct(
    year = season, season_pool 
  )

# calculate average inflation rate
pool_infl <- season_pools %>%
  filter(year != 2020 & year != target_year) %>%
  arrange(year) %>%
  mutate(
    infl = season_pool/lag(season_pool)
  ) %>%
  summarise(
    infl = mean(infl, na.rm= T)
  ) %>%
  pull(infl)

# assume inflation will slow a bit going forward
pool_infl <- ((pool_infl-1)*0.75)

# calculate nominal earnings: pred_earnings*(current_pool/base_pool)
# add expected inflation: nominal earnings * (1+ (inflation rate * number of years out))
# normalize earnings (individual earnings must add up to the size of the total pool)
earnings_proj_final <- earnings_proj %>%
  left_join(season_pools) %>%
  mutate(
    base_pool = base_pool,
    pred_earnings_real = pred_earnings,
    pred_earnings = pred_earnings*(season_pool/base_pool),
    pred_earnings = pred_earnings*(1+(projection_year - year)*pool_infl),
    season_pool = season_pool * (1+(projection_year - year)*pool_infl)
  ) %>%
  group_by(projection_year) %>%
  mutate(
    pred_earnings = (pred_earnings/sum(pred_earnings))*season_pool,
    pred_earnings_real = (pred_earnings_real/sum(pred_earnings_real))*base_pool,
    pred_earnings_pct = pred_earnings/season_pool,
    pred_earnings_real_pct = pred_earnings_real/base_pool
  ) %>%
  arrange(projection_year, -pred_earnings)

conn <- pg_connect()
dbWriteTable(
  conn,
  SQL("gold.earnings_projections"),
  earnings_proj_final %>%
    mutate(last_updated = lubridate::now()),
  append = F,
  row.names = F,
  overwrite = T
)
dbDisconnect(conn)

