# 1. fit aging curves for each of the 4 SG categories (putt, arg, app, ott)
# 2. estimate impact of each sg category on future performance (similar to dg model https://datagolf.com/predictive-model-methodology)
# - ex. SG OTT has stronger signal for future performance than SG PUTT
# - when forecasting into future, give more weight to OTT skill changes

library(tidyverse)
library(lubridate)
library(RPostgreSQL)
library(DBI)
# library(odbc)
# library(RODBC)
library(lme4)
library(mgcv)
library(splines)
library(zoo)
# library(brms)

source("./helpers.R")
source("./get_data.R")

conn <- pg_connect()
rounds <- get_rounds(conn)
adj_sg <- get_adj_sg(conn)
events <- get_events(conn)
players <- get_players(conn)
dbDisconnect(conn)

rounds <- rounds %>% filter(round_score > 0) # remove bad event (zurich match play)
rounds <- rounds %>%
  left_join(events, by = c("event_id", "year" = "calendar_year", "tour")) %>% 
  mutate(
    event_year = paste(year, event_id, sep = "_"),
    round_score_ou = round_score - course_par,
    date = date + (round_num - 1)
  )

# add adj sg
rounds <- rounds %>% left_join(adj_sg)

# add age
rounds <- rounds %>%
  left_join(players %>% dplyr::select(dg_id, birthdate)) %>%
  mutate(
    age = lubridate::time_length(interval(birthdate, date), unit ="years")
  )

# impute age
rounds <- rounds %>%
  group_by(tour, year) %>%
  mutate(
    imp_age = ifelse(is.na(age),1,0),
    age = coalesce(age, quantile(age, 0.5, na.rm=T))
  ) %>%
  ungroup()

# flip the sign
rounds <- rounds %>%
  mutate(
    across(c(starts_with('sg_')), ~.*-1)
  )


# putting
mod_sg_putt <- lmer(
  data = rounds %>% 
    drop_na(sg_putt) %>%
    mutate(round_event_year = paste(round_num, event_year, sep = "_")) %>%
    group_by(round_event_year) %>%
    mutate(mean_sg_putt = mean(sg_putt)),
  sg_putt ~ bs(age, df = 4) + (1|dg_id) + mean_sg_putt
)

summary(mod_sg_putt)

x <- tibble(age = seq(20,40,0.1))
x$mean_sg_putt = 0
x$estimate <- predict(mod_sg_putt, x, re.form = NA)

ggplot(x) + geom_line(aes(x = age, y = estimate))

# arg
mod_sg_arg <- lmer(
  data = rounds %>% 
    drop_na(sg_arg) %>%
    mutate(round_event_year = paste(round_num, event_year, sep = "_")) %>%
    group_by(round_event_year) %>%
    mutate(mean_sg_arg = mean(sg_arg)),
  sg_arg ~ bs(age, df = 4) + (1|dg_id) + mean_sg_arg
)

summary(mod_sg_arg)

x <- tibble(age = seq(20,40,0.1))
x$mean_sg_arg = 0
x$estimate <- predict(mod_sg_arg, x, re.form = NA)

ggplot(x) + geom_line(aes(x = age, y = estimate))

# ott
mod_sg_off_tee <- lmer(
  data = rounds %>% 
    drop_na(sg_off_tee) %>%
    mutate(round_event_year = paste(round_num, event_year, sep = "_")) %>%
    group_by(round_event_year) %>%
    mutate(mean_sg_off_tee = mean(sg_off_tee)),
  sg_off_tee ~ bs(age, df = 4) + (1|dg_id) + mean_sg_off_tee
)

summary(mod_sg_off_tee)

x <- tibble(age = seq(20,40,0.1))
x$mean_sg_off_tee = 0
x$estimate <- predict(mod_sg_off_tee, x, re.form = NA)

ggplot(x) + geom_line(aes(x = age, y = estimate))

# app
mod_sg_app <- lmer(
  data = rounds %>% 
    drop_na(sg_app) %>%
    mutate(round_event_year = paste(round_num, event_year, sep = "_")) %>%
    group_by(round_event_year) %>%
    mutate(mean_sg_app = mean(sg_app)),
  sg_app ~ bs(age, df = 4) + (1|dg_id) + mean_sg_app
)

summary(mod_sg_app)

x <- tibble(age = seq(20,40,0.1))
x$mean_sg_app = 0
x$estimate <- predict(mod_sg_app, x, re.form = NA)

ggplot(x) + geom_line(aes(x = age, y = estimate))

# impact of detailed categories on total sg
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

years <- rounds %>%
  filter(has_sg == T) %>%
  group_by(dg_id, year) %>%
  summarise(
    rounds = n(),
    adj_sg = mean(adj_sg_total),
    sg_putt = mean(sg_putt),
    sg_arg = mean(sg_arg),
    sg_app = mean(sg_app),
    sg_off_tee = mean(sg_off_tee)
  ) %>%
  group_by(dg_id) %>%
  arrange(year) %>%
  mutate(
    adj_sg_next = lead(adj_sg),
    rounds_next = lead(rounds)
  ) %>%
  drop_na(adj_sg_next) %>%
  ungroup() %>%
  mutate(
    wt1 = normalize(rounds),
    wt2 = normalize(rounds_next)
  )

# mod_sg <- lm(
#   data = years,
#   adj_sg ~ sg_putt + sg_arg + sg_app + sg_off_tee,
#   weights = wt1
# )

mod_sg_next <- lm(
  data = years,
  adj_sg_next ~ sg_putt + sg_arg + sg_app + sg_off_tee,
  weights = (wt1 + wt2)/2
)

# summary(mod_sg)
summary(mod_sg_next)

saveRDS(mod_sg_putt, "models/agining_curve_putt.rds")
saveRDS(mod_sg_app, "models/agining_curve_app.rds")
saveRDS(mod_sg_arg, "models/agining_curve_arg.rds")
saveRDS(mod_sg_off_tee, "models/agining_curve_ott.rds")
saveRDS(mod_sg_next, "models/sg_categories_skill_impact.rds")
