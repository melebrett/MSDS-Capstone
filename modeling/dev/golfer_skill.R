library(tidyverse)
library(lubridate)
library(RPostgreSQL)
library(DBI)
# library(odbc)
# library(RODBC)
library(lme4)
library(mgcv)
library(splines)

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("MSDS_DB"),
  host = Sys.getenv("MSDS_HOST"),
  port = 5432,
  user = Sys.getenv("MSDS_USER"),
  password = Sys.getenv("MSDS_PWD")
)

rounds <- dbGetQuery(conn, "select * from gold.rounds")
adj_sg <- dbGetQuery(conn, "select * from gold.adjusted_strokes_gained")
events <- dbGetQuery(conn, "select calendar_year, date, event_id, tour from gold.events where tour in ('pga','kft')")
players <- dbGetQuery(conn,
                      "select x.dg_id, x.dg_player_name, b.*
from gold.player_bio b
left join gold.player_xref x on x.espn_id = b.espn_id
where x.dg_id is not null"
)

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

# impute
rounds <- rounds %>%
  group_by(tour, year) %>%
  mutate(
    imp_age = ifelse(is.na(age),1,0),
    age = coalesce(age, quantile(age, 0.5, na.rm=T))
    ) %>%
  ungroup()
  

# eda

ggplot(data = rounds %>% filter(tour == 'pga'))  +
  geom_point(aes(x=age, y=adj_sg_total)) +
  geom_smooth(aes(x=age, y=adj_sg_total), se = F,method = "loess")

# (1) Golfer Skill Estimates
mod1 <- lmer(
  data = rounds,
  adj_sg_total ~ bs(age) + (1 | dg_id)
)

tst <- rounds %>%
  filter(player_name == 'McIlroy, Rory') %>%
  distinct(dg_id) %>%
  crossing(age = seq(20,40,0.1))

ggplot() + geom_line(aes(x=tst$age, y = predict(mod1, tst)))
mod_gam <- gam(adj_sg_total ~ s(age, bs = "tp") + s(dg_id, bs = "re"), data = rounds)
summary(mod_gam)
ggplot() + geom_line(aes(x=tst$age, y = predict(mod_gam, tst)))



est_skill <- predict(mod1, rounds)

rounds$latent_skill <- est_skill

rounds %>%
  group_by(dg_id, year) %>%
  summarise(latent_skill = mean(latent_skill)) %>%
  arrange(latent_skill)


# (2) model residual based on recent performance
# how much will skill change from this baseline based on recent performance
rounds %>%
  filter(dg_id == 19195) %>%
  arrange(date) %>%
  mutate(
    ema = TTR::EMA(adj_sg_total*0.75 + latent_skill*0.25, ratio = 0.3)
  ) %>%
  ggplot() +
  geom_smooth(aes(x=date, y = ema)) +
  geom_line(aes(x = date, y = latent_skill)) +
  labs(title = "Latent Skill Estimate vs Adjusted SG Moving Average", subtitle = "John Rahm")

# calculate_ema <- function(x, ema_period){
#   if(length(x) < ema_period){
#     return(TTR::EMA(x, n = length(x) - 1, wilder = TRUE))
#   }else{
#     return(TTR::EMA(x, n = ema_period, wilder = TRUE))
#   }
# }
# 
# 
# rounds_w_ema <- rounds %>%
#   filter(dg_id == 19195) %>%
#   group_by(dg_id) %>%
#   arrange(date) %>%
#   mutate(
#     # sma = TTR::SMA(adj_sg_total*0.75 + latent_skill*0.25, ratio = 0.3)
#     ema50 = calculate_ema(adj_sg_total*0.75 + latent_skill*0.25, ema_period = 50),
#     # ema = coalesce(ema, adj_sg_total*0.75 + latent_skill*0.25)
#   ) %>%
#   ungroup()

calculate_ema <- function(data, time_window){
  # print(data$adj_sg_total)
  # print(data$date)
  obj <- zoo::zoo(data$adj_sg_total*0.75 + data$adj_sg_total*0.25, data$date)
  results <- zoo::rollapplyr(obj, width = time_window, FUN = mean,partial = TRUE, align = "right")
  tibble(date = time(results),
         ema = as.numeric(results))
}

emas <- rounds %>%
  group_by(dg_id) %>%
  arrange(date) %>%
  nest() %>%
  mutate(
    ema_60 = purrr::map(data, ~calculate_ema(.,60)),
    ema_180 = purrr::map(data, ~calculate_ema(.,180)),
    ema_365 = purrr::map(data, ~calculate_ema(.,365)),
  )

emas <- emas %>%
  dplyr::select(dg_id, starts_with('ema')) %>%
  unnest(c(ema_60, ema_180, ema_365), names_sep = "_") %>%
  dplyr::select(
    dg_id, date = ema_60_date, ema_60 = ema_60_ema, ema_180 = ema_180_ema, ema_365 = ema_365_ema
  )

# rounds %>%
#   left_join(emas) %>%

# normally distributed, which is nice. outlier values are players with few rounds
rounds %>%
  group_by(dg_id, year) %>%
  summarise(
    rounds = n(),
    mean_adj_sg = mean(adj_sg_total),
    mean_latent_skill = mean(latent_skill),
    resid_skill = mean(adj_sg_total - latent_skill)
  ) %>%
  ggplot() +
  geom_density(aes(x=resid_skill))

years <- rounds %>%
  group_by(dg_id, year) %>%
  summarise(
    rounds = n(),
    mean_adj_sg = mean(adj_sg_total),
    mean_latent_skill = mean(latent_skill),
    resid_skill = mean(adj_sg_total - latent_skill)
  )

# add next year's residual
years <- years %>%
  group_by(dg_id) %>%
  arrange(year) %>%
  mutate(
    resid_skill_next = lead(resid_skill),
    mean_adj_sg_next = lead(mean_adj_sg)
  )


mas <- emas %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(dg_id, year) %>%
  arrange(date) %>%
  slice_tail(n=1) %>%
  ungroup()


years <- mas %>%
  left_join(years) %>%
  rename(last_round = date)


model_df <- years %>% 
  drop_na(resid_skill_next) %>%
  mutate(
    diff_60 = ema_60 - mean_latent_skill,
    diff_180 = ema_180 - mean_latent_skill,
    diff_365 = ema_365 - mean_latent_skill
  )


mod_resid_next <- lm(
  data = model_df %>% filter(year != 2022),
  resid_skill_next ~ resid_skill + diff_60 + diff_180 + diff_365
)

mod_sg_next <- lm(
  data = model_df %>% filter(year != 2022),
  mean_adj_sg_next ~ mean_latent_skill + ema_60 + ema_365
)

summary(mod_resid_next)
summary(mod_sg_next)

model_df %>% filter(year != 2022) %>% mutate(pred = predict(mod_sg_next)) %>%
  arrange(pred)

years %>% arrange(-mean_latent_skill) %>%
  ggplot() +
  geom_point(aes(x=mean_latent_skill, y = mean_adj_sg))

