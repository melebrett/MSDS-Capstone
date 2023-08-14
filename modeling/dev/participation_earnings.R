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
winnings <- get_winnings(conn)
primary_tour <- get_primary_tour(conn)
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

# add skill prediction
rounds$latent_skill <- predict_ls(mod_ls_current, rounds)

# moving averages (by round, not date; ~60 rounds in 1 year)
mas <- get_mas(rounds)

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

# add variance estimates
ranefs <- ranef(mod_ls_current, condVar = TRUE)
ls_variation <- data.frame(ranefs)
ls_variation <- ls_variation[ls_variation[, "term"] == "(Intercept)", c("grp","condsd")]
ls_variation$grp <- as.integer(as.character(ls_variation$grp))

years <- years %>%
  left_join(ls_variation, by = c("dg_id" = "grp")) %>%
  rename(sd_latent_skill = condsd)

# [1] PROMOTION & [2] ATTRITION
# promotion = kft -> pga
# attrition = pga dropout
kft_players <- primary_tour %>%
  group_by(dg_id) %>%
  arrange(year) %>%
  mutate(primary_tour_next = lead(primary_tour)) %>%
  filter(primary_tour == 'kft') %>%
  mutate(primary_tour_next = coalesce(primary_tour_next, 'kft')) %>%
  ungroup()

pga_players <- primary_tour %>%
  group_by(dg_id) %>%
  arrange(year) %>%
  mutate(primary_tour_next = lead(primary_tour)) %>%
  filter(primary_tour == 'pga') %>%
  ungroup()

age <- rounds %>%
  group_by(dg_id, year) %>%
  summarise(age = mean(age))

kft_players <- kft_players %>%
  left_join(years) %>%
  left_join(age) %>%
  mutate(
    promotion = ifelse(primary_tour_next == 'pga',1,0)
  )

pga_players <- pga_players %>%
  left_join(years) %>%
  left_join(age) %>%
  mutate(
    attrition = ifelse(primary_tour_next != 'pga' | is.na(primary_tour_next),1,0)
  )

mod_promotion_prob <- gam(
  data = kft_players %>% filter(year < 2022),
  promotion ~ s(mean_latent_skill, k = 8) + age,
  family = binomial(link = "logit"),
  select = T
)

# summary(mod_promotion_prob)
# gam.check(mod_promotion_prob)
# plot(mod_promotion_prob)
# tst <- kft_players %>% filter(year < 2022)
# tst$pred <- predict(mod_promotion_prob, tst, type = 'response')
# ggplot(tst) + geom_point(aes(x=mean_latent_skill, y = pred))

mod_attrition_prob <- gam(
  data = pga_players %>% filter(year < 2022),
  attrition ~ s(mean_latent_skill, k= 8) + s(age, k = 5),
  family = binomial(link = "logit"),
  weights = rounds,
  select = T
)

# summary(mod_attrition_prob)
# gam.check(mod_attrition_prob)
# plot(mod_attrition_prob)
# tst <- pga_players %>% filter(year < 2022)
# tst$pred <- predict(mod_attrition_prob, tst, type = 'response')
# ggplot(tst) + geom_point(aes(x=mean_latent_skill, y = pred))

kft_players$promotion_prob <- predict(mod_promotion_prob, kft_players, type = "response")
pga_players$attrition_prob <- predict(mod_attrition_prob, pga_players, type = "response")

ggplot(kft_players) + geom_point(aes(x=mean_latent_skill, y = promotion_prob, color = age))
ggplot(pga_players) + geom_point(aes(x=mean_latent_skill, y = attrition_prob, color = age))+ facet_wrap(.~attrition)

saveRDS(mod_promotion_prob, "models/promotion_prob.rds")
saveRDS(mod_attrition_prob, "models/attrition_prob.rds")

# [3] MAJOR PARTICIPATION (AUTOMATIC QUALIFIERS)
# will take the auto qualifiers and n next-best based on skill projections (where n = limit - auto qualified)
# top 70 players and ties make cut
rounds %>%
  filter(tour == 'pga') %>%
  distinct(year, tour, season, event_id, event_name, dg_id, player_name, fin_text) %>%
  group_by(year, tour, season, event_id, event_name) %>%
  tally() %>%
  left_join(events, by = c("year" = "calendar_year", "event_id", "tour")) %>%
  arrange(date) %>%
  filter(
    str_detect(event_name, "Masters|U.S. Open|Open Championship|PGA Championship") & season == 2022
  )

major_winners <- rounds %>%
  filter(
    str_detect(event_name, "Masters|U.S. Open|Open Championship|PGA Championship")
    & fin_text == '1'
  ) %>%
  distinct(
    tour, year, season, event_name, dg_id, player_name
  )

t12_masters <- rounds %>%
  filter(
    str_detect(event_name, "Masters")
    & (fin_text %in% as.character(c(1:12)) |
         fin_text %in% paste("T",c(1:12), sep = ""))
  ) %>%
  distinct(
    tour, year, season, event_name, dg_id, player_name
  )

t15_pga <- rounds %>%
  filter(
    str_detect(event_name, "PGA Championship")
    & (fin_text %in% as.character(c(1:15)) | fin_text %in% paste("T",c(1:15), sep = ""))
  ) %>%
  distinct(
    tour, year, season, event_name, dg_id, player_name
  )

t10_open <- rounds %>%
  filter(
    str_detect(event_name, "Open Championship")
    & (fin_text %in% as.character(c(1:10)) | fin_text %in% paste("T",c(1:10), sep = ""))
  ) %>%
  distinct(
    tour, year, season, event_name, dg_id, player_name
  )

t10_us <- rounds %>%
  filter(
    str_detect(event_name, "U.S. Open")
    & (fin_text %in% as.character(c(1:10)) | fin_text %in% paste("T",c(1:10), sep = ""))
  ) %>%
  distinct(
    tour, year, season, event_name, dg_id, player_name
  )

players_winners <- rounds %>%
  filter(
    str_detect(event_name, "THE PLAYERS")
    & fin_text == '1'
  ) %>%
  distinct(
    tour, year, season, event_name, dg_id, player_name
  )

t4_other <- rounds %>%
  filter(
    str_detect(event_name, "U.S. Open|Open Championship|PGA Championship")
    & (fin_text %in% as.character(c(1:4)) | fin_text %in% paste("T",c(1:4), sep = ""))
  ) %>%
  distinct(
    tour, year, season, event_name, dg_id, player_name
  )

tour_winners <- rounds %>%
  filter(
    tour == 'pga'
    & !str_detect(event_name, "Puerto Rico|Barracuda|Barbasol|Corales")
    & fin_text == '1'
  ) %>%
  distinct(
    tour, year, season, event_name, dg_id, player_name
  )


major_qualifiers <- bind_rows(

  # all majors next 5 years
  major_winners %>%
    filter(str_detect(event_name, "Masters")) %>%
    crossing(future_year = c(min(season):(max(season)+5))) %>%
    filter(future_year - year <= 5 & future_year - year > 0) %>%
    distinct(dg_id, player_name, future_year) %>%
    crossing(event = c("masters", "open", "pga", "us")) %>%
    mutate(qual = 1),
  
  # same
  major_winners %>%
    filter(str_detect(event_name, "Open Championship")) %>%
    crossing(future_year = c(min(season):(max(season)+5))) %>%
    filter(future_year - year <= 5 & future_year - year > 0) %>%
    distinct(dg_id, player_name, future_year) %>%
    crossing(event = c("masters", "open", "pga", "us")) %>%
    mutate(qual = 1),

  # same
  major_winners %>%
    filter(str_detect(event_name, "PGA Championship")) %>%
    crossing(future_year = c(min(season):(max(season)+5))) %>%
    filter(future_year - year <= 5 & future_year - year > 0) %>%
    distinct(dg_id, player_name, future_year) %>%
    crossing(event = c("masters", "open", "pga", "us")) %>%
    mutate(qual = 1),
  
  # same
  major_winners %>%
    filter(str_detect(event_name, "U.S. Open")) %>%
    crossing(future_year = c(min(season):(max(season)+5))) %>%
    filter(future_year - year <= 5 & future_year - year > 0) %>%
    distinct(dg_id, player_name, future_year) %>%
    crossing(event = c("masters", "open", "pga", "us")) %>%
    mutate(qual = 1),

  # all majors next 3 years
  players_winners %>%
    crossing(future_year = c(min(season):(max(season)+5))) %>%
    filter(future_year - year <= 3 & future_year - year > 0) %>%
    distinct(dg_id, player_name, future_year) %>%
    crossing(event = c("masters", "open", "pga", "us")) %>%
    mutate(qual = 1),
  
  # masters next year
  t12_masters %>%
    crossing(future_year = c(min(season):(max(season)+1))) %>%
    filter(future_year - year == 1) %>%
    distinct(dg_id, player_name, future_year) %>%
    mutate(event = "masters", qual = 1),
  
  # pga next year
  t15_pga %>%
    crossing(future_year = c(min(season):(max(season)+1))) %>%
    filter(future_year - year == 1) %>%
    distinct(dg_id, player_name, future_year) %>%
    mutate(event = "pga", qual = 1),
  
  # open next year
  t10_open %>%
    crossing(future_year = c(min(season):(max(season)+1))) %>%
    filter(future_year - year == 1) %>%
    distinct(dg_id, player_name, future_year) %>%
    mutate(event = "open", qual = 1),
  
  # us open next year
  t10_us %>%
    crossing(future_year = c(min(season):(max(season)+1))) %>%
    filter(future_year - year == 1) %>%
    distinct(dg_id, player_name, future_year) %>%
    mutate(event = "us", qual = 1),
  
  # masters next year
  t4_other %>%
    crossing(future_year = c(min(season):(max(season)+1))) %>%
    filter(future_year - year == 1) %>%
    distinct(dg_id, player_name, future_year) %>%
    mutate(event = "masters", qual = 1),
  
  # masters next year
  tour_winners %>%
    crossing(future_year = c(min(season):(max(season)+1))) %>%
    filter(future_year - year == 1) %>%
    distinct(dg_id, player_name, future_year) %>%
    mutate(event = "masters", qual = 1)
)


major_qualifiers <- major_qualifiers %>%
  group_by(dg_id, player_name, future_year, event) %>%
  summarise_all(~max(.)) %>%
  tidyr::pivot_wider(
    id_cols = c(dg_id, player_name, future_year),
    values_from = qual,
    names_from = event
  )

conn <- pg_connect()
dbWriteTable(
  conn,
  SQL("gold.major_qualifiers"),
  major_qualifiers %>%
    mutate(last_updated = lubridate::now()),
  append = F,
  row.names = F,
  overwrite = T
)
dbDisconnect(conn)

# earnings (regression & normalize)
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

pga_players %>%
  left_join(money, by = c("dg_id", "year" = "season", "primary_tour" = "tour")) %>%
  mutate(total_money = coalesce(total_money, 0)) %>%
  arrange(
    -total_money
  )

# projected earnings each of next 3 seasons (present day $)
money %>%
  filter(total_money_real > 0) %>%
  ggplot() +
  geom_density(aes(x=total_money_real))

model_df <- years %>%
  left_join(
    age
  ) %>%
  left_join(
    money %>% distinct(dg_id, year = season, total_money_real, total_money_pct)
  ) %>%
  mutate(
    total_money_real = coalesce(total_money_real, 0)
  )

train <- model_df %>% filter(year <= target_year - 2)
test <- model_df %>% filter(year == target_year - 1)

mod_earnings1 <- mgcv::gam(
  data = train,
  total_money_real ~ mean_latent_skill + age,
  family = tw()
)

summary(mod_earnings1)
AIC(mod_earnings1)
gam.check(mod_earnings1)
train_preds <- predict(mod_earnings1, type = "response")
test_preds <- predict(mod_earnings1, test, type = "response")

ggplot() +
  geom_point(aes(x = train$total_money_real, y=train_preds)) +
  geom_abline()

ggplot() +
  geom_point(aes(x = test$total_money_real, y=test_preds)) +
  geom_abline()

mod_earnings2 <- mgcv::gam(
  data = train,
  total_money_real ~ s(mean_latent_skill) + age,
  family = tw(),
  select = T
)

AIC(mod_earnings1, mod_earnings2)
gam.check(mod_earnings2)
train_preds <- predict(mod_earnings5, type = "response")
test_preds <- predict(mod_earnings5, test, type = "response")

ggplot() +
  geom_density(aes(x=train_preds, color = "pred")) +
  geom_density(aes(x=train$total_money_real, color = "act"))

ggplot() +
  geom_point(aes(x = train$total_money_real, y=train_preds)) +
  geom_abline()

ggplot() +
  geom_density(aes(x=test_preds, color = "pred")) +
  geom_density(aes(x=test$total_money_real, color = "act"))

ggplot() +
  geom_point(aes(x = test$total_money_real, y=test_preds)) +
  geom_abline()

test$preds <- test_preds

test %>%
  left_join(players) %>%
  dplyr::select(
    dg_player_name, year,age, mean_adj_sg, mean_latent_skill,total_money_real, preds
  ) %>%
  arrange(-preds)

mod_earnings3 <- mgcv::gam(
  data = train,
  total_money_real ~ bs(mean_latent_skill) + sd_latent_skill + age,
  family = tw(),
  select = T
)

summary(mod_earnings3)
AIC(mod_earnings1, mod_earnings2, mod_earnings3)
gam.check(mod_earnings3)
train_preds <- predict(mod_earnings3, type = "response")
test_preds <- predict(mod_earnings3, test, type = "response")

test_preds_norm <- (test_preds/sum(test_preds))*base_pool

ggplot() +
  geom_point(aes(x = train$total_money_real, y=train_preds)) +
  geom_abline()

ggplot() +
  geom_point(aes(x = test$total_money_real, y=test_preds_norm)) +
  geom_abline()

test %>%
  left_join(players) %>%
  dplyr::select(
    dg_player_name, year,age, mean_adj_sg, mean_latent_skill,total_money_real, preds
  ) %>%
  arrange(-preds)

mod_earnings4 <- mgcv::gam(
  data = train,
  total_money_real ~ bs(mean_latent_skill) + sd_latent_skill + age,
  family = tw(),
  select = T,
  method = 'REML'
)

summary(mod_earnings4)
AIC(mod_earnings1, mod_earnings2, mod_earnings3, mod_earnings4)
gam.check(mod_earnings4)
train_preds <- predict(mod_earnings4, type = "response")
test_preds <- predict(mod_earnings4, test, type = "response")

test_preds_norm <- (test_preds/sum(test_preds))*base_pool

ggplot() +
  geom_point(aes(x = train$total_money_real, y=train_preds)) +
  geom_abline()

ggplot() +
  geom_point(aes(x = test$total_money_real, y=test_preds_norm)) +
  geom_abline()

# final model

mod_earnings_final <- mgcv::gam(
  data = bind_rows(train, test),
  total_money_real ~ bs(mean_latent_skill) + sd_latent_skill + age,
  family = tw(),
  select = T,
  method = 'REML'
)

summary(mod_earnings_final)

saveRDS(mod_earnings_final, "models/earnings.rds")

# not going to bother with simulating the PGA tour season otherwise, just going to predict earnings and call it
# too complicated to incorporate all the logic within the timeframe
