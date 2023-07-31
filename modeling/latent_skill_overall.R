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

# impute age
rounds <- rounds %>%
  group_by(tour, year) %>%
  mutate(
    imp_age = ifelse(is.na(age),1,0),
    age = coalesce(age, quantile(age, 0.5, na.rm=T))
  ) %>%
  ungroup()

test <- rounds %>% filter(year != 2022)
train <- rounds %>% filter(year < 2022)

# (1) Golfer Skill Estimates (within sample)
mod_ls <- lmer(
  data = train,
  adj_sg_total ~ bs(age) + (1 | dg_id)
)

# use conditional variance to create sampling distributions from for future years
#ranefs <- ranef(mod_ls, condVar = TRUE)
# data.frame(ranefs)

saveRDS(mod_ls, "models/latent_skill_overall.rds")

# (2) future latent skill
train$latent_skill <- predict(mod_ls, train)

# should have some rtm here
calculate_ma <- function(data, time_window){
  z <- zoo::zoo(data$adj_sg_total, data$date)
  # g <- zoo(,seq(start(z), end(z), "day"))
  # zm <- merge(z,g)
  results <- zoo::rollapplyr(z, width = time_window, FUN = function(x) mean(x, na.rm=T), partial = TRUE, align = "right", fill = NA)
  tibble(date = time(results),
         ma = as.numeric(results))
}

# moving averages (by round, not date; ~60 rounds in 1 year)
mas <- train %>%
  group_by(dg_id) %>%
  arrange(date) %>%
  nest() %>%
  mutate(
    ma_1 = purrr::map(data, ~calculate_ma(.,15)),
    # ma_90 = purrr::map(data, ~calculate_ma(.,90)),
    ma_2 = purrr::map(data, ~calculate_ma(.,30)),
    ma_3 = purrr::map(data, ~calculate_ma(.,60))
    # ma_500 = purrr::map(data, ~calculate_ma(.,500)),
  )

mas <- mas %>%
  dplyr::select(dg_id, starts_with('ma')) %>%
  unnest(c(ma_1, ma_2, ma_3), names_sep = "_") %>%
  dplyr::select(
    dg_id, date = ma_1_date, ma_1 = ma_1_ma, ma_2 = ma_2_ma, ma_3 = ma_3_ma
  )

mas %>%
  filter(dg_id == 19195) %>%
  ggplot() +
  geom_line(aes(x=date, y = ma_1), color = "blue") +
  geom_line(aes(x=date, y = ma_2), color = "red") + 
  geom_line(aes(x=date, y = ma_3), color = "green")

# summarize within year
years <- train %>%
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

# get the last observation for each player in a year, add to yearly summary
years <- mas %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(dg_id, year) %>%
  arrange(date) %>%
  slice_tail(n=1) %>%
  ungroup() %>%
  left_join(years) %>%
  rename(last_round = date)

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

# drop anyone with missing data in following year, calculate differences between ma and skill estimate
model_df <- years %>% 
  group_by(year) %>%
  mutate(wt = normalize(rounds)) %>%
  drop_na(resid_skill_next) %>%
  mutate(
    diff_1 = ma_1 - mean_latent_skill,
    diff_2 = ma_2 - mean_latent_skill,
    diff_3 = ma_3 - mean_latent_skill
  )

# a. model residual based on recent performance
# - how much will skill change from this baseline based on recent performance
# - & use the original model with aging curve to predict future year
mod_ls_resid <- lm(
  data = model_df,
  resid_skill_next ~ resid_skill + diff_1 + diff_2 + diff_3,
)

summary(mod_ls_resid)

# b. model actual adjusted sg based on recent performance and average skill estimate within season
mod_ls_next <- lm(
  data = model_df,
  mean_adj_sg_next ~ mean_latent_skill + ma_1 + ma_2 + ma_3,
  weights = rounds
)

summary(mod_ls_next)

saveRDS(mod_ls, "models/latent_skill_residual.RDS")

# Next steps:
# - test on held out data (both approaches)
# - fit with 2022 included
# - write historical predictions to db

# then aging curves for:
# - sg skills
# - other skills
# - model to adjust baseline for changes in other skills

# then:
# work on tournament predictions

