# 1. estimate "latent skill" of golfers within sample
# - for each year estimate (mean) adjusted strokes gained
# - standard deviation of random effects from this model to be used in predicting tournament outcomes
# - residual standard deviation from this model used this to model round by round variance
# 2. estimate aging curve for skill
# 3. predict skill in following year using average adjusted strokes gained (latent skill, from [1]) in that year while accounting for aging (from [2]) and recent performance

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

rounds <- rounds %>%
  filter(round_score > 0) %>% # remove bad event (zurich match play)
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


train <- rounds %>% filter(year < 2022)
test <- rounds %>% filter(year == 2022)

# (1a) Golfer Skill Estimates (within sample)
mod_ls <- lmer(
  data = train %>%
    group_by(dg_id) %>%
    mutate(
      day = lubridate::time_length(interval(date, max(date)), unit ="days")
    ) %>%
    ungroup(),
  adj_sg_total ~ (1 + ns(day, df = 4) | dg_id),
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 5000))
)

summary(mod_ls)

rahm <- train %>%
  group_by(dg_id) %>%
  mutate(
    day = lubridate::time_length(interval(date, max(date)), unit ="days"),
  ) %>%
  filter(dg_id == 19195)

rahm$pred <- predict(mod_ls, rahm)

ggplot(rahm) + 
  geom_line(aes(x=day, y = pred) , color = "red") + 
  geom_point(aes(x = day, y = adj_sg_total)) +
  theme_bw() +
  labs(title = "Latent Skill Estimate", subtitle = "John Rahm")
# geom_line(aes(x=day, y = pred2), color = "blue")

# 1b Aging Curve
mod_ls_ac1 <- lmer(
  data = train,
  adj_sg_total ~ ns(age, df = 4) + (1| dg_id)
)

mod_ls_ac2 <- lmer(
  data = train,
  adj_sg_total ~ bs(age, df = 4) + (1| dg_id)
)

summary(mod_ls_ac1)
summary(mod_ls_ac2)

AIC(mod_ls_ac1, mod_ls_ac2)

homa <- tibble(dg_id = c(17538)) %>%
  crossing(age = seq(20, 60, by = 0.1))

homa$pred1 <- predict(mod_ls_ac1, homa, re.form = NA)
homa$pred2 <- predict(mod_ls_ac2, homa, re.form = NA)
homa$pred3 <- predict(mod_ls_ac3, homa, re.form = NA)

ggplot(homa) + geom_line(aes(x=age, y = pred1), color = 'red') +
  geom_line(aes(x=age, y = pred2), color = 'blue') +
  geom_line(aes(x=age, y = pred3), color = 'green')

ggplot() + 
  geom_point(data = train %>% filter(year == 2021), aes(x = age, y = adj_sg_total)) +
  geom_line(data = homa, aes(x=age, y=pred2), color = 'red')

# use conditional variance to create sampling distributions from for future years
#ranefs <- ranef(mod_ls, condVar = TRUE)
# data.frame(ranefs)

# test

# summary(mod_ls)
# plot(mod_ls, which = 1)
# qqnorm(resid(mod_ls))
# qqline(resid(mod_ls))
# 
# train$pred <- predict(mod_ls, train)
# test$pred <- predict(mod_ls, test, allow.new.levels = T)
# 
# train_sum <- train %>%
#   group_by(
#     dg_id, year
#   ) %>%
#   summarise(
#     pred = mean(pred),
#     act = mean(adj_sg_total),
#     rounds = n()
#   ) %>%
#   ungroup()
# 
# test_sum <- test %>%
#   group_by(
#     dg_id, year
#   ) %>%
#   summarise(
#     pred = mean(pred),
#     act = mean(adj_sg_total),
#     rounds = n()
#   ) %>%
#   ungroup()
# 
# cor(train_sum$pred, train_sum$act, use = "complete.obs")**2
# cor(test_sum$pred, test_sum$act, use = "complete.obs")**2
# 
# ggplot(data = train_sum %>% filter(rounds > 10)) + geom_point(aes(x=act,y = pred, size = rounds), color = "red") + geom_abline()
# ggplot(data = test_sum %>% filter(rounds > 10)) + geom_point(aes(x=act,y = pred, size = rounds), color = "red") + geom_abline()

# final models (to use on new data)
mod_ls_final <- lmer(
  data = bind_rows(train, test) %>%
    group_by(dg_id) %>%
    mutate(
      day = lubridate::time_length(interval(date, max(date)), unit ="days")
    ) %>%
    ungroup(),
  adj_sg_total ~ (1 + ns(day, df = 4) | dg_id),
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 5000))
)

saveRDS(mod_ls_final, "models/latent_skill.rds")

mod_ls_ac_final <- lmer(
  data = bind_rows(train, test),
  adj_sg_total ~ ns(age, df = 4) + (1| dg_id)
)

saveRDS(mod_ls_ac_final, "models/aging_curve.rds")


age_df <- tibble(age = seq(20,60, by = .5))
age_df$skill <- predict(mod_ls_ac_final, age_df, re.form = NA)

ggplot(age_df) + 
  geom_line(aes(x=age, y = skill)) +
  theme_bw() +
  labs(title = "PGA Aging Curve", y = "Skill (aSG)")


# (2) future latent skill
# should have some rtm here
calculate_ma <- function(data, time_window){
  z <- zoo::zoo(data$adj_sg_total, data$date)
  # g <- zoo(,seq(start(z), end(z), "day"))
  # zm <- merge(z,g)
  results <- zoo::rollapplyr(z, width = time_window, FUN = function(x) mean(x, na.rm=T), partial = TRUE, align = "right", fill = NA)
  tibble(date = time(results),
         ma = as.numeric(results))
}

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}


prep_df <- function(df){
  
  df <- df %>%
    group_by(dg_id) %>%
    mutate(
      day = lubridate::time_length(interval(date, max(date)), unit ="days")
    ) %>%
    ungroup()
    
  
  df$latent_skill <- predict(mod_ls_final, df, allow.new.levels = T)
  
  # moving averages (by round, not date; ~60 rounds in 1 year)
  mas <- df %>%
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
  
  # mas %>%
  #   filter(dg_id == 19195) %>%
  #   ggplot() +
  #   geom_line(aes(x=date, y = ma_1), color = "blue") +
  #   geom_line(aes(x=date, y = ma_2), color = "red") + 
  #   geom_line(aes(x=date, y = ma_3), color = "green")
  
  # summarize within year
  years <- df %>%
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
  
  # drop anyone with missing data in following year, calculate differences between ma and skill estimate
  years %>% 
    group_by(year) %>%
    mutate(wt = normalize(rounds)) %>%
    drop_na(resid_skill_next) %>%
    mutate(
      diff_1 = ma_1 - mean_latent_skill,
      diff_2 = ma_2 - mean_latent_skill,
      diff_3 = ma_3 - mean_latent_skill
    )
}


# a. model residual based on recent performance
# - how much will skill change from this baseline based on recent performance
# - & use the original model with aging curve to predict future year
# model_df <- prep_df(train)

# mod_ls_resid <- lm(
#   data = model_df,
#   resid_skill_next ~ resid_skill + diff_1 + diff_2 + diff_3,
# )
# 
# summary(mod_ls_resid)

# b. model actual adjusted sg based on recent performance and average skill estimate within season
model_df <- prep_df(train)

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

age$delta_ls <- predict(mod_ls_ac_final,age %>% dplyr::select(age = age_next), re.form = NA) - predict(mod_ls_ac_final,age %>% dplyr::select(age), re.form = NA)

model_df <- model_df %>%
  left_join(age %>% distinct(dg_id, year, delta_ls)) %>%
  mutate(mean_latent_skill = mean_latent_skill + delta_ls)

# train
# lm
mod_ls_next1 <- lm(
  data = model_df,
  mean_adj_sg_next ~ mean_latent_skill + ma_1 + ma_2 + ma_3,
  weights = rounds
)

summary(mod_ls_next1)

# gams
mod_ls_next2 <- mgcv::gam(
  data = model_df,
  mean_adj_sg_next ~ s(mean_latent_skill) + ma_1 + ma_2 + ma_3,
  weights = rounds
)

summary(mod_ls_next2)
gam.check(mod_ls_next2)
plot(mod_ls_next2)

mod_ls_next3 <- mgcv::gam(
  data = model_df,
  mean_adj_sg_next ~ s(mean_latent_skill, ma_3) + ma_1 + ma_2,
  weights = rounds
)

summary(mod_ls_next3)
gam.check(mod_ls_next3)
plot(mod_ls_next3)

mod_ls_next4 <- mgcv::gam(
  data = model_df,
  mean_adj_sg_next ~ s(mean_latent_skill, ma_3) + ma_2,
  weights = rounds
)

summary(mod_ls_next4)
gam.check(mod_ls_next4)
plot(mod_ls_next4)

# test
# need this to make sure moving avgs are correct
model_df <- prep_df(bind_rows(train,test))

model_df <- model_df %>%
  left_join(age %>% distinct(dg_id, year, delta_ls)) %>%
  mutate(mean_latent_skill = mean_latent_skill + delta_ls)

# filter for the held out data
model_df_test <- model_df %>%
  filter(year == 2021)

# make prediction
model_df_test$pred1 <- predict(mod_ls_next1, model_df_test)
model_df_test$pred2 <- predict(mod_ls_next2, model_df_test)
model_df_test$pred3 <- predict(mod_ls_next3, model_df_test)
model_df_test$pred4 <- predict(mod_ls_next4, model_df_test)

# r2
cor(model_df_test$mean_adj_sg_next, model_df_test$pred1)**2
cor(model_df_test$mean_adj_sg_next, model_df_test$pred2)**2
cor(model_df_test$mean_adj_sg_next, model_df_test$pred3)**2
cor(model_df_test$mean_adj_sg_next, model_df_test$pred4)**2

model_df_test %>%
  summarise(
    rmse1 = sqrt(mean((mean_adj_sg_next - pred1)**2)),
    rmse2 = sqrt(mean((mean_adj_sg_next - pred2)**2)),
    rmse3 = sqrt(mean((mean_adj_sg_next - pred3)**2)),
    rmse4 = sqrt(mean((mean_adj_sg_next - pred4)**2))
  )


# final models on full dataset

# mod_ls_resid_final <- lm(
#   data = model_df,
#   resid_skill_next ~ resid_skill + diff_1 + diff_2 + diff_3,
# )

mod_ls_next_final <- mgcv::gam(
  data = model_df,
  mean_adj_sg_next ~ s(mean_latent_skill, ma_3) + ma_2,
  weights = rounds
)


# saveRDS(mod_ls_resid_final, "models/latent_skill_residual.rds")
saveRDS(mod_ls_next_final, "models/latent_skill_next.rds")

