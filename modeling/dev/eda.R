library(tidyverse)
library(lubridate)
library(RPostgreSQL)
library(DBI)
library(odbc)
library(RODBC)
library(lme4)

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("MSDS_DB"),
  host = Sys.getenv("MSDS_HOST"),
  port = 5432,
  user = Sys.getenv("MSDS_USER"),
  password = Sys.getenv("MSDS_PWD")
)


rounds <- dbGetQuery(conn, "select * from gold.rounds")
events <- dbGetQuery(conn, "select calendar_year, date, event_id, tour from gold.events where tour in ('pga','kft')")
players <- dbGetQuery(conn,
  "select x.dg_id, x.dg_player_name, b.*
from gold.player_bio b
left join gold.player_xref x on x.espn_id = b.espn_id
where x.dg_id is not null"
)

dbDisconnect(conn)

rounds %>% filter(round_score == 0) %>% distinct(year, event_id)
rounds <- rounds %>% filter(round_score > 0) # remove bad event (zurich match play)
rounds <- rounds %>%
  left_join(events, by = c("event_id", "year" = "calendar_year", "tour")) %>% 
  mutate(
    tour_event_year = paste(tour, year, event_id, sep = "_"),
    round_score_ou = round_score - course_par,
    date = date + (round_num - 1)
  )

#### EDA ####
# dupe check
players %>%
  group_by(dg_id) %>%
  tally() %>%
  filter(n > 1)

# number of events by tour, year
rounds %>%
  distinct(tour, year, event_id) %>%
  group_by(tour, year) %>%
  tally()

# number of rounds by year
rounds %>%
  group_by(tour, year) %>%
  tally()

# number of golfers
rounds %>%
  distinct(dg_id) %>%
  tally()

# Only rounds with shot traking should have SG data:
rounds %>% 
  group_by(tour) %>%
  dplyr::select(tour, year, season, date, event_id, event_name, course_num, course_par, dg_id, round_num, round_score, sg_total) %>%
  summarise_if(is.numeric, ~sum(ifelse(is.na(.),1,0)))

rounds %>% 
  filter(has_sg == TRUE) %>%
  group_by(tour) %>%
  dplyr::select(tour, year, season, date, event_id, event_name, course_num, course_par, dg_id, round_num, round_score, sg_total) %>%
  summarise_if(is.numeric, ~sum(ifelse(is.na(.),1,0)))
  
rounds %>%
  distinct(tour,year, season, event_id, has_sg) %>%
  group_by(tour, year) %>%
  summarise(events = n(),
            events_w_sg = sum(has_sg),
            pct = mean(has_sg))

# multi-course events
rounds %>%
  distinct(year, event_id, tour, course_num) %>%
  group_by(year, event_id, tour) %>%
  tally() %>%
  filter(n > 1)
# only 33, going to assume same course for all events for simplicity

# score by round (expect later rounds to have lower scores)
rounds %>%
  filter(tour == 'pga') %>%
  mutate(round_num = factor(round_num)) %>%
  ggplot() + geom_violin(aes(x=round_num, y = round_score_ou))

# scoring avg. plot by date
rounds %>% 
  filter(has_sg == TRUE) %>%
  filter(player_name == 'Homa, Max') %>%
  ggplot() +
  geom_point(
    aes(x = date, y = round_score_ou)
  )