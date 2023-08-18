# helper functions to load data
pg_connect <- function(){
  
  conn <- dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("MSDS_DB"),
    host = Sys.getenv("MSDS_HOST"),
    port = 5432,
    user = Sys.getenv("MSDS_USER"),
    password = Sys.getenv("MSDS_PWD")
  )
  
}

get_earnings <- function(conn, expected = F){
  if(expected == F){
    dbGetQuery(conn, "select * from gold.earnings")
  }else{
    dbGetQuery(conn, "select * from gold.earnings_expected")
  }
}

get_rankings <- function(conn){
  dbGetQuery(conn, "select * from gold.rankings")
}

get_events <- function(conn){
  dbGetQuery(conn, "select calendar_year, date, event_id, tour from gold.events where tour in ('pga','kft')")
}

get_rounds <- function(conn){
  
  events <- get_events(conn)
  
  dbGetQuery(conn, "select * from gold.rounds") %>%
    left_join(events, by = c("event_id", "year" = "calendar_year", "tour")) %>% 
    mutate(
      start_date = date,
      event_year = paste(year, event_id, sep = "_"),
      round_score_ou = round_score - course_par,
      across(c(starts_with('sg_')), ~.*-1),
      date = date + (round_num - 1)
    )
}

get_adj_sg <- function(conn, components = F){
  if(components== T){
    dbGetQuery(conn, "select * from gold.adjusted_strokes_gained_components")
  }else{
    dbGetQuery(conn, "select * from gold.adjusted_strokes_gained")
  }
}

get_players <- function(conn){
  dbGetQuery(conn,
             "select x.dg_id, x.dg_player_name, b.*
from gold.player_bio b
left join gold.player_xref x on x.espn_id = b.espn_id
where x.dg_id is not null"
  )
}

get_winnings <- function(conn){
  dbGetQuery(conn,
             "select w.*
, x.dg_event_id, x.dg_event_name, x.calendar_year
from gold.winnings w
left join gold.event_xref x on x.pga_event_name = w.event_name and x.pga_season = w.season"
             )
}

get_primary_tour <- function(conn){
  
  get_rounds(conn) %>%
    mutate(
      tour = ifelse(str_detect(event_name, "Puerto Rico|Barracuda|Barbasol|Corales"), 'kft',tour) # alternate pga events, don't count
    ) %>%
    group_by(dg_id, year) %>%
    summarise(
      pga = mean(ifelse(tour == 'pga',1,0))
    ) %>%
    mutate(
      primary_tour = ifelse(pga > 0.67, 'pga', 'kft')
    )
  
}

get_major_qualifiers <- function(conn){
  dbGetQuery(conn, "select * from gold.major_qualifiers")
}

get_projections <- function(conn, type = "skill"){
  
  if(type == 'skill'){
    dbGetQuery(conn,"select * from gold.skill_projections")
  }else if(type == 'majors'){
    dbGetQuery(conn, "select * from gold.major_projections")
  }else if(type == 'earnings'){
    dbGetQuery(conn, "select * from gold.earnings_projections")
  }else{
    stop(str_glue("invalid projection type: {type}"))
  }
  
}

get_major_perf <- function(rounds){
  rounds %>%
    distinct(event_id, event_name, year, dg_id, fin_text) %>%
    filter(str_detect(event_name, "Masters|U.S. Open|Open Championship|PGA Championship")) %>%
    filter(
      fin_text %in% as.character(c(1:10)) |
        fin_text %in% paste("T",c(1:10), sep = "")) %>%
    mutate(fin = as.integer(str_remove(fin_text, "T")),
           win = ifelse(fin == 1, 1, 0),
           t5 = ifelse(fin <= 5, 1, 0),
           t10 = ifelse(fin <= 10,1,0)
    ) %>%
    group_by(
      dg_id, year
    ) %>%
    summarise(
      major_wins = sum(win),
      major_t5s = sum(t5),
      major_t10s = sum(t10)
    )
}
