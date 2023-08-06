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

get_rounds <- function(conn){
  dbGetQuery(conn, "select * from gold.rounds")
}

get_adj_sg <- function(conn){
  dbGetQuery(conn, "select * from gold.adjusted_strokes_gained")
}

get_events <- function(conn){
  dbGetQuery(conn, "select calendar_year, date, event_id, tour from gold.events where tour in ('pga','kft')")
}

get_players <- function(conn){
  dbGetQuery(conn,
             "select x.dg_id, x.dg_player_name, b.*
from gold.player_bio b
left join gold.player_xref x on x.espn_id = b.espn_id
where x.dg_id is not null"
  )
}

