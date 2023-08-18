library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(reactable)
library(DT)
library(gt)
library(ztable)
library(ggplot2)
library(DBI)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(lme4)
library(splines)
library(renv)

source("./get_data.R")
source("./helpers.R")
source("./style.R")
mod_skill <- readRDS("./latent_skill.rds")

conn <- pg_connect()
players <- get_players(conn)
players_alt <- dbGetQuery(conn, "select * from gold.players")
rounds <- get_rounds(conn)
events <- get_events(conn)
adj_sg <- get_adj_sg(conn, components = F)
adj_sg_comp <- get_adj_sg(conn, components = T)
rankings <- get_rankings(conn)
skill_proj <- get_projections(conn, "skill")
major_proj <- get_projections(conn, 'majors')
major_perf <- get_major_perf(rounds)
earnings_act <- get_earnings(conn)
earnings_exp <- get_earnings(conn, expected = T)
earnings_proj <- get_projections(conn, "earnings")
dbDisconnect(conn)

rounds$latent_skill <- predict_ls(mod_skill, rounds)
players_alt$name_id <- str_glue("{players_alt$name} ({players_alt$dg_id})")

get_dg_id <- function(name_id){
  players_alt[players_alt$name_id == name_id,]$dg_id
}

source("./plots.R", local = T)
source("./tables.R", local = T)

# - year by year table
#   - historical performance + projections
#   - skill, component skill, earnings, expected earnings, earnings over expected, major finishes, expected major finishes

ui <- navbarPage(
  
  theme = shinytheme("lumen"),
  title = "PGA Forecast",
  # selected = "Main",
  useShinyjs(),
  
  navlistPanel(
    selected = "Rankings",
    widths = c(1,11),
    
    tabPanel(
      "Rankings",
      fluidRow(
        column(width = 1),
        column(
          h3("Current Rankings"),
          h4(str_glue("updated {unique(as.Date(rankings$updated_at))}")),
          align = "left",
          DT::dataTableOutput("tab_rankings_dt_main"),
          width = 10
        ),
        column(width = 1)
      )
    ),
    
    tabPanel(
      "Projections",
      fluidRow(
        column(width = 1),
        column(
          h3("Three Year Projections"),
          align = "left",
          DT::dataTableOutput("tab_proj_dt_main"),
          width = 10
        ),
        column(width = 1)
      )
    ),

    tabPanel(
      "Stats",
      fluidRow(
        column(width = 1),
        column(
          h3("Historical Adjusted Performance"),
          align = "left",
          DT::dataTableOutput("tab_stats_dt_main"),
          width = 10
        ),
        column(width = 1)
      )
    ),

    tabPanel(
      "Earnings",
      fluidRow(
        column(width = 1),
        column(
          h3("Historical Earnings"),
          align = "left",
          DT::dataTableOutput("tab_xearnings_dt_main"),
          width = 10
        ),
        column(width = 1)
      )
    ),

    tabPanel(
      "Players",
      fluidRow(
        column(
          width = 11,
          selectizeInput('player', '', multiple = FALSE, selected = 'Rahm, Jon (19195)', choices = sort(unique(players_alt$name_id))),
          align = "left"
        )
      ),
      fluidRow(
        # column(width = 1),
        column(
          2,
          div(
            h1(textOutput("player_name")),
            h4(textOutput("player_age")),
            h4(textOutput("player_country")),
          ),
          align = 'center'
        ),
        column(
          width = 9,
          align = "left",
          gt::gt_output("tab_proj_summary"),
        )
      ),

      br(),

      fluidRow(
        column(
          width = 6,
          align = 'center',
          plotOutput("plot_skill_proj")
        ),
        column(
          width = 5,
          align = 'center',
          plotOutput("plot_sg_components")
        )
      ),

      br(),

      fluidRow(
        column(
          width = 11,
          align = 'center',
          gt::gt_output("tab_stats_and_proj")
        )
      ),

      br(),

      fluidRow(
        column(
          width = 11,
          align = 'center',
          gt::gt_output("tab_event_history")
        )
      ),

    )
  )
)

server <- function(input,output,session){
  
  output$tab_rankings_dt_main <- renderDataTable({
    tab_rankings_dt_main(rankings = rankings, skill_proj = skill_proj) 
  })
  
  output$tab_proj_dt_main <- renderDataTable({
    tab_proj_dt_main(skill_proj = skill_proj, major_proj = major_proj, earnings_proj = earnings_proj)
  })

  output$tab_stats_dt_main <- renderDataTable({
    tab_stats_dt_main(rounds = rounds, adj_sg = adj_sg, adj_sg_comp = adj_sg_comp)
  })

  output$tab_xearnings_dt_main <- renderDataTable({
    tab_xearnings_dt_main(earnings_exp = earnings_exp, earnings_act = earnings_act)
  })

  output$tab_proj_summary <- render_gt({
    tab_options(tab_proj_summary(skill_proj = skill_proj, id = get_dg_id(input$player)), table.width = pct(100), table.font.size = 'large')
  })

  output$player_name <- renderText({

    last_first <- str_split(players_alt[players_alt$name_id == input$player, ]$name,", ")
    str_glue("{last_first[[1]][2]} {last_first[[1]][1]}")

  })

  output$player_age <- renderText({

    age <- skill_proj %>%
      filter(dg_id == get_dg_id(input$player) & projection_year == lubridate::year(Sys.Date())) %>%
      dplyr::select(future_age, birthdate)

    str_glue("Birthdate: {age$birthdate} ({round(age$future_age,1)})")

  })

  output$player_country <- renderText({
    str_glue("Country: {players_alt[players_alt$name_id == input$player,]$country_code}")
  })

  output$plot_skill_proj <- renderPlot({
    plot_skill_proj(rounds,skill_proj, id = get_dg_id(input$player))
  })

  output$plot_sg_components <- renderPlot({
    plot_sg_components(rounds, adj_sg_comp, id = get_dg_id(input$player))
  })

  output$tab_event_history <- render_gt({
    tab_options(tab_event_history(rounds, adj_sg, adj_sg_comp, id = get_dg_id(input$player)), table.width = pct(100), table.font.size = 'medium')
  })

  output$tab_stats_and_proj <- render_gt({
    tab_options(tab_stats_and_proj(rounds, adj_sg, adj_sg_comp, major_perf, skill_proj, major_proj, earnings_proj, id = get_dg_id(input$player)), table.width = pct(100), table.font.size = 'large')
  })

}

shinyApp(ui = ui, server = server)

