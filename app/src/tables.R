# 1. Current rankings, expected rankings
tab_rankings_dt_main <- function(rankings, skill_proj){
  rankings %>%
    mutate(year = lubridate::year(updated_at)) %>%
    left_join(
      skill_proj %>% 
        dplyr::select(dg_id, projection_year, future_age, pred_latent_skill) %>%
        group_by(projection_year) %>%
        arrange(pred_latent_skill) %>%
        mutate(proj_rank = row_number()),
      by = c("year" = "projection_year", "dg_id")
    ) %>%
    # mutate(`+/-` = )
    dplyr::select(player, age = future_age, primary_tour, country, dg_rank, owgr_rank, proj_rank) %>%
    rename_all(~toupper(str_replace(.,"_", " "))) %>%
    DT::datatable(rownames = FALSE, options = table_options, filter = 'bottom') %>%
    formatRound('AGE', digits = 1)
}

# 2. projections (skill, ranking, majors, $) by year
tab_proj_dt_main <- function(skill_proj, major_proj, earnings_proj){
  skill_proj %>%
    dplyr::select(dg_id, current_age, projection_year, proj_skill = pred_latent_skill) %>%
    filter(projection_year <= min(projection_year) + 2) %>%
    tidyr::pivot_wider(
      id_cols = c(dg_id,current_age),
      names_from = projection_year,
      values_from = proj_skill,
      names_prefix = "THCP_"
    ) %>%
    left_join(
      players_alt %>% select(dg_id, player = name, country = country_code)
    ) %>%
    left_join(
      major_proj %>%
        group_by(dg_id) %>%
        summarise(
          across(c(win, t5, t10), ~sum(.))
        )
    ) %>%
    left_join(
      earnings_proj %>%
        filter(projection_year <= min(projection_year) + 2) %>%
        group_by(dg_id) %>%
        summarise(
          earnings = sum(pred_earnings)/1e6
        )
    ) %>%
    rename(
      major_wins = win, major_t5 = t5, major_t10 = t10
    ) %>%
    dplyr::select(player,current_age,country, everything()) %>%
    dplyr::select(-dg_id) %>%
    mutate_if(is.numeric, ~round(.,2)) %>%
    mutate(current_age = round(current_age,1)) %>%
    rename_all(~toupper(str_replace(.,"_", " "))) %>%
    DT::datatable(rownames = FALSE, options = table_options)
}

# 3. historical stats
tab_stats_dt_main <- function(rounds, adj_sg, adj_sg_comp){
  
  rounds %>%
    left_join(
      adj_sg
    ) %>%
    group_by(
      dg_id, season
    ) %>%
    summarise(
      rounds = n(),
      sg = mean(adj_sg_total)
    ) %>%
    left_join(
      rounds %>%
        inner_join(
          adj_sg_comp
        ) %>%
        group_by(
          dg_id, season
        ) %>%
        summarise(
          putt = mean(adj_sg_putt),
          arg = mean(adj_sg_arg),
          app = mean(adj_sg_app),
          ott = mean(adj_sg_ott)
        )
    ) %>%
    group_by(season) %>%
    mutate(
      across(
        c(sg, putt, arg, app, ott),
        ~100*percent_rank(desc(ifelse(rounds > 24,.,NA))),
        .names = "{.col}_n")
    ) %>%
    left_join(
      players_alt %>% select(dg_id, player = name, country = country_code)
    ) %>%
    arrange(sg) %>%
    dplyr::select(player, country, season, everything()) %>% 
    dplyr::select(-dg_id) %>%
    mutate_if(is.numeric,~round(.,2)) %>%
    mutate(
      across(c(ends_with('_n')), ~round(.))
    ) %>%
    rename_all(~toupper(str_replace(.,"_", " "))) %>%
    drop_na(SG) %>%
    DT::datatable(rownames = FALSE, options = table_options, filter = 'bottom') %>%
    formatStyle(
      c('SG N', 'PUTT N', 'ARG N', 'APP N', 'OTT N'),
      backgroundColor = styleInterval(color_gradient(-10,110,5)[[1]], color_gradient(-10,110,5)[[2]]))
}

#4. earnings vs expected
tab_xearnings_dt_main <- function(earnings_exp, earnings_act){
  earnings_act %>%
    left_join(
      earnings_exp %>% dplyr::select(dg_id, year, pred_earnings),
      by = c("dg_id", "season" = "year")
    ) %>%
    left_join(
      players_alt %>% select(dg_id, player = name, country = country_code)
    ) %>%
    mutate(
      earnings = round(earnings/1e6,2),
      pred_earnings = coalesce(round(pred_earnings/1e6,2),0),
      `+/-` = earnings - pred_earnings
    ) %>%
    dplyr::select(
      player, country, season, earnings, x_earnings = pred_earnings, `+/-`
    ) %>%
    arrange(-`+/-`) %>%
    rename_all(~toupper(str_replace(.,"_", " "))) %>%
    DT::datatable(rownames = FALSE, options = table_options, filter = 'bottom') %>%
    formatStyle(
      c('+/-'),
      backgroundColor = styleInterval(color_gradient(-30,30,5)[[1]], color_gradient(-30,30,5)[[2]])
    )
}

# - Summary table of "peak" ranking (lowest ranking in next 3 years), major performance and earnings
tab_proj_summary <- function(skill_proj, id){

  skill_proj %>%
    dplyr::select(dg_id, current_age, projection_year, skill = pred_latent_skill) %>%
    filter(projection_year <= min(projection_year) + 2) %>%
    group_by(projection_year) %>%
    arrange(skill) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    filter(dg_id == id) %>%
    left_join(
      players_alt %>% select(dg_id, player = name, country = country_code)
    ) %>%
    left_join(
      major_proj %>%
        group_by(dg_id, year) %>%
        summarise(
          across(c(win, t5, t10), ~sum(.))
        ),
      by = c("projection_year" = "year", "dg_id")
    ) %>%
    left_join(
      earnings_proj %>%
        filter(projection_year <= min(projection_year) + 2) %>%
        mutate(
          earnings = pred_earnings/1e6
        )
    ) %>%
    arrange(projection_year) %>%
    rename(major_wins = win, major_t5s = t5, major_t10s = t10) %>%
    dplyr::select(projection_year, proj_rank = rank, skill, major_wins, major_t5s, major_t10s, earnings) %>%
    rename_all(~toupper(str_replace(.,"_", " "))) %>%
    mutate_if(is.numeric,~round(.,3)) %>%
    gt() %>%
    gtExtras::gt_theme_pff() %>%
    grand_summary_rows(
      columns = c(starts_with("MAJOR"), EARNINGS),
      fns = list(
        TOTAL ~ sum(.)
      )
    )
}

tab_proj_summary(skill_proj, 19195)

# last 10 events
tab_event_history <- function(rounds, adj_sg, adj_sg_comp, id){
  rounds %>%
    left_join(adj_sg) %>%
    left_join(adj_sg_comp %>% dplyr::select(-last_updated)) %>%
    group_by(year,event_name, start_date, dg_id, fin_text) %>%
    summarise(
      adj_sg = sum(adj_sg_total),
      adj_putt = sum(adj_sg_putt),
      adj_arg = sum(adj_sg_arg),
      adj_app = sum(adj_sg_app),
      adj_ott = sum(adj_sg_ott)
    ) %>% 
    group_by(year, event_name) %>%
    mutate(
      sg_rank = min_rank(adj_sg),
      putt_rank = min_rank(adj_putt),
      arg_rank = min_rank(adj_arg),
      app_rank = min_rank(adj_app),
      ott_rank = min_rank(adj_ott)
    ) %>%
    ungroup() %>%
    filter(dg_id == id) %>%
    arrange(start_date) %>%
    slice_tail(n = 10) %>%
    arrange(desc(start_date)) %>%
    dplyr::select(year, event_name, start_date, fin = fin_text, starts_with('adj'), ends_with('rank')) %>%
    rename_all(~toupper(str_replace(.,"_", " "))) %>%
    mutate(
      across(c(starts_with('adj')),~round(.,1))
    )%>%
    gt() %>%
    gtExtras::gt_theme_pff() %>%
    tab_header(
      title = "",
      subtitle = "Last 10 Events"
    )
}

tab_stats_and_proj <- function(rounds, adj_sg, adj_sg_comp, major_perf, skill_proj, major_proj, earnings_proj, id){
  act <- rounds %>%
    filter(season != 2023) %>%
    left_join(
      adj_sg
    ) %>%
    group_by(
      dg_id, season
    ) %>%
    summarise(
      rounds = n(),
      sg = mean(adj_sg_total)
    ) %>%
    left_join(
      rounds %>%
        inner_join(
          adj_sg_comp
        ) %>%
        group_by(
          dg_id, season
        ) %>%
        summarise(
          putt = mean(adj_sg_putt),
          arg = mean(adj_sg_arg),
          app = mean(adj_sg_app),
          ott = mean(adj_sg_ott)
        )
    ) %>%
    filter(dg_id == id) %>%
    left_join(
      major_perf, by = c("dg_id", "season" = "year")
    ) %>%
    mutate(
      across(c(starts_with('major')), ~coalesce(.,0))
    ) %>%
    left_join(earnings_act[, c("dg_id","season", "earnings")]) %>%
    left_join(earnings_exp[, c("dg_id", "year", "pred_earnings")], by = c("dg_id", "season" = "year")) %>%
    mutate(
      earnings = earnings/1e6,
      pred_earnings = pred_earnings/1e6
    )
  
  proj <- skill_proj %>%
    filter(dg_id == id) %>%
    filter(projection_year <= min(projection_year) + 2) %>%
    left_join(
      major_proj %>%
        group_by(dg_id, year) %>%
        summarise(
          across(c(win, t5, t10), ~sum(.))
        ),
      by = c("projection_year" = "year", "dg_id")
    ) %>%
    left_join(
      earnings_proj %>%
        dplyr::select(-last_updated) %>%
        mutate(
          pred_earnings = pred_earnings/1e6
        )
    ) %>%
    dplyr::select(dg_id, season = projection_year, sg = pred_latent_skill, major_wins = win, major_t5s = t5, major_t10s = t10, pred_earnings)
  
  
  bind_rows(act,proj) %>%
    ungroup() %>%
    dplyr::select(-dg_id) %>%
    arrange(-season) %>%
    mutate(across(c(starts_with('major'), contains('earn'), sg, putt, arg, app, ott), ~round(.,2))) %>%
    rename_all(~toupper(str_replace(.,"_", " "))) %>%
    gt() %>%
    gtExtras::gt_theme_pff() %>%
    sub_missing(missing_text = "--") %>%
    gtExtras::gt_highlight_rows(rows = SEASON > 2022) %>%
    tab_header(
      title = "",
      subtitle = "Performance + Projections"
    )
}