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
    gtExtras::gt_theme_pff()
}

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