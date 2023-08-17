# 1. Current rankings, expected rankings
plot_rankings_dt_main <- function(rankings, skill_proj){
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
plot_proj_dt_main <- function(skill_proj, major_proj, earnings_proj){
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
plot_stats_dt_main <- function(rounds, adj_sg, adj_sg_comp){
  
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
plot_xearnings_dt_main <- function(earnings_exp, earnings_act){
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

# - skill projection plot
#   - graph of skill w/ error estimates
plot_skill_proj <- function(rounds,skill_proj, id){
  
  quantiles <- quantile(skill_proj[skill_proj$primary_tour == 'pga', ]$pred_latent_skill, c(.05, .10, .25, .50))
  
  df <- rounds %>% 
    filter(dg_id == id & year < min(skill_proj$projection_year)) %>%
    group_by(year) %>%
    summarise(latent_skill = mean(latent_skill)) %>%
    ungroup()
  
  y_upper <- max(df$latent_skill) + 1.5
  y_lower <- min(df$latent_skill) - 1.75
  x_min <- min(df$year)
  x_max <- max(skill_proj$projection_year)
  
  skill_proj %>%
    filter(dg_id == id) %>%
    # mutate(date = as.Date(str_glue("{projection_year}-05-01"))) %>%
    ggplot() +
    geom_ribbon(aes(x = projection_year, ymin = sd_latent_skill*-2 + pred_latent_skill, ymax =  sd_latent_skill*2 + pred_latent_skill),
                fill = 'darkgreen', alpha = 0.1) +
    geom_line(aes(x=projection_year, y = pred_latent_skill), color = 'black', linetype = "longdash") +
    geom_line(aes(x=projection_year, y = sd_latent_skill*2 + pred_latent_skill), color = "darkgreen", alpha = 0.2) +
    geom_line(aes(x=projection_year, y = sd_latent_skill*-2 + pred_latent_skill), color = "darkgreen", alpha = 0.2) +
    geom_point(data = df, aes(x= year, y = latent_skill), color = 'black') +
    geom_hline(aes(yintercept=quantiles[1]), color = "darkred", alpha = 0.9) +
    geom_hline(aes(yintercept=quantiles[2]), color = "darkred", alpha = 0.9) +
    geom_hline(aes(yintercept=quantiles[3]), color = "darkred", alpha = 0.9) +
    geom_hline(aes(yintercept=quantiles[4]), color = "darkred", alpha = 0.9) +
    annotate("text", x=x_max, y=quantiles[1]+0.075, size = 2.5, label="95th", color = "darkred") +
    annotate("text", x=x_max, y=quantiles[2]+0.075, size = 2.5, label="90th", color = "darkred") +
    annotate("text", x=x_max, y=quantiles[3]+0.075, size = 2.5, label="75th", color = "darkred") +
    annotate("text", x=x_max, y=quantiles[4]+0.075, size = 2.5, label="50th", color = "darkred") +
    # geom_point(data = df, aes(x= date, y = adj_sg_total), color = 'gray50', alpha = 0.5) +
    lims(y=c(y_lower, y_upper)) +
    scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min,x_max, by = 1)) +
    theme_bw() +
    labs(y = "SKILL" , x = "YEAR", title = "Historical Skill + Projections*", caption = "*with 95% confidence interval")
}



# - skill summary (box plots relative to average, bar plots of percentiles, etc.)
plot_sg_components <- function(rounds, adj_sg_comp, id){
  rounds %>%
    inner_join(
      adj_sg_comp %>% dplyr::select(-last_updated)
    ) %>%
    group_by(
      dg_id, season
    ) %>%
    summarise(
      rounds = n(),
      putt = mean(adj_sg_putt),
      arg = mean(adj_sg_arg),
      app = mean(adj_sg_app),
      ott = mean(adj_sg_ott)
    ) %>%
    group_by(season) %>%
    mutate(
      PUTT = 1-percent_rank((rounds*putt + 15*mean(putt))/(rounds+15)),
      ARG = 1-percent_rank((rounds*arg + 15*mean(arg))/(rounds+15)),
      APP = 1-percent_rank((rounds*app + 15*mean(app))/(rounds+15)),
      OTT = 1-percent_rank((rounds*ott + 15*mean(ott))/(rounds+15))
    ) %>%
    filter(dg_id == id) %>%
    dplyr::select(dg_id, season, PUTT, ARG, APP, OTT) %>%
    tidyr::pivot_longer(
      c(PUTT, ARG, APP, OTT),
      names_to = "skill",
      values_to = "ntile"
    ) %>%
    mutate(ntile = ntile * 100) %>%
    ggplot() +
    geom_bar(stat = 'identity', position = 'dodge', aes(x=season, y = ntile, fill = skill)) +
    facet_wrap(.~skill) +
    theme_bw() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
    lims(y = c(-0.01,101)) +
    labs(y = "PERCENTILE", x = "SEASON", title = "Adjusted SG Components")
  
}
