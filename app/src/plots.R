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
