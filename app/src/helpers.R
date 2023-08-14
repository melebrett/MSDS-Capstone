normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

calculate_ma <- function(data, time_window){
  z <- zoo::zoo(data$adj_sg_total, data$date)
  # g <- zoo(,seq(start(z), end(z), "day"))
  # zm <- merge(z,g)
  results <- zoo::rollapplyr(z, width = time_window, FUN = function(x) mean(x, na.rm=T), partial = TRUE, align = "right", fill = NA)
  tibble(date = time(results),
         ma = as.numeric(results))
}

get_mas <- function(rounds_df){
  
  mas <- rounds_df %>%
    group_by(dg_id) %>%
    arrange(date) %>%
    nest() %>%
    mutate(
      ma_1 = purrr::map(data, ~calculate_ma(.,15)),
      ma_2 = purrr::map(data, ~calculate_ma(.,30)),
      ma_3 = purrr::map(data, ~calculate_ma(.,60))
    ) %>%
    dplyr::select(dg_id, starts_with('ma')) %>%
    unnest(c(ma_1, ma_2, ma_3), names_sep = "_") %>%
    dplyr::select(
      dg_id, date = ma_1_date, ma_1 = ma_1_ma, ma_2 = ma_2_ma, ma_3 = ma_3_ma
    )
  
  return(mas)
}


predict_ls <- function(model, rounds_df){
  
  predict(
    model,
    rounds_df %>%
      group_by(dg_id) %>%
      mutate(
        day = lubridate::time_length(interval(date, max(date)), unit ="days")
      ) %>%
      ungroup()
  )
  
}
