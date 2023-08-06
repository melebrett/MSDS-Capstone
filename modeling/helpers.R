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

