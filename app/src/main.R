library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(reactable)
library(DT)
library(gt)

library(ggplot2)
library(DBI)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

source("./helpers.R")
source("./get_data.R")

# Landing Page:
# 1. Current rankings, expected rankings, expected earnings
# 2. projections (skill, ranking, majors, $) by year
# 3. historical stats
# Player Page
# - Summary table of "peak" ranking (lowest ranking in next 3 years), major performance and earnings
# - skill summary (box plots relative to average, bar plots of percentiles, etc.)
# - skill projection plot
#   - graph of skill w/ error estimates
# - year by year table
#   - historical performance + projections
#   - skill, component skill, earnings, expected earnings, earnings over expected, major finishes, expected major finishes