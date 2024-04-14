if (!require("pacman", quietly = T)) {
  install.packages("pacman")
  library(pacman)
} else(
  library(pacman)
)

p_load(tidyverse, plotly, hrbrthemes, ggthemes, rvest, sf, RColorBrewer,
       janitor, scales, httr, jsonlite, shiny, shinydashboard, 
       shinydashboardPlus, gt, tidymodels, modeltime, timetk, shinyWidgets,
       leaflet, digest, bslib, bsicons, reactablefmtr)
