# Overview Section -------------------------------------------
## Row 1 -------------------------------------------------

total_regions_monitored <- function(data) {
  length(unique(data$country))
}


yearly_emission <- function(data, my_year) {
  data |> 
    filter(year == my_year) |> 
    summarize(
      tot_sum = round(sum(emission), 2)
    ) |> 
    mutate(tot_sum = paste0(ceiling(tot_sum/1000000000), " Billion Tonne CO2e"))
}


total_emission <- function(data) {
  data |> 
    summarize(
      tot_emis = round(sum(emission)/1e9, 1)
    )
}


total_emission(emis_tbl)

percent_change_year <- function(data, filter_year) {
  data |> 
    summarize(
      .by = c(year),
      total_emission = sum(emission)
    ) |> 
    mutate(
      last_emission = lag(total_emission),
      percent_change = round((total_emission - last_emission)/last_emission * 100, 2),
      status = case_when(total_emission > last_emission ~ "bad",
                         .default = "good")
    ) |> 
    drop_na(percent_change) |> 
    filter(year == {{ filter_year }})
}




## Row 2 --------------------------------------------------

### Element data for plot--------------------------------------------
percent_change_country <- function(data, filter_year) {
  data |> 
    summarize(
      .by = c(year, country),
      total_emission = sum(emission)
    ) |> 
    group_by(country) |> 
    mutate(
      last_emission = lag(total_emission),
      percent_change = round((total_emission - last_emission) / last_emission * 100, 2),
      status = case_when(percent_change > 0 ~ "bad",
                         .default = "good")
    ) |> 
    ungroup() |> 
    filter(year == {{ filter_year }})
}

top_5_region_emission <- function(data, filter_year) {
  data |> 
    filter(year == {{ filter_year }}) |> 
    summarize(
      .by = c(year, country),
      tot_emission = sum(emission)
    ) |> 
    slice_max(tot_emission, n = 5)
}

least_5_emitter <- function(data, filter_year) {
  data |> 
    filter(year == {{ filter_year }}) |> 
    summarize(
      .by = c(year, country),
      tot_emission = sum(emission)
    ) |> 
    slice_min(tot_emission, n = 5)
}

### Plot element -----------------------------------------------------
plot_yearly_change <- function(data) {
  plot_ly(
    data, x = ~country, y = ~round(percent_change, 1)/ 100,
    color = ~status, type = "bar", showlegend = FALSE, colors = "YlOrRd"
  ) |> 
    layout(
      xaxis = list(title = "", tickangle = 320, tickfont = list(size = 10)),
      yaxis = list(title = "", tickformat = "1%")
    )
}

plot_top_emitter <- function(data) {
  data |> 
    ggplot(aes(tot_emission, fct_reorder(country, tot_emission), fill = tot_emission)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(
      low = "green", mid = "gray", 
      high = muted("red"), labels = comma
    ) +
    geom_label(
      aes(label = paste0(round(tot_emission/1e6, 2), "M")),
      fill = "white",
      col = muted("red")
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.y = element_text(face = "bold", size = 10),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(
      x = "", y = "",
      fill = "Total Emission"
    )
}


plot_least_emitter <- function(data) {
  data |> 
    ggplot(aes(tot_emission, fct_reorder(country, tot_emission, .fun = min), fill = tot_emission)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(
      low = "gray", mid = "white", 
      high = muted("springgreen"), labels = comma
    ) +
    geom_label(
      aes(label = paste0(round(tot_emission/1e6, 2), "M")),
      fill = "white",
      col = muted("springgreen")
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.y = element_text(face = "bold", size = 10),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(
      x = "", y = "",
      fill = "Total Emission"
    )
}

## 3 Row

gas_prop <- function(data, filter_year) {
  data |> 
    filter(year == {{ filter_year }} ) |> 
    summarize(
      .by = c(gas, year),
      total_emission = sum(emission)
    ) |> 
    arrange(desc(total_emission))
}


plot_gas_prop <- function(gas_prop_data) {
  gas_prop_data |> 
    plot_ly(
      labels = ~ gas, values = ~ total_emission, direction = "clockwise",
      type = "pie", textposition = "inside", textinfo = "label+percent",
      insidetextfont = list(color = I("white"), size = 15), hoverinfo = "text",
      text = ~paste0(round(total_emission/1000000000, 2), " Billion Tonnes"),
      showlegend = FALSE, legendrank = 10, automargin = TRUE, hole = .4
    ) |> 
    layout(hoverlabel=list(font=list(color = I("white"))))
}



# Row 3 -------------------------------------------------------
## Map -------------------------------------------------------
emis_country <- emis_tbl |>
  mutate(
    country = case_when(country == "United States of America" ~ "United States",
                        country == "Turkiye" ~ "Turkey",
                        .default = country)
  ) |> 
  summarize(
    .by = c(country, year),
    emission = sum(emission),
    population = mean(population),
    per_capital = mean(per_capital)
  ) |> 
  mutate(emission = round(emission/1e8)) |> 
  select(name = country, year, emission)

euro_union_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark",
                        "Estonia", "Finland", "France", "Germany", "Greece",
                        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                        "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal",
                        "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

world_sf <- read_sf("data/world/TM_WORLD_BORDERS_SIMPL-0.3.shp") |> clean_names()

world_sf <- world_sf |> 
  select(name, lon, lat, area, geometry)

world_sf <- world_sf |> 
  mutate(
    name = case_when(name %in% euro_union_countries ~ "European Union",
                     .default = name)
  )
locations <- world_sf |> 
  st_drop_geometry() |> 
  select(name)

locations <- locations |> left_join(emis_country, join_by(name), relationship = "many-to-many")

incomplete_map_tbl <- locations |> select(name, year)

complete_year_tbl <- incomplete_map_tbl |> 
  complete(name, year = min(emis_country$year):max(emis_country$year)) |> 
  filter(!is.na(year))


complete_map_tbl <- complete_year_tbl |> 
  left_join(locations, join_by(name), relationship = "many-to-many") |> 
  select(name, year = year.x, emission) |> 
  summarize(
    .by = c(name, year),
    emission = mean(emission)
  )

map_tbl <- world_sf |> 
  full_join(complete_map_tbl, by = "name", relationship = "many-to-many")


map_react <- function(input_year) {
  map_tbl |> 
    filter(year %in% {{ input_year }})
}


my_bins <- c(0, 50, 100, 150)

color_palette <- colorBin(
  palette = "YlOrBr",
  domain = map_tbl$emission,
  na.color = "transparent",
  bins = my_bins
)
