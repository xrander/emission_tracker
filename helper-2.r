# Comparison Section ---------------------------------------------------------
  ## Row 1 ---------------------------------------------------------------
total_emis_by_country <- function(data, country_var) {
  data |> 
    summarize(
      .by = c(year, country),
      emission_per_pop = mean(emission_per_pop)
    ) |> 
    filter(country %in% {{country_var}})
}

plot_emis_per_person <- function(data) {
  data |> 
    ggplot(aes(year, emission_per_pop, fill = country, col = country)) +
    geom_line(linewidth = .5) +
    geom_area(position = position_dodge(width = .1), alpha = .3) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    labs(
      x = "Year", y = "CO2-equivalent per person") +
    scale_x_continuous(breaks = seq(1990, 2021, 3))
}

population_trend <- function(data, country_var) {
  data |> 
    summarize(
      .by = c(year, country),
      population = mean(population)
    ) |> 
    mutate(
      .by = country,
      pop_growth = (population - lag(population)) / lag(population) * 100
    ) |> 
    slice_max(year, n = 5, by = country) |> 
    filter(!is.na(pop_growth) & country %in% {{ country_var }}) |> 
    summarize(
      .by = country,
      pop_growth = round(mean(pop_growth), 1)
    ) |> 
    mutate(
      pop_growth = case_when(country == "European Union" ~ round(pop_growth/27, 1),
                             .default = pop_growth),
      status = case_when(pop_growth > 0 ~ "increasing", .default = "decreasing")
    )
}

samp_country <- c("Australia","Canada", "Japan", "European Union", "Turkiye")
  
  

tbl_export <- function(data){
  ggplot(data, aes(pop_growth, country, fill = status)) +
    geom_col(width = .05) +
    geom_point(size = 5, stroke = 1, col = "black") +
    geom_vline(xintercept = 0, col = "black") +
    geom_label(aes(label = pop_growth), fontface = "bold") +
    labs(x = "", y = "") +
    scale_fill_manual(values = c("red", "springgreen3")) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(face = "bold", size = 10)
    )
}

# tbl_export(population_trend(emis_tbl, samp_country))

per_capital <- function(data, country_var){
  data |> 
   summarize(
     .by = c(country,year),
     per_capital = ceiling(mean(per_capital, na.rm = TRUE))
   ) |> 
    slice_max(
      year,
      by = country
    ) |> 
    select(country, per_capital) |> 
    filter(country %in% {{ country_var }}) |> 
    rename(
      Country = country,
      "Per Capital" = per_capital
    )
}

per_cap <- function(data){
  data |> 
    ggplot(aes(`Per Capital`, fct_reorder(Country, `Per Capital`))) +
    geom_col(width = .05, fill = "red") +
    geom_point(size = 5, stroke = 1, col = "black") +
    labs(x = "", y = "") +
    geom_label(
      aes(label = paste0("$",`Per Capital`)), 
      nudge_x = -3000, fontface = "bold"
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(face = "bold", size = 10)
    )
    
}

plot_gas_trend <- function(data, input_year) {
  label_year <- tibble(
    emission = 10000,
    country = "Turkiye",
    year = input_year
  )
  
  data |> 
    summarize(.by = c(year, country),
      emission = sum(emission)
    ) |> 
    mutate(emission = round(emission/1e6, 1)) |> 
    filter(year == {{ input_year }}) |> 
    ggplot(aes(emission, fct_reorder(country, emission), fill = emission)) +
    geom_col(position = "dodge") +
    expand_limits(xlim = c(0, 21000)) +
    geom_label(
      aes(label = paste0(emission, " M")),
      fill = "white", col = muted("red"), nudge_x = 1000
    ) +
    geom_label(
      data = label_year,
      aes(emission, country, label = paste0("Year ", year)),
      size = 13, fill = "brown", col = "white"
    ) +
    labs(x = "", y = "", fill = "Emission (CO2e Tonne)") +
    scale_fill_gradient(low = "gray", high = muted("red")) +
    theme_clean() +
    theme(
      axis.text.y = element_text(face = "bold", size = 10),
      axis.line.x.bottom = element_blank(),
      axis.line.y.left = element_line(color = "black", linewidth = .5),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank() ,
      axis.ticks.length.y = unit(.2, "cm")
    )
}


### Table 3 -----------------------------------------------------------

tot_emis_country <- function(data, country_var){
  data |> 
    summarize(
      .by = c(country, year),
      emission = sum(emission)
    ) |> 
    mutate(
      .by = year,
      percent_emission = round(emission/sum(emission) * 100, 2)
    ) |>
    select(!emission) |> 
    filter(country %in% {{ country_var }}) |> 
    pivot_wider(
      names_from = country,
      values_from = percent_emission
    ) |> 
    arrange(desc(year)) |> 
    mutate(Year = as.character(year), .after = 1) |> 
    select(-year)
}

react_output <- function(data){
  reactable(
    data, defaultColDef = colDef(
      cell = data_bars(
        data, fill_color =  c("lightgray", "green"),
        fill_gradient = TRUE, brighten_text = TRUE,
        round_edges = TRUE, background = "lightgray",
        bar_height = 6, text_position = "outside-end",
        number_fmt = label_percent(accuracy = .01, scale = 1)
      )
    )
  )
}

select_gas_year <- function(data, input_year, choice_gas) {
  data |> 
    filter(year ==  {{ input_year }} & gas %in% {{ choice_gas }})
}


plot_comp_gas <- function(choice_gas_data) {
  choice_gas_data |> 
    ggplot(aes(emission, fct_reorder(country, emission), fill = fct_reorder(gas, emission))) +
    geom_col(position = position_dodge()) +
    scale_fill_colorblind() +
    labs(fill = "Greenhouse Gas") +
    theme_clean() +
    theme(
      legend.position = "bottom",
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(face = "bold", size = 10)
    )
}
