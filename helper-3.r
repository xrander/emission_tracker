forecast_mod <- forecast_mod |> 
  extract_nested_future_forecast()

forecast_mod <- forecast_mod |> 
  set_names("gas","model_id", "model_type", "key", "date", "emission", "conf_low", "conf_high")

# Forecast ----------------------------------------------------------------

# Row 1 -------------------------------------------------------------------

five_year_forecast <- function(data, today_date) {
  data |> 
    filter(date >= today_date) |> 
    mutate(emission = round(emission/1e6, 1)) |> 
    pivot_wider(
      id_cols = date,
      names_from = gas,
      values_from = emission
    ) |> 
    head()
}

render_five_year_forecast <- function(data) {
  data |> 
    gt() |> 
    tab_header(
      title = md("**Emission For Next Five Years**"),
      subtitle = "Emission in million CO2e Tonne"
    ) |> 
    opt_interactive(use_compact_mode = TRUE)
}



forecast_mod_trans <- forecast_mod |> 
  mutate(model_type = case_when(model_type == "ACTUAL" ~ "Observed",
                                .default = "forecast")
        )


# Row 2 -------------------------------------------------------------------

get_forecast_range <- function(date_range) {
  date_rng <- as.Date(date_range)
  forecast_mod_trans |> 
    filter(between(date, date_rng[1], date_rng[2]))
}

plot_forecast_tbl <- function(data) {
  data |> 
    ggplot(aes(x = date, col = model_type)) +
    geom_line(aes(y = round(emission/1e6, 1))) +
    facet_wrap(~gas, scales = "free_y") +
    geom_ribbon(
      aes(ymin = round(conf_low/1e6, 1), ymax = round(conf_high/1e6, 1)),
      col = "lightgray",
      linetype = 1, 
      alpha = .1
    ) +
    theme_clean() +
    labs(y = "Million CO2e", col = "Type", x = "Year") +
    scale_y_continuous(label = label_comma())
}

# Scenario Analysis -------------------------------------------------------
 
scenario_anal <- forecast_mod_trans |> 
  mutate(
    emission = round(emission/1e6, 1),
    conf_high = round(conf_high/1e6, 1),
    conf_low = round(conf_low/1e6, 1),
    two_high = case_when(!is.na(conf_low) ~ emission + (.02 * emission), .default = emission),
    two_low = case_when(!is.na(conf_low) ~ emission - (.02 * emission), .default = emission),
    five_high = case_when(!is.na(conf_low) ~ emission + (.05 * emission), .default = emission),
    five_low = case_when(!is.na(conf_high) ~ emission - (.05 * emission), .default = emission)
  ) |> 
  pivot_longer(cols = two_high:five_low,
               names_to = "percent_type",
               values_to = "scenario_emission") |> 
  mutate(
    percent_type = case_when(is.na(conf_low) ~ "Observed",
                             str_detect(percent_type, "two_high") ~ "+ 2 degrees",
                             str_detect(percent_type, "two_low") ~ "- 2 degrees",
                             str_detect(percent_type, "five_high") ~ "+ 5 degrees",
                             .default = "- 5 degrees"
                             )
    )

filter_scenario_gas <- function(input_gas){
  scenario_anal |> 
    filter(gas %in% {})
}

select_gas_percent <- function(data, date_range, gas_input, percent_type){
  date_rng <- as.Date(date_range)
  
  data |> 
    filter(
      between(date, date_rng[1], date_rng[2]),
      gas %in% {{ gas_input }}, percent_type %in% {{ percent_type }}
    )
}


plot_scenario_tbl <- function(data){
  data |> 
    ggplot(aes(date, scenario_emission, group = percent_type, col = percent_type)) +
    geom_line() +
    geom_line(aes(date, emission), col = "darkred") +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = .05, col = "lightgray") +
    facet_wrap(~ gas, scales = "free_y") +
    labs(x = "Year", y = "Million Co2e Tonne") +
    theme_clean() +
    theme(
      axis.text.y = element_text(face = "bold", size = 12)
    )
}