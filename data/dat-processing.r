
# Load Package ------------------------------------------------------------

source("data/dependencies.R")

base_url <- paste("https://data.un.org/ws/rest/data/UNSD,DF_UNData_UNFCC,1.0/.EN_ATM_METH_XLULUCF+",
                  "EN_ATM_CO2E_XLULUCF+EN_CLC_GHGE_XLULUCF+EN_ATM_HFCE+",
                  "EN_ATM_NO2E_XLULUCF+EN_ATM_PFCE+EN_ATM_SF6E.AUS+AUT+BLR+BEL+BGR+CAN+HRV+",
                  "CYP+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ITA+JPN+LVA+",
                  "LIE+LTU+LUX+MLT+MCO+NLD+NZL+NOR+POL+PRT+ROU+RUS+SVK+SVN+ESP+SWE+CHE+TUR+UKR+GBR+USA+EU1./",
                  "ALL/?detail=full&dimensionAtObservation=TIME_PERIOD",
                  sep = "")

# Connecting to API to support auto updates
res <- GET(base_url,
           add_headers(""),
           accept("text/csv"))

content <- content(res, type = "text", encoding = "utf-8")

ghg_data <- read_csv(content, col_types = cols()) |> 
  clean_names()

ghg_data <- ghg_data |> 
  mutate(
    indicator = str_remove_all(indicator, pattern = "._XLULUCF$"),
    indicator = str_remove_all(indicator, pattern = "(EN_ATM_|EN_CLC_)")
  )

ghg_data <- ghg_data |> 
  left_join(
    countrycode::codelist |>  select(country.name.en, genc3c),
    join_by(ref_area == genc3c)
  ) |> 
  select(country.name.en, indicator, time_period, obs_value) |> 
  rename(country = country.name.en) |> 
  mutate(
    country = case_when(
      is.na(country) ~ "European Union",
      country == "Turkey" ~ "Turkiye",
      country == "United States" ~ "United States of America",
      .default = country
  ),
  country = str_trim(country, side = "both")
  )


# Metrics needed for bubble chart
population_data <- read_csv(
  "https://raw.githubusercontent.com/xrander/greenhouse_data_analysis/master/data/population_data.csv",
  col_types = list("Country or Area" = col_character(),
                   "Year" = col_double(),
                   "Area" = col_character(),
                   "Sex" = col_character(),
                   "Record Type" = col_character(),
                   "Value" = col_double(),
                   "Value Footnotes" = col_double()
  )
)


un_pop <- population_data |> 
  select(`Country or Area`, Sex, Year, Area, Value) |> 
  filter(Sex == "Both Sexes" & Area == "Total") |> 
  select(c(1,3,5)) |> 
  rename("country" = "Country or Area",
         "year" = "Year",
         "population" = "Value") |> 
  mutate(
    year = as.integer(year),
    country = case_when(
      country == "Russian Federation" ~ "Russia",
      country == "Türkiye" ~ "Turkiye",
      country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      country != "United Kingdom of Great Britain and Northern Ireland" ~ country
      ),
    country = str_trim(country, side = "both")
  )

european_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
                        "Estonia", "Finland", "France", "Germany", "Greece",
                        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                        "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                        "Spain", "Sweden") # EU is not represented as a data point.

eu_pop <- un_pop |> 
  filter(country %in% european_countries) |> 
  mutate(is_eu = "European Union") |> 
  group_by(year, is_eu) |> 
  summarize(population = sum(population)) |> 
  rename("country" = is_eu) |> 
  relocate(country)

un_pop <- un_pop |> 
  bind_rows(eu_pop) |> 
  arrange(country, year)

# The same will be repeated for the GDP Per Capital

per_capital_data <-read_csv("https://raw.githubusercontent.com/xrander/greenhouse_data_analysis/review/data/gdp.csv") |> 
  rename(country = region) |> 
  mutate(
    country = case_when(
      country == "United States" ~ "United States of America",
      country == "Russian Federation" ~ "Russia",
      country == "Türkiye" ~ "Turkiye",
      country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      country != "United States" ~ country
    ),
    country = str_trim(country, side = "both")
  )

eu_per_capital <- per_capital_data|> 
  filter(country %in% european_countries) |> 
  mutate(
    is_eu = "European Union"
  ) |> 
  group_by(year, is_eu) |> 
  summarize(per_capital = mean(gdp)) |> 
  rename("country" = is_eu) |> 
  relocate(country)


un_per_capital <- per_capital_data |> 
  rename("per_capital" = "gdp") |> 
  bind_rows(eu_per_capital) |> 
  arrange(country, year)

un_data <- un_pop |> 
  right_join(un_per_capital, join_by(country, year))

un_data <- un_data |> 
  group_by(country, year) |> 
  summarize(population = mean(population, na.rm = T),
            per_capital = mean(per_capital, na.rm = T))

# The data

ghg_data_merged <- ghg_data |> 
  left_join(un_data, join_by(country == country, time_period == year)) |> 
  rename(
    year = time_period,
    gas = indicator,
    emission = obs_value
  )

ghg_data_merged <- ghg_data_merged |> 
  filter(country != "Monaco")

## Further processing -------------------------------------------------------------------
glimpse(ghg_data_merged)


ghg_data_merged <- ghg_data_merged |> 
  filter(!country %in% european_countries)


ghg_data_merged <- ghg_data_merged |> 
  mutate(
    emission = emission * 1000, 
    emission_per_capital = emission/per_capital,
    emission_per_pop = emission/population
  ) |> 
  group_by(gas) |> 
  mutate(change_in_emission = (emission - lag(emission))/lag(emission) * 100) |> 
  ungroup()


write_csv(ghg_data_merged, "data/data.csv")