library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("tidyverse")
library("plotly")
library("hrbrthemes")
library("scales")
library("janitor")


files <- list.files(pattern = "\\.csv$", full.names = T)

# files
files <- files[-c(3, 11)] # previously saved csv files from when script was original developed are removed.
# uncomment and run 'files' first before running this code.



ghg_data <- map_df(files, read_csv) %>% 
  clean_names() %>% 
  select(-2) %>% 
  rename("gas" = series_code,
         "region" = country_or_area,
         "emission_value" = value) %>%
  mutate_if(is.character, factor)


format_large_number <- function(x) {
  if(x >= 1e12) {
    return(paste(format(round(x/1e12), nsmall = 1), " Trillion"))
  } else if (x >=1e9) {
    return(paste(format(round(x/1e9), nsmall = 1), "Billion"))
  } else if (x >=1e6) {
    return(paste(format(round(x/1e6), nsmall = 1), "Million"))
  } else if (x >=1e3) {
    return(paste(format(round(x/1e3), nsmall = 1), "Thousand"))
  } else {
    return(as.character(x))
  }
} # output needed to be in readable format and not long numbers

ghg_pie <-ghg_data %>%
  mutate(gas2 = fct_collapse(gas,
                             "Others" = c("HFC", "MIX", "N2O", "NF3", "PFC", "SF6")))

# Metrics needed for bubble chart
un_population <- read_csv("population_data.csv",
                          col_types = list("Country or Area" = col_character(),
                                           "Year" = col_double(),
                                           "Area" = col_character(),
                                           "Sex" = col_character(),
                                           "Record Type" = col_character(),
                                           "Value" = col_double(),
                                           "Value Footnotes" = col_double()
                                           )
                          )


un_population <- un_population %>%
  select(`Country or Area`, Sex, Year, Area, Value) %>% 
  filter(Sex == "Both Sexes" & Area == "Total") %>% 
  select(c(1,3,5)) %>% 
  rename("region" = "Country or Area",
         "year" = "Year",
         "population" = "Value")


EU <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
        "Estonia", "Finland", "France", "Germany", "Greece",
        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
        "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
        "Spain", "Sweden") # EU is not represented as a data point.
#list will be created to include EU in the data point

eu_pop <- un_population %>% 
  filter(region %in% EU) %>% 
  mutate(is_eu = "European Union") %>% 
  group_by(year, is_eu) %>% 
  summarize(population = sum(population)) %>% 
  rename("region" = is_eu) %>% 
  relocate(region)

un_population <- un_population %>% 
  bind_rows(eu_pop) %>% 
  mutate(region = ifelse(region == "United Kingdom of Great Britain and Northern Ireland",
                         "United Kingdom", region))

# The same will be repeated for the GDP Per Capital

un_per_capital <-read_csv("gdp.csv")

eu_per_capital <- un_per_capital %>% 
  filter(region %in% EU) %>% 
  mutate(is_eu = "European Union") %>% 
  group_by(year, is_eu) %>% 
  summarize(per_capital = mean(gdp)) %>% 
  rename("region" = is_eu) %>% 
  relocate(region)

un_per_capital <- un_per_capital %>% 
  rename("per_capital" = gdp) %>% 
  bind_rows(eu_per_capital) %>% 
  mutate(region = case_when(region == "United States" ~ "United States of America",
                            region == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                            region != "United States" ~ region))

un_data <- un_population %>% 
  left_join(un_per_capital, join_by(region, year))

un_data <- un_data %>% 
  group_by(region, year) %>% 
  summarize(population = mean(population, na.rm = T),
            per_capital = mean(per_capital, na.rm = T))

un_data <- un_data[complete.cases(un_data), ]

ghg_data <- ghg_data %>% 
  left_join(un_data, join_by(region, year))

ui <- ui <- dashboardPage(skin = "green",
                          dashboardHeader(title = "GHG Emission Explorer (1990 - 2020)",
                                          titleWidth = 400),
                          dashboardSidebar(
                            sidebarMenu(
                              menuItem("Overview", tabName = "overview",
                                       icon = icon("dashboard", lib = "glyphicon")),
                              menuItem("Comparison Tool", tabName = "comparison",
                                       icon = icon("duplicate", lib = "glyphicon")), # to include map
                              menuItem("Scenario Analysis", tabName = "scenario",
                                       icon = icon("hourglass", lib = "glyphicon")),
                              menuItem("Predictive Analysis", tabName = "predictive_analysis",
                                       icon = icon("stats", lib = "glyphicon"))
                            )
                          ),
                          dashboardBody(
                            tabItems(
                              tabItem(
                                tabName = "overview",
                                h1("Overview of Emissions"),
                                
                                fluidRow(
                                  box(
                                    pickerInput("single_year", "Select Year",
                                                choices = unique(ghg_data$year),
                                                options = list(`actions-box` = T),
                                                multiple = T,
                                                selected = 2010),
                                    pickerInput("single_gas", "Select Greenhouse Gas",
                                                choices = unique(ghg_data$gas),
                                                options = list(`actions-box` = T),
                                                multiple = T,
                                                selected = "CH4"),
                                    width = 2,
                                    background = "olive"
                                  ),
                                  
                                  infoBoxOutput("total_emission", width = 2),
                                  
                                  box(
                                    title = "Top Emitting Countries",
                                    width = 4,
                                    status = "danger",
                                    solidHeader = T,
                                    collapsible = T,
                                    plotOutput("top_emitting_countries")
                                  ), 
                                  
                                  
                                  box(
                                    width = 4,
                                    title = "Proportion of Gas Emission",
                                    plotlyOutput("piechart"),
                                    status = "warning",
                                    solidHeader = T,
                                    collapsible = T
                                  )
                                ),
                                
                                fluidRow(
                                  column(width = 12,
                                         box(
                                           width = 3,
                                           background = "green",
                                           sliderTextInput("year_range", "Choose year range",
                                                           choices = sort(unique(ghg_data$year)),
                                                           selected = c(1990, 2000),
                                                           from_min = 1990,
                                                           from_max = 2005,
                                                           to_min = 2010,
                                                           to_max = 2020)
                                         ),
                                         box(
                                           status = "warning",
                                           solidHeader = T,
                                           plotlyOutput("line_plot"),
                                           width = 9
                                         )
                                  )
                                )
                                
                                
                              ),
                              
                              tabItem(
                                tabName = "comparison",
                                h2("Comparison Tool"),
                                
                                fluidRow(
                                  box(
                                    h6("The plot on your right to show how each gas compares across the various regions.
                                     Select a gas to begin 'Methane(CH4)' is chosen by default, click dropdown to make choice"),
                                    hr(),
                                    collapsible = T,
                                    pickerInput("gas", "Select Gas",
                                                choices = unique(ghg_data$gas),
                                                options = list(style = "btn-warning")),
                                    sliderTextInput("year_range_2", " Select Time Span",
                                                    choices = sort(unique(ghg_data$year)),
                                                    selected = c(1990, 2005),
                                                    from_min = 1990,
                                                    from_max = 2000,
                                                    to_min = 2010,
                                                    to_max = 2020),
                                    width = 4),
                                  box(
                                    title = "Regional Comparison of Gas Emission",
                                    width = 8,
                                    plotlyOutput("line_plot_2"),
                                    collapsible = T)
                                ),
                                
                                fluidRow(
                                  box(
                                    h6( "Emission of gases in relation to Population and Per Capital($) "),
                                    pickerInput("gas2", "Select Gas",
                                                choices = unique(ghg_data$gas),
                                                options = list(style = "btn-warning")),
                                    sliderTextInput("year_range_3", "Year",
                                                    choices = sort(unique(ghg_data$year)),
                                                    selected = 1990,
                                                    animate = T),
                                    
                                    width = 4),
                                  
                                  box(plotlyOutput("bubble_plot"))
                                )
                              ),
                              tabItem(
                                tabName = "scenario",
                                h2("Scenario Analysis")
                              ),
                              tabItem(
                                tabName = "predictive_analysis",
                                h2("Predictive Analysis")
                              )
                            )
                          )
)



server <- function(input, output) {
  
  output$total_emission <- renderInfoBox({
    infoBox(
      "Total Emission",
      paste0(ghg_data %>% 
               filter(year %in% input$single_year & gas %in% input$single_gas) %>% 
               group_by(gas) %>% 
               summarize(total_emission = sum(emission_value)) %>% 
               pull(total_emission) %>% 
               sum() %>% 
               format_large_number()," KTCO2e", sep = " "),
      icon = icon("cloud", lib = "glyphicon"), color = "red",
      fill = T)
  })
  
  output$top_emitting_countries <- renderPlot({
    ghg_data %>% 
      filter(year %in% input$single_year & gas %in% input$single_gas) %>% 
      group_by(region) %>% 
      summarize(total_emission = sum(emission_value)) %>% 
      arrange(desc(total_emission)) %>% 
      top_n(n = 10, wt = total_emission) %>% 
      ggplot(aes(fct_reorder(region, total_emission), total_emission, fill = total_emission)) +
      geom_bar(stat = "identity",
               show.legend = F) +
      labs(x = "Region",
           y = "Emission in KTCO2e") +
      scale_fill_distiller(palette = "Reds",
                           direction = 1) +
      scale_y_continuous(label = scales::comma) +
      coord_flip() +
      theme_tinyhand() +
      theme(axis.title = element_text(face = "bold",
                                      size = 12),
            axis.text = element_text(face = "bold.italic",
                                     size = 9))
  })
  
  output$piechart <- renderPlotly({
    ghg_pie %>%
      filter(year %in% input$single_year) %>% 
      group_by(gas2) %>%
      summarize(total_emission = sum(emission_value)) %>%
      mutate(gas_proportion = round(total_emission/sum(total_emission) * 100, 1),
             ylab_pos = cumsum(gas_proportion + 1) - 0.5 * gas_proportion) %>% 
      plot_ly(labels = ~gas2, values = ~gas_proportion, type = "pie",
              textposition = "inside",
              textinfo = "label+percent",
              showlegend = F)
    
  })
  
  output$line_plot <- renderPlotly({
    emis_plot <- ghg_data %>% 
      filter(between(year, min(input$year_range), max(input$year_range))) %>%
      group_by(year, gas) %>% 
      summarize(emission_value = sum(emission_value)) %>% 
      ggplot(aes(year, emission_value, col = gas, fill = gas)) +
      geom_line()+
      geom_area(alpha = 0.7)+
      theme_tinyhand() +
      labs(y = "Emissions (KTCO2e)",
           x = "Year",
           title = paste0("Emission from", sep = "", min(input$year_range), " to ", max(input$year_range))) +
      
      scale_y_continuous(labels = scales::comma)# 4th result to be used for dashboard 1
    
    ggplotly(emis_plot)
  })
  
  output$line_plot_2 <- renderPlotly({
    comp_plot <- ghg_data %>% 
      filter(gas == input$gas & between(year, min(input$year_range_2), max(input$year_range_2))) %>% 
      ggplot(aes(year, emission_value, col = region)) +
      geom_line() +
      theme_tinyhand()+
      labs(x = "Year",
           y = "Emission (KTCO2e)",
           title = paste0("Emission of ", input$gas, " From ", min(input$year_range_2), " to ", max(input$year_range_2))) +
      scale_y_comma()
    
    ggplotly(comp_plot)
  })
  
  output$bubble_plot <- renderPlotly({
    ghg_data %>% 
      filter(year == input$year_range_3 & gas == input$gas2) %>% 
      ggplot(aes(population, per_capital, size = emission_value), show.legend = F) +
      geom_point(aes(col = region)) +
      theme_tinyhand()+
      scale_x_log10() +
      scale_y_log10() +
      labs(x = "Population (log 10)",
           y = "GDP Per Capital in USD (log 10)")
  })
  
}

shinyApp(ui, server)

