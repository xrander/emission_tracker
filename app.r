source("setup.r")
source("helper-1.r")
source("helper-2.r")
source("helper-3.r")

ui <- page_navbar(

## Theme set ---------------------------------------------------------------

   theme = bs_theme(
     bootswatch = "bootstrap",
     version = 5,
     success = "lightgreen",
     danger = "orange",
     "text-danger" = "darkred",
     #"table-color" = "#86C7ED",
     base_font = font_google("Lato")
   ),

## Title -------------------------------------------------------------------

  title = "EMT Tracker",

# Navigation section -------------------------------------------------------

## Nav_1 --------------------------------------------------------------------
  nav_panel(
    title = "Overview",
    icon = bs_icon("speedometer2"),
  
    # Nav bar content
    layout_columns(
      
      value_box(
        title = "Regions Tracked",
        value = total_regions_monitored(emis_tbl),
        showcase = bs_icon("map-fill"),
        p("The European Union as a union and not individual nations within the union are represented",
          bs_icon("emoji-grimace")),
        theme = "success",
        max_height = "140px"
      ),
      
      uiOutput("year_emission_vb"),
      
      value_box(
        title = "Emission to date",
        value = paste0(total_emission(emis_tbl), " Billion Tonne CO2e"),
        showcase = bs_icon("clouds"),
        theme = "danger",
        max_height = "140px"
      ),
      uiOutput("percent_year_vb"),
      
      noUiSliderInput(
        inputId = "specific_year",
        label = "Year",
        min = min(as.integer(emis_tbl$year)),
        max = max(as.integer(emis_tbl$year)),
        step = 1,
        value = 2021,
        orientation = "vertical",
        width = "10px",
        behaviour = "drag",
        tooltips = TRUE,
        height = "100px",
        direction = "rtl"
      ),
      
      card(
        uiOutput("country_change_header"),
        max_height = "350px",
        plotlyOutput("percent_change_country"),
        full_screen = TRUE
      ),
      card(
        uiOutput("top_emitter_header"),
        plotOutput("top_emitter")
      ),
      card(
        uiOutput("least_emitter_header"),
        plotOutput("least_emitter")
      ),
      card(
        uiOutput("select_year"),
        plotlyOutput("plot_gas_prop"),
        full_screen = TRUE
      ),
      card(
        leafletOutput("maps")
      ), 
      col_widths = c(3, 3, 3, 2, 1, # first row
                     4, 4, 4, # second row
                     4, 8),
      row_heights = c(3, 5.5, 8)
    ),

  ),

## Nav_2--------------------------------
  nav_panel(
    title = "Comparison Tool",
    icon = bs_icon("activity"),

    # Nav bar content 
    layout_columns(
      card(
        card_header(strong("Emission Per Person")),
        title = "Emission_per_pop",
        min_height = "300px",
        theme = "light",
        plotlyOutput("plot_emis_per_pop")
      ),
      layout_column_wrap(
          width = "150px",
          card(
            card_header(strong("Population Growth (Last 5 years)")),
            plotOutput("pop_trend")
          ),
          card(
            card_header(strong("Current per capital")),
            plotOutput("per_cap_plot")
          )
      ),
      card(
        card_header(strong("Proportion of Selected Countries Emission")),
        title = "emission_per_gdp",
        reactableOutput("country_comp_table"),
        min_height = "300px"
      ),
      card(
        min_height = "300px",
        pickerInput(
          inputId = "country_1",
          label = "Select Country 1",
          choices = sort(unique(emis_tbl$country)),
          selected = sort(unique(emis_tbl$country))[3],
          options = list(
            style = "btn-dark"
          )
        ),
        
        hr(),
        
        pickerInput(
          inputId = "country_2",
          label = "Select Country 2",
          choices = sort(unique(emis_tbl$country)),
          selected = sort(unique(emis_tbl$country))[4],
          option = list(
            style = "btn-dark"
          )
        ),
        
        hr(),
        
        pickerInput(
          inputId = "country_3",
          label = "Select Country 3",
          choices = sort(unique(emis_tbl$country)),
          selected = sort(unique(emis_tbl$country))[12],
          option = list(
            style = "btn-dark"
          )
        )
      ),
      card(
        card_body(
          
          prettyCheckboxGroup(
            inputId = "gas",
            label = "Choose Greenhouse Gas:", 
            choices = sort(unique(emis_tbl$gas)),
            selected = unique(emis_tbl$gas)[1],
            icon = icon("check"), 
            status = "primary",
            outline = TRUE,
            animation = "jelly"
            ),
          sliderInput(
            inputId = "animated_year",
            label = "Year",
            value = min(unique(emis_tbl$year)),
            min = min(unique(emis_tbl$year)),
            max = max(unique(emis_tbl$year)),
            animate = animationOptions(
              interval = 500,
              loop = F, 
              playButton = bs_icon("play-fill", class = "danger")
            )
          ),
          switchInput(
            inputId = "log",
            label = "Logarithm Scale",
            size = "large"
          )
        )
      ),
      navset_card_pill(
        nav_panel(
          class = "bg-dark",
          title = "Total Gas Emission",
          card(
            plotOutput("plot_year_gas_trend")
          ),
          
        ),
        
        nav_panel(
          class = "bg-dark",
          title = "Individual Gas Comparison",
          card(
            uiOutput("ui_gas_comp"),
            plotOutput("plot_gas_comp")
          )
        )
      ),
      col_widths = c(4, 2, 4, 2, 4, 8),
      row_heights = c(5,7)
    )
  ),


## Nav_3 -----------------------------------------------------------------
  nav_panel(
      title = strong("Prediction Tool"),
      icon = bs_icon("alarm"),
      
      navset_card_tab(
        
        layout_columns(
          value_box(
            title = "Today's Date",
            value = today(),
            theme = "success",
            showcase = bs_icon("calendar3"),
            fill = TRUE
          ),
          value_box(
            title = "Greenhouse Gases Monitored",
            value = length(unique(forecast_mod$gas)),
            theme = "success",
            showcase = bs_icon("fuel-pump-fill", class = "warning"),
            fill = TRUE
          ),
          value_box(
            title = "Number of Model Used",
            theme = "success",
            value = length(unique(forecast_mod$model_type)),
            showcase = bs_icon("gear-wide-connected"),
            fill = TRUE
          ),
          max_height = "100px",
          row_heights = 2
        ),
        
        ## Subtab_1 -----------------------------------------------
        
        nav_panel(
          title = strong("Forecast"),
          icon = bs_icon("alarm"),
          card(
            full_screen = TRUE,
            card_header(
              strong("Emission Forecast For Monitored Greenhouse Gases"),
              class = "bg-dark"
            ),
            layout_sidebar(
              sidebar = sidebar(
                strong("Control"),
                hr(),
                sliderInput(
                  inputId = "forecast_year",
                  label = "Year Range",
                  value = c(min(unique(forecast_mod_trans$date)), unique(forecast_mod_trans$date)[40]),
                  min = min(unique(forecast_mod_trans$date)),
                  max = max(unique(forecast_mod_trans$date))
                )
              ),
              gt_output(outputId = "year_tbl_forecast"),
              plotlyOutput("plot_forecast")
            ),
          )
      ),
        
        ## Subtab_2 ----------------------------------
      nav_panel(
          title = "Scenario Analysis",
          icon = bs_icon("globe-europe-africa"),
          card(
            full_screen = TRUE,
            card_header(
              strong("Individual Greenhous Gas Scenario Analysis"),
              class = "bg-dark"
            ),
            layout_sidebar(
              sidebar = sidebar(
                strong("Simulate a Climate Scenario"),
                hr(),
                sliderInput(
                  inputId = "scenario_year",
                  label = "Year Range",
                  value = c(min(unique(scenario_anal$date)), unique(scenario_anal$date)[40]),
                  min = min(unique(scenario_anal$date)),
                  max = max(unique(scenario_anal$date))
                ),
                
                checkboxGroupButtons(
                  inputId = "samp_scenario",
                  label = "Sample Scenario",
                  choices = unique(scenario_anal$percent_type),
                  selected = unique(scenario_anal$percent_type)[1],
                  direction = "vertical",
                  status = "danger",
                  width = "90%"
                ),
                hr(),
                checkboxGroupButtons(
                  inputId = "scenario_gas",
                  label = "Select Green House Gas",
                  choices = sort(unique(scenario_anal$gas)),
                  selected = unique(scenario_anal$gas)[2],
                  status = "danger",
                  width = "100%"
                ),
                width = "25%"
              ),
              plotOutput("plot_scenario")
            )
        )
      )
    )
  )
)
server <- function(input, output, session) {

## Reactive elements -----------------------------------
    ### Tab 1 -----------------------------------------
  top_region <- reactive({
    top_5_region_emission(emis_tbl, input$specific_year)
  })
  
  least_region <- reactive({
    least_5_emitter(emis_tbl, input$specific_year)
  })
  
  yearly_emis <- reactive({
    yearly_emission(emis_tbl, input$specific_year)
  })
  
  percent_year <- reactive({
     percent_change_year(emis_tbl, input$specific_year) |> 
      pull(percent_change)
  })
  
  percent_country <- reactive({
    percent_change_country(emis_tbl, input$specific_year)
  })
  
  percent_status <- reactive({
    percent_change_year(emis_tbl, input$specific_year) |> 
      pull(status)
  })
  
  year_gas_prop <- reactive({
    gas_prop(emis_tbl, input$specific_year)
  })
  
  make_map_tbl <- reactive({
    map_react(input_year = input$specific_year)
  })
  
  ### Tab 2 --------------------------------------------------
  selected_country <- reactive({
    country <- c(input$country_1, input$country_2, input$country_3)
  })
  
  per_cap_tbl <- reactive({
    per_capital(emis_tbl, country_var = selected_country())
  }) 
  
  pop_trend <- reactive({
    population_trend(emis_tbl, country_var = selected_country())
  })
  
  emis_per_pop <- reactive({
    total_emis_by_country(emis_tbl, country_var = selected_country())
  })
  
  
  comp_tbl <- reactive({
    tot_emis_country(emis_tbl, country_var = selected_country())
  })
    
  gas_comp_tbl <- reactive({
    select_gas_year(emis_tbl, input$animated_year, input$gas)
  })
  
  ### Tab 3 ------------------------------------------------------
  today_forecast <- reactive({
    five_year_forecast(forecast_mod, today())
  })
  
  user_forecast_tbl <- reactive({
    get_forecast_range(input$forecast_year)
  })

  gas_percent_tbl <- reactive({
    select_gas_percent(scenario_anal, input$scenario_year, input$scenario_gas, input$samp_scenario)
  })
# Outputs ------------------------------------------------------------
  ## Tab 1 ----------------------------------------------------
  output$year_emission_vb <- renderUI({
    value_box(
      title = paste0("Year ",  input$specific_year, " Total Emission"),
      value = yearly_emis(),
      showcase = bs_icon("wind"),
      theme = "danger",
      max_height = "140px"
    )
  })
  
  output$country_change_header <-  renderUI({
    card_header(
      class = "bg-dark",
      strong(paste0("Percent Change in Emission between Regions for Year ",input$specific_year))
    )
  })
  
  output$top_emitter_header <- renderUI({
    card_header(
      class = "bg-dark",
      strong(paste0("Top Emitting Countries for ", input$specific_year))
    )
  })
  
  output$least_emitter_header <- renderUI({
    card_header(
      class = "bg-dark",
      strong(paste0("Least Emitting Countries for ",input$specific_year))
    )
  })
  
  output$select_year <- renderUI({
    card_header(
      class = "bg-dark",
      strong(paste0("Proportion of Gas Emission for ",input$specific_year))
    )
  })
  
  output$percent_year_vb <- renderUI({
    value_box(
      title = "Percentage change",
      value = paste0(abs(percent_year()), "%"),
      theme = "success",
      showcase = if(percent_status() == "bad") {
        bs_icon("arrow-up-circle-fill", class = "text-danger")
      } else
        bs_icon("arrow-down-circle-fill", class = "text-light"),
      showcase_layout = "top right",
      max_height = "140px"
    )
  })
  
  output$percent_change_country <- renderPlotly({
    plot_yearly_change(percent_country())
  })
  
  output$top_emitter <- renderPlot({
    plot_top_emitter(top_region())
  })
  
  output$least_emitter <- renderPlot({
    plot_least_emitter(least_region())
  })
  
  output$plot_gas_prop <- renderPlotly({
    plot_gas_prop(year_gas_prop())
  })
  
  output$maps <- renderLeaflet({
    leaflet(make_map_tbl()) |> 
      addTiles() |> 
      setView(lat = 10, lng = 0, zoom = 2) |> 
      addPolygons(
        stroke = TRUE,
        fillOpacity = .7,
        fillColor = ~ color_palette(emission),
        weight = .3
      ) |> 
      addLegend(
        pal = color_palette, values = ~ emission, 
        opacity = .9, title = "Emission (10 M)",
        position = "bottomright"
      )
    
    
  })
  
  ## Tab 2 -----------------------------------------------------
  output$per_cap_plot <- renderPlot({
    per_cap(per_cap_tbl())
  })
  
  output$pop_trend <- renderPlot({
    tbl_export(pop_trend())
  })
  output$country_comp_table <- renderReactable({
    react_output(comp_tbl())
  })
  
  output$plot_emis_per_pop <- renderPlotly({
    plot_a <- plot_emis_per_person(emis_per_pop())
  })
  
  output$plot_year_gas_trend <- renderPlot({
    plot_gas_trend(emis_tbl, input$animated_year)
  })
  
  output$ui_gas_comp <- renderUI({
    card_header(strong("Greenhouse Gas Comparison for ", input$animated_year))
  })

  
  output$plot_gas_comp <- renderPlot({
    plot_comp <- plot_comp_gas(choice_gas_data = gas_comp_tbl())

    if (input$log == TRUE) {
      plot_comp +
        scale_x_log10(label = label_log())
    } else{
      plot_comp +
        scale_x_continuous(label = label_comma())
    }
  })

  
  ## Tab 3 -------------------------------------------------------
  output$year_tbl_forecast <- render_gt({
    render_five_year_forecast(today_forecast())
  })
  output$plot_forecast <- renderPlotly({
    plot <- plot_forecast_tbl(user_forecast_tbl())
    
    ggplotly(plot)
  })
  
  output$plot_scenario <- renderPlot({
    plot_scenario_tbl(gas_percent_tbl())
  })
  
}

shinyApp(ui, server)