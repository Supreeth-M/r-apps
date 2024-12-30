library(shiny)
library(shinydashboard)
library(leaflet)
library(httr)
library(jsonlite)
library(plotly)
library(ggplot2)
library(dplyr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Global Disaster Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Disaster Map", tabName = "disaster_map", icon = icon("globe")),
      menuItem("Climate Simulator", tabName = "climate_sim", icon = icon("chart-line")),
      menuItem("Carbon Footprint", tabName = "carbon_fp", icon = icon("leaf"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "disaster_map",
              fluidRow(
                box(title = "Live Global Disaster Map", width = 12, status = "primary",
                    leafletOutput("disasterMap", height = 600))
              )
      ),
      tabItem(tabName = "climate_sim",
              fluidRow(
                box(title = "Climate Change Simulator", width = 12, status = "primary",
                    sliderInput("policy", "Policy Effectiveness (1 = Low, 10 = High):", min = 1, max = 10, value = 5),
                    actionButton("simulate", "Run Simulation"),
                    plotlyOutput("simPlot", height = 400))
              )
      ),
      tabItem(tabName = "carbon_fp",
              fluidRow(
                box(title = "Personal Carbon Footprint Calculator", width = 12, status = "primary",
                    textInput("transport", "Transportation (km/week):", value = "100"),
                    textInput("energy", "Electricity Use (kWh/month):", value = "300"),
                    textInput("diet", "Diet Type (e.g., vegetarian, non-vegetarian):", value = "non-vegetarian"),
                    actionButton("calc_fp", "Calculate Footprint"),
                    verbatimTextOutput("footprint"))
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Disaster Map
  output$disasterMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  observe({
    # Fetch earthquake data from USGS
    url <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.geojson"
    response <- fromJSON(content(GET(url), "text"))
    
    if (!is.null(response$features)) {
      eq_data <- response$features
      
      # Extract necessary information
      eq_df <- data.frame(
        lat = sapply(eq_data, function(x) x$geometry$coordinates[2]),
        lng = sapply(eq_data, function(x) x$geometry$coordinates[1]),
        mag = sapply(eq_data, function(x) x$properties$mag),
        place = sapply(eq_data, function(x) x$properties$place)
      )
      
      leafletProxy("disasterMap") %>%
        clearMarkers() %>%
        addCircleMarkers(data = eq_df, lat = ~lat, lng = ~lng, 
                         radius = ~mag * 2, color = "red",
                         popup = ~paste0("<b>Location:</b> ", place, "<br><b>Magnitude:</b> ", mag))
    }
  })
  
  # Climate Simulator
  observeEvent(input$simulate, {
    set.seed(123)  # Ensure reproducibility
    years <- seq(2023, 2073, by = 1)
    policy_effect <- input$policy
    warming <- cumsum(runif(length(years), min = 0.02, max = 0.05)) * (11 - policy_effect) / 10
    
    sim_data <- data.frame(year = years, temperature_increase = warming)
    
    output$simPlot <- renderPlotly({
      ggplot(sim_data, aes(x = year, y = temperature_increase)) +
        geom_line(color = "blue") +
        geom_point() +
        ggtitle("Projected Global Temperature Increase") +
        xlab("Year") +
        ylab("Temperature Increase (°C)")
    })
  })
  
  # Carbon Footprint Calculator
  observeEvent(input$calc_fp, {
    transport_fp <- as.numeric(input$transport) * 0.21  # kg CO2 per km
    energy_fp <- as.numeric(input$energy) * 0.5  # kg CO2 per kWh
    diet_fp <- ifelse(input$diet == "vegetarian", 100, 300)  # kg CO2/month
    
    total_fp <- transport_fp + energy_fp + diet_fp
    
    output$footprint <- renderText({
      paste0("Your estimated monthly carbon footprint is: ", round(total_fp, 2), " kg CO2")
    })
  })
}

# Run the app
shinyApp(ui, server)
