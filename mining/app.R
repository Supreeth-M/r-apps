library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(rgl)
library(threejs)
library(DT)

# Mock data for mining resources and locations
mining_data <- data.frame(
  Resource = rep(c("Coal", "Gold", "Iron", "Copper", "Bauxite"), each = 5),
  Location = c("China", "USA", "Australia", "Russia", "India", 
               "South Africa", "Canada", "Peru", "Brazil", "Indonesia", 
               "India", "Russia", "China", "USA", "Australia", 
               "Chile", "Peru", "USA", "Australia", "Zambia", 
               "Australia", "China", "Brazil", "India", "Indonesia"),
  Quantity = sample(50:500, 25, replace = TRUE),
  stringsAsFactors = FALSE
)

top_mined_places <- data.frame(
  Location = c("China", "USA", "Australia", "Russia", "India", 
               "South Africa", "Canada", "Peru", "Brazil", "Indonesia", 
               "Chile", "Mexico", "Kazakhstan", "Mongolia", "Zambia", 
               "Saudi Arabia", "Norway", "Germany", "Turkey", "Egypt"),
  Latitude = c(35.8617, 37.0902, -25.2744, 61.5240, 20.5937, 
               -30.5595, 56.1304, -9.1900, -14.2350, -0.7893, 
               -35.6751, 23.6345, 48.0196, 46.8625, -13.1339, 
               23.8859, 60.4720, 51.1657, 38.9637, 26.8206),
  Longitude = c(104.1954, -95.7129, 133.7751, 105.3188, 78.9629, 
                22.9375, -106.3468, -75.0152, -51.9253, 113.9213, 
                -71.5430, -102.5528, 66.9237, 103.8467, 27.8493, 
                45.0792, 8.4689, 10.4515, 35.2433, 30.8025)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = div(img(src = "https://upload.wikimedia.org/wikipedia/en/5/5f/Baahubali_The_Beginning_poster.jpg", height = "50px"), "Mining Analytics")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resource Statistics", tabName = "statistics", icon = icon("chart-line")),
      menuItem("Global Mining Map", tabName = "map", icon = icon("globe")),
      menuItem("Predictions", tabName = "predictions", icon = icon("magic"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body { 
          background-color: black; 
          color: white; 
        }
        .box { 
          background-color: rgba(255, 255, 255, 0.1) !important; 
          color: white !important; 
        }
        .tab-content { 
          color: white; 
        }
      "))
    ),
    tabItems(
      # Statistics Tab
      tabItem(tabName = "statistics",
              fluidRow(
                box(title = "Mining Data Summary", width = 12, solidHeader = TRUE, status = "primary",
                    DTOutput("mining_table"))
              ),
              fluidRow(
                box(title = "Resource Extraction Over Time", width = 6, solidHeader = TRUE, status = "success",
                    plotOutput("line_graph")),
                box(title = "Resource Share by Location", width = 6, solidHeader = TRUE, status = "info",
                    plotOutput("bar_graph"))
              )
      ),
      # Map Tab
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Global Mining Map", width = 12, solidHeader = TRUE, status = "info",
                    leafletOutput("mining_map", height = 500)),
                box(title = "3D Globe View", width = 12, solidHeader = TRUE, status = "warning",
                    globeOutput("globe_view", height = "500px"))
              )
      ),
      # Predictions Tab
      tabItem(tabName = "predictions",
              fluidRow(
                box(title = "Future Predictions", width = 12, solidHeader = TRUE, status = "danger",
                    textOutput("prediction_text"),
                    plotOutput("prediction_plot"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Mining Table
  output$mining_table <- renderDT({
    datatable(mining_data, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Line Graph: Resource Extraction Over Time
  output$line_graph <- renderPlot({
    yearly_data <- data.frame(
      Year = rep(2010:2024, each = 5),
      Resource = rep(c("Coal", "Gold", "Iron", "Copper", "Bauxite"), 15),
      Quantity = runif(75, min = 50, max = 500)
    )
    
    ggplot(yearly_data, aes(x = Year, y = Quantity, color = Resource, group = Resource)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(title = "Resource Extraction Over Time", x = "Year", y = "Quantity (in tons)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "black")
      )
  })
  
  # Bar Graph: Resource Share by Location
  output$bar_graph <- renderPlot({
    ggplot(mining_data, aes(x = reorder(Location, -Quantity), y = Quantity, fill = Resource)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Resource Share by Location", x = "Location", y = "Quantity (in tons)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "black")
      )
  })
  
  # Map: Global Mining Map
  output$mining_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        lng = top_mined_places$Longitude, 
        lat = top_mined_places$Latitude,
        label = top_mined_places$Location,
        color = "gold",
        radius = 8,
        popup = paste("Location:", top_mined_places$Location)
      )
  })
  
  # Globe View
  output$globe_view <- renderGlobe({
    globejs(
      lat = top_mined_places$Latitude,
      long = top_mined_places$Longitude,
      value = rep(10, nrow(top_mined_places)),
      color = "red",
      atmosphere = TRUE
    )
  })
  
  # Predictions
  output$prediction_text <- renderText({
    "If mining continues at the current rate, we predict a 25% depletion of key resources by 2050."
  })
  
  output$prediction_plot <- renderPlot({
    years <- 2024:2050
    depletion <- 100 - (0.75 ^ (years - 2024) * 100)
    
    ggplot(data.frame(Year = years, Depletion = depletion), aes(x = Year, y = Depletion)) +
      geom_line(color = "red", size = 1.2) +
      geom_point(color = "white", size = 2) +
      labs(title = "Predicted Resource Depletion", x = "Year", y = "Depletion (%)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        title = element_text(color = "white")
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)