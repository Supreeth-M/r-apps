library(shiny)
library(leaflet)
library(threejs)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(tidyr)

# Enhanced sample data
weapon_stats <- data.frame(
  Weapon = paste("Weapon", 1:10),
  Usage = sample(1000:10000, 10),
  Country = c("USA", "Russia", "China", "India", "UK", "France", "Germany", "Japan", "Israel", "South Korea")
)

country_stats <- data.frame(
  Country = c("USA", "Russia", "China", "India", "UK", "France", "Germany", "Japan", "Israel", "South Korea", "Brazil", "Italy", "Australia", "Canada", "Turkey"),
  Latitude = c(37.0902, 61.5240, 35.8617, 20.5937, 55.3781, 46.6034, 51.1657, 36.2048, 31.0461, 35.9078, -14.2350, 41.8719, -25.2744, 56.1304, 38.9637),
  Longitude = c(-95.7129, 105.3188, 104.1954, 78.9629, -3.4360, 2.2137, 10.4515, 138.2529, 34.8516, 127.7669, -51.9253, 12.5674, 133.7751, -106.3468, 35.2433),
  MilitaryBudget = sample(50:800, 15) * 1e9,  # Random budget data
  ActivePersonnel = sample(100000:2000000, 15),
  Reserves = sample(50000:500000, 15)
)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$style("body { background-color: white; color: black; }"),
  
  titlePanel("Ultimate Army Statistics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      selectInput("chartType", "Chart Type:", 
                  choices = c("Bar Chart", "Pie Chart"),
                  selected = "Bar Chart"),
      selectInput("mapData", "Map Data:", 
                  choices = c("Military Budget", "Active Personnel", "Reserves"),
                  selected = "Military Budget"),
      hr(),
      h4("Scatter Plot Controls"),
      sliderInput("scatterPoints", "Number of Points:", min = 10, max = 100, value = 50),
      hr(),
      h4("Data Table"),
      DTOutput("dataTable")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Weapon Statistics", plotlyOutput("weaponChart")),
        tabPanel("Map View", leafletOutput("map")),
        tabPanel("Globe View", globeOutput("globe")),
        tabPanel("Data Summary", plotlyOutput("summaryChart")),
        tabPanel("Live Scatter Plot", plotlyOutput("scatterPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Weapon chart
  output$weaponChart <- renderPlotly({
    if (input$chartType == "Bar Chart") {
      p <- ggplot(weapon_stats, aes(x = reorder(Weapon, Usage), y = Usage, fill = Weapon)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.text = element_text(color = "black"),
              axis.title = element_text(color = "black")) +
        labs(x = "Weapon", y = "Usage", title = "Top 10 Weapons Usage")
      ggplotly(p)
    } else {
      plot_ly(weapon_stats, labels = ~Weapon, values = ~Usage, type = 'pie',
              textinfo = 'label+percent',
              marker = list(colors = RColorBrewer::brewer.pal(10, "Set3"))) %>%
        layout(title = "Top 10 Weapons Usage", 
               paper_bgcolor = "white",
               font = list(color = "black"))
    }
  })
  
  # Map view
  output$map <- renderLeaflet({
    map_data <- switch(input$mapData,
                       "Military Budget" = country_stats$MilitaryBudget,
                       "Active Personnel" = country_stats$ActivePersonnel,
                       "Reserves" = country_stats$Reserves)
    
    leaflet(country_stats) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        ~Longitude, ~Latitude, weight = 1, radius = ~sqrt(map_data / 1e9) * 3,
        color = "blue", fillOpacity = 0.7,
        label = ~paste(Country, "\n", input$mapData, ":", format(map_data, big.mark = ",")),
        labelOptions = labelOptions(style = list("color" = "black"),
                                    textsize = "12px", sticky = TRUE)
      )
  })
  
  # Globe view
  output$globe <- renderGlobe({
    globejs(lat = country_stats$Latitude, lon = country_stats$Longitude, 
            color = "red", pointsize = 2, atmosphere = TRUE)
  })
  
  # Data table
  output$dataTable <- renderDT({
    datatable(country_stats, options = list(scrollX = TRUE, pageLength = 5),
              style = "bootstrap", class = "cell-border stripe")
  })
  
  # Summary chart
  output$summaryChart <- renderPlotly({
    summary_data <- country_stats %>%
      select(Country, MilitaryBudget, ActivePersonnel, Reserves) %>%
      pivot_longer(cols = -Country, names_to = "Metric", values_to = "Value")
    
    p <- ggplot(summary_data, aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            axis.text = element_text(color = "black"),
            axis.title = element_text(color = "black")) +
      labs(title = "Country Statistics Overview")
    
    ggplotly(p)
  })
  
  # Live scatter plot
  output$scatterPlot <- renderPlotly({
    scatter_data <- data.frame(
      x = rnorm(input$scatterPoints),
      y = rnorm(input$scatterPoints)
    )
    
    p <- ggplot(scatter_data, aes(x = x, y = y)) +
      geom_point(color = "blue", size = 2) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            axis.text = element_text(color = "black"),
            axis.title = element_text(color = "black")) +
      labs(title = "Live Scatter Plot", x = "X-axis", y = "Y-axis")
    
    ggplotly(p)
  })
}

shinyApp(ui, server)