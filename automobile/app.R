library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Global Automotive Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Country Insights", tabName = "country", icon = icon("globe")),
      menuItem("Company Metrics", tabName = "company", icon = icon("industry")),
      menuItem("Global Map", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(
      ".skin-blue .main-header {background-color: #000000;} 
       .skin-blue .main-sidebar {background-color: #000000;} 
       .content-wrapper {background-color: #1E1E1E;} 
       .box {background-color: #2E2E2E; color: white;} 
       .value-box {background-color: #333333; color: white;} 
       .sidebar-menu > li > a {color: white;}"
    ))),
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Global Sales Trends", status = "primary", solidHeader = TRUE, plotlyOutput("salesTrendPlot"), width = 6),
                box(title = "Expected Stock Trends", status = "danger", solidHeader = TRUE, plotlyOutput("stockTrendPlot"), width = 6)
              ),
              fluidRow(
                box(title = "Key Metrics", status = "info", solidHeader = TRUE, 
                    valueBoxOutput("globalRevenue"),
                    valueBoxOutput("globalMarketShare"),
                    valueBoxOutput("globalProduction"), width = 4),
                box(title = "Expected Price Trends", status = "warning", solidHeader = TRUE, plotlyOutput("priceTrendPlot"), width = 8)
              ),
              fluidRow(
                box(title = "Top-Selling Models", status = "success", solidHeader = TRUE, tableOutput("topModelsTable"), width = 12)
              )
      ),
      
      # Country Insights Tab
      tabItem(tabName = "country",
              fluidRow(
                box(title = "Country-Wise Performance", status = "warning", solidHeader = TRUE, 
                    selectInput("country", "Select Country:", 
                                choices = c("India", "USA", "Germany", "Japan", "China", "UK", "Brazil", "South Korea", "Canada", "Australia")),
                    plotlyOutput("countryPlot"), width = 12)
              )
      ),
      
      # Company Metrics Tab
      tabItem(tabName = "company",
              fluidRow(
                box(title = "Top Companies", status = "danger", solidHeader = TRUE, 
                    tableOutput("companyTable"), width = 6),
                box(title = "Revenue Trends", status = "primary", solidHeader = TRUE, plotlyOutput("revenuePlot"), width = 6)
              )
      ),
      
      # Global Map Tab
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Top Sellers by Country", status = "primary", solidHeader = TRUE, 
                    leafletOutput("salesMap"), width = 12)
              )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Real-World Data (Example)
  mapData <- data.frame(
    Country = c("India", "USA", "Germany", "Japan", "China", "UK", "Brazil", "South Korea", "Canada", "Australia"),
    Company = c("Maruti Suzuki", "Ford", "Volkswagen", "Toyota", "BYD", "BMW", "Chevrolet", "Hyundai", "Tesla", "Mazda"),
    Latitude = c(20.5937, 37.0902, 51.1657, 36.2048, 35.8617, 55.3781, -14.2350, 35.9078, 56.1304, -25.2744),
    Longitude = c(78.9629, -95.7129, 10.4515, 138.2529, 104.1954, -3.4360, -51.9253, 127.7669, -106.3468, 133.7751)
  )
  
  salesData <- data.frame(
    Company = c("Maruti Suzuki", "Ford", "Volkswagen", "Toyota", "BYD", "BMW", "Chevrolet", "Hyundai", "Tesla", "Mazda"),
    Sales = c(1600000, 2100000, 3000000, 8500000, 1900000, 2300000, 1400000, 3500000, 900000, 1200000),
    Country = mapData$Country,
    CO2_Emissions = c(120, 160, 140, 110, 100, 150, 170, 130, 90, 125),
    FuelEfficiency = c(21, 18, 25, 22, 27, 20, 15, 23, 30, 24)
  )
  
  topModels <- data.frame(
    Model = c("Swift", "F-150", "Golf", "Corolla", "Tang", "3 Series", "Silverado", "Elantra", "Model 3", "CX-5"),
    Sales = c(400000, 900000, 450000, 1200000, 600000, 550000, 300000, 800000, 350000, 280000),
    Company = salesData$Company
  )
  
  # Render Sales Trends
  output$salesTrendPlot <- renderPlotly({
    ggplot(salesData, aes(x = reorder(Company, -Sales), y = Sales, fill = Company)) +
      geom_bar(stat = "identity") + theme_minimal() + 
      theme(plot.background = element_rect(fill = "#1E1E1E", color = NA),
            panel.background = element_rect(fill = "#2E2E2E", color = NA),
            legend.background = element_rect(fill = "#2E2E2E"),
            legend.text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white")) + 
      labs(title = "Global Sales by Company", x = "Company", y = "Sales")
  })
  
  # Render Expected Stock Trends
  output$stockTrendPlot <- renderPlotly({
    ggplot(salesData, aes(x = reorder(Company, -Sales), y = Sales * 0.8, fill = Company)) +
      geom_bar(stat = "identity") + theme_minimal() + 
      theme(plot.background = element_rect(fill = "#1E1E1E", color = NA),
            panel.background = element_rect(fill = "#2E2E2E", color = NA),
            legend.background = element_rect(fill = "#2E2E2E"),
            legend.text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white")) + 
      labs(title = "Expected Stock Trends", x = "Company", y = "Expected Stock")
  })
  
  # Render Expected Price Trends
  output$priceTrendPlot <- renderPlotly({
    ggplot(salesData, aes(x = reorder(Company, -Sales), y = Sales * 0.05, fill = Company)) +
      geom_line(stat = "identity", group = 1) + theme_minimal() + 
      theme(plot.background = element_rect(fill = "#1E1E1E", color = NA),
            panel.background = element_rect(fill = "#2E2E2E", color = NA),
            legend.background = element_rect(fill = "#2E2E2E"),
            legend.text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white")) + 
      labs(title = "Expected Price Trends", x = "Company", y = "Price ($K)")
  })
  
  # Render Global Metrics
  output$globalRevenue <- renderValueBox({
    valueBox("$2.5T", "Global Revenue", icon = icon("dollar-sign"), color = "blue")
  })
  
  output$globalMarketShare <- renderValueBox({
    valueBox("38%", "Global Market Share", icon = icon("chart-pie"), color = "green")
  })
  
  output$globalProduction <- renderValueBox({
    valueBox("95M", "Vehicles Produced", icon = icon("car"), color = "yellow")
  })
  
  # Render Top Models Table
  output$topModelsTable <- renderTable({
    topModels
  })
  
  # Render Country Insights
  output$countryPlot <- renderPlotly({
    salesData %>% filter(Country == input$country) %>% 
      ggplot(aes(x = Company, y = Sales, fill = Company)) +
      geom_bar(stat = "identity") + theme_minimal() + 
      theme(plot.background = element_rect(fill = "#1E1E1E", color = NA),
            panel.background = element_rect(fill = "#2E2E2E", color = NA),
            legend.background = element_rect(fill = "#2E2E2E"),
            legend.text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white")) + 
      labs(title = paste("Top Companies in", input$country), x = "Company", y = "Sales")
  })
  
  # Render Company Metrics
  output$companyTable <- renderTable({
    salesData %>% select(Company, Sales, CO2_Emissions, FuelEfficiency)
  })
  
  output$revenuePlot <- renderPlotly({
    ggplot(salesData, aes(x = reorder(Company, -Sales), y = Sales, fill = Company)) +
      geom_bar(stat = "identity") + theme_minimal() + 
      theme(plot.background = element_rect(fill = "#1E1E1E", color = NA),
            panel.background = element_rect(fill = "#2E2E2E", color = NA),
            legend.background = element_rect(fill = "#2E2E2E"),
            legend.text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white")) + 
      labs(title = "Company Revenue Trends", x = "Company", y = "Revenue ($B)")
  })
  
  # Render Global Map
  output$salesMap <- renderLeaflet({
    leaflet(mapData) %>% 
      addTiles() %>% 
      addCircleMarkers(~Longitude, ~Latitude, label = ~paste("Country:", Country, "<br>Top Seller:", Company), color = "blue", radius = 5, fillOpacity = 0.8)
  })
}

# Run the app
shinyApp(ui, server)
