library(shiny)
library(bslib)  
library(tidyverse)
library(plotly)

url <- "https://raw.githubusercontent.com/abhijaymovva/shiny-esports-project/refs/heads/main/esports_player_performance_tournament_analytics.csv"

esports_data <- read_csv(url)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "lux"), # Using 'lux' for a theme
  titlePanel("Pro E-Sports Analytics: Player Shooting Accuracy Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore how player roles and accuracy impact K/D ratios"),
      
      # Dynamic Query 1: Filter by Player Role
      selectInput("role_input", "Select Player Role:", 
                  choices = c("All", unique(esports_data$player_role)),
                  selected = "All"),
      
      # Dynamic Query 2: Filter by Accuracy Threshold
      sliderInput("accuracy_input", "Minimum Accuracy (%)", 
                  min = 0, max = 100, value = 50),
      
      hr(),
      p("This dashboard analyzes 'KD' (Kills/Deaths) across different maps and match types."),
      p(em("Data Source: Kaggle E-Sports Tournament Analytics"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Performance Scatter", plotlyOutput("performance_plot")),
        tabPanel("Team Stats", tableOutput("team_summary"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to filter data once (Modular Code)
  filtered_data <- reactive({
    df <- esports_data
    
    if (input$role_input != "All") {
      df <- df %>% filter(player_role == input$role_input)
    }
    
    df %>% filter(accuracy_percent >= input$accuracy_input)
  })
  
  # Output 1: High-Density Visualization (Kills vs Deaths)
  output$performance_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = deaths, y = kills, color = team_name)) +
      geom_point(aes(text = paste("Player ID:", player_id, "<br>Map:", map_played)), 
                 alpha = 0.6, size = 3) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") + # K/D parity line
      theme_minimal() +
      labs(title = "Kills vs. Deaths (K/D Analysis)",
           x = "Total Deaths", y = "Total Kills")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Output 2: Summary Table for Teams
  output$team_summary <- renderTable({
    filtered_data() %>%
      group_by(team_name) %>%
      summarise(
        Avg_Kills = mean(kills),
        Avg_Assists = mean(assists),
        Avg_Accuracy = mean(accuracy_percent),
        Match_Count = n()
      ) %>%
      arrange(desc(Avg_Kills))
  })
}

shinyApp(ui = ui, server = server)
