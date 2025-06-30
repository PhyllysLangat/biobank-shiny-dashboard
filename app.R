# Load necessary libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Sample Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Status by Protocol", tabName = "status", icon = icon("check-circle")),
      menuItem("Turnover", tabName = "turnover", icon = icon("chart-bar")),
      menuItem("00:00 Timestamps", tabName = "timestamps", icon = icon("clock")),
      menuItem("Pre Analytical Delay", tabName = "delay", icon = icon("hourglass-half")),
      menuItem("Missing Values", tabName = "missing", icon = icon("exclamation-triangle")),
      menuItem("Analytical Delay", tabName = "analytical", icon = icon("flask"))
    )
  ),
  dashboardBody(
    tabItems(
      # Status Tab
      tabItem(tabName = "status",
              fluidRow(
                box(width = 12,
                    title = "Select a Protocol",
                    selectInput("protocol_select", "Protocol:", choices = NULL)
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Monthly Sample Status Distribution",
                    plotOutput("status_plot"))
              )
      ),
      
      # Turnover Tab
      tabItem(tabName = "turnover",
              fluidRow(
                box(width = 6,
                    title = "Select Protocol",
                    selectInput("turnover_protocol", "Protocol:", choices = NULL)
                ),
                box(width = 6,
                    title = "Choose Time Unit",
                    selectInput("time_unit", "Time Unit:", choices = c("Week", "Month"))
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Sample Turnover (Parent vs Derivative)",
                    plotOutput("turnover_plot"))
              )
      ),
      
      # 00:00 Timestamps Tab
      tabItem(tabName = "timestamps",
              fluidRow(
                box(width = 12,
                    title = "Select Protocol",
                    selectInput("timestamp_protocol", "Protocol:", choices = NULL)
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "00:00 Timestamps Count",
                    plotOutput("timestamp_plot"))
              )
      ),
      
      # Delay Tab
      tabItem(tabName = "delay",
              fluidRow(
                box(width = 4,
                    title = "Select Protocol",
                    selectInput("delay_protocol", "Protocol:", choices = NULL, selected = "All")
                ),
                box(width = 4,
                    title = "Select Time Unit",
                    selectInput("delay_time_unit", "Time Unit:", choices = c("All", "Month", "Week"), selected = "All")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Delay Distribution for Parent Samples",
                    plotOutput("delay_plot"))
              )
      ),
      
      # Missing Values Tab
      tabItem(tabName = "missing",
              fluidRow(
                box(width = 6,
                    title = "Select Protocol",
                    selectInput("missing_protocol", "Protocol:", choices = NULL, selected = "All")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Missing Timestamp Values by Status",
                    plotOutput("missing_plot"))
              )
      ),
      
      # Analytical Delay Tab
      tabItem(tabName = "analytical",
              fluidRow(
                box(width = 12,
                    title = "Derivative Samples Exceeding Targets",
                    plotOutput("derivative_delay_plot")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Analytical Delay Distribution (Boxplot)",
                    plotOutput("boxplot_delay_plot")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Clean and prepare data
  cleaned_df <- reactive({
    df %>%
      filter(!is.na(dt_reception)) %>%
      mutate(
        dt_reception = ymd_hms(dt_reception),
        dt_collect = ymd_hms(dt_collect),
        dt_transform = ymd_hms(dt_transform),
        dt_storage = ymd_hms(dt_storage),
        support_statut = as.character(support_statut),
        nom_du_protocole = as.character(nom_du_protocole),
        month = floor_date(dt_reception, unit = "month"),
        week = floor_date(dt_reception, unit = "week", week_start = 1),
        sample_type = ifelse(is.na(type_dechantillon_parent), "Derivative", "Parent"),
        delay_hours = as.numeric(difftime(dt_reception, dt_collect, units = "hours"))
      )
  })
  
  # Update protocol dropdowns
  observe({
    protocols <- sort(unique(cleaned_df()$nom_du_protocole))
    updateSelectInput(session, "protocol_select", choices = protocols)
    updateSelectInput(session, "turnover_protocol", choices = protocols)
    updateSelectInput(session, "timestamp_protocol", choices = protocols)
    updateSelectInput(session, "delay_protocol", choices = c("All", protocols))
    updateSelectInput(session, "missing_protocol", choices = c("All", protocols))
  })
  
  # Status plot (your existing plot code goes here)
  output$status_plot <- renderPlot({
    # Your existing code for status plot
  })
  
  # Turnover plot (your existing plot code goes here)
  output$turnover_plot <- renderPlot({
    # Your existing code for turnover plot
  })
  
  # 00:00 timestamps plot (your existing plot code goes here)
  output$timestamp_plot <- renderPlot({
    # Your existing code for timestamps plot
  })
  
  # Delay plot (your existing plot code goes here)
  output$delay_plot <- renderPlot({
    # Your existing code for delay plot
  })
  
  # Missing values plot (your existing plot code goes here)
  output$missing_plot <- renderPlot({
    # Your existing code for missing values plot
  })
  
  # Derivative delay plot - fully implemented
  output$derivative_delay_plot <- renderPlot({
    req(exists("derivative_targets"))
    df %>%
      filter(is.na(type_dechantillon_parent)) %>%  # derivative samples only
      mutate(
        dt_transform = ymd_hms(dt_transform),
        dt_reception = ymd_hms(dt_reception),
        delay_hours = as.numeric(difftime(dt_transform, dt_reception, units = "hours"))
      ) %>%
      left_join(derivative_targets, by = "nom_du_protocole") %>%
      filter(!is.na(target_hours)) %>%
      mutate(exceeds = delay_hours > target_hours) %>%
      count(nom_du_protocole, exceeds) %>%
      ggplot(aes(x = nom_du_protocole, y = n, fill = exceeds)) +
      geom_col(position = "stack") +
      labs(title = "Derivative Samples Exceeding Target", x = "Protocol", y = "Count", fill = "Exceeds Target") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Analytical delay boxplot - fully implemented
  output$boxplot_delay_plot <- renderPlot({
    df %>%
      mutate(
        dt_collect = ymd_hms(dt_collect),
        dt_reception = ymd_hms(dt_reception),
        dt_transform = ymd_hms(dt_transform),
        dt_storage = ymd_hms(dt_storage)
      ) %>%
      transmute(
        collect_to_reception = as.numeric(difftime(dt_reception, dt_collect, units = "hours")),
        reception_to_transform = as.numeric(difftime(dt_transform, dt_reception, units = "hours")),
        transform_to_storage = as.numeric(difftime(dt_storage, dt_transform, units = "hours"))
      ) %>%
      pivot_longer(everything(), names_to = "interval", values_to = "delay") %>%
      filter(!is.na(delay)) %>%
      ggplot(aes(x = interval, y = delay)) +
      geom_boxplot(fill = "skyblue") +
      labs(title = "Analytical Delay Distributions", x = "Interval", y = "Delay (hours)") +
      theme_minimal()
  })
  
}

# Run the app
shinyApp(ui, server)
