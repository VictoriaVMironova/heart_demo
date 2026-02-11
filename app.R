library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(plotly)
source("R/helpers.R")
source("R/mod_download_plot.R")

heart <- readRDS("data/heart.rds")

ui <- page_sidebar(
  title = tags$span(
    tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px; filter: invert(1);"),
    "Heart Attack Dashboard"
  ),
  theme = bs_theme(bootswatch = "pulse"),
  sidebar = sidebar(
    selectInput(
      inputId = "outcome",
      label = "Outcome:",
      choices = c("All", "Survived", "Died")
    ),
    selectInput(
      inputId = "diagnosis",
      label = "Diagnosis:",
      choices = c("All", sort(unique(as.character(heart$DIAGNOSIS)))),
      selected = "All"
    ),
    selectInput(
      inputId = "drg",
      label = "DRG:",
      choices = c("All", sort(unique(as.character(heart$DRG)))),
      selected = "All"
    ),
    sliderInput(
      inputId = "age_range",
      label = "Age Range:",
      min = min(heart$AGE),
      max = max(heart$AGE),
      value = c(min(heart$AGE), max(heart$AGE))
    ),
    actionButton(
      inputId = "reset",
      label = "Reset",
      icon = bsicons::bs_icon("arrow-counterclockwise")
    )
  ),
  #------------------------------------------------
  #tabs
  navset_tab(
    nav_panel(
      "Overview",
      layout_column_wrap(
        width = 1/2,
        value_box(
          title = "Female Mortality",
          value = textOutput("f_mortality"),
          theme = "danger",
          showcase = bsicons::bs_icon("gender-female")
        ),
        value_box(
          title = "Male Mortality",
          value = textOutput("m_mortality"),
          theme = "primary",
          showcase = bsicons::bs_icon("gender-male")
        )
      ),
      card(
        card_header("Age Distribution"),
        plotOutput("age_hist"),
        mod_download_plot_ui("dl_age", label = "Download")
      )
      
      ),
    nav_panel(
      "Explore",
      plotlyOutput("scatter_plot")
      ),
    nav_panel(
      "Data", 
      DT::dataTableOutput("data_table"))
    )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    d <- heart
    if (input$outcome != "All") {
      d <- d[d$DIED == input$outcome, ]
    }
    if (input$diagnosis != "All") {
      d <- d[as.character(d$DIAGNOSIS) == input$diagnosis, ]
    }
    if (input$drg != "All") {
      d <- d[as.character(d$DRG) == input$drg, ]
    }
    d <- d[d$AGE >= input$age_range[1] & d$AGE <= input$age_range[2], ]
    d
  })
  
  # Female stats
  output$f_mortality <- renderText({
    compute_mortality(filtered_data()[filtered_data()$SEX == "Female", ])
  })
  
  # Male stats
  output$m_mortality <- renderText({
    compute_mortality(filtered_data()[filtered_data()$SEX == "Male", ])
  })
  
  
  # Create the age plot as a reactive (reusable)
  age_plot <- reactive({
    req(nrow(filtered_data()) >= 2)
    ggplot(filtered_data(), aes(x = AGE, fill = DIED)) +
      geom_density(alpha = 0.5) +
      labs(x = "Age", y = "Density", fill = "DIED") +
      facet_wrap(~ SEX) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
  
  # Display the plot
  output$age_hist <- renderPlot({
    age_plot()
  })
  
  mod_download_plot_server("dl_age", filename = "age_distribution", figure = age_plot)
  
  
  output$scatter_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) >= 1)
    if(nrow(df) > 1000) {
      df <- df[sample(nrow(df), 1000), ]
    }
    p <- ggplot(df, aes(x = AGE, y = LOS, color = SEX)) +
      geom_point(alpha = 0.3) +
      labs(x = "Age", y = "Length of Stay (days)", color = "Sex") +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal()
    ggplotly(p)
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "outcome", selected = "All")
    updateSelectInput(session, "diagnosis", selected = "All")
    updateSelectInput(session, "drg", selected = "All")
    updateSliderInput(session, "age_range",
                      value = c(min(heart$AGE), max(heart$AGE)))
  })
  
  
  output$data_table <- DT::renderDataTable({
    filtered_data()
  })
  
}

shinyApp(ui = ui, server = server)
