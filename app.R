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
      card(
        card_header("Age vs Length of Stay"),
        plotlyOutput("scatter_plot"),
        mod_download_plot_ui("dl_scatter", label = "Download")
      )
      ),
    nav_panel(
      "Charges",
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Avg Bill (CHARGES)",
          value = textOutput("avg_bill"),
          theme = "success",
          showcase = bsicons::bs_icon("cash-stack")
        ),
        value_box(
          title = "Avg LOS (days)",
          value = textOutput("avg_los"),
          theme = "info",
          showcase = bsicons::bs_icon("calendar3")
        ),
        value_box(
          title = "Cost per Day",
          value = textOutput("cost_per_day"),
          theme = "warning",
          showcase = bsicons::bs_icon("receipt")
        )
      )
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
  
  fmt_num <- function(x, digits = 1) {
    if (is.na(x) || length(x) == 0) return("NA")
    formatC(x, format = "f", digits = digits, big.mark = ",")
  }
  
  charges_metrics <- reactive({
    df <- filtered_data()
    req(nrow(df) >= 1)
    
    # Ensure numeric (defensive; harmless if already numeric)
    chg <- suppressWarnings(as.numeric(df$CHARGES))
    los <- suppressWarnings(as.numeric(df$LOS))
    
    # Avg bill: exclude missing CHARGES
    n_bill <- sum(!is.na(chg))
    avg_bill <- if (n_bill > 0) mean(chg, na.rm = TRUE) else NA_real_
    
    # Avg LOS: include LOS=0 (still valid), exclude NA if any
    n_los <- sum(!is.na(los))
    avg_los <- if (n_los > 0) mean(los, na.rm = TRUE) else NA_real_
    
    # Cost per day: exclude CHARGES NA and LOS <= 0 (avoid divide-by-zero)
    ok <- !is.na(chg) & !is.na(los) & los > 0
    n_cpd <- sum(ok)
    cost_per_day <- if (n_cpd > 0) mean(chg[ok] / los[ok]) else NA_real_
    
    list(
      avg_bill_text = paste0(fmt_num(avg_bill, 0), " (n=", n_bill, ")"),
      avg_los_text = paste0(fmt_num(avg_los, 1), " (n=", n_los, ")"),
      cost_per_day_text = paste0(fmt_num(cost_per_day, 0), " (n=", n_cpd, ")")
    )
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
  
  
  
  # output$scatter_plot <- renderPlotly({
  #   df <- filtered_data()
  #   req(nrow(df) >= 1)
  #   if(nrow(df) > 1000) {
  #     df <- df[sample(nrow(df), 1000), ]
  #   }
  #   p <- ggplot(df, aes(x = AGE, y = LOS, color = SEX)) +
  #     geom_point(alpha = 0.3) +
  #     labs(x = "Age", y = "Length of Stay (days)", color = "Sex") +
  #     geom_smooth(method = "lm", se = FALSE) +
  #     theme_minimal()
  #   ggplotly(p)
  # })
  
  scatter_plot_obj <- reactive({
    df <- filtered_data()
    req(nrow(df) >= 1)
    if(nrow(df) > 1000) {
      df <- df[sample(nrow(df), 1000), ]
    }
    ggplot(df, aes(x = AGE, y = LOS, color = SEX)) +
      geom_point(alpha = 0.3) +
      labs(x = "Age", y = "Length of Stay (days)", color = "Sex") +
      theme_minimal()
  })
  
  # Display as interactive plotly
  output$scatter_plot <- renderPlotly({
    ggplotly(scatter_plot_obj())
  })
  
  mod_download_plot_server("dl_scatter", filename = "scatter_age_los", figure = scatter_plot_obj)
  
  observeEvent(input$reset, {
    updateSelectInput(session, "outcome", selected = "All")
    updateSelectInput(session, "diagnosis", selected = "All")
    updateSelectInput(session, "drg", selected = "All")
    updateSliderInput(session, "age_range",
                      value = c(min(heart$AGE), max(heart$AGE)))
  })
  
  output$avg_bill <- renderText({
    charges_metrics()$avg_bill_text
  })
  
  output$avg_los <- renderText({
    charges_metrics()$avg_los_text
  })
  
  output$cost_per_day <- renderText({
    charges_metrics()$cost_per_day_text
  })
  
  
  
  output$data_table <- DT::renderDataTable({
    filtered_data()
  })
  
}

shinyApp(ui = ui, server = server)
