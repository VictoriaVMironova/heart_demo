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
      ),
      card(
        card_header("Summary Statistics"),
        tableOutput("overview_summary")
      )
      
      ),
    nav_panel(
      "Length of Stay",
      layout_column_wrap(
        width = 1/2,
        value_box(
          title = "Female Avg LOS",
          value = textOutput("f_los_avg"),
          theme = "danger",
          showcase = bsicons::bs_icon("gender-female")
        ),
        value_box(
          title = "Male Avg LOS",
          value = textOutput("m_los_avg"),
          theme = "primary",
          showcase = bsicons::bs_icon("gender-male")
        )
      ),
      card(
        card_header("Length of Stay Distribution"),
        plotOutput("los_density")
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
      ),
      card(
        card_header("Daily Charges (Cost per Day)"),
        plotlyOutput("charges_boxplot", height = "450px"),
        mod_download_plot_ui("dl_charges_boxplot", label = "Download")
      )
    ),
    nav_panel(
      "Data",
      downloadButton("dl_filtered_csv", "Download filtered data (CSV)"),
      tags$br(),
      DT::dataTableOutput("data_table")
    ),
    nav_panel(
      "About",
      card(
        card_header("About This Dashboard"),
        tags$p("This dashboard explores heart attack outcomes and hospital utilization patterns."),
        tags$p("The data source is a historical cohort of 12,844 heart attack patients from New York State in 1993."),
        tags$h5("Why women can have higher heart attack mortality"),
        tags$ul(
          tags$li("Women are often older at presentation and have more comorbidities, which increases baseline risk."),
          tags$li("Women more often present without classic chest pain, which can delay diagnosis and treatment."),
          tags$li("Women with STEMI can experience longer delays to reperfusion and longer ischemic time."),
          tags$li("In routine care, women may receive less guideline-directed therapy in some settings."),
          tags$li("Sex-related biology differences (including more MINOCA) can complicate diagnosis and management.")
        ),
        tags$p("These findings are summarized from the project note in heart_attack.md.")
      )
    )
    
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
  
  output$overview_summary <- renderTable({
    df <- filtered_data()
    req(nrow(df) >= 1)
    
    sexes <- sort(unique(as.character(df$SEX)))
    
    do.call(rbind, lapply(sexes, function(sx) {
      d <- df[df$SEX == sx, , drop = FALSE]
      age <- suppressWarnings(as.numeric(d$AGE))
      los <- suppressWarnings(as.numeric(d$LOS))
      charges <- suppressWarnings(as.numeric(d$CHARGES))
      
      data.frame(
        Sex = sx,
        `Patients (n)` = format(nrow(d), big.mark = ","),
        `Average Age` = fmt_num(mean(age, na.rm = TRUE), 1),
        `Median Age` = fmt_num(median(age, na.rm = TRUE), 1),
        `Average LOS (days)` = fmt_num(mean(los, na.rm = TRUE), 1),
        `Median LOS (days)` = fmt_num(median(los, na.rm = TRUE), 1),
        `Average Charges` = fmt_num(mean(charges, na.rm = TRUE), 0),
        `Median Charges` = fmt_num(median(charges, na.rm = TRUE), 0),
        `Mortality Rate` = compute_mortality(d),
        check.names = FALSE
      )
    }))
  }, striped = TRUE, spacing = "s", bordered = TRUE, align = "lrrrrrrrr")
  
  output$f_los_avg <- renderText({
    df <- filtered_data()[filtered_data()$SEX == "Female", , drop = FALSE]
    los <- suppressWarnings(as.numeric(df$LOS))
    if (length(los) == 0 || all(is.na(los))) return("N/A")
    paste0(fmt_num(mean(los, na.rm = TRUE), 1), " days")
  })
  
  output$m_los_avg <- renderText({
    df <- filtered_data()[filtered_data()$SEX == "Male", , drop = FALSE]
    los <- suppressWarnings(as.numeric(df$LOS))
    if (length(los) == 0 || all(is.na(los))) return("N/A")
    paste0(fmt_num(mean(los, na.rm = TRUE), 1), " days")
  })
  
  los_density_plot <- reactive({
    df <- filtered_data()
    los <- suppressWarnings(as.numeric(df$LOS))
    df <- df[!is.na(los), , drop = FALSE]
    req(nrow(df) >= 2)
    df$LOS <- los[!is.na(los)]
    
    ggplot(df, aes(x = LOS, fill = DIED)) +
      geom_density(alpha = 0.5) +
      labs(x = "Length of Stay (days)", y = "Density", fill = "DIED") +
      facet_wrap(~ SEX) +
      theme_minimal() +
      density_axis_theme
  })
  
  output$los_density <- renderPlot({
    los_density_plot()
  })
  
  density_axis_theme <- theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
  
  
  # Create the age plot as a reactive (reusable)
  age_plot <- reactive({
    req(nrow(filtered_data()) >= 2)
    ggplot(filtered_data(), aes(x = AGE, fill = DIED)) +
      geom_density(alpha = 0.5) +
      labs(x = "Age", y = "Density", fill = "DIED") +
      facet_wrap(~ SEX) +
      theme_minimal() +
      density_axis_theme
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
  
  charges_boxplot_obj <- reactive({
    df <- filtered_data()
    req(nrow(df) >= 1)
    
    # Defensive numeric conversion
    chg <- suppressWarnings(as.numeric(df$CHARGES))
    los <- suppressWarnings(as.numeric(df$LOS))
    
    # Row-level cost/day (exclude bad rows)
    ok <- !is.na(chg) & !is.na(los) & los > 0
    df2 <- df[ok, , drop = FALSE]
    req(nrow(df2) >= 1)
    
    df2$cost_per_day <- chg[ok] / los[ok]
    
    # Optional scalability: if DRG == "All", facet only top N DRGs by count
    if (isTruthy(input$drg) && input$drg == "All") {
      top_n <- 12
      top_drg <- df2 %>%
        count(DRG, sort = TRUE) %>%
        slice_head(n = top_n) %>%
        pull(DRG)
      df2 <- df2 %>% filter(DRG %in% top_drg)
    }
    
    # Ensure clean facet labels & consistent ordering
    df2 <- df2 %>%
      mutate(
        SEX = factor(SEX),
        DRG = factor(DRG)
      )
    
    ggplot(df2, aes(x = SEX, y = cost_per_day)) +
      geom_boxplot(outlier.alpha = 0.25, na.rm = TRUE) +
      labs(
        x = "Sex",
        y = "Charges per day",
        caption = "Cost per day = CHARGES / LOS (excluding LOS ≤ 0 and missing values)"
      ) +
      facet_wrap(~ DRG, scales = "free_y") +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11)
      )
  })
  
  output$charges_boxplot <- renderPlotly({
    ggplotly(charges_boxplot_obj(), tooltip = "text") %>%
      layout(boxmode = "group")
  })
  
  mod_download_plot_server(
    "dl_charges_boxplot",
    filename = "daily_charges_boxplot",
    figure = charges_boxplot_obj
  )
  
  output$data_table <- DT::renderDataTable({
    filtered_data()
  })
  output$dl_filtered_csv <- downloadHandler(
    filename = function() {
      paste0("heart_filtered_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      df <- filtered_data()
      # Write as CSV
      write.csv(df, file, row.names = FALSE, na = "")
    }
  )
  
}

shinyApp(ui = ui, server = server)
