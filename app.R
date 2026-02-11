library(shiny)
library(bslib)
library(DT)

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
    )
  ),
  navset_tab(
    nav_panel("Overview", "Overview content coming soon..."),
    nav_panel("Explore", "Explore content coming soon..."),
    nav_panel(
      "Data", 
      DT::dataTableOutput("data_table"))
    )
)

server <- function(input, output, session) {
  output$data_table <- DT::renderDataTable({
    heart
  })
  
}

shinyApp(ui = ui, server = server)
