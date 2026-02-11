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
    "Filters coming soon..."
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
