library(shiny)
library(bslib)

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
    nav_panel("Data", "Data content coming soon...")
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui = ui, server = server)
