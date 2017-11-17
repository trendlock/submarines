#devtools::install_github("nstrayer/shinysense")
library(shiny)
library(shinythemes)
library(shinysense)
library(tidyverse)
library(shiny)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("shinydrawr no reveal"),
  hr(),
  fluidRow(
    column(width = 8,
           shinydrawrUI("outbreak_stats")),
    column(width = 3, offset = 1,
           h2("Drawn values:"), tableOutput("displayDrawn"))
  )
)


server <- function(input, output) {


  cutoff <- 0


  df <- submarines::knots_df
  #server side call of the drawr module
  drawChart <- callModule(
    shinydrawr,
    "outbreak_stats",
    data = df,
    draw_start = cutoff,
    x_key = "row.id",
    y_key = "ran.vbl",
    y_max = 1,
    y_min = 0
  )




  #logic for what happens after a user has drawn their values. Note this will fire on editing again too.
  observeEvent(drawChart(), {
    drawnValues <- drawChart()

    message("drawnValues")
    print(drawnValues)


    drawn_data <- tibble(drawn = drawnValues) %>%
      mutate(row.id = row_number()) %>%
      select(row.id, drawn)

    output$displayDrawn <- renderTable(drawn_data)
  })

}

shinyApp(ui = ui, server = server)
# Run the application
