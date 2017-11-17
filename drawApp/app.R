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
    fluidRow(
      column(8, plotOutput("new_plot"))
    )
  )
)


server <- function(input, output) {





  df <- submarines::knots_df

  #server side call of the drawr module
  drawChart <- callModule(
    shinydrawr,
    "outbreak_stats",
    data = df,
    draw_start = 0,
    x_key = "knots",
    y_key = "eff.prop",
    y_max = 1,
    y_min = 0
  )




  #logic for what happens after a user has drawn their values. Note this will fire on editing again too.
  observeEvent(drawChart(), {

    in_df <- submarines::knots_df

    drawnValues <- drawChart()

    message("drawnValues")
    print(drawnValues)
    print(length(drawnValues))

    drawnValues <- c(drawnValues, last(drawnValues))

    drawn_data <- in_df %>%
      mutate(eff.prop = drawnValues)



    output$new_plot <- renderPlot({

      ggplot(drawn_data, aes(x = knots, y = eff.prop)) +
        geom_line()

    })

  })

}

shinyApp(ui = ui, server = server)
# Run the application
