
library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(lubridate)
library(shiny.semantic)

#devtools::install_github("nstrayer/shinysense")
library(shinysense)

#devtools::install_github("trendlock/submarines", auth_token = "")
library(submarines)

shinyServer(function(input, output) {



  df <- submarines::knots_df

  #server side call of the drawr module
  drawChart <- callModule(
    shinydrawr,
    "outbreak_stats",
    data = df,
    draw_start = 0.5,
    x_key = "kts",
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

    get_out <<- drawnValues

    drawnValues <- c(drawnValues, last(drawnValues))

    drawn_data <- in_df %>%
      mutate(eff.prop = drawnValues)



    output$new_plot <- renderPlotly({
      ggplot(drawn_data, aes(x = knots, y = eff.prop)) +
        geom_line() +
        theme_classic()
      ggplotly()
    })
    output$new_plot1 <- renderPlotly({
      ggplot(drawn_data, aes(x = knots, y = eff.prop)) +
        geom_line()+
        theme_classic()
      ggplotly()
    })
    output$new_plot2 <- renderPlotly({
      ggplot(drawn_data, aes(x = knots, y = eff.prop)) +
        geom_line()+
        theme_classic()
      ggplotly()
    })
    output$new_plot3 <- renderPlotly({
      ggplot(drawn_data, aes(x = knots, y = eff.prop)) +
        geom_line()+
        theme_classic()
      ggplotly()
    })

  })

})
