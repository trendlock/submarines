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


shinyUI(
  navbarPage(
    theme = shinytheme("united"),
    title = "subsApp",



    # Tab 1 ====
    tabPanel(
      title = "First Tab Title",


      # CSS for progress bar ====
      # tags$head(
      #   tags$style(HTML("
      #                   .progress-striped .bar {
      #                   background-color: #149bdf;
      #                   background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
      #                   background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      #                   background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      #                   background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      #                   background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      #                   -webkit-background-size: 40px 40px;
      #                   -moz-background-size: 40px 40px;
      #                   -o-background-size: 40px 40px;
      #                   background-size: 40px 40px;
      #                   }")),
      #       tags$style(type = 'text/css', "footer{position: absolute; bottom:5%; padding:5px;}")),

      shinydrawrUI("outbreak_stats"),

      fluidRow(

        # Main Subs Plot ====
        column(6, shinydrawrUI("outbreak_stats")),
        column(6,
               h3("Assumptions"),
               semanticPage(
                 fluidRow(
                   column(4,
                          # Hotel Load slider ====
                          div(class = "ui card",
                              div(class = "content",
                                  div(class = "right floated meta", "Other info"),
                                  uiicon("settings"),
                                  "Hotel Load"),
                              div(class = "content",
                                  sliderInput("hotel_load", "Hotel Load", 75, 300, 500),
                                  textOutput("cor_finder_day_text"),
                                  uiOutput("hide_events")),
                              div(class = "image",
                                  plotOutput("cor_finder_day_plot0")))),
                   column(4,

                             # Hotel Load Match ====
                             div(class = "ui card",
                                 div(class = "content",
                                     div(class = "right floated meta", "Other info"),
                                     uiicon("settings"),
                                     "Hotel Load"),
                                 div(class = "content",
                                     actionButton("go_but", "Select"),
                                     actionButton("HL_jet_but", "Jet"), actionButton("HL_prop_but", "Prop"),
                                     textOutput("cor_finder_day_text"),
                                     uiOutput("hide_events")),
                                 div(class = "image",
                                     plotOutput("cor_finder_day_plot0"))))
                 )
                )))
               )
      )
  )
