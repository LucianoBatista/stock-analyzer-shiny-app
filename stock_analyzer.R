# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - LAYOUT -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary

# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

library(plotly)
library(tidyquant)
library(tidyverse)


source(file = "00_scripts/stock_analysis_functions.R")

stock_list_tbl <- get_stock_list("SP500")

# UI ----
# a function that is built using nested html components
# controls to look and appearance of our web app

# fluidPage() create a web page that we can add elements to it
ui <- navbarPage(
  title = "Stock Analyzer",
  collapsible = TRUE,
  theme = shinytheme("flatly"),
  
  tabPanel(
    title = "Analysis",
    # CSS ----
    # themeSelector(),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "style.css"
      )
    ),
    
    # 1.0 Header ----
    div(
      class = "container",
      id = "header",
      h1(class = "page-header", "Stock Analyzer", tags$small("by LUBA")),
      p(class = "lead", "This is the first mini-project completed in our Shiny Course")
    ),
    
    # 2.0 APP UI ----
    div(
      class = "container",
      id = "application_ui",
      # columns keep things responsive!!!
      column(
        width = 4,
        wellPanel(
          useShinyjs(),
          div(
            id = "input_main",
            pickerInput(
              inputId = "stock_selection",
              label = "Stock List (Pick One to Analyze)",
              choices = stock_list_tbl$label,
              multiple = F,
              selected = stock_list_tbl %>% filter(label %>% str_detect("AAPL")) %>% pull(label),
              options = pickerOptions(
                liveSearch = T,
                size = 10
              )
            )
          ),
          div(
            id = "input_buttons",
            actionButton(
              inputId = "analyze",
              label = "Analyze",
              icon = icon("download")
            ),
            div(
              class = "pull-right",
              actionButton(
                inputId = "settings_toggle",
                label = NULL,
                icon = icon("cog")
              )
            )
          ),
          div(
            id = "input_settings",
            hr(),
            sliderInput(
              inputId = "short_moving_avg",
              label = "Short Moving Average",
              value = 20,
              min = 5,
              max = 40,
              step = 1
            ),
            sliderInput(
              inputId = "long_moving_avg",
              label = "Long Moving Average",
              value = 60,
              min = 50,
              max = 120,
              step = 1
            )
          ) %>% shinyjs::hidden()
        )
      ),
      column(
        width = 8,
        div(
          class = "panel",
          div(
            class = "panel-header",
            h4(textOutput(outputId = "plot_header"))
          ),
          div(
            # plotting
            class = "panel-body",
            plotlyOutput(outputId = "plotly_plot")
          )
        )
      )
    ),
    
    # 3.0 Analyst commentary ----
    div(
      class = "container",
      id = "commentary",
      column(
        width = 12,
        div(
          class = "panel",
          div(
            class = "panel-header",
            h4("Analyst Commentary")
          ),
          div(
            class = "panel-body",
            verbatimTextOutput(outputId = "analyst_comment")
          )
        )
      )
    )
  )
)
  

# SERVER ----
# This is where reactive code is run
# Our backend
server <- function(input, output, session) {

  # stock symbol
  stock_symbol <- eventReactive(
    input$analyze,
    valueExpr = {
      get_symbol_from_user_input(input$stock_selection)
    },
    ignoreNULL = F
  )

  # plot header
  plot_header <- eventReactive(
    input$analyze,
    valueExpr = {
      input$stock_selection
    },
    ignoreNULL = F
  )

  output$plot_header <- renderText({
    plot_header()
  })

  # get stock data
  stock_data_tbl <- reactive({
    stock_symbol() %>%
      get_stock_data(
        mavg_short = input$short_moving_avg,
        mavg_long = input$long_moving_avg
      )
  })

  # Plotly plot
  output$plotly_plot <- renderPlotly({
    stock_data_tbl() %>%
      plot_stock_data()
  })

  # Generate commentary
  commentary <- eventReactive(
    eventExpr = input$analyze,
    valueExpr = {
      generate_commentary(
        stock_data = stock_data_tbl(),
        user_input = input$stock_selection
      )
    },
    ignoreNULL = F
  )

  output$analyst_comment <- renderText(commentary())
  
  shinyjs::onclick(
    id = "settings_toggle",
    expr = {
      shinyjs::toggle(
        id = "input_settings",
        anim = TRUE
      )
    }
  )
}

# RUN APP ----
shinyApp(ui = ui, server = server)
