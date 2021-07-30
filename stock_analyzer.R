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
source(file = "00_scripts/info_card.R")
source(file = "00_scripts/generate_favorite_card.R")

stock_list_tbl <- get_stock_list("SP500")
current_user_favorites <- c("GOOG", "NFLX")

# UI ----
# a function that is built using nested html components
# controls to look and appearance of our web app

# fluidPage() create a web page that we can add elements to it
ui <- navbarPage(
  title = "Stock Analyzer",
  collapsible = TRUE,
  theme = shinytheme("paper"),
  
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
    
    # 2.0 FAVORITES ----
    div(
      class = "container",
      style = "padding: 0px;",
      id = "favorite_container",
      # user inputs ----
      div(
        class = "container",
        column(
          width = 12,
          h5(class = "pull-left", "Favorites"),
          actionButton(
            class = "pull-right",
            inputId = "favorites_clear",
            "Clear Favorites"
          ),
          actionButton(
            class = "pull-right",
            inputId = "favorites_toggle",
            "Show/Hide"
          )
        )
      ),
      # favorite cards ----
      div(
        class = "container hidden-sm hidden-xs",
        id = "favorite_cards",
        uiOutput(outputId = "multicard")
      )
    ),
    
    
    # 3.0 APP UI ----
    div(
      class = "container",
      id = "application_ui",
      # columns keep things responsive!!!
      # user inputs ----
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
                inputId = "favorites_add",
                label = NULL,
                icon = icon("heart")
              ),
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
            ),
            actionButton(
              inputId = "apply_and_save",
              label = "Apply and Save",
              icon = icon("save")
            )
          ) %>% shinyjs::hidden()
        )
      ),
      # plot panel ----
      column(
        width = 8,
        uiOutput("stock_charts")
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
  
  # Apply and Save ----
  mavg_short <- eventReactive(
    eventExpr = input$apply_and_save,
    valueExpr = {
      input$short_moving_avg
    },
    ignoreNULL = FALSE
  )
  
  mavg_long <- eventReactive(
    eventExpr = input$apply_and_save,
    valueExpr = {
      input$long_moving_avg
    },
    ignoreNULL = FALSE
  )
  
  stock_selected <- eventReactive(
    eventExpr = input$apply_and_save,
    valueExpr = {
      if (is.character(input$tab_panel_stock_chart)) {
        stock_selected <- input$tab_panel_stock_chart
      } else {
        stock_selected <- "Last Analysis"
      }
      
      return(stock_selected)
    },
    ignoreNULL = FALSE
  )

  output$plot_header <- renderText({
    plot_header()
  })

  # get stock data
  stock_data_tbl <- reactive({
    stock_symbol() %>%
      get_stock_data(
        mavg_short = mavg_short(),
        mavg_long = mavg_long()
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
  
  # favorites ----
  # reactive values
  reactive_values <- reactiveValues(favorites_list = current_user_favorites)
  
  # add favorites from inputs ----
  observeEvent(
    eventExpr = input$favorites_add,
    handlerExpr = {
      new_symbol <- get_symbol_from_user_input(input$stock_selection)
      reactive_values$favorites_list <- 
        c(reactive_values$favorites_list, new_symbol) %>% 
        unique()
    }
  )
  
  # render favorites cards ----
  output$multicard <- renderUI({
    generate_favorite_cards(
      favorites = reactive_values$favorites_list,
      mavg_short = mavg_short(),
      mavg_long = mavg_long()
    )
  })
  
  # delete favorites ----
  observeEvent(
    eventExpr = input$favorites_clear,
    handlerExpr = {
      modalDialog(
        title = "Clear Favorites",
        size = "m",
        easyClose = TRUE,
        p("Are you sure you want to remove favorites?"),
        br(),
        div(
          selectInput(inputId = "drop_list", label = "Remove single favorite",
                      choices = reactive_values$favorites_list %>% sort()),
          actionButton(inputId = "remove_single_favorite", label = "Clear Single",
                       class = "btn-warning"),
          actionButton(inputId = "remove_all_favorites", label = "Clear ALL",
                       class = "btn-danger")
        ),
        footer = modalButton("Exit")
      ) %>% showModal()
    }
  )
  
  # clear single ----
  observeEvent(
    eventExpr = input$remove_single_favorite,
    handlerExpr = {
      reactive_values$favorites_list <- setdiff(reactive_values$favorites_list, input$drop_list)
      
      updateSelectInput(session = session,
                        inputId = "drop_list",
                        choices = reactive_values$favorites_list %>% sort())
    }
  )
  
  # clear all ----
  observeEvent(
    eventExpr = input$remove_all_favorites,
    handlerExpr = {
      reactive_values$favorites_list <- vector()
      
      updateSelectInput(
        session = session,
        inputId = "drop_list",
        choices = reactive_values$favorites_list
      )
    }
  )
  
  # show/hide favorites ----
  shinyjs::onclick(
    id = "favorites_toggle",
    expr = {
      shinyjs::toggle(
        id = "favorite_cards",
        anim = TRUE
      )
    }
  )
  
  # favorite plots
  output$stock_charts <- renderUI({
    
    # last analysis panel ----
    tab_panel_1 <- list(tabPanel(
      title = "Last Analysis",
      div(
        class = "panel",
        div(
          class = "panel-header",
          h4(stock_symbol())
        ),
        div(
          # plotting
          class = "panel-body",
          plotlyOutput(outputId = "plotly_plot")
        )
      )
    ))

    # all others tabs panels ----
    favorite_tab_panels <- function(favorite_stock) {
        list(
          tabPanel(
            title = favorite_stock,
            div(
              class = "panel",
              div(
                class = "panel-header",
                h4(favorite_stock)
              ),
              div(
                # plotting
                class = "panel-body",
                favorite_stock %>% 
                  get_stock_data(mavg_short = mavg_short(),
                                 mavg_long = mavg_long()) %>% 
                  plot_stock_data()
              )
            )
          )
        )
    }
    
    # iterating throughout the favorites_list of stocks
    favorite_tab_panels_list <- vector()
    favorite_tab_panels_list <- reactive_values$favorites_list %>% 
      map(favorite_tab_panels)

    args <- list(id = "tab_panel_stock_chart",
                 type = "tabs",
                 selected = stock_selected(),
                 tab_panel_1) %>% 
      append(favorite_tab_panels_list)

    pmap(args, tabsetPanel)
    
  })
  
}

# RUN APP ----
shinyApp(ui = ui, server = server)
