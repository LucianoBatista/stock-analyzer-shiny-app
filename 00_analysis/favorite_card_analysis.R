# libraries
library(tidyquant)
library(tidyverse)
library(shiny)

# helprs
source("00_scripts/stock_analysis_functions.R")
source("00_scripts/info_card.R")

composing_info_cards <- function(data) {
  column(
    width = 3,
    info_card(
      stock_name = data$stock,
      short_mavg = data$n_short,
      large_mavg = data$n_long
    )
  )
}


stock_list_tbl <- get_stock_list("SP500")
favorite_list_on_start <- c("AAPL", "GOOG", "NFLX")

stock_data_favorites_tbl <- favorite_list_on_start %>% 
  map(get_stock_data) %>% 
  set_names(favorite_list_on_start)

# moving avg

data <- stock_data_favorites_tbl[["AAPL"]]

get_stock_mavg_info <- function(data) {
  n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
  n_long <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
  
  data %>% 
    tail(1) %>% 
    mutate(
      mavg_warning_flag = mavg_short < mavg_long,
      n_short = n_short,
      n_long = n_long,
      pct_chg = (mavg_short - mavg_long) / mavg_long
    )
}

stock_data_favorites_tbl %>% 
  map(get_stock_mavg_info) %>% 
  bind_rows(.id = "stock")


# generate favorite cards
favorites <- favorite_list_on_start

generate_favorite_cards <- function(favorites, mavg_short, mavg_long) {
  pmap(list(stock_symbol = favorites, mavg_short = mavg_short, mavg_long = mavg_long), .f = get_stock_data) %>% 
    set_names(favorite_list_on_start) %>% 
    map(.f = get_stock_mavg_info) %>% 
    bind_rows(.id = "stock") %>% 
    split(.$stock) %>% 
    map(.f = composing_info_cards) %>% 
    tagList()
}

