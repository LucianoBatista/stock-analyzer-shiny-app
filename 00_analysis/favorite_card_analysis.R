# libraries
library(tidyquant)
library(tidyverse)

# helprs
source("00_scripts/stock_analysis_functions.R")
source("00_scripts/info_card.R")

stock_list_tbl <- get_stock_list("SP500")
favorite_list_on_start <- c("AAPL", "GOOG", "NFLX")

