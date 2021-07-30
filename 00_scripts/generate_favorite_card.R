composing_info_cards <- function(data) {
  column(
    width = 3,
    info_card(
      stock_name = data$stock,
      short_mavg = data$n_short,
      large_mavg = data$n_long,
      increase_pct = round(data$pct_chg, digits = 5)
    )
  )
}

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

generate_favorite_cards <- function(favorites, mavg_short, mavg_long, 
                                    from = today() - days(180), 
                                    to = today()) {
  pmap(list(
    stock_symbol = favorites, 
    mavg_short = mavg_short, 
    mavg_long = mavg_long,
    from = from,
    to = to
    ), .f = get_stock_data) %>% 
    set_names(favorites) %>% 
    map(.f = get_stock_mavg_info) %>% 
    bind_rows(.id = "stock") %>% 
    split(.$stock) %>% 
    map(.f = composing_info_cards) %>% 
    tagList()
}
