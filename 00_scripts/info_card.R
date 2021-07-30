info_card <- function(
  stock_name = "FOO",
  short_mavg = 20,
  large_mavg = 50,
  increase_pct = 0.2
) {
  
  if (increase_pct > 0) {
    arrow_html = "text-success"
    arrow_orientation = "arrow-up"
  } else {
    arrow_html = "text-danger"
    arrow_orientation = "arrow-down"
  }
  
  short_mavg_str = toString(short_mavg)
  large_mavg_str = toString(large_mavg)
  increase_pct_str = toString(increase_pct * 100)
  
  div(
    class = "panel panel-default",
    style = "padding: 0px;",
    div(
      class = "panel-body bg-default text-default",
      p(
        class = "pull-right",
        icon(class = "fa-3x", "chart-line")
      ),
      h4(stock_name),
      p(str_glue("{short_mavg_str} day"), tags$small(str_glue("vs {large_mavg_str} day"))),
      p(
        class = arrow_html,
        icon(arrow_orientation),
        tags$small(str_glue("{increase_pct_str}%"))
      )
    )
  )
}