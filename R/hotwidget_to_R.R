hotwidget_to_R <- function(dat) {
  headers <- map_chr(dat$headers, unlist)

  char_cols <- map_chr(dat$data_types$character, unlist)
  int_cols <- map_chr(dat$data_types$integer, unlist)
  date_cols <- map_chr(dat$data_types$date, unlist)
  factor_cols <- map_chr(dat$data_types$factor, unlist)
  num_cols <- map_chr(dat$data_types$numeric, unlist)

  tryCatch(
    map(
      dat$data,
      \(.x) {
        as_tibble(
          map(.x, \(.x) ifelse(is.null(.x),NA,.x)),
          .name_repair = ~headers)
      }
    )|>
      bind_rows() |>
      mutate(
        across(any_of(num_cols), as.numeric),
        across(any_of(int_cols), as.integer),
        across(any_of(date_cols), as.Date),
        across(any_of(factor_cols), as.factor),
        across(any_of(char_cols), as.character)
      ),
    error = function(e) {
      cli::cli_alert_danger(paste(e))
      return(invisible())
    }
  )
}

