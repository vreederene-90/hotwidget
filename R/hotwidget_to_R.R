hotwidget_to_R <- function(dat) {
  headers <- map_chr(dat$headers, unlist)

  char_cols <- map_chr(dat$data_types$character, unlist)
  int_cols <- map_chr(dat$data_types$integer, unlist)
  date_cols <- map_chr(dat$data_types$date, unlist)
  factor_cols <- map_chr(dat$data_types$factor, unlist)
  num_cols <- map_chr(dat$data_types$numeric, unlist)

  map(
    dat$data,
    \(.x) as_tibble(.x, .name_repair = ~headers) |>
    mutate(across(everything(),as.character)))|>
    bind_rows() |>
    mutate(
      across(any_of(num_cols), as.numeric),
      across(any_of(int_cols), as.integer),
      across(any_of(date_cols), as.Date),
      across(any_of(factor_cols), as.factor),
      across(any_of(char_cols), as.character)
    )
}

