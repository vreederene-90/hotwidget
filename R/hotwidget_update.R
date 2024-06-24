#' hotwidget_update
#'
#' @description
#' Updates the data set with the changes made by the user in the hotwidget.
#' It works through collecting the changes made by the user via different
#' hooks in the handsontable.js library.
#' - afterChange
#' - afterCreateRow
#' - afterRemoveRow
#' - afterCreateCol
#' - afterRemoveCol
#'
#' Data types are inferred via the original data set. This leads to the
#' following behaviour
#' - non-numerics in numeric columns become NA
#' - invalid dates are NOT updated
#' - factors where the value is not in the levels become NA
#'
#' @param input the list of input from the server function
#' @param hotwidget_data the originally initiated data set
#' @param hotwidget_data_updated a reactiveVal(), initiated with the original
#' data set. This object will contain the data which has been updated by the
#' user.
#'
#' @return Doesnt return anything, but as a side effect updates
#' hotwidget_data_updated.
#'
#' @export
#'
hotwidget_update <- function(input, hotwidget_data, hotwidget_data_updated) {

  observe(
    if (!is.null(input$hotwidget_afterchange)) {
      print("afterchange")

      hotwidget_data_updated(
        hotwidget_data_updated() |>
          mutate(row = row_number(),.before = 1) |>
          rows_update(
            pmap(
              list(
                row = input$hotwidget_afterchange$row,
                col = input$hotwidget_afterchange$col,
                val = input$hotwidget_afterchange$val
              ),
              \(row, col, val) tibble(row = row + 1, col = col, val = if(is.null(val)) NA else val)) |>
              bind_rows() |>
              pivot_wider(names_from = col, values_from = val) |>
              mutate(
                across(any_of(colnames(select(hotwidget_data, where(is.numeric)))), as.numeric),
                across(any_of(colnames(select(hotwidget_data, where(is.character)))), as.character),
                across(any_of(colnames(select(hotwidget_data, where(is.logical)))), as.logical),
                across(any_of(colnames(select(hotwidget_data, where(is.Date)))), as.Date),
                across(
                  any_of(colnames(select(hotwidget_data, where(is.factor)))),
                  ~factor(., levels = levels(hotwidget_data[[cur_column()]]))
                )
              ),
            by = "row"
          ) |> select(-row)
      )
    }
  ) |> bindEvent(input$hotwidget_afterchange)

  observe(
    if (!is.null(input$hotwidget_afterremoverow)) {
      print("afterremoverow")

      rows_to_remove <- input$hotwidget_afterremoverow$physicalRows |> map_int(~.x + 1)

      hotwidget_data_updated(
        hotwidget_data_updated() |>
          slice(-rows_to_remove)
      )
    }
  ) |> bindEvent(input$hotwidget_afterremoverow)

  observe(
    if(!is.null(input$hotwidget_aftercreaterow)) {
      print("aftercreaterow")

      hotwidget_data_updated(
        hotwidget_data_updated() |>
          add_row(
            !!names(hotwidget_data_updated())[1] := NA, .after = input$hotwidget_aftercreaterow$index
          )
      )
    }
  ) |> bindEvent(input$hotwidget_aftercreaterow)

  observe(
    if (!is.null(input$hotwidget_afterremovecol)) {

      cols_to_remove <- input$hotwidget_afterremovecol$physicalColumns |> map_int(~.x + 1)
      hotwidget_data_updated(
        hotwidget_data_updated() |>
          select(-all_of(cols_to_remove))
      )

    }
  ) |> bindEvent(input$hotwidget_afterremovecol)

  observe(
    if (!is.null(input$hotwidget_aftercreatecol)) {
      print("aftercreatecol")
      print(input$hotwidget_aftercreatecol)
    }
  ) |> bindEvent(input$hotwidget_aftercreatecol)

}
