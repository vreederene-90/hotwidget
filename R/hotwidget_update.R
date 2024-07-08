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
#' - afterUndo
#' - afterRedo
#'
#' Data types are inferred via the original data set. This leads to the
#' following behaviour
#' - non-numerics in numeric columns become NA
#' - invalid dates are become NA
#' - factors where the value is not in the levels become NA
#'
#' known issues:
#' \href{https://github.com/handsontable/handsontable/issues/10610}{GitHub issue}
#' - combination of sorting and undo/redo doesnt function properly.
#' - hotwidget disables undo/redo if you want column sorting
#'
#' @param input the list of input from the server function
#' @param hotwidget_data_updated a reactiveVal(), initiated with the original
#' data set. This object will contain the data which has been updated by the
#' user.
#' @param id the id of the hotwidget
#' @param verbose logical, if TRUE print debug messages
#'
#' @return Doesnt return anything, but as a side effect updates
#' hotwidget_data_updated.
#'
#' @export
#'
hotwidget_update <- function(
    input,
    id,
    hotwidget_data_updated,
    verbose = FALSE) {

  id_afterchange <- paste0(id, "_afterchange")
  id_aftercreaterow <- paste0(id, "_aftercreaterow")
  id_afterremoverow <- paste0(id, "_afterremoverow")
  id_afterundo <- paste0(id, "_afterundo")
  id_afterredo <- paste0(id, "_afterredo")

  observe(
    if (!is.null(input[[id_afterchange]])) {
      if(verbose) print("afterchange")

      hotwidget_data_updated(
        hotwidget_data_updated() |>
          mutate(row = row_number(),.before = 1) |>
          rows_update(
            pmap(
              list(
                row = input[[id_afterchange]][["row"]],
                col = input[[id_afterchange]][["col"]],
                val = input[[id_afterchange]][["val"]]
              ),
              \(row, col, val) tibble(row = row + 1, col = col, val = if(is.null(val)) NA else val)) |>
              bind_rows() |>
              pivot_wider(names_from = col, values_from = val) |>
              mutate(
                across(any_of(colnames(select(hotwidget_data_updated(), where(is.numeric)))), as.numeric),
                across(any_of(colnames(select(hotwidget_data_updated(), where(is.character)))), as.character),
                across(any_of(colnames(select(hotwidget_data_updated(), where(is.logical)))), as.logical),
                across(any_of(colnames(select(hotwidget_data_updated(), where(is.Date)))), as_date),
                across(
                  any_of(colnames(select(hotwidget_data_updated(), where(is.factor)))),
                  ~factor(., levels = levels(hotwidget_data_updated()[[cur_column()]]))
                )
              ),
            by = "row"
          ) |> select(-row)
      )
    }
  ) |> bindEvent(input[[id_afterchange]])

  observe(
    if (!is.null(input[[id_afterremoverow]])) {
      if(verbose) print("afterremoverow")

      rows_to_remove <- input[[id_afterremoverow]][["physicalRows"]] |> map_int(~.x + 1)

      hotwidget_data_updated(
        hotwidget_data_updated() |>
          slice(-rows_to_remove)
      )
    }
  ) |> bindEvent(input[[id_afterremoverow]])

  observe(
    if(!is.null(input[[id_aftercreaterow]])) {
      if(verbose) print("aftercreaterow")

      hotwidget_data_updated(
        hotwidget_data_updated() |>
          add_row(
            !!names(hotwidget_data_updated())[1] := NA, .after = input[[id_aftercreaterow]][["index"]]
          )
      )
    }
  ) |> bindEvent(input[[id_aftercreaterow]])

  observe(
    if (!is.null(input[[id_afterundo]])) {

      switch(
        input[[id_afterundo]][["action"]],
        # undo change
        "change" =
          {
            if(verbose) print("undochange")
            hotwidget_data_updated(
              hotwidget_data_updated() |>
                mutate(row = row_number(), .before = 1) |>
                rows_update(
                  pmap(
                    list(
                      row = input[[id_afterundo]][["row"]],
                      col = map(input[[id_afterundo]][["col"]], ~names(hotwidget_data_updated())[.x + 1]),
                      val = input[[id_afterundo]][["val"]]
                    ),
                    \(row, col, val) tibble(row = row + 1, col = col, val = if(is.null(val)) NA else val)) |>
                    bind_rows() |>
                    pivot_wider(names_from = col, values_from = val) |>
                    mutate(
                      across(any_of(colnames(select(hotwidget_data_updated(), where(is.numeric)))), as.numeric),
                      across(any_of(colnames(select(hotwidget_data_updated(), where(is.character)))), as.character),
                      across(any_of(colnames(select(hotwidget_data_updated(), where(is.logical)))), as.logical),
                      across(any_of(colnames(select(hotwidget_data_updated(), where(is.Date)))), as.Date),
                      across(
                        any_of(colnames(select(hotwidget_data_updated(), where(is.factor)))),
                        ~factor(., levels = levels(hotwidget_data_updated()[[cur_column()]]))
                      )
                    ),
                  by = "row"
                ) |> select(-row)
            )
          },
        # undo removed row
        "remove_row" = {
          if(verbose) print("undoremoverow")

          data <- do.call(rbind, lapply(input[[id_afterundo]][["data"]], rbind))
          data[sapply(data, is.null)] <- NA

          row_to_insert <-
            data |> as_tibble() |> unnest(cols = c(index, test, sepal_length, sepal_width, petal_length, petal_width, species)) |>
            mutate(
              across(any_of(colnames(select(hotwidget_data_updated(), where(is.numeric)))), as.numeric),
              across(any_of(colnames(select(hotwidget_data_updated(), where(is.character)))), as.character),
              across(any_of(colnames(select(hotwidget_data_updated(), where(is.logical)))), as.logical),
              across(any_of(colnames(select(hotwidget_data_updated(), where(is.Date)))), as.Date),
              across(
                any_of(colnames(select(hotwidget_data_updated(), where(is.factor)))),
                ~factor(., levels = levels(hotwidget_data_updated()[[cur_column()]]))
              )
            )

          hotwidget_data_updated(
            hotwidget_data_updated() |>
              add_row(row_to_insert, .after = input[[id_afterundo]][["index"]])
          )

        },
        # undo created row
        "insert_row" = {
          if(verbose) print("undoinsertrow")
          rows_to_remove <- input[[id_afterundo]][["index"]] |> map_int(~.x + 1)

          hotwidget_data_updated(
            hotwidget_data_updated() |>
              slice(-rows_to_remove)
          )
        }
      )

    }
  ) |> bindEvent(input[[id_afterundo]])

  observe(
    if (!is.null(input[[id_afterredo]])) {

      switch(
        input[[id_afterredo]][["action"]],
        "change" = {
          if(verbose) print("redochange")
          hotwidget_data_updated(
            hotwidget_data_updated() |>
              mutate(row = row_number(),.before = 1) |>
              rows_update(
                pmap(
                  list(
                    row = input[[id_afterredo]][["row"]],
                    col = map(input[[id_afterredo]][["col"]], ~names(hotwidget_data_updated())[.x + 1]),
                    val = input[[id_afterredo]][["val"]]
                  ),
                  \(row, col, val) tibble(row = row + 1, col = col, val = if(is.null(val)) NA else val)) |>
                  bind_rows() |>
                  pivot_wider(names_from = col, values_from = val) |>
                  mutate(
                    across(any_of(colnames(select(hotwidget_data_updated(), where(is.numeric)))), as.numeric),
                    across(any_of(colnames(select(hotwidget_data_updated(), where(is.character)))), as.character),
                    across(any_of(colnames(select(hotwidget_data_updated(), where(is.logical)))), as.logical),
                    across(
                      any_of(colnames(select(hotwidget_data_updated(), where(is.Date)))),
                      ~tryCatch(as_date(pick(cur_column())), error = function(e) NA_Date_)
                    ),
                    across(
                      any_of(colnames(select(hotwidget_data_updated(), where(is.factor)))),
                      ~factor(., levels = levels(hotwidget_data_updated()[[cur_column()]]))
                    )
                  ),
                by = "row"
              ) |> select(-row)
          )
        },
        "insert_row" = {

          if(verbose) print('redoinsertrow')
          hotwidget_data_updated(
            hotwidget_data_updated() |>
              add_row(
                !!names(hotwidget_data_updated())[1] := NA, .after = input[[id_afterredo]][["index"]]
              )
          )
        },
        "remove_row" =
          {

            if(verbose) print('redoremoverrow')

            index <- input[[id_afterredo]][["actionList"]][["index"]] + 1
            amount <- length(input[[id_afterredo]][["actionList"]][["data"]])
            rows_to_remove <- index:{index+amount-1}

            hotwidget_data_updated(
              hotwidget_data_updated() |>
                slice(-rows_to_remove)
            )
          }
      )
    }
  ) |> bindEvent(input[[id_afterredo]])

}
