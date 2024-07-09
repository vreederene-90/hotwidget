#' hotwidget: Handsontable widget for R
#'
#' @param data A data frame or matrix
#' @param width Width of the widget
#' @param height Height of the widget
#' @param rowHeaders Show row headers
#' @param columnSorting Enable column sorting (if you enable this, undo/redo functionality is disabled)
#' @param autoWrapRow Enable auto wrap for rows
#' @param autoWrapCol Enable auto wrap for columns
#' @param filters Enable filters
#' @param dropdownMenu Enable dropdown menu
#' @param contextMenu Enable context menu
#' @param licenseKey License key
#' @param columns Column definition, if not assigned will be guessed
#' e.g.:
#' \preformatted{
#' list(
#'   list(
#'     data = "{column_name}",
#'     type = "data type",
#'     source = c("choices", "for", "dropdown"),
#'     numericFormat = list(
#'       pattern = "0.00"
#'     ),
#'     dateFormat = "YYYY-MM-DD"
#'   ),
#'   list(...)
#' )
#' }
#' More details at: \href{https://handsontable.com/docs/javascript-data-grid/cell-type/}{Documentation Handsontable.js}
#'
#' @param allowRemoveRow Allow the removal of rows
#' @param allowInsertRow Allow row inserting
#' @param key_column Key column to identify rows (has to be integer), client will be updated with max + 1 for new rows
#' @param undo Enable undo/redo functionality, if columnSorting is enabled, undo/redo is disabled
#'
#' @import htmlwidgets
#' @description
#' known issues:
#' - combination of sorting and undo/redo doesnt function properly.
#' - hotwidget disables undo/redo if you want column sorting
#'
#' @export
hotwidget <- function(
    data,
    key_column = NULL,
    columns = NULL,
    width = NULL,
    height = NULL,
    rowHeaders = FALSE,
    undo = TRUE,
    columnSorting = TRUE,
    allowRemoveRow = TRUE,
    allowInsertRow = TRUE,
    autoWrapRow = TRUE,
    autoWrapCol = TRUE,
    filters = TRUE,
    dropdownMenu = TRUE,
    contextMenu = TRUE,
    licenseKey = NA
    ) {

  if (is.na(licenseKey)) {
    cli::cli_alert_warning(
      "Missing license key"
    )
  }

  col_types <- map_chr(data, class)

  # handsontable data types
  # autocomplete
  # checkbox
  # date
  # dropdown
  # numeric
  # text

  if (is.null(columns))
    columns <- imap(
      col_types,
      \(x, idx) {
        list(
          data = idx,
          type = switch(
            x,
            character = 'text',
            numeric = 'numeric',
            Date = 'date',
            factor = 'dropdown',
            integer = 'numeric',
            'text'
          ),
          source = switch(
            x,
            factor = levels(data[[idx]]),
            NA
          ),
          numericFormat = switch(
            x,
            numeric = list(pattern = '0'),
            NA
          ),
          dateFormat = switch(
            x,
            Date = 'YYYY-MM-DD',
            NA
          )
        )
      }
    ) |> unname()

  # forward options using x
  x = list(
    data =
      jsonlite::toJSON(
      data,
      na = "null",
      dataframe =
        "rows",
      digits = NA
      ),
    columns = columns,
    key_column = key_column,
    undo = if(columnSorting) FALSE else undo,
    allowRemoveRow = allowRemoveRow,
    allowInsertRow = allowInsertRow,
    rowHeaders = rowHeaders,
    colHeaders = names(data),
    columnSorting = columnSorting,
    autoWrapRow = autoWrapRow,
    autoWrapCol = autoWrapCol,
    filters = filters,
    dropdownMenu = if(dropdownMenu) {
      list(
        'clear_column',
        "filter_by_condition",
        "filter_by_condition2",
        "filter_operators",
        "filter_by_value",
        "filter_action_bar"
      )
    } else FALSE,
    contextMenu = if(contextMenu) {
      list(
        'row_above',
        'row_below',
        'remove_row',
        'clear_column'
      )
    } else FALSE,
    licenseKey = licenseKey
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'hotwidget',
    x,
    width = width,
    height = height,
    package = 'hotwidget'
  )
}

#' Shiny bindings for hotwidget
#'
#' Output and render functions for using hotwidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a hotwidget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name hotwidget-shiny
#'
#' @export
hotwidgetOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'hotwidget', width, height, package = 'hotwidget')
}

#' @rdname hotwidget-shiny
#' @export
renderHotwidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, hotwidgetOutput, env, quoted = TRUE)
}
