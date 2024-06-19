#' hotwidget: Handsontable widget for R
#'
#' @param data A data frame or matrix
#' @param width Width of the widget
#' @param height Height of the widget
#' @param elementId Id of the widget
#' @param rowHeaders Show row headers
#' @param colHeaders Show column headers
#' @param columnSorting Enable column sorting
#' @param autoWrapRow Enable auto wrap for rows
#' @param autoWrapCol Enable auto wrap for columns
#' @param filters Enable filters
#' @param dropdownMenu Enable dropdown menu
#' @param contextMenu Enable context menu
#' @param licenseKey License key
#'
#' @import htmlwidgets
#'
#' @export
hotwidget <- function(
    data,
    columns = NULL,
    width = NULL,
    height = NULL,
    elementId = NULL,
    rowHeaders = FALSE,
    columnSorting = TRUE,
    autoWrapRow = TRUE,
    autoWrapCol = TRUE,
    filters = TRUE,
    dropdownMenu = TRUE,
    contextMenu = TRUE,
    licenseKey = 'non-commercial-and-evaluation'
    ) {

  # browser()

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
            factor = unique(data[[idx]]),
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
      rownames = FALSE),
    data_types = list(
      character = colnames(select(data, where(is.character))),
      numeric = colnames(select(data, where(is.numeric))),
      date = colnames(select(data, where(is.Date))),
      factor = colnames(select(data, where(is.factor))),
      integer = colnames(select(data, where(is.integer)))
    ),
    columns = columns,
    rowHeaders = rowHeaders,
    colHeaders = names(data),
    columnSorting = columnSorting,
    autoWrapRow = autoWrapRow,
    autoWrapCol = autoWrapCol,
    filters = filters,
    dropdownMenu = dropdownMenu,
    contextMenu = contextMenu,
    licenseKey = 'non-commercial-and-evaluation'
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'hotwidget',
    x,
    width = width,
    height = height,
    package = 'hotwidget',
    elementId = elementId
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
