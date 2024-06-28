#' Test app for debugging, test and possibly example purposes
#' @param hotwidget_data data set
#'
#' @return app
#' @export run_app
run_app <-
  function(
    hotwidget_data =
      iris |>
      janitor::clean_names() |>
      mutate(
        .before = 1,
        index = row_number(),
        test = as_date(paste(Sys.Date()))
      ) |> head(6)) {

    ui <- fluidPage(
      fluidRow(
        column(
          width = 6,
          hotwidgetOutput("hotwidget")
        ),
        column(
          width = 6,
          shiny::verbatimTextOutput("table")
        )
      )
    )

    server <- function(input, output, session) {

      hotwidget_update(input, hotwidget_data, hotwidget_data_updated)
      hotwidget_data_updated <- reactiveVal(hotwidget_data)

      output$hotwidget <-
        renderHotwidget(
          hotwidget(
            columnSorting = FALSE,
            rowHeaders = TRUE,
            licenseKey = 'non-commercial-and-evaluation',
            data = hotwidget_data
          )
        )

      output$table <- renderPrint(hotwidget_data_updated())

    }
    shinyApp(ui,server)
  }
