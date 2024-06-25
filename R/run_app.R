#' Test app for debugging, test and possibly example purposes
#' @param hotwidget_data data set
#'
#' @return app
#' @export run_app
run_app <- function(
    hotwidget_data =
      iris |>
      janitor::clean_names() |>
      mutate(
        .before = 1,
        test = as_date(paste(Sys.Date())),
        index = row_number()
      ) |> head(6)
) {
  ui <- fluidPage(
    hotwidgetOutput("hotwidget")
  )

  server <- function(input, output, session) {

    hotwidget_data_updated <- reactiveVal(hotwidget_data)

    observe(
      {
        print("hotwidget_data_updated")
        print(hotwidget_data_updated() |> head())
      }
    )

    hotwidget_update(input, hotwidget_data, hotwidget_data_updated)

    output$hotwidget <-
      renderHotwidget(
        hotwidget(
          rowHeaders = TRUE,
          licenseKey = 'non-commercial-and-evaluation',
          data = hotwidget_data
        )
      )
  }
  shinyApp(ui,server)
}
