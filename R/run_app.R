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
        test = as_date(paste(Sys.Date()))
      )
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
    ) |>
      bindEvent(
        input$hotwidget_afterchange, input$hotwidget_afterremoverow, input$hotwidget_aftercreaterow
      )

    hotwidget_update(input, hotwidget_data, hotwidget_data_updated)

    output$hotwidget <-
      renderHotwidget(
        hotwidget(
          licenseKey = 'non-commercial-and-evaluation',
          data = hotwidget_data
        )
      )


  }
  shinyApp(ui,server)
}
