# Purely for debugging purposes
run_app <- function() {
  ui <- fluidPage(
    actionButton("browser","browser"),
    hotwidgetOutput("hotwidget")
  )

  server <- function(input, output, session) {

    hotwidget_data <-
      iris |>
      janitor::clean_names() |>
      mutate(
        .before = 1,
        test = as_date(paste(Sys.Date()))
      )

    hotwidget_data_updated <- reactiveVal(hotwidget_data)

    observe(
      {
        print("hotwidget_data_updated")
        print(hotwidget_data_updated()|> head())
      }
    )

    hotwidget_update(input, hotwidget_data, hotwidget_data_updated)

    output$hotwidget <-
      renderHotwidget(
        hotwidget(
          licenseKey = 'non-commercial-and-evaluation',
          data = hotwidget_data
        )
      )

    observeEvent(input$browser, browser())
  }
  shinyApp(ui,server)
}
