# Purely for debugging purposes
run_app <- function() {
  ui <- fluidPage(
    actionButton("browser","browser"),
    hotwidgetOutput("hotwidget")
  )

  server <- function(input, output, session) {

    output$hotwidget <-
      renderHotwidget(
        hotwidget(
          licenseKey = 'non-commercial-and-evaluation',
          data = iris |>
            janitor::clean_names() |>
            mutate(
              .before = 1,
              id = row_number(),
              test = as_date(paste(Sys.Date()))
            )
        )
    )

    observeEvent(input$browser, browser())

    observe(
      {
        if (!is.null(input$hotwidget)) {
          # try(print(head(hotwidget_to_R(input$hotwidget))))
          try(print(input$hotwidget))
        }
      }
    )
  }

  shinyApp(ui,server)

}
