# Purely for debugging purposes
run_app <- function() {
  ui <- fluidPage(
    actionButton("browser","browser"),
    hotwidgetOutput("hotwidget")
  )

  server <- function(input, output, session) {

    data <- mtcars |> dplyr::mutate(
      id = dplyr::row_number(),
      .before = 1
    )

    output$hotwidget <- renderHotwidget(
      hotwidget(
        data = data
        )
    )

    observeEvent(input$browser, browser())

    observe(
      {
        if (!is.null(input$hotwidget)) {
          try(print(head(hotwidget_to_R(input$hotwidget))))
        }
      }
    )
  }

  shinyApp(ui,server)

}
