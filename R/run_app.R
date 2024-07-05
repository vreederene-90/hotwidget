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
          textInput("name", "name dataset", value = "dataset"),
          hotwidgetOutput("hotwidget")
        ),
        column(
          width = 6,
          h5("hotwidget_data_updated"),
          shiny::verbatimTextOutput("table"),
          column(
            width = 6,
            h5("undo data passed via hotwidget.js"),
            shiny::verbatimTextOutput("undo")
          ),
          column(
            width = 6,
            h5("redo data passed via hotwidget.js"),
            shiny::verbatimTextOutput("redo")
          )
        )
      )
    )

    server <- function(input, output, session) {

      hotwidget_update(input, "hotwidget", hotwidget_data, hotwidget_data_updated)
      hotwidget_data_updated <- reactiveVal(hotwidget_data)

      output$hotwidget <-
        renderHotwidget(
          hotwidget(
            columnSorting = FALSE,
            undo = TRUE,
            rowHeaders = TRUE,
            licenseKey = 'non-commercial-and-evaluation',
            data = hotwidget_data
          )
        )

      output$undo <- renderPrint(input$hotwidget_afterundo)
      output$redo <- renderPrint(input$hotwidget_afterredo)

      output$table <- renderPrint(hotwidget_data_updated())

      session$onSessionEnded(
        function() {

            assign(
              x = isolate(input$name),
              value = isolate(hotwidget_data_updated()),
              envir = .GlobalEnv
            )
          }
        )

    }
    shinyApp(ui,server)
  }
