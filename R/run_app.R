#' Test app for debugging, test and possibly example purposes
#' @param hotwidget_data data set
#'
#' @return app
run_app <-
  function() {
    ui <- fluidPage(
      fluidRow(
        column(
          width = 6,
          hotwidgetOutput("hotwidget"),
        ),
        column(
          width = 6,
          actionButton("btn", "Debug"),
          actionButton("switch_key_column", "Switch key_column"),
          column(
            width = 6,
            h5("afterundo data passed via hotwidget.js"),
            shiny::verbatimTextOutput("undo"),
            h5("beforeundo data passed via hotwidget.js"),
            shiny::verbatimTextOutput("beforeundo"),
            h5("afterchange data passed via hotwidget.js"),
            shiny::verbatimTextOutput("afterchange"),
            h5("beforeremoverow data passed via hotwidget.js"),
            shiny::verbatimTextOutput("beforeremoverow")
          ),
          column(
            width = 6,
            h5("redo data passed via hotwidget.js"),
            shiny::verbatimTextOutput("redo"),
            h5("beforeredo data passed via hotwidget.js"),
            shiny::verbatimTextOutput("beforeredo"),
            h5("afterremoverow data passed via hotwidget.js"),
            shiny::verbatimTextOutput("afterremoverow"),
            h5("aftercreaterow data passed via hotwidget.js"),
            shiny::verbatimTextOutput("aftercreaterow"),
          )
        )
      )
    )

    server <- function(input, output, session) {
      observeEvent(
        input$switch_key_column,
        {
          if (input$switch_key_column %% 2 != 0) rv$keys <- list("index_1", "index_2") else rv$keys <- list("index_1")
        }
      )

      rv <- reactiveValues(keys = list("index_1"))

      output$hotwidget <-
        renderHotwidget({
          hotwidget(
            key_column = rv$keys,
            constraints = list(unique = list("index_1", "index_2")),
            key_column_plus = c("index_1"),
            columnSorting = F,
            undo = TRUE,
            rowHeaders = TRUE,
            licenseKey = "non-commercial-and-evaluation",
            data = head(iris) |> janitor::clean_names() |> mutate(.before = 1, index_1 = 1:6, index_2 = letters[1:6])
          )
        })

      observeEvent(input$btn, browser())

      output$undo <- renderPrint(input$hotwidget_afterundo)
      output$redo <- renderPrint(input$hotwidget_afterredo)
      output$afterchange <- renderPrint(input$hotwidget_afterchange)
      output$afterremoverow <- renderPrint(input$hotwidget_afterremoverow)
      output$aftercreaterow <- renderPrint(input$hotwidget_aftercreaterow)

      output$beforeremoverow <- renderPrint(input$hotwidget_beforeremoverow)
      output$beforeundo <- renderPrint(input$hotwidget_beforeundo)
      output$beforeredo <- renderPrint(input$hotwidget_beforeredo)
    }
    shinyApp(ui, server)
  }
