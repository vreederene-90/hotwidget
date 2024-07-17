#' Test app for debugging, test and possibly example purposes
#' @param hotwidget_data data set
#'
#' @return app
run_app <-
  function(
    hotwidget_data =
      iris |>
      janitor::clean_names() |>
      mutate(
        .before = 1,
        index = row_number(),
        test = as_date(paste(Sys.Date())),
        species = as.character(species)
      ) |> head(6)) {

    ui <- fluidPage(
      fluidRow(
        column(
          width = 6,
          textInput("name", "name dataset", value = "dataset"),
          hotwidgetOutput("hotwidget"),
          shinyWidgets::pickerInput(
            inputId = "select",
            label = "Choose dimensions",
            choices =
              c(
                "test",
                "sepal_length",
                "sepal_width",
                "petal_length",
                "petal_width",
                "species"),
            selected =
              c("index",
                "test",
                "sepal_length"),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              maxOptions = 15,
              selectedTextFormat = "count"),
          )
        ),
        column(
          width = 6,
          h5("hotwidget_data_updated"),
          shiny::verbatimTextOutput("table"),
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

      # hotwidget_update(input, "hotwidget", hotwidget_data_rv, verbose = TRUE)

      hotwidget_data_rv <- reactiveVal()

      observe(
        hotwidget_data_rv(hotwidget_data |> select(any_of(input$select)))
      )

      output$hotwidget <-
        renderHotwidget(
          {
            hotwidget(
              key_column = "index",
              columnSorting = F,
              undo = TRUE,
              rowHeaders = TRUE,
              # constraints = list(
              #   unique = list("species")
              # ),
              licenseKey = 'non-commercial-and-evaluation',
              columns = list(
                list(
                  data = "species"
                ),
                list(
                  data = "sepal_length"
                ),
                list(
                  data = "sepal_width"
                ),
                list(
                  data = "petal_length"
                ),
                list(
                  data = "petal_width"
                ),
                list(
                  data = "index"
                ),
                list(
                  data = "test"
                )
              ) |>
                keep(\(.x) .x[["data"]] %in% c("index",input$select)),
              data = hotwidget_data_rv(),
              # hiddenColumns =
              #   list(
              #     indicators = TRUE,
              #     columns = list(0)
              #   )
            )
          }
        )

      output$undo <- renderPrint(input$hotwidget_afterundo)
      output$redo <- renderPrint(input$hotwidget_afterredo)
      output$afterchange <- renderPrint(input$hotwidget_afterchange)
      output$afterremoverow <- renderPrint(input$hotwidget_afterremoverow)
      output$aftercreaterow <- renderPrint(input$hotwidget_aftercreaterow)

      output$beforeremoverow <- renderPrint(input$hotwidget_beforeremoverow)
      output$beforeundo <- renderPrint(input$hotwidget_beforeundo)
      output$beforeredo <- renderPrint(input$hotwidget_beforeredo)

      output$table <- renderPrint(hotwidget_data_rv())

      session$onSessionEnded(
        function() {

            assign(
              x = isolate(input$name),
              value = isolate(hotwidget_data_rv()),
              envir = .GlobalEnv
            )
          }
        )

    }
    shinyApp(ui,server)
  }
