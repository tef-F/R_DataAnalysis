source("server.d/sidebar.dataset.r")
source("server.d/main.plot.r")
source("server.d/sidebar.working.r")

server = function(input, output, session) {
  #---------------------------- Handle output data ---------------------

  # output$boxPlotNumeric = renderPlot({boxplot_numeric})
  output$boxPlotNumeric = renderPlotly(ggplotly({boxplot_numeric}))
  output$longFact = renderPlotly(ggplotly({chart_longfact}))

  output$matrixPearson = renderPlot({matrix_pearson})
  output$matrixKendall = renderPlot({matrix_kendall})
  output$matrixPearson_1 = renderPlotly(ggplotly({matrix_pearson}))
  output$matrixKendall_1 = renderPlotly(ggplotly({matrix_kendall}))

  output$matrixConfusion = renderPlotly({confusion_matrix})
  output$boxPlot10 = renderPlotly({plot_metrics})


  output$tableCoefficients = renderText({table_coefficients})
  output$tablePrediction = renderText({table_prediction})
  output$perfomanceSummary = renderText({perfomance_summary})
  output$tableMetrics = renderText({table_metrics_long})

  output$tablePredict = renderDataTable(
    first_training_prediction_full_tbl, 
    options = list(
          searching = TRUE,
          scrollX=TRUE
    )
  )

  observeEvent(input$checkNa, {
    if(input$checkNa == TRUE) {
      output$table = renderDataTable(
        heart_dataset_clean_tbl[, input$field , drop = FALSE],
        options = list(
          searching = TRUE,
          scrollX=TRUE
        )
      )
    } else if(input$checkNa == FALSE) {
      output$table = renderDataTable(
        heart_disease_dataset[, input$field , drop = FALSE],
        options = list(
          searching = TRUE,
          scrollX=TRUE
        )
      )
    }

  })

  output$table = renderDataTable(
    heart_disease_dataset[, input$field , drop = FALSE],
    options = list(
      searching = TRUE,
      scrollX=TRUE
    )
  )

  updateCheckboxGroupInput(session, inputId = "field", choices = names(heart_disease_dataset),selected = names(heart_disease_dataset))
  output$result = renderPrint({
    paste(url, sep = "/", input$dataset)
  })

  output$plot = renderPlotly({
    # cor = cor(matrix(rnorm(100), ncol = 10))
    corr = round(cor(d), 1)
    ggplotly(ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE))
  })
  output$summary = renderPrint(summary(heart_disease_dataset))

  output$plot1 = renderPlot({chart})
}