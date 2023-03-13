options(shiny.maxRequestSize = 200*1024^2)
require(plotly)

shinyServer(function(input, output, session) {

    source("design_ui.R", local = TRUE)
    userLog <- reactiveValues()

    ##--------------------------------------
    ##---------main-------------------------
    ##--------------------------------------
    output$mainpage <- renderUI({
        tab_main()
    })

    ##--------------------------------------
    ##---------exit-------------------------
    ##--------------------------------------
    observeEvent(input$close, {
        stopApp()})


    ##--------------------------------------
    ##---------plot-------------------------
    ##--------------------------------------
    output$pltMDD <- renderPlotly({
        rst <- get_plot()
        ggplotly(rst)
    })

})
