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
    output$pltNbyPower <- renderPlotly({
        dta <- get_data()
        if (is.null(dta))
            return(NULL)

        rst <- ggplot(data = dta, aes(x = Power, y = N)) +
            geom_line() +
            theme_bw()

        if (input$inYlimN > 0)
            rst <- rst +
                ylim(0, input$inYlimN)

        ggplotly(rst)
    })

    output$pltMDDbyPower <- renderPlotly({
        dta <- get_data()
        if (is.null(dta))
            return(NULL)

        rst <- ggplot(data = dta, aes(x = Power, y = MDD)) +
            geom_line() +
            theme_bw()

        if (input$inYlimMDD > 0)
            rst <- rst +
                ylim(0, input$inYlimMDD)

        ggplotly(rst)
    })

    output$pltMDDbyN <- renderPlotly({
        dta <- get_data()
        if (is.null(dta))
            return(NULL)

        rst <- ggplot(data = dta, aes(x = N, y = MDD)) +
            geom_line() +
            theme_bw()

        if (input$inYlimMDD > 0)
            rst <- rst +
                ylim(0, input$inYlimMDD)

        ggplotly(rst)
    })

    output$pltPowerbyMDD <- renderPlotly({
        dta <- get_data()
        if (is.null(dta))
            return(NULL)

        rst <- ggplot(data = dta, aes(x = MDD, y = Power)) +
            geom_line() +
            theme_bw()

        ggplotly(rst)
    })

})
