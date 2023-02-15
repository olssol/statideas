options(shiny.maxRequestSize = 200*1024^2)
require(plotly)

shinyServer(function(input, output, session) {

    source("design_ui.R", local = TRUE);

    userLog          <- reactiveValues()
    userLog$data     <- NULL
    userLog$pri_x    <- NULL
    userLog$obs_x    <- NULL
    userLog$pos_x    <- NULL
    userLog$hier_smp <- NULL

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
    output$outFreq <- renderUI({
        HTML(get_freq_rst())
    })

    output$outBayes <- renderUI({
        HTML(get_bayes_rst())
    })


    output$pltPri <- renderPlot({
        rst <- plot_pri()

        if (is.null(rst))
            return(NULL)

        rst
    })

    output$pltObs <- renderPlot({
        rst <- plot_obs()

        if (is.null(rst))
            return(NULL)

        rst
    })

    output$pltPost <- renderPlot({
        rst <- plot_post()

        if (is.null(rst))
            return(NULL)

        rst
    })

    output$pltOverlay <- renderPlot({
        rst <- plot_overlay()

        if (is.null(rst))
            return(NULL)

        rst
    })

    output$pltHier <- renderPlot({
        rst <- plot_hier()

        if (is.null(rst))
            return(NULL)

        rst
    })

})
