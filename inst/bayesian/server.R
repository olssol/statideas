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

    ##--------------------------------------
    ##---------DEMO 2-----------------------
    ##--------------------------------------
    output$pltD2Arm <- renderPlot({
        rst <- plot_d2_byarm()

        if (is.null(rst))
            return(NULL)
        rst
    })

    output$pltD2Diff <- renderPlot({
        rst <- plot_d2_diff()
        rst
    })

    output$pltD2Map <- renderPlot({
        map_tipping <- get_d2_tipping()

        if (is.null(map_tipping))
            return(NULL)

        pr <- input$inClinDiff
        c1 <- input$inThresh1
        c2 <- input$inThresh2

        f_col <- function(vec) {
            prob <- mean(vec > pr)
            if (prob > c1) {
                rst <- "red"
            } else if (prob < c2) {
                rst <- "gray30"
            } else {
                rst <- "white"
            }

            rst
        }

        si_bayes_plot_tipping(map_tipping,
                              f_col,
                              add_pval = input$inChkPval)
    })



    ##--------------------------------------
    ##---------DEMO 3-----------------------
    ##--------------------------------------

    output$pltHier <- renderPlot({
        rst <- plot_hier()

        if (is.null(rst))
            return(NULL)

        rst
    })

    ##--------------------------------------
    ##---------DEMO 4-----------------------
    ##--------------------------------------
    output$pltMixture <- renderPlot({
        dat <- rbind(data.frame(group = "Weekly Informative",
                                x     = get_d4_flat()),
                     data.frame(group = "Existing Study",
                                x     = get_d4_study()),
                     data.frame(group = "Mixture",
                                x     = get_d4_mixture())
                     )

        rst <- ggplot(data = dat, aes(x = x)) +
            geom_density(aes(group = group, col = group),
                         adjust = 1.5) +
            theme_bw() +
            theme(legend.position = "top",
                  legend.title    = element_blank())

        rst
    })

})
