shinyServer(function(input, output, session) {

    source("design_ui.R", local = TRUE)

    userLog         <- reactiveValues()
    userLog$n_first <- 1

    ##--------------------------------------
    ##---------main-------------------------
    ##--------------------------------------
    output$mainpage <- renderUI({
       tab_main()
    })

    ##--------------------------------------
    ##         DEMO 1: single trial
    ##--------------------------------------
    observe({
        if (is.null(input$inN))
            return(NULL)

        updateSliderInput(session, "inNfirst", max = input$inN)
    })


    output$pltArm <- renderPlotly({
        dta <- get_data_single()
        if (is.null(dta))
            return(NULL)

        type <- input$rdoArmOpt
        if (is.null(type)) {
            type <- "y"
        }

        rst <- si_bd_plt_single(dta,
                                type     = type,
                                ##n_first  = userLog$n_first,
                                n_first  = input$inNfirst,
                                ref_line = c(input$inTrtMean, 0))

        ggplotly(rst)
    })

    output$pltDiff <- renderPlotly({
        dta   <- get_data_single()
        if (is.null(dta))
            return(NULL)

        type  <- input$rdoDiffOpt
        if (is.null(type)) {
            type <- "difference"
        }

        ## rejection line
        ref_line <- NULL
        if (input$chkThresh) {
            alpha    <- get_alpha()
            ref_line <- switch(type,
                               zscore = qnorm(1 - alpha),
                               pvalue = alpha)
        }

        rst <- si_bd_plt_single(dta,
                                type     = type,
                                n_first  = input$inNfirst,
                                ## n_first  = userLog$n_first,
                                col      = "red",
                                ref_line = ref_line)

        ggplotly(rst)
    })

    ##--------------------------------------
    ##         DEMO 2: Rejection
    ##--------------------------------------

    output$pltTest <- renderPlot({
        dta <- get_data_rej()
        if (is.null(dta))
            return(NULL)

        si_bd_plt_rep(dta,
                      sel  = get_sel(),
                      type = input$rdoTestOpt)
    })

    output$tblTest <- renderTable({
        get_summary_rej()$rej_tbl
    }, digits = 4)

    output$pltAlpha <- renderPlot({
        dta <- get_summary_rej()$rej
        if (is.null(dta))
            return(NULL)

        max_y <- get_alpha_boundary()
        rst   <- si_bd_plt_alpha(dta, alpha = input$inAlpha)

        if (!is.null(max_y))
            rst <- rst + ylim(0, max_y * 1.05)

        rst
    })

    output$txtBd <- renderPrint({
        details <- get_bds()
        print(details)
    })

    ##--------------------------------------
    ##         DEMO 3: Boundaries
    ##--------------------------------------

    output$pltBds <- renderPlot({

        designs <- input$chkDesigns
        if (is.null(designs))
            return(NULL)

            si_bd_plt_bd(n_interim = input$inNInterim2,
                         alpha     = input$inAlpha,
                         designs   = designs,
                         type      = input$rdoRpact)
    })


    ## ---------------------------------------------------
    ##            DEMO 4: Alpha Spending
    ## ---------------------------------------------------

    output$pltASTest <- renderPlot({
        dta <- get_data_rej_as()
        if (is.null(dta))
            return(NULL)

        si_bd_plt_rep(dta,
                      sel  = get_sel(),
                      type = "zscore")
    })

    output$tblASTest <- renderTable({
        get_summary_rej_as()$rej
    }, digits = 4)


    output$txtASBd <- renderPrint({
        details <- get_bds_as()
        print(details)
    })

})
