shinyServer(function(input, output, session) {

    source("design_ui.R",      local = TRUE)
    source("design_ui_data.R", local = TRUE)

    userLog            <- reactiveValues()
    userLog$n_first    <- 1
    userLog$all_design <- list()

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

    ## ---------------------------------------------------
    ##            DEMO 5: User Defined Design
    ## ---------------------------------------------------
    output$tblDesn5 <- renderDataTable({
        rst <- get_design_5()

        if (is.null(rst))
            return(NULL)

        rst <- rst %>%
            select(-inx, -study_alpha, -study_power,
                   -nominal_alpha_left) %>%
            mutate(nominal_alpha    = 2 * nominal_alpha,
                   ia_alpha_spent   = 2 * ia_alpha_spent,
                   cumu_alpha_spent = 2 * cumu_alpha_spent) %>%
            set_gsd_tbl()

        rst

    }, options = list(dom = 't'))

    output$pltout5 <- renderPlot({
        get_design_plot_5()
    }, height = 600)

    output$pltoutCurve5 <- renderPlotly({
        rst <- get_curve_plot_5()
        ggplotly(rst, tooltip = c("text"))
    })

    output$uiChkbox5 <- renderUI({
        dta <- get_design_plot_data_5()

        if (is.null(dta)) {
            return(NULL)
        }

        choices <- unique(dta$Rejection2)

        checkboxGroupInput("inChkbox5",
                           "",
                           choices = choices,
                           selected = choices)
    })

    ## ---------------------------------------------------
    ##            DEMO 6: User Defined Design
    ## ---------------------------------------------------
    output$tblDesn6 <- renderDataTable({
        rst <- userLog$all_design$design

        if (is.null(rst))
            return(NULL)

        rst <- rst %>%
            select(-inx, -study_alpha, -study_power,
                   -nominal_alpha_left) %>%
            set_gsd_tbl(pre_col = "Design")

        rst
    }, options = list(dom = "t", pageLength = 100))

    output$tblOut6 <- renderDataTable({
        rst <- userLog$all_design$outcome

        if (is.null(rst))
            return(NULL)

        rst <- rst %>%
            mutate(
                across(
                    c("expected_duration",
                      "expected_samplesize",
                      "saved_duration",
                      "saved_sample",
                      "power"),
                    ~ format(round(.x, digits = 2),
                             nsmall = 2)))

        colnames(rst) <- c("Design",
                           "Expected Duration (Months)",
                           "Expected Sample Size",
                           "Expected Saved Duration (%)",
                           "Expected Saved Sample (%)",
                           "Power")

        rst
    }, options = list(dom = "t"))

    output$pltout6 <- renderPlotly({
        rst <- get_design_plot_6()
        if (is.null(rst))
            return(NULL)

        ggplotly(rst, tooltip = "text")
    })

})
