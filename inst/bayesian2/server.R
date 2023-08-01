options(shiny.maxRequestSize = 200*1024^2)

library(MASS)
library(data.table)
library(tidyverse)
library(dplyr)
library(rms)
library(ggplot2)
library(shiny)

require(plotly)
require(statidea)


shinyServer(function(input, output, session) {

    source("design_ui.R", local = TRUE)

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

    output$pltObsFreq <- renderPlot({
        rst <- plot_obs_freq()

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
    output$outFreq_p <- renderUI({
        HTML(get_freq_rst_p())
    })

    output$outBayes_p <- renderUI({
        HTML(get_bayes_rst_p())
    })

    output$pltMixture <- renderPlot({
        dat <- rbind(data.frame(group = "Weekly Informative",
                                x     = get_d4_flat()),
                     data.frame(group = "Existing Study",
                                x     = get_d4_study()),
                     data.frame(group = "Prior",
                                x     = get_d4_mixture())
                     )

        rst <- ggplot(data = dat, aes(x = x)) +
            geom_density(aes(group = group, col = group),
                         adjust = 1.5) +
            theme_bw() +
            xlim(0, 1) +
            theme(legend.position = "top",
                  legend.title    = element_blank()) +
            labs(x = "Success Rate", y = "Density") +
            scale_color_manual(values = c("Existing Study" = "black",
                                          "Weekly Informative" = "cyan",
                                          "Prior" = "brown"))

        rst
    })

    output$pltOverlay_p <- renderPlot({
        dat <- rbind(data.frame(group = "Posterior",
                                x     = get_d4_posterior()),
                     data.frame(group = "Existing Study",
                                x     = get_d4_study()),
                     data.frame(group = "Current Study",
                                x     = get_d4_study_cur()),
                     data.frame(group = "Prior",
                                x     = get_d4_mixture())
                     )

        rst <- ggplot(data = dat, aes(x = x)) +
            geom_density(aes(group = group, col = group),
                         adjust = 1.5) +
            theme_bw() +
            xlim(0, 1) +
            theme(legend.position = "top",
                  legend.title    = element_blank()) +
            labs(x = "Success Rate", y = "Density") +
            scale_color_manual(values = c("Posterior" = "red",
                                          "Existing Study" = "black",
                                          "Current Study" = "green",
                                          "Prior" = "brown"))

        rst
    })

    ##--------------------------------------
    ##---------DEMO FIX---------------------
    ##--------------------------------------

    output$table_FIX = renderTable({table_FIX()})
    output$density_m = renderPlot({
      density_m() + coord_cartesian(xlim = input$slider_m_zoom)
    })
    output$density_cv = renderPlot({
      density_cv() + coord_cartesian(xlim = input$slider_cv_zoom)
    })
    output$density_y_tilde = renderPlot({
      density_y_tilde() + coord_cartesian(xlim = input$slider_y_tilde_zoom)
    })

    ##-----------------slider--------------
    output$slider_m = renderUI({
      return(sliderInput("slider_m", 
                         "Threshold of Mean of FIX Functional Activity (%)",
                         min=0,max=200,step=5,value=c(35,70),width="90%"))
    })
    output$slider_cv = renderUI({
      return(sliderInput("slider_cv", 
                         "Threshold of Coefficient of Variation of FIX Functional Activity",
                         min=0,max=3,step=0.1,value=c(0.5,1.2),width="90%"))
    })
    output$slider_y_tilde = renderUI({
      return(sliderInput("slider_y_tilde", 
                         "Threshold of Predicted FIX Functional Activity (%)",
                         min=0,max=200,step=5,value=c(5,150),width="90%"))
    })
    
    output$slider_m_zoom = renderUI({
      return(sliderInput("slider_m_zoom", "Zoom in Mean of FIX Functional Activity (%)",
                         min=0,max=round(max(fit_FIX()$post_m)),step=5,
                         value=c(0,round(max(fit_FIX()$post_m))),width="90%"))
    })
    output$slider_cv_zoom = renderUI({
      return(sliderInput("slider_cv_zoom", 
                         "Zoom in Coefficient of Variation of FIX Functional Activity",
                         min=0,max=round(max(fit_FIX()$post_cv)),step=0.1,
                         value=c(0,round(max(fit_FIX()$post_cv))),width="90%"))
    })
    output$slider_y_tilde_zoom = renderUI({
      return(sliderInput("slider_y_tilde_zoom", 
                         "Zoom in Predicted FIX Functional Activity (%)",
                         min=0,max=round(max(fit_FIX()$post_y_tilde)),step=5,
                         value=c(0,round(max(fit_FIX()$post_y_tilde))),width="90%"))
    })

})
