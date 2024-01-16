options(shiny.maxRequestSize = 200*1024^2)

library(MASS)
library(data.table)
library(tidyverse)
library(dplyr)
library(rms)
library(ggplot2)
library(shiny)
library(shinyjs)

require(plotly)
require(statidea)


shinyServer(function(input, output, session) {

    source("design_ui.R", local = TRUE)

    userLog          <- reactiveValues()
    userLog$outcomes <- list()

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
    ##---------data-------------------------
    ##--------------------------------------

    output$tblTrt <- DT::renderDataTable({
        simu_data_trt()
    },
    selection = 'single',
    server    = TRUE,
    options   = list())

    output$tblCtl <- DT::renderDataTable({
        simu_data_ctl()
    },
    selection = 'single',
    server    = TRUE,
    options   = list())


    ##--------------------------------------
    ##---------door-------------------------
    ##--------------------------------------

    output$tblAnaTypical <- DT::renderDataTable({
        dta <- simu_data()
        if (is.null(dta))
            return(NULL)
        rst_1 <- si_door_ana(dta, userLog$outcomes)
        rst_2 <- get_door_bs()

        if (!is.null(rst_2))
            rst_2 <- rst_2$door

        rbind(rst_1, rst_2)
    }, options = list())

    output$tblDoorComp <- DT::renderDataTable({
        get_door_comparison()
    }, options = list())

    output$tblDoorBS <- DT::renderDataTable({
        rst <- get_door_bs()

        if (!is.null(rst))
            rst <- rst$bs

        rst
    }, options = list())

})
