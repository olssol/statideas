options(shiny.maxRequestSize = 200*1024^2)

shinyServer(function(input, output, session) {

    source("design_ui.R", local = TRUE);

    userLog          <- reactiveValues();
    userLog$data     <- NULL;


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


    observeEvent(input$btnTest, {
        if (0 == input$btnTest)
            return(NULL)

        pval <- c(input$inP1, input$inP2, input$inP3,
                  input$inP4, input$inP5, input$inP6)

        if (any(!is.numeric(pval)))
            return(NULL)

        userLog$data <- get_rst(pval)
    })

    output$outRst <- renderTable({
        userLog$data
    })
})
