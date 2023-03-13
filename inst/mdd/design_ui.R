
## -----------------------------------------------------------
##
##          FUNCTIONS
##
## -----------------------------------------------------------

f_get_data <- function(power_range, delta, sigma, ratio, by = 0.02) {

    power <- seq(power_range[1], power_range[2], by = by)
    rst   <- NULL
    for (p in power) {
        cur_n   <- si_mdd_samplesize(delta, sigma, p, ratio = ratio)
        cur_mdd <- si_mdd_mdd(cur_n[1], cur_n[2], sigma)
        rst     <- rbind(rst, c(p, cur_n, cur_mdd))
    }

    colnames(rst) <- c("Power", "n0", "n1", "N", "MDD")
    data.frame(rst)
}

##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------
##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_slides(),
                tab_mdd()
                )
}


tab_slides <- function() {
    tabPanel("Concept",
             wellPanel(
                 img(src = "MDD.png",
                     width = "80%")))
}

tab_mdd <- function() {
    tabPanel("Demo",
             fluidRow(
                 column(3,
                        wellPanel(
                            numericInput(
                                "inDelta",
                                withMathJax("$$\\Delta = \\theta_1 - \\theta_0$$"),
                                value = 3, min = 0, step = 1),

                            numericInput(
                                "inSigma",
                                withMathJax("$$\\sigma$$"),
                                value = 6, min = 0, step = 1),

                            sliderInput(
                                "inPower",
                                "Power",
                                value = c(0.5, 0.9),
                                min = 0, max = 1, step = 0.05),

                            numericInput(
                                "inRatio",
                                "Randomization Ratio (Treatment vs. Control)",
                                value = 1, min = 0, step = 1)
                        ),

                        wellPanel(
                            numericInput(
                                "inYlimN",
                                "Y Limit: N",
                                value = 0, min = 0, step = 10),

                            numericInput(
                                "inYlimMDD",
                                "Y Limit: MDD",
                                value = 0, min = 0, step = 1)
                        )),
                 column(9,
                        fluidRow(
                            column(6,
                                   h4("Sample Size by Power"),
                                   plotlyOutput("pltNbyPower",   height = "350px"),
                                   h4("MDD by Power"),
                                   plotlyOutput("pltMDDbyPower", height = "350px")
                                   ),
                            column(6,
                                   h4("MDD by Sample Size"),
                                   plotlyOutput("pltMDDbyN", height = "350px"),
                                   h4("Power by MDD"),
                                   plotlyOutput("pltPowerbyMDD", height = "350px")
                                   ))
                        )))
}


##-------------------------------------------------------------
##           DATA MANIPULATION
##-------------------------------------------------------------
get_data <- reactive({

    power_range <- input$inPower
    delta       <- input$inDelta
    sigma       <- input$inSigma
    ratio       <- input$inRatio

    if (is.null(power_range) |
        is.null(delta) |
        is.null(sigma) |
        is.null(ratio)) {

        return(NULL)

    }

    rst <- f_get_data(power_range, delta, sigma, ratio, by = 0.02)
    rst
})

##-------------------------------------------------------------
##           PLOTS
##-------------------------------------------------------------
