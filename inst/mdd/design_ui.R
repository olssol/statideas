
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
        rst     <- rbind(rst, c(p, cur_n, cur_mdd, delta, sigma))
    }

    colnames(rst) <- c("Power", "n0", "n1", "N", "MDD",
                       "Delta", "Sigma")
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
                            textInput(
                                "inDelta",
                                withMathJax("$$\\Delta = \\theta_1 - \\theta_0$$"),
                                value = "3, 3.5"),

                            textInput(
                                "inSigma",
                                withMathJax("$$\\sigma$$"),
                                value = "5, 6"),

                            sliderInput(
                                "inPower",
                                "Power",
                                value = c(0.5, 0.9),
                                min = 0, max = 1, step = 0.05),

                            numericInput(
                                "inRatio",
                                "Randomization Ratio (Treatment vs. Control)",
                                value = 1, min = 0, step = 1)
                        )),
                 column(9,
                        radioButtons("inType",
                                     "Select Type of Figure",
                                     choices =
                                         c("Sample Size by Power" = "nbypower",
                                           "MDD by Power"         = "mddbypower",
                                           "MDD by Sample Size"   = "mddbyn",
                                           "Power by MDD"         = "powerbymdd"
                                           )
                                     ),
                        plotlyOutput("pltMDD", height = "500px")
                        )))
}


##-------------------------------------------------------------
##           DATA MANIPULATION
##-------------------------------------------------------------
get_data <- reactive({

    power_range <- input$inPower
    delta       <- tkt_assign(input$inDelta)
    sigma       <- tkt_assign(input$inSigma)
    ratio       <- input$inRatio

    if (is.null(power_range) |
        is.null(delta) |
        is.null(sigma) |
        is.null(ratio)) {

        return(NULL)

    }
    rst_all <- NULL
    for (d in delta) {
        for (s in sigma) {
            cur_rst <- f_get_data(power_range, d, s, ratio, by = 0.02)
            rst_all <- rbind(rst_all, cur_rst)
        }
    }

    rst_all %>%
        mutate(Delta = factor(Delta),
               Sigma = paste("Sigma =", Sigma))
})

##-------------------------------------------------------------
##           PLOTS
##-------------------------------------------------------------

get_plot <- reactive({
    dat <- get_data()

    if (is.null(dat))
        return(NULL)

    type <- input$inType

    xy <- switch(type,
                 nbypower   = c("Power", "N"),
                 mddbypower = c("Power", "MDD"),
                 powerbymdd = c("MDD",   "Power"),
                 mddbyn     = c("N",     "MDD"))

    dat$x <- dat[[xy[1]]]
    dat$y <- dat[[xy[2]]]

    ggplot(data = dat, aes(x = x, y = y, group = Delta)) +
        geom_line(aes(col = Delta)) +
        theme_bw() +
        labs(x = xy[1], y = xy[2]) +
        facet_wrap( ~ Sigma)
})
