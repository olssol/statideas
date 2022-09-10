##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------

##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_slides(),
                tab_demo_1(),
                tab_demo_2(),
                tab_demo_3(),
                tab_demo_4()
                )
}

tab_slides <- function() {
    tabPanel("Presentation",
             tags$iframe(src    = "slides.html",
                         style  = "border:none;width:100%",
                         height = 800),
             )
}

tab_demo_1 <- function() {
    tabPanel("Demo I",
             fluidRow(
                 column(4,
                        numericInput("inN",
                                     "Total Sample Size",
                                     value = 50),
                        numericInput("inTrtMean",
                                     "Treatment Group Mean (Control Mean = 0)",
                                     value = 0),
                        ## numericInput("inInc",
                        ##             "Increment Size",
                        ##             value = 10,
                        ##             min   = 1),
                        numericInput("inAlpha",
                                     "One-Sided Alpha Level",
                                     value = 0.025,
                                     min   = 0.01, max = 0.1),
                        ## actionButton("btnAdd",
                        ##              "Enroll Patients",
                        ##              width = "120px"),
                        numericInput("inSeed",
                                     "Random Seed",
                                     value = 0, min = 0),
                        actionButton("btnReset",
                                     "Reset",
                                     width = "120px")
                        ),

                 column(8,
                        h4("Enrollment"),
                        sliderInput("inNfirst", "", value = 5,
                                    min = 0,
                                    max = 50, step = 1,
                                    width = "100%"),
                        h4("By Arm"),
                        radioButtons("rdoArmOpt",
                                     "",
                                     choices = c("Individual Value" = "y",
                                                 "Cumulative Mean"  = "mean" )
                                    ),
                        plotlyOutput("pltArm",  height = "350px"),
                        h4("By Difference Between Two Arms"),
                        radioButtons("rdoDiffOpt",
                                     "",
                                     choices = c("Standardized Difference" = "zscore",
                                                 "Difference" = "difference",
                                                 "P Value" = "pvalue")
                                     ),
                        plotlyOutput("pltDiff", height = "350px"),
                        checkboxInput("chkThresh",
                                      "Show Rejection Threshold",
                                      value = FALSE)
                        )
                 )
             )
}

tab_demo_2 <- function() {
    tabPanel("Demo II",
             fluidRow(
                 column(4,
                        numericInput("inRep",
                                     "Number of Replications",
                                     value = 500,
                                     min   = 10),
                        numericInput("inN2",
                                     "Total Sample Size",
                                     value = 50),
                        numericInput("inTrtMean2",
                                     "Treatment Group Mean (Control Mean = 0)",
                                     value = 0),
                        numericInput("inNSel",
                                     "Number of Selected Replications",
                                     value = 500,
                                     min   = 1),
                        numericInput("inNInterim",
                                     "Number of Interim Analysis",
                                     value = 4, min = 0),
                        radioButtons("rdoBd",
                                     "Boundary",
                                     choices = c("No adjustment" = "NO",
                                                 "Pocock" = "P",
                                                 "O'Brien-Fleming" = "OF",
                                                 "Haybittle-Peto"  = "HP")),
                        actionButton("btnReset2",
                                     "Reset",
                                     width = "120px")
                        ),
                 column(8,
                        h4("Illustration"),
                        h6("Each line in the figure represents a replication of the trial."),
                        radioButtons("rdoTestOpt",
                                     "",
                                     choices = c("Difference" = "difference",
                                                 "Standardized Difference" = "zscore",
                                                 "P Value" = "pvalue")),
                        plotOutput("pltTest", height = "350px"),
                        h4("Proportion of Rejection"),
                        plotOutput("pltAlpha", height = "350px"),
                        tableOutput("tblTest"),
                        h4("Boundary Details"),
                        verbatimTextOutput("txtBd")
                        )
             ))
}

tab_demo_3 <- function() {
    tabPanel("Demo III",
             fluidRow(
                 column(4,
                        numericInput("inNInterim2",
                                     "Number of Interim Analysis",
                                     value = 5, min = 0),
                        checkboxGroupInput(
                            "chkDesigns",
                            "Designs",
                            choices = c("Pocock"          = "P",
                                        "O'Brien-Fleming" = "OF",
                                        "Haybittle-Peto"  = "HP",
                                        "Alpha Spending: Pocock" = "asP",
                                        "Alpha Spending: OF"     = "asOF"),
                            selected = c("P", "OF", "HP"))
                        ),
                 column(8,
                        h4("Comparison of designs"),
                        radioButtons("rdoRpact",
                                     "",
                                     choices = c("Boundaries"       = 1,
                                                 "Alpha"            = 3,
                                                 "Cumulative Alpha" = 4)),
                        plotOutput("pltBds", height = "350px")
                        )
             ))
}

tab_demo_4 <- function() {
    tabPanel("Demo IV",
             fluidRow(
                 column(4,
                        textInput("inInterFrac",
                                  "Interim Analysis Information Fraction",
                                  value = "0.3, 0.6, 1"),
                        radioButtons(
                            "rdoASBd",
                            "Designs",
                            choices = c("Alpha Spending: Pocock" = "asP",
                                        "Alpha Spending: OF"     = "asOF"))
                        ),
                 column(8,
                        h4("Illustration"),
                        plotOutput("pltASTest", height = "350px"),
                        h4("Proportion of Rejection"),
                        tableOutput("tblASTest"),
                        h4("Boundary Details"),
                        verbatimTextOutput("txtASBd")
                        )
             ))
}


##-------------------------------------------------------------
##           UI DATA
##-------------------------------------------------------------
get_data_all <- reactive({

    input$btnReset2
    n_rep    <- input$inRep
    trt_mean <- input$inTrtMean2
    n_tot    <- input$inN2

    if (any(is.null(c(n_rep, trt_mean, n_tot))))
        return(NULL)

    rst <- si_bd_simu_all(n_rep,
                          n_tot    = n_tot,
                          trt_mean = trt_mean,
                          n_cores  = parallel::detectCores() - 1)

    rst
})

get_data_single <- reactive({

    input$btnReset
    trt_mean <- input$inTrtMean
    n_tot    <- input$inN

    if (is.null(trt_mean) | is.null(n_tot))
        return(NULL)

        rst <- si_bd_simu_trial(n_tot    = n_tot,
                                trt_mean = trt_mean,
                                seed     = input$inSeed)
        si_bd_ana_trial(rst)
})

## ---------------demo 1--------------------------------------

## ---------------demo 2--------------------------------------
get_data_rej <- reactive({
    dta     <- get_data_all()
    bds     <- get_bds()

    if (is.null(dta) | is.null(bds))
        return(NULL)

    si_bd_gs_all(dta, bds$bds)
})

get_bds <- reactive({
    n_interim <- input$inNInterim
    type      <- input$rdoBd
    alpha     <- input$inAlpha

    if (is.null(n_interim) |
        is.null(type) |
        is.null(alpha)) {
        return(list(bds = cbind(0, 1.96)))
    }

    si_bd_get_bd(type, alpha = alpha, n_interim = n_interim)
})



## get the maximum rejection rate when no adjustment is taken
get_alpha_boundary <- reactive({
    n_interim <- input$inNInterim
    alpha     <- input$inAlpha
    dta       <- get_data_all()

    if (is.null(n_interim) |
        is.null(alpha)     |
        is.null(dta))
        return(NULL)

    bds     <- si_bd_get_bd("NO", alpha = alpha, n_interim = n_interim)
    rej     <- si_bd_gs_all(dta, bds$bds)
    rej_sum <- si_bd_sum_rep(rej)

    max(rej_sum$rej_wide$prop_cum_rej)
})

get_summary_rej <- reactive({
    dat_rej <- get_data_rej()

    if (is.null(dat_rej))
        return(NULL)

    rst <- si_bd_sum_rep(dat_rej)
    rst
})

get_sel <- reactive({
    input$btnReset2
    sel   <- input$inNSel
    n_rep <- input$inRep

    if (is.null(sel) | sel <= 0)
        sel <- n_rep

    sample(n_rep, min(sel, n_rep))
})


observeEvent(input$btnAdd, {
    if (0 == input$btnAdd |
        is.null(input$inInc))
        return(NULL)

    userLog$n_first <- userLog$n_first + input$inInc
})

observeEvent(input$btnReset, {
    if (0 == input$btnReset)
        return(NULL)

    userLog$n_first <- 1
})

## alpha level
get_alpha <- reactive({
    if (is.null(input$inAlpha))
        return(0.025)

   input$inAlpha
})

## --------------demo 4: alpha spending --------------------------
get_data_rej_as <- reactive({
    dta     <- get_data_all()
    bds     <- get_bds_as()

    if (is.null(dta) | is.null(bds))
        return(NULL)

    si_bd_gs_all(dta, bds$bds)
})

get_bds_as <- reactive({
    info_frac <- tkt_assign(input$inInterFrac)
    type      <- input$rdoASBd
    alpha     <- get_alpha()

    if (is.null(info_frac) |
        is.null(type) |
        is.null(alpha)) {
        return(list(bds = cbind(0, 1.96)))
    }

    si_bd_get_bd(type, alpha = alpha, info_frac = info_frac)
})

get_summary_rej_as <- reactive({
    dat_rej <- get_data_rej_as()

    if (is.null(dat_rej))
        return(NULL)

    si_bd_sum_rep(dat_rej)
})
