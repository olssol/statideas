require(optMTP)

## -----------------------------------------------------------
##
##          FUNCTIONS
##
## -----------------------------------------------------------

## frequentist test
get_freq_txt <- function(obs_y, obs_n, p_h0) {
    rst <- binom.test(x = obs_y, n = obs_n, p = p_h0,
                      alternative = "greater")

    txt <- sprintf("<p>The mean response rate is <b>%5.2f</b>
                    with 95%% confidence interval <b>(%5.2f, %5.2f)</b>. </p>
                    <p>The p-value is <b>%5.4f</b>.</p>",
                   rst$estimate, rst$conf.int[1], rst$conf.int[2], rst$p.value)

    txt
}

get_bayes_txt <- function(pos_ab, cred_level, quants) {

    post_mu  <- pos_ab[1] / sum(pos_ab)
    cred_int <- qbeta(c((1 - cred_level) / 2, 1 - (1 - cred_level)/2),
                        pos_ab[1], pos_ab[2])
    qs       <- qbeta(quants, pos_ab[1], pos_ab[2])
    qs       <- round(qs, 2)


    txt <- sprintf("<p>The posterior mean response rate is <b>%5.2f</b>
                    with <b>%5.0f%%</b> credible interval <b>(%5.2f, %5.2f)</b>. </p>
                    <p>The %s percent quantiles are <b> %s </b>, respectively.</p>",
                   post_mu, cred_level * 100, cred_int[1], cred_int[2],
                   paste(quants * 100, collapse = ","),
                   paste(qs, collapse = ","))

    txt
}

##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------
##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_slides(),
                tab_basic(),
                tab_hier()
                )
}


tab_slides <- function() {
    tabPanel("Presentation",
             tags$iframe(src    = "slides.html",
                         style  = "border:none;width:100%",
                         height = 800),
             )
}


tab_hier <- function() {
    tabPanel("Hierarchical Model",
             fluidRow(
                 column(3,
                        wellPanel(
                            textInput("inHierY",
                                      "Number of Responders (Sep. by Comma)",
                                      value = "5, 10, 80"),
                            textInput("inHierN",
                                      "Total Number of Patients (Sep by Comma)",
                                      value = "20, 20, 200"),
                            numericInput("inHierSig",
                                         "Variation Among Studies",
                                         value = 2,
                                         min = 0, max = 10, step = 5),
                            actionButton("btnSmp", "Bayes Sample")
                        )),
                 column(9,
                        plotOutput("pltHier",
                                   height = "800px"))
             ))
}


tab_basic <- function() {
    tabPanel("Basic Concepts",
             fluidRow(
                 column(3,
                        wellPanel(
                            h4("Prior Elicitation"),
                            sliderInput(
                                "inPriRng",
                                "Range of Response Rate",
                                value = c(0.6, 0.8),
                                min = 0, max = 1, step = 0.05),
                            numericInput(
                                "inPriConf",
                                "Confidence Level (%)",
                                value = 80,
                                min   = 0, max = 95, step = 5),

                            h4("Observed Data"),
                            numericInput(
                                "inObsY",
                                "Number of Responders",
                                value = 15, min = 0, step = 5),
                            numericInput(
                                "inObsN",
                                "Total Number of Patients",
                                value = 20, min = 0, step = 5)),

                        wellPanel(
                            h4("Bayesian Results"),
                            numericInput(
                                "inCredLevel",
                                "Credible Level (%) ",
                                value = 95,
                                min = 0, max = 100, step = 5),
                            textInput("inQuants",
                                      "Quantiles (Sep by Comma)",
                                      value = "25, 50, 75"),
                            htmlOutput("outBayes")),

                        wellPanel(
                            h4("Frequentist Results"),
                            numericInput(
                                "inH0",
                                "Null Hypothesis (Greater Than)",
                                value = 0.5,
                                min = 0, max = 1, step = 0.05),
                            htmlOutput("outFreq"))),
                 column(9,
                        fluidRow(
                            column(6,
                                   h4("Prior Distribution"),
                                   plotOutput("pltPri",
                                              click    = "pri_click",
                                              dblclick = "pri_dblclick")),
                            column(6,
                                   h4("Observed Data"),
                                   plotOutput("pltObs",
                                              click    = "obs_click",
                                              dblclick = "obs_dblclick"))
                        ),
                        fluidRow(
                            column(6,
                                   h4("Posterior Distribution"),
                                   plotOutput("pltPost",
                                              click    = "pos_click",
                                              dblclick = "pos_dblclick")),
                            column(6,
                                   h4("All Distributions"),
                                   plotOutput("pltOverlay"))
                        )))
             )
}


##-------------------------------------------------------------
##           DATA MANIPULATION
##-------------------------------------------------------------

## elicit prior distribution
get_pri_ab <- reactive({
    pri_range <- as.numeric(input$inPriRng)
    pri_p     <- as.numeric(input$inPriConf)

    if (is.null(pri_range) |
        is.null(pri_p))
        return(NULL)

    rst <- si_bayes_elicit(x_range = pri_range,
                           x_prob  = pri_p / 100)
})

## prior data for plot
get_pri_dat <- reactive({
    pri_ab <- get_pri_ab()

    if (is.null(pri_ab))
        return(NULL)

    rst <- si_bayes_beta_post(obs_y  = 0,
                              obs_n  = 0,
                              pri_ab = pri_ab)
})

## observed data for plot
get_obs_dat <- reactive({

    obs_y <- input$inObsY
    obs_n <- input$inObsN

    if (is.null(obs_y) |
        is.null(obs_n)) {
        return(NULL)
    }

    rst <- si_bayes_beta_post(obs_y  = obs_y,
                              obs_n  = obs_n,
                              pri_ab = c(0.5, 0.5))
})

## posterior data for plot
get_pos_dat <- reactive({

    obs_y  <- input$inObsY
    obs_n  <- input$inObsN
    pri_ab <- get_pri_ab()

    if (is.null(obs_y) |
        is.null(obs_n) |
        is.null(pri_ab)) {
        return(NULL)
    }

    rst <- si_bayes_beta_post(obs_y  = obs_y,
                              obs_n  = obs_n,
                              pri_ab = pri_ab)
})

## overall data
get_overlay_dat <- reactive({
    dat_pri <- get_pri_dat()$density
    dat_obs <- get_obs_dat()$density
    dat_pos <- get_pos_dat()$density

    rst <- NULL
    if (!is.null(dat_pri)) {
        dat_pri$group <- "Prior"
        rst           <- rbind(rst, dat_pri)
    }

    if (!is.null(dat_obs)) {
        dat_obs$group <- "Observed"
        rst           <- rbind(rst, dat_obs)
    }

    if (!is.null(dat_pos)) {
        dat_pos$group <- "Posterior"
        rst           <- rbind(rst, dat_pos)
    }

    if (is.null(rst))
        return(NULL)

    rst
})

## get max
get_max_y <- reactive({
    dat_overlay <- get_overlay_dat()

    if (is.null(dat_overlay))
        return(NULL)

    max(dat_overlay$y * 1.05)
})

## get hover data
observeEvent(input$pri_click, {
    userLog$pri_x <- input$pri_click$x
})
observeEvent(input$pri_dblclick, {
    userLog$pri_x <- NULL
})

observeEvent(input$obs_click, {
    userLog$obs_x <- input$obs_click$x
})
observeEvent(input$obs_dblclick, {
    userLog$obs_x <- NULL
})

observeEvent(input$pos_click, {
    userLog$pos_x <- input$pos_click$x
})
observeEvent(input$pos_dblclick, {
    userLog$pos_x <- NULL
})


##-------------------------------------------------------------
##           Text Results
##-------------------------------------------------------------
get_freq_rst <- reactive({
    obs_y  <- input$inObsY
    obs_n  <- input$inObsN
    p_h0   <- input$inH0

    if (is.null(obs_y) |
        is.null(obs_n)) {
        return(NULL)
    }

    txt <- get_freq_txt(obs_y, obs_n, p_h0)
    txt
})

get_bayes_rst <- reactive({

    cred_level  <- as.numeric(input$inCredLevel) / 100
    quants      <- tkt_assign(input$inQuants) / 100
    pos_ab      <- get_pos_dat()$pos_ab

    if (is.null(cred_level) |
        is.null(quants)     |
        is.null(pos_ab)) {
        return(NULL)
    }

    txt <- get_bayes_txt(pos_ab, cred_level, quants)
    txt
})

##-------------------------------------------------------------
##           PLOTS
##-------------------------------------------------------------

## plot prior distribution
plot_pri <- reactive({
    dat <- get_pri_dat()$density
    rst <- si_bayes_plt_dist(dat, ver_x = userLog$pri_x)

    max_y <- get_max_y()
    if (!is.null(max_y) & !is.null(rst))
        rst <- rst +
            ylim(0, max_y)

    pri_range <- as.numeric(input$inPriRng)
    if (!is.null(pri_range))
        rst <- rst +
            geom_vline(xintercept = pri_range[1], col = "gray", lty = 2) +
            geom_vline(xintercept = pri_range[2], col = "gray", lty = 2)

    rst
})

## plot observed
plot_obs <- reactive({
    dat <- get_obs_dat()$density
    rst <- si_bayes_plt_dist(dat, ver_x = userLog$obs_x)

    max_y <- get_max_y()
    if (!is.null(max_y) & !is.null(rst))
        rst <- rst +
            ylim(0, max_y)

    rst
})

## plot posterior distribution
plot_post <- reactive({
    dat <- get_pos_dat()$density
    rst <- si_bayes_plt_dist(dat, ver_x = userLog$pos_x)

    max_y <- get_max_y()
    if (!is.null(max_y) & !is.null(rst))
        rst <- rst +
            ylim(0, max_y)

    rst
})

## plot overlay
plot_overlay <- reactive({
    dat <- get_overlay_dat()

    if (is.null(dat))
        return(NULL)

    si_bayes_plt_dist(dat, group = "group") +
        theme(legend.position = c(0.1, 0.9),
              legend.title    = element_blank())
})


##-------------------------------------------------------------
##           Hierarchical
##-------------------------------------------------------------
observeEvent(input$btnSmp, {

    if (0 == input$btnSmp) {
        return(NULL)
    }

    vec_y <- tkt_assign(input$inHierY)
    vec_n <- tkt_assign(input$inHierN)

    if (is.null(vec_y) |
        is.null(vec_n)) {
        return(NULL)
    }

    userLog$hier_smp <- si_bayes_hierarchy(
        vec_y,
        vec_n,
        pri_sig  = as.numeric(input$inHierSig),
        iter     = 1500,
        by_study = TRUE)
})

## plot hierarchical samples
plot_hier <- reactive({

    if (0 == input$btnSmp)
        return(NULL)

    dat <- userLog$hier_smp

    if (is.null(dat))
        return(NULL)

    ggplot(data = dat, aes(x = x, y = y)) +
        geom_line(aes(group = grp, col = grp)) +
        facet_wrap(~study, ncol = 1) +
        theme_bw() +
        labs(x = "Rate", y = "Density") +
        theme(legend.position = "top",
              legend.title    = element_blank())
})
