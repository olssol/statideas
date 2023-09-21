## -----------------------------------------------------------
##
##          FUNCTIONS
##
## -----------------------------------------------------------

stb_tl_bayes_beta_post <- function(obs_y  = 1,
                                   obs_n  = 10,
                                   pri_ab = c(1, 1),
                                   x      = seq(0, 1, by = 0.005),
                                   n_post = 3000) {
    pos_a   <- pri_ab[1] + obs_y
    pos_b   <- pri_ab[2] + obs_n - obs_y

    rst_den <- NULL
    if (!is.null(x)) {
        rst <- sapply(x, function(v) {
            c(dbeta(v, pos_a, pos_b),
              1 - pbeta(v, pos_a, pos_b))
        })

        rst_den <- cbind(x     = x,
                         y_pdf = rst[1, ],
                         y_cdf = rst[2, ])
    }

    post_smp <- NULL
    if (!is.null(n_post)) {
        post_smp <- rbeta(n_post, pos_a, pos_b)
    }


    list(pri_ab   = pri_ab,
         pos_ab   = c(pos_a, pos_b),
         density  = rst_den,
         post_smp = post_smp)
}

get_txt_tempaltes <- function() {
    list(freq = "<p>The mean success rate is <b>%5.2f</b>
                    with 95%% confidence interval <b>(%5.2f, %5.2f)</b>. </p>
                    <p>The p-value is <b>%5.4f</b>.</p>",
         bayes_1 = "<p>The posterior mean success rate is <b>%5.2f</b>
                    with <b>%5.0f%%</b> credible interval <b>(%5.2f, %5.2f)</b>.
                    </p> <p>The %s percent quantiles are <b> %s </b>,
                    respectively.</p>",
         bayes_2 = "<p>The posterior probability that the success rate is
                         greater than <b>%5.2f</b> is <b>%5.2f</b>.</p>")
}


## frequentist test
get_freq_txt <- function(obs_y, obs_n, p_h0) {
    rst <- binom.test(x = obs_y, n = obs_n, p = p_h0,
                      alternative = "greater")

    txt <- sprintf(get_txt_tempaltes()[["freq"]],
                   rst$estimate, rst$conf.int[1], rst$conf.int[2], rst$p.value)

    txt
}

## bayesian text
get_bayes_txt <- function(pos_ab, cred_level, quants, q_r = NULL) {

    post_mu  <- pos_ab[1] / sum(pos_ab)
    cred_int <- qbeta(c((1 - cred_level) / 2, 1 - (1 - cred_level)/2),
                        pos_ab[1], pos_ab[2])
    qs       <- qbeta(quants, pos_ab[1], pos_ab[2])
    qs       <- round(qs, 2)
    txt      <- sprintf(get_txt_tempaltes()[["bayes_1"]],
                    post_mu, cred_level * 100, cred_int[1], cred_int[2],
                    paste(quants * 100, collapse = ","),
                    paste(qs, collapse = ","))

    if (!is.null(q_r)) {
        qs <- 1 - pbeta(q_r, pos_ab[1], pos_ab[2])
        txt_2 <- sprintf(get_txt_tempaltes()[["bayes_2"]],
                         q_r, round(qs, 2))
        txt <- paste(txt, txt_2)
    }

    txt
}

get_bayes_txt_2 <- function(smps, cred_level, quants, q_r = NULL) {
    cred_int <- quantile(smps,
                         c((1 - cred_level) / 2,
                           1 - (1 - cred_level) / 2))
    qs       <- quantile(smps, quants)
    qs       <- round(qs, 2)
    txt      <- sprintf(get_txt_tempaltes()[["bayes_1"]],
                        mean(smps), cred_level * 100, cred_int[1], cred_int[2],
                        paste(quants * 100, collapse = ","),
                        paste(qs, collapse = ","))

    if (!is.null(q_r)) {
        qs <- mean(smps > q_r)
        txt_2 <- sprintf(get_txt_tempaltes()[["bayes_2"]],
                         q_r, round(qs, 2))
        txt <- paste(txt, txt_2)
    }

    txt
}

##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------
##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_basic(),
                tab_mixture(),
                tab_fix(),
                tab_2arm(),
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
    tabPanel("Subgroup Analysis",
             fluidRow(
                 column(3,
                        wellPanel(
                            textInput("inHierY",
                                      "Number of Responders in Each Subgroup (Sep. by Comma)",
                                      value = "5, 10, 80"),
                            textInput("inHierN",
                                      "Number of Patients in Each Subgroup (Sep. by Comma)",
                                      value = "20, 20, 200"),
                            numericInput("inHierSig",
                                         "Variation Among Subgroups",
                                         value = 2,
                                         min = 0, max = 10, step = 5),
                            actionButton("btnSmp", "Bayes Sample")
                        )),
                 column(9,
                        plotOutput("pltHier",
                                   height = "800px"))
             ))
}


tab_mixture <- function() {
    tabPanel("Leveraging Adult Data",
             wellPanel(
                 fluidRow(
                     column(3,
                            wellPanel(
                                h4("Existing Adult Data"),
                                numericInput("inD4ObsY",
                                             "Number of Successes in the Adult Study",
                                             value = 65, min = 0, step = 1),
                                numericInput("inD4ObsN",
                                             "Number of Patients in the Adult Study",
                                             value = 120, min = 0, step = 1)),
                            wellPanel(
                                sliderInput("inD4Weight",
                                            "Weight on Existing Study",
                                            value = 0.5,
                                            min = 0, max = 1, step = 0.05))
                            ),
                     column(9,
                            plotOutput("pltMixture")))
             ),
             wellPanel(
                 fluidRow(
                     column(3,
                            wellPanel(
                                h4("Observed Pediatric Data"),
                                numericInput(
                                    "inObsY_p",
                                    "Number of Successes",
                                    value = 6, min = 0, step = 1),
                                numericInput(
                                    "inObsN_p",
                                    "Total Number of Patients",
                                    value = 20, min = 0, step = 5)),

                            wellPanel(
                                h4("Typical (Frequentist) Results"),
                                numericInput(
                                    "inH0_p",
                                    "Null Hypothesis (Greater Than)",
                                    value = 0.5,
                                    min = 0, max = 1, step = 0.05),
                                htmlOutput("outFreq_p")),

                            wellPanel(
                                h4("Bayesian Results"),
                                numericInput(
                                    "inCredLevel_p",
                                    "Credible Level (%) ",
                                    value = 95,
                                    min = 0, max = 100, step = 5),
                                textInput("inQuants_p",
                                          "Quantiles (Sep by Comma)",
                                          value = "25, 50, 75"),
                                htmlOutput("outBayes_p")
                            )),
                     column(9,
                            fluidRow(
                                plotOutput("pltOverlay_p")
                            ))
                 ))
             )
}


tab_2arm <- function() {
    tabPanel("A POC Phase 2 Study",
             fluidRow(
                 column(3,
                        wellPanel(
                            h4("Observed Data"),
                            numericInput(
                                "inD2ObsY0",
                                "Number of Responders (Control)",
                                value = 6, min = 0, step = 1),
                            numericInput(
                                "inD2ObsY1",
                                "Number of Responders (Treatment)",
                                value = 8, min = 0, step = 1),
                            numericInput(
                                "inD2ObsN0",
                                "Number of Patients (Control)",
                                value = 20, min = 0, step = 5),
                            numericInput(
                                "inD2ObsN1",
                                "Number of Patients (Treatment)",
                                value = 20, min = 0, step = 5)),
                        wellPanel(
                            h4("Decision-Making"),
                            numericInput(
                                "inClinDiff",
                                "Clinically Meaningful Difference (R)",
                                value = 0.15, min = 0, max = 1, step = 0.01),
                            numericInput(
                                "inThresh1",
                                "Success Threshold (C1)",
                                value = 0.8, min = 0, max = 1, step = 0.01),
                            numericInput(
                                "inThresh2",
                                "Futility Threshold (C2)",
                                value = 0.2, min = 0, max = 1, step = 0.01),

                            checkboxInput(
                                "inChkPval",
                                "Present P-Values",
                                value = FALSE)
                            )
                        ),
                 column(9,
                        fluidRow(
                            column(6,
                                   h4("Response Rate by Arm"),
                                   plotOutput("pltD2Arm")),
                            column(6,
                                   h4("Effect"),
                                   plotOutput("pltD2Diff"))
                        ),
                        fluidRow(
                            column(12,
                                   h4("Decision Map for Futility (Gray) or Efficacy (Red)"),
                                   plotOutput("pltD2Map", height = "600px")),
                            column(6)
                        ))
             ))
}


tab_basic <- function() {
    tabPanel("Incorporating Prior",
             wellPanel(
                 fluidRow(
                     column(3,
                            wellPanel(
                                h4("Observed Data"),
                                numericInput(
                                    "inObsY",
                                    "Number of Successes",
                                    value = 6, min = 0, step = 1),
                                numericInput(
                                    "inObsN",
                                    "Total Number of Patients",
                                    value = 20, min = 0, step = 5)),
                            wellPanel(
                                h4("Typical (Frequentist) Results"),
                                numericInput(
                                    "inH0",
                                    "Null Hypothesis (Greater Than)",
                                    value = 0.5,
                                    min = 0, max = 1, step = 0.05),
                                htmlOutput("outFreq"))),
                     column(9,
                            fluidRow(
                                column(6,
                                       h4("Observed Data"),
                                       plotOutput("pltObsFreq")),
                                column(6,
                                       h4("Observed Success Rate"),
                                       plotOutput("pltObs",
                                                  click    = "obs_click",
                                                  dblclick = "obs_dblclick"))
                            ))
                 )),

             wellPanel(
                 fluidRow(
                     column(3,
                            wellPanel(
                                h4("Prior Elicitation"),
                                sliderInput(
                                    "inPriRng",
                                    "Range of Success Rate",
                                    value = c(0.6, 0.8),
                                    min = 0, max = 1, step = 0.05),

                                numericInput(
                                    "inPriConf",
                                    "Confidence Level (%)",
                                    value = 80,
                                    min   = 0, max = 95, step = 5)),

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
                                htmlOutput("outBayes"))
                            ),

                     column(9,
                            fluidRow(
                                column(6,
                                       h4("Prior Distribution of Success Rate"),
                                       plotOutput("pltPri",
                                                  click    = "pri_click",
                                                  dblclick = "pri_dblclick")),
                                column(6,
                                       h4("Posterior Distribution of Success Rate"),
                                       plotOutput("pltPost",
                                                  click    = "pos_click",
                                                  dblclick = "pos_dblclick"))),
                            fluidRow(
                                column(6,
                                       h4("All Distributions"),
                                       plotOutput("pltOverlay"))
                            ))
                 ))
             )
}


tab_fix_outcome <- function() {
    tabsetPanel(type = "tabs",
                tabPanel("Posterior FIX Activity Mean",
                         plotOutput('density_m',
                                    width = "100%",
                                    height = "500px"),
                         htmlOutput("slider_m"),
                         htmlOutput("slider_m_zoom")
                         ),
                tabPanel("Predicted FIX Activity for the Next Patient",
                         plotOutput('density_y_tilde',
                                    width = "100%",
                                    height = "500px"),
                         htmlOutput("slider_y_tilde"),
                         htmlOutput("slider_y_tilde_zoom")
                         ),
                tabPanel("Summary of FIX Activity Mean",
                         tableOutput("table_FIX"),
                         )
                )
}

tab_fix <- function() {
    tabPanel(
        "FIX Activity",
        wellPanel(
            fluidRow(
                column(3,
                       wellPanel(
                           h4("Observed Data"),
                           textInput("y",
                                     "FIX Functional Activity Data (%, split by ',')",
                                     "20, 30, 130")),
                       wellPanel(
                           h4("Prior"),
                           sliderInput("bdCV",
                                       "Bounds of Coefficient of Variation (CV)",
                                       min = 0, max = 3,
                                       value = c(0.4, 1.6),
                                       step = 0.1),
                           sliderInput("bdMean",
                                       "Bounds of Mean",
                                       min = 0, max = 500,
                                       value = c(5, 120),
                                       step = 5)
                       )),
                column(9,
                       wellPanel(
                           tab_fix_outcome())
                       )
            ))
    )
}


##-------------------------------------------------------------
##           DATA MANIPULATION: DEMO 1
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
    p_h0        <- input$inH0

    if (is.null(cred_level) |
        is.null(quants)     |
        is.null(pos_ab)) {
        return(NULL)
    }

    txt <- get_bayes_txt(pos_ab, cred_level, quants, p_h0)
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

## plot observed
plot_obs_freq <- reactive({
    obs_y  <- input$inObsY
    obs_n  <- input$inObsN

    ggplot(data = data.frame(Group  = c("Successes", "Failures"),
                             Number = c(obs_y, obs_n- obs_y)),
           aes(x = Group, y = Number)) +
        geom_col(alpha = 0.8, width = 0.5) +
        theme_bw()
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
##           DEMO 2
##-------------------------------------------------------------

## get tippling point
get_d2_tipping <- reactive({
    n0 <- input$inD2ObsN0
    n1 <- input$inD2ObsN1
    if (is.null(n0) | is.null(n1))
        return(NULL)

    si_bayes_tipping(n0, n1, n_post = 5000)
})

## two arm analysis
get_arm2_rst <- reactive({

    if (is.null(input$inD2ObsY0) |
        is.null(input$inD2ObsY1) |
        is.null(input$inD2ObsN0) |
        is.null(input$inD2ObsN1))

        return(NULL)

    rst_trt <- stb_tl_bayes_beta_post(obs_y  = input$inD2ObsY1,
                                      obs_n  = input$inD2ObsN1)

    rst_ctl <- stb_tl_bayes_beta_post(obs_y  = input$inD2ObsY0,
                                      obs_n  = input$inD2ObsN0)


    rst_diff <- rst_trt$post_smp - rst_ctl$post_smp
    den_diff <- density(rst_diff, from = -1, to = 1, n = 501, adj = 1.5)
    diff_x   <- den_diff$x
    diff_y   <- den_diff$y
    diff_px  <- sapply(diff_x, function(x) mean(rst_diff > x))

    rbind(data.frame(rst_trt$density) %>%
          mutate(Group = "Treatment"),
          data.frame(rst_ctl$density) %>%
          mutate(Group = "Control"),
          data.frame(x     = diff_x,
                     y_pdf = diff_y,
                     y_cdf = diff_px,
                     Group = "Difference"))
})

## plot posterior
plot_d2_byarm <- reactive({
    dat_rst <- get_arm2_rst()

    if (is.null(dat_rst))
        return(NULL)

    dat <- dat_rst %>%
        filter(Group != "Difference")

    ggplot(dat, aes(x = x, y = y_pdf)) +
        geom_line(aes(group = Group, col = Group)) +
        theme_bw() +
        labs(x = "Rate", y = "Density") +
        theme(legend.position = c(0.8, 0.8),
              legend.title    = element_blank())
})

## plot posterior
plot_d2_diff <- reactive({
    dat_rst <- get_arm2_rst()

    if (is.null(dat_rst))
        return(NULL)

    dat <- dat_rst %>%
        filter(Group == "Difference") %>%
        rename(x  = x,
               y  = y_pdf,
               px = y_cdf) %>%
        select(-Group)

    si_bayes_plt_dist(dat,
                      ver_x = input$inClinDiff,
                      xlim  = c(-1, 1))
})

##-------------------------------------------------------------
##           DEMO 3: Hierarchical
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

    if (is.null(dat)) {
        return(NULL)
    }

    dat <- dat %>%
        mutate(study = paste("Subgroup", study))

    ggplot(data = dat, aes(x = x, y = y)) +
        geom_line(aes(group = grp, col = grp)) +
        facet_wrap(~study, ncol = 1) +
        theme_bw() +
        labs(x = "Rate", y = "Density") +
        theme(legend.position = "top",
              legend.title    = element_blank())
})


##-------------------------------------------------------------
##           DEMO 4: Mixture Prior
##-------------------------------------------------------------

get_d4_flat <- reactive({
    rbeta(3000, 1, 1)
})

get_d4_study <- reactive({
    rbeta(3000, input$inD4ObsY + 1, input$inD4ObsN - input$inD4ObsY + 1)
})

get_d4_study_cur <- reactive({
    rbeta(3000, input$inObsY_p + 1, input$inObsN_p - input$inObsY_p + 1)
})


get_d4_mixture <- reactive({

    weight  <- input$inD4Weight

    rbeta(3000,
          weight * input$inD4ObsY + 1,
          weight * (input$inD4ObsN - input$inD4ObsY) + 1)

    ## s_flat  <- get_d4_flat()
    ## s_study <- get_d4_study()
    ## weight  <- input$inD4Weight

    ## if (1 == weight) {
    ##     rst <- s_study
    ## } else if (0 == weight) {
    ##     rst <- s_flat
    ## } else {
    ##     s1 <- sample(s_study,  floor(3000 * weight))
    ##     s2 <- sample(s_flat, floor(3000 * (1 - weight)))
    ##     rst <- c(s1, s2)
    ## }

    ## rst
})


get_d4_posterior <- reactive({
    weight  <- input$inD4Weight
    exist_y <- input$inD4ObsY
    exist_n <- input$inD4ObsN
    cur_y   <- input$inObsY_p
    cur_n   <- input$inObsN_p

    rbeta(3000,
          cur_y + weight * input$inD4ObsY + 1,
          cur_n - cur_y + weight * (input$inD4ObsN - input$inD4ObsY) + 1)

    ## si_bayes_mixture(cur_y, cur_n, exist_y, exist_n, weight)
})

get_freq_rst_p <- reactive({
    obs_y  <- input$inObsY_p
    obs_n  <- input$inObsN_p
    p_h0   <- input$inH0_p

    if (is.null(obs_y) |
        is.null(obs_n)) {
        return(NULL)
    }

    txt <- get_freq_txt(obs_y, obs_n, p_h0)
    txt
})


get_bayes_rst_p <- reactive({

    cred_level  <- as.numeric(input$inCredLevel_p) / 100
    quants      <- tkt_assign(input$inQuants_p) / 100
    p_h0        <- input$inH0_p
    post_smp    <- get_d4_posterior()

    if (is.null(cred_level) |
        is.null(quants)     |
        is.null(post_smp)) {
        return(NULL)
    }

    txt <- get_bayes_txt_2(post_smp, cred_level, quants, p_h0)
    txt
})


##-------------------------------------------------------------
##           DEMO 5: FIX
##-------------------------------------------------------------


##------------------------FIX--------------------------
fix_y = reactive({
    as.numeric(str_split(input$y,
                         ",",
                         simplify = TRUE)) # percentage
})

##-----------Summary Table-----------
table_FIX = reactive({
    table = data.frame(matrix(c(mean(fix_y()),
                                sd(fix_y()),
                                sd(fix_y())/mean(fix_y()),
                                quantile(fix_y())),
                              nrow = 1))

    colnames(table) = c("Mean (%)","SD (%)","CV","Min (%)",
                        "Q1 (%)","Median (%)","Q3 (%)","Max (%)")
    return(table)
})

##----------Posterior Sampling----------
fit_FIX = reactive({
    fit = si_bayes_fix(fix_y(),
                       input$bdMean[1],input$bdMean[2],
                       input$bdCV[1], input$bdCV[2])
    return(fit)
})


##----------------------Density Plot----------------------
density_m = reactive({
  if (is.null(input$slider_m)){return(0)} # temperal value before having input value
  pdf = fun_kernel_density(x = fit_FIX()$post_m,
                           cut = input$slider_m,
                           xlab_name = "Mean of FIX Functional Activity (m%)",
                           para_notation = bquote(m),percent = "Y",
                           from = 0,to = NA)
  return(pdf)
})
density_cv = reactive({
  if (is.null(input$slider_cv)){return(0)} # temperal value before having input value
  pdf = fun_kernel_density(x = fit_FIX()$post_cv,
                           cut = input$slider_cv,
                           xlab_name = "Coefficient of Variation of FIX Functional Activity (cv)",
                           para_notation = bquote(cv),percent = "N",
                           from = 0,to = NA)
  return(pdf)
})
density_y_tilde = reactive({
  if (is.null(input$slider_y_tilde)){return(0)} # temperal value before having input value
  pdf = fun_kernel_density(x = fit_FIX()$post_y_tilde,
                           cut = input$slider_y_tilde,
                           xlab_name = bquote("Predicted FIX Functional Activity ("*tilde(y)*"%)"),
                           para_notation = bquote(tilde(y)),
                           percent = "Y",from = 0,to = NA)
  return(pdf)
})
