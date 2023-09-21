##-------------------------------------------------------------
##           FUNCTION
##-------------------------------------------------------------


##show different type of messages
msg_box <- function(contents, type="info") {
    switch(type,
           info    = cls <- "cinfo",
           warning = cls <- "cwarning",
           success = cls <- "csuccess",
           error   = cls <- "cerror");
    rst <- '<div class="';
    rst <- paste(rst, cls, '">');
    rst <- paste(rst, contents);
    rst <- paste(rst, "</div>");
    HTML(rst);
}

set_gsd_tbl <- function(dta, pre_col = NULL) {

    dta <- dta %>%
        mutate(mdd_percent = mdd_percent * 100) %>%
        mutate(across(
            c("boundary_zscore", "mdd_percent", "ia_power", "cumu_power"),
            ~ format(round(.x, digits = 2), nsmall = 2))) %>%
        mutate(across(
            c("nominal_alpha", "ia_alpha_spent", "cumu_alpha_spent"),
            ~ format(round(.x, digits = 3), nsmall = 3)))

    colnames(dta) <- c(pre_col,
                       "Information Fraction",
                       "Z-score Boundary",
                       "Nominal Alpha at IA",
                       "Alpha Spent at IA",
                       "Cumulative Alpha Spent",
                       "MDD As % of Exp. Eff. Size",
                       "Power at IA",
                       "Cumulative Power")

    dta
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

## --------------demo 5: alpha spending --------------------------
get_design_5 <- reactive({

    if (0 == input$btnCalc5) {
        return(NULL)
    }

    info_fracs <- isolate(tkt_assign(input$inInterFrac5))
    boundary   <- isolate(tkt_assign(input$inNominalAlpha) / 2)
    alpha      <- isolate(as.numeric(input$inAlpha5) / 2)
    power      <- isolate(as.numeric(input$inPower5))
    rst        <- simutb::stb_tl_gsd_solve(info_fracs = info_fracs,
                                           boundary   = boundary,
                                           alpha      = alpha,
                                           power      = power)

    rst
})

get_design_plot_data_5 <- reactive({

    design <- get_design_5()

    if (is.null(design)) {
        return(NULL)
    }

    n_ana      <- nrow(design)
    info_fracs <- design$info_frac[(n_ana - 1):n_ana]
    boundary   <- design$boundary_zscore[(n_ana - 1):n_ana]
    alpha      <- design$study_alpha[1]
    power      <- design$study_power[1]
    alpha2     <- alpha * 2
    boundary   <- 2 * (1 - pnorm(boundary))
    without_ia <- input$inRdoAna5

    if ("type1" == input$inRdo5) {
        power <- 0
    }

    sig  <- stb_tl_gsd_cov(info_fracs)
    m    <- stb_tl_gsd_mean(info_fracs = info_fracs,
                            alpha      = alpha,
                            power      = power)

    nsmp <- 100000
    smps <- rmvnorm(n = nsmp, mean = m, sigma = sig)
    smps <- 1 - pnorm(smps)

    col   <- rep(NA,  nsmp)
    col2  <- rep(NA,  nsmp)
    if ("fa" == without_ia) {
        inx       <- which(smps[, 2] < alpha2)
        col2[inx] <- "Rejected at Final When No IA"
        col[inx]  <- paste("Rejected at Final When No IA (n = ",
                          length(inx), ", % = ",
                          round(length(inx) / nsmp, 3),
                          ")",
                          sep = "")
    } else if ("ia" == without_ia) {
        inx       <- which(smps[, 1] < boundary[1])
        col2[inx] <- "Rejected at IA"
        col[inx]  <- paste("Rejected at IA (n = ",
                          length(inx), ", % = ",
                          round(length(inx) / nsmp, 3),
                          ")",
                          sep = "")
    } else {
        inx <- which(smps[, 1] < boundary[1] &
                     smps[, 2] > alpha2)

        col2[inx] <- "Rejected: Only With IA"
        col[inx]  <- paste("Rejected: Only With IA (n = ",
                          length(inx), ", % = ",
                          round(length(inx) / nsmp, 3),
                          ")",
                          sep = "")

        inx <- which(smps[, 1] > boundary[1] &
                     smps[, 2] > boundary[2] &
                     smps[, 2] < alpha2)

        col2[inx] <- "Fail to Reject With IA"
        col[inx]  <- paste("Fail to Reject With IA (n = ",
                          length(inx), ", % = ",
                          round(length(inx) / nsmp, 3),
                          ")",
                          sep = "")

        inx <- which(smps[, 2] < boundary[2])
        col2[inx] <- "Rejected: Anyway"
        col[inx] <- paste("Rejected: Anyway (n = ",
                          length(inx), ", % = ",
                          round(length(inx) / nsmp, 3),
                          ")",
                          sep = "")

        inx <- which(smps[, 1] < boundary[1] &
                     smps[, 2] > boundary[2] &
                     smps[, 2] < alpha2)
        col2[inx] <- "Rejected: Now at IA"
        col[inx] <- paste("Rejected: Now at IA (n = ",
                          length(inx), ", % = ",
                          round(length(inx) / nsmp, 3),
                          ")",
                          sep = "")
    }

    inx       <- which(is.na(col))
    col2[inx] <- "Fail to Reject"
    col[inx]  <- paste("Fail to Reject (n = ",
                      length(inx), ", % = ",
                      round(length(inx) / nsmp, 3),
                      ")",
                      sep = "")

    dta <- data.frame(
        z1         = smps[, 1],
        z2         = smps[, 2],
        Rejection  = col2,
        Rejection2 = col)

})

get_design_plot_5 <- reactive({

    dta <- get_design_plot_data_5()
    if (is.null(dta)) {
        return(NULL)
    }

    design <- get_design_5()
    if (is.null(design)) {
        return(NULL)
    }

    n_ana      <- nrow(design)
    info_fracs <- design$info_frac[(n_ana - 1):n_ana]
    boundary   <- design$boundary_zscore[(n_ana - 1):n_ana]
    alpha      <- design$study_alpha[1]
    power      <- design$study_power[1]
    alpha2     <- alpha * 2
    boundary   <- 2 * (1 - pnorm(boundary))
    without_ia <- input$inRdoAna5

    if (!is.null(input$inChkbox5)) {
        dta <- dta %>%
            filter(Rejection2 %in% input$inChkbox5)
    }

    rst <- ggplot(data = dta, aes(x = z1, y = z2)) +
        geom_point(aes(color = Rejection)) +
        geom_hline(yintercept = alpha2, lty = 2, lwd = 2, col = "black") +
        xlim(0, input$inLim5) +
        ylim(0, input$inLim5) +
        labs(x = "Interim Analysis Alpha", y = "Final Analysis Alpha") +
        theme_bw() +
        theme(text = element_text(size = 20)) +
        scale_color_manual(values = c("Fail to Reject"         = "gray",
                                      "Rejected: Only With IA" = "green",
                                      "Rejected: Anyway"       = "brown",
                                      "Rejected: Now at IA"    = "purple",
                                      "Fail to Reject With IA" = "gray30",
                                      "Rejected at IA"         = "cyan",
                                      "Rejected at Final When No IA" = "yellow"
                                      ))


    if ("fia" == without_ia) {
        rst <- rst +
            geom_vline(xintercept = boundary[1],
                       lty = 2,
                       lwd = 2,
                       col = "red") +
            geom_hline(yintercept = boundary[2],
                       lty = 2,
                       lwd = 2,
                       col = "red")
    } else if ("ia" == without_ia) {
        rst <- rst +
            geom_vline(xintercept = boundary[1],
                       lty = 2,
                       lwd = 2,
                       col = "red")
    }

    rst
})

get_curve_plot_5 <- reactive({
    ia_alpha <- input$inIaAlpha5
    ia_frac  <- input$inIaIf5
    alpha    <- as.numeric(input$inAlpha5)

    if (is.null(ia_alpha) |
        is.null(ia_frac))
        return(NULL)

    rst <- NULL
    for (i in seq(ia_frac[1], ia_frac[2], 0.05)) {
        cur_rst <- simutb::stb_tl_gsd_solve(info_fracs = c(i, 1),
                                            boundary   = c(ia_alpha, NA),
                                            alpha      = alpha,
                                            power      = 0)
        rst <- rbind(rst,
                     c(i, cur_rst$nominal_alpha[2]))
    }

    dta <- data.frame(x    = rst[, 1],
                      y    = rst[, 2]) %>%
        mutate(text = paste("Information Fraction:", x,
                            "\n Nominal Alpha at IA:", ia_alpha,
                            "\n Nominal Alpha at FA:", round(y, 3)))

    ggplot(data = dta, aes(x = x, y = y, text = text)) +
        geom_point() +
        geom_bar(position = "stack",
                 stat     = "identity",
                 fill     = "brown",
                 alpha    = 0.3) +
        geom_line(aes(x = x, y = y), col = "black") +
        geom_hline(yintercept = alpha, lty = 2, col = "red") +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        ylim(0, alpha) +
        labs(x = "Information Fraction at Interim Analysis",
             y = "Nominal Alpha at Final Analysis")
})


## --------------demo 6: alpha spending functions--------------------------
get_design_6 <- reactive({

    info_fracs <- tkt_assign(input$inInterFrac6)
    boundary   <- tkt_assign(input$inCumuAlpha6) / 2
    alpha      <- as.numeric(input$inAlpha6) / 2
    power      <- as.numeric(input$inPower6)
    gamma      <- as.numeric(input$inKDgamma6)
    designs    <- input$chkDesigns6
    use_gsdes  <- input$inGsDes6

    n           <- as.numeric(input$inSample6)
    enroll_rate <- as.numeric(input$inEnrollRate6)
    min_fu      <- as.numeric(input$inMinFu6)
    all_design  <- userLog$all_design$design

    rst     <- NULL
    rst_out <- NULL
    for (i in designs) {
        cur_dsgn <- switch(i,
                           "asP"    = "Pocock Type",
                           "asOF"   = "O'Brien-Fleming Type",
                           "asKD"   = paste("Kim-Demets(gamma=",
                                           gamma,")", sep = ""),
                           "asUser" = "User Defined")

        cur_dsgn <- paste(cur_dsgn, "(",
                          paste(info_fracs, collapse = ","),
                          ")", sep = "")

        if (!is.null(all_design)) {
            if (cur_dsgn %in% all_design$design)
                next
        }

        cur_boundary <- boundary
        if ("asUser" != i)
            cur_boundary <- NULL

        if (("asKD" == i) & use_gsdes) {
            rpkg <- "gsDesign"
        } else {
            rpkg <- "rpact"
        }

        if ("rpact" == rpkg) {
            cur_rst  <- simutb::stb_tl_gsd_boundary(
                                    typeOfDesign   = i,
                                    info_fracs     = info_fracs,
                                    alpha          = alpha,
                                    power          = power,
                                    gammaA         = gamma,
                                    rpackage       = rpkg,
                                    alpha_spending = cur_boundary
                                )
        } else {
            cur_rst  <- simutb::stb_tl_gsd_boundary(
                                    typeOfDesign   = i,
                                    info_fracs     = info_fracs,
                                    alpha          = alpha,
                                    power          = power,
                                    sfupar         = gamma,
                                    rpackage       = rpkg,
                                    alpha_spending = cur_boundary
                                )
        }

        cur_out <- simutb::stb_tl_gsd_outcome(cur_rst$info_frac,
                                              cur_rst$ia_power,
                                              n           = n,
                                              enroll_rate = enroll_rate,
                                              min_fu      = min_fu)
        ## append
        rst     <- rbind(rst,
                         cbind(design = cur_dsgn, cur_rst))

        rst_out <- rbind(rst_out,
                         data.frame(design              = cur_dsgn,
                                    expected_duration   = cur_out[1],
                                    expected_samplesize = cur_out[2],
                                    tot_duration        = cur_out[3],
                                    tot_sample          = cur_out[4],
                                    saved_duration      = cur_out[5] * 100,
                                    saved_sample        = cur_out[6] * 100,
                                    power               = cur_out[7]))
    }

    if (!is.null(rst)) {
        rst <- rst %>%
            mutate(nominal_alpha    = 2 * nominal_alpha,
                   ia_alpha_spent   = 2 * ia_alpha_spent,
                   cumu_alpha_spent = 2 * cumu_alpha_spent)
    }


    if (!is.null(rst_out)) {
        tot_sample   <- rst_out$tot_sample[1]
        tot_duration <- rst_out$tot_duration[1]
        rst_out      <- rst_out %>%
            select(-tot_sample, -tot_duration)

        rst_out     <- rbind(data.frame(design = "No Interim Analysis",
                             expected_duration   = tot_duration,
                             expected_samplesize = tot_sample,
                             saved_duration      = 0,
                             saved_sample        = 0,
                             power               = power),
                             rst_out)
    }

    list(design  = rst,
         outcome = rst_out)
})

get_design_plot_6 <- reactive({
    dta_design <- userLog$all_design$design
    if (is.null(dta_design))
        return(NULL)

    y_lab <- input$inRdo6
    y_col <- switch(y_lab,
                    "Nominal Alpha Spent"    = "nominal_alpha",
                    "Cumulative Alpha Spent" = "cumu_alpha_spent",
                    "Cumulative Power"       = "cumu_power",
                    "Z-score Boundary"       = "boundary_zscore",
                    "MDD"                    = "mdd_percent")

    dta_design   <- dta_design %>%
        rename(Design = design)

    dta_design$Y <- dta_design[[y_col]]
    ggplot(data = dta_design,
           aes(x = info_frac,
               y = Y,
               text = paste(
                   "Design:", Design,
                   "\n Information Fraction:", info_frac,
                   "\n", y_lab, ":", round(Y, 3)))) +
        geom_line(aes(group = Design, col = Design),
                  lwd = 1.5) +
        geom_point(aes(group = Design)) +
        labs(x = "Interim Analysis (Information Fraction)",
             y = y_lab) +
        theme_bw() +
        theme(text = element_text(size = 15))
})


## clear all the designs
observeEvent(input$btnReset6, {
    userLog$all_design <- list()
})

observeEvent(input$btnAdd6, {

    cur_design <- rbind(userLog$all_design$design,
                        get_design_6()$design)

    cur_outcome <- rbind(userLog$all_design$outcome,
                         get_design_6()$outcome) %>%
        distinct()

    userLog$all_design <- list(design  = cur_design,
                               outcome =  cur_outcome)
})
